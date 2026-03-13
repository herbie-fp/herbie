#lang racket

(require racket/format
         racket/list
         racket/runtime-path
         racket/set
         racket/string
         "../../src/core/points.rkt"
         "../../src/core/sampling.rkt"
         "../../src/syntax/batch.rkt"
         "../../src/syntax/load-platform.rkt"
         "../../src/syntax/platform.rkt"
         "../../src/syntax/sugar.rkt"
         "../../src/syntax/types.rkt"
         "../../src/utils/common.rkt")

(define-runtime-path default-output-path "accelerator-table.tex")

(struct accelerator (impl-name name vars spec cost) #:transparent)

;; Fill this in manually as needed.
(define accelerator->origin
  (hash
   'sinprod "PROJ"
   'cosprod "PROJ"
   'cosquot "RPOJ"
   'sinquot "PROJ"
   'log1pmd "PROJ"
   'logfabs1p "PROJ"
   'invgud "PROJ"
   'hypot "PROJ"
   'verdcos "PROJ"
   'powcos2 "BASILISK"
   'powcos4 "BASILISK"
   'powcos6 "BASILISK"
   'powcos "BASILISK"
   'cosn1 "BASILISK"
   'pow1ms "BASILISK"
   'pow1ms "COOLPROP"
   'powquot2 "COOLPROP"
   'pown2o3 "COOLPROP"
   'pow2o5 "COOLPROP"
   'pow3o5 "COOLPROP"
   'pow5o3 "COOLPROP"
   'pown16o5 "COOLPROP"))

(define (impl-fpcore-name impl-name)
  (match (impl-info impl-name 'fpcore)
    [`(! ,_props ... (,name ,_args ...)) name]
    [`(,name ,_args ...) name]
    [(? symbol? name) name]
    [_ impl-name]))

(define (growlibm-only-impls)
  (activate-platform! "vanilla")
  (define vanilla-impls (list->seteq (platform-impls (*active-platform*))))
  (activate-platform! "growlibm")
  (filter (lambda (impl) (not (set-member? vanilla-impls impl)))
          (platform-impls (*active-platform*))))

(define (collect-accelerators)
  (sort
   (for/list ([impl-name (in-list (growlibm-only-impls))])
     (accelerator impl-name
                  (impl-fpcore-name impl-name)
                  (impl-info impl-name 'vars)
                  (impl-info impl-name 'spec)
                  (impl-info impl-name 'cost)))
   symbol<?
   #:key accelerator-name))

(define (escape-latex s)
  (define out (open-output-string))
  (for ([ch (in-string s)])
    (display
     (case ch
       [(#\\) "\\textbackslash{}"]
       [(#\{) "\\{"]
       [(#\}) "\\}"]
       [(#\$) "\\$"]
       [(#\&) "\\&"]
       [(#\#) "\\#"]
       [(#\_) "\\_"]
       [(#\%) "\\%"]
       [(#\~) "\\textasciitilde{}"]
       [(#\^) "\\textasciicircum{}"]
       [else (string ch)])
     out))
  (get-output-string out))

(define (score->accuracy score repr)
  (define bits (representation-total-bits repr))
  (~a (~r (- 100 (* 100 (/ score bits))) #:precision '(= 2)) "%"))

(define (format-cost cost)
  (if (real? cost)
      (~r cost #:precision '(= 3))
      (~a cost)))

(define (make-pcontext spec-prog ctx)
  (define-values (batch brfs) (progs->batch (list (prog->spec spec-prog))))
  (apply mk-pcontext (sample-points '(TRUE) batch brfs (list ctx))))

;; Return average error in bits for an expression over a given pcontext.
(define (get-error expr pcontext ctx)
  (errors-score (errors expr pcontext ctx)))

(define (compute-row acc)
  (define repr (get-representation 'binary64))
  (define ctx (context (accelerator-vars acc)
                       repr
                       (make-list (length (accelerator-vars acc)) repr)))
  (define spec-prog (fpcore->prog (accelerator-spec acc) ctx))
  (define accelerator-prog (cons (accelerator-impl-name acc) (accelerator-vars acc)))
  (define cost-proc (platform-cost-proc (*active-platform*)))
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (eprintf "Warning: failed to compute metrics for ~a: ~a\n"
                              (accelerator-name acc)
                              (exn-message exn))
                     (hash 'name (symbol->string (accelerator-name acc))
                           'origin (hash-ref accelerator->origin (accelerator-name acc) "")
                           'spec (~s (accelerator-spec acc))
                           'cost (format-cost (accelerator-cost acc))
                           'accuracy "n/a"
                           'spec-cost "n/a"
                           'spec-accuracy "n/a"))])
    (define pcontext (make-pcontext spec-prog ctx))
    (define accelerator-error (get-error accelerator-prog pcontext ctx))
    (define spec-error (get-error spec-prog pcontext ctx))
    (hash 'name (symbol->string (accelerator-name acc))
          'origin (hash-ref accelerator->origin (accelerator-name acc) "")
          'spec (~s (accelerator-spec acc))
          'cost (format-cost (accelerator-cost acc))
          'accuracy (score->accuracy accelerator-error repr)
          'spec-cost (format-cost (cost-proc spec-prog repr))
          'spec-accuracy (score->accuracy spec-error repr))))

(define (render-rows rows)
  (string-join
   (for/list ([row (in-list rows)])
     (format
      "~a & ~a & \\texttt{~a} & ~a & ~a & ~a & ~a \\\\"
      (escape-latex (hash-ref row 'name))
      (escape-latex (hash-ref row 'origin))
      (escape-latex (hash-ref row 'spec))
      (escape-latex (hash-ref row 'cost))
      (escape-latex (hash-ref row 'accuracy))
      (escape-latex (hash-ref row 'spec-cost))
      (escape-latex (hash-ref row 'spec-accuracy))))
   "\n"))

(define (render-latex rows)
  (format
   (string-append
    "\\documentclass{article}\n"
    "\\usepackage[T1]{fontenc}\n"
    "\\usepackage[margin=1in]{geometry}\n"
    "\\begin{document}\n"
    "\\section*{growlibm accelerators}\n"
    "\\begin{tabular}{lllllll}\n"
    "\\hline\n"
    "name & origin & spec & cost & accuracy & cost of spec & accuracy of spec \\\\\n"
    "\\hline\n"
    "~a\n"
    "\\hline\n"
    "\\end{tabular}\n"
    "\\end{document}\n")
   (render-rows rows)))

(define output-path (make-parameter (path->string default-output-path)))
(define seed (make-parameter 1))

(command-line
 #:program "generate-table.rkt"
 #:once-each
 [("--seed") n "Random seed for sampling points."
  (define parsed (string->number n))
  (unless (and parsed (exact-integer? parsed))
    (error 'generate-table.rkt "Invalid --seed value: ~a" n))
  (seed parsed)]
 [("--output") path "Output LaTeX path."
  (output-path path)])

(define accelerators
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (error 'generate-table.rkt
                            (string-append
                             "Failed to activate vanilla/growlibm platform. "
                             "Run `make compile-accelerators` first.\n"
                             (exn-message exn))))])
    (collect-accelerators)))

(set-seed! (seed))

(define (row<? row-a row-b)
  (define origin-a (hash-ref row-a 'origin ""))
  (define origin-b (hash-ref row-b 'origin ""))
  (if (string=? origin-a origin-b)
      (string<? (hash-ref row-a 'name "") (hash-ref row-b 'name ""))
      (string<? origin-a origin-b)))

(define rows
  (sort
   (for/list ([acc (in-list accelerators)])
     (compute-row acc))
   row<?))

(call-with-output-file (output-path)
  (lambda (out)
    (display (render-latex rows) out))
  #:exists 'replace)

(printf "Wrote ~a with ~a accelerators.\n" (output-path) (length rows))
