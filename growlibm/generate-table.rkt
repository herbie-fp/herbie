#lang racket

(require racket/format
         racket/list
         racket/runtime-path
         racket/string
         "../src/core/points.rkt"
         "../src/core/sampling.rkt"
         "../src/syntax/batch.rkt"
         "../src/syntax/load-platform.rkt"
         "../src/syntax/platform.rkt"
         "../src/syntax/sugar.rkt"
         "../src/syntax/types.rkt"
         "../src/utils/common.rkt")

(define-runtime-path growlibm-platform-path "platforms/growlibm.rkt")
(define-runtime-path default-output-path "accelerator-table.html")

(struct accelerator (impl-name name vars spec cost) #:transparent)

;; Fill this in manually as needed.
(define accelerator->origin
  (hash
   'sinprod "PROJ"))

(define (keyword-ref fields kw)
  (let loop ([fields fields])
    (match fields
      ['() #f]
      [(list key val rest ...)
       (if (eq? key kw)
           val
           (loop rest))]
      [_ #f])))

(define (parse-accelerator-form form)
  (match form
    [`(define-operation (,impl-name ,args ...) ,_otype ,fields ...)
     (define impl-expr (keyword-ref fields '#:impl))
     (match impl-expr
       [`(from-accelerators (quote ,name))
        (define vars
          (for/list ([arg (in-list args)])
            (match arg
              [`(,var ,_repr) var]
              [_ (error 'generate-table.rkt "Malformed argument ~a in ~a" arg impl-name)])))
        (accelerator impl-name
                     name
                     vars
                     (keyword-ref fields '#:spec)
                     (keyword-ref fields '#:cost))]
       [_ #f])]
    [_ #f]))

(define (parse-accelerators platform-path)
  (define stx
    (parameterize ([read-accept-reader #t])
      (call-with-input-file platform-path
        (lambda (in)
          (read-syntax platform-path in)))))
  (define module-datum (syntax->datum stx))
  (define module-body (cadddr module-datum))
  (unless (and (list? module-body)
               (pair? module-body)
               (eq? (car module-body) '#%module-begin))
    (error 'generate-table.rkt "Unexpected module body in ~a" platform-path))
  (sort (filter-map parse-accelerator-form (cdr module-body))
        symbol<?
        #:key accelerator-name))

(define (escape-html s)
  (define s1 (string-replace s "&" "&amp;"))
  (define s2 (string-replace s1 "<" "&lt;"))
  (define s3 (string-replace s2 ">" "&gt;"))
  (define s4 (string-replace s3 "\"" "&quot;"))
  s4)

(define (score->accuracy score repr)
  (define bits (representation-total-bits repr))
  (~a (~r (- 100 (* 100 (/ score bits))) #:precision '(= 2)) "%"))

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
                           'cost (~a (accelerator-cost acc))
                           'accuracy "n/a"
                           'spec-cost "n/a"
                           'spec-accuracy "n/a"))])
    (define pcontext (make-pcontext spec-prog ctx))
    (define accelerator-error (get-error accelerator-prog pcontext ctx))
    (define spec-error (get-error spec-prog pcontext ctx))
    (hash 'name (symbol->string (accelerator-name acc))
          'origin (hash-ref accelerator->origin (accelerator-name acc) "")
          'spec (~s (accelerator-spec acc))
          'cost (~a (accelerator-cost acc))
          'accuracy (score->accuracy accelerator-error repr)
          'spec-cost (~a (cost-proc spec-prog repr))
          'spec-accuracy (score->accuracy spec-error repr))))

(define (render-rows rows)
  (string-join
   (for/list ([row (in-list rows)])
     (format
      "<tr><td>~a</td><td>~a</td><td><code>~a</code></td><td>~a</td><td>~a</td><td>~a</td><td>~a</td></tr>"
      (escape-html (hash-ref row 'name))
      (escape-html (hash-ref row 'origin))
      (escape-html (hash-ref row 'spec))
      (escape-html (hash-ref row 'cost))
      (escape-html (hash-ref row 'accuracy))
      (escape-html (hash-ref row 'spec-cost))
      (escape-html (hash-ref row 'spec-accuracy))))
   "\n"))

(define (render-html rows)
  (format
   (string-append
    "<!DOCTYPE html>\n"
    "<html lang=\"en\">\n"
    "<head>\n"
    "  <meta charset=\"UTF-8\">\n"
    "  <title>growlibm Accelerators</title>\n"
    "</head>\n"
    "<body>\n"
    "  <h1>growlibm accelerators</h1>\n"
    "  <table border=\"1\" cellspacing=\"0\" cellpadding=\"6\">\n"
    "    <thead>\n"
    "      <tr>\n"
    "        <th>name</th>\n"
    "        <th>origin</th>\n"
    "        <th>spec</th>\n"
    "        <th>cost</th>\n"
    "        <th>accuracy</th>\n"
    "        <th>cost of spec</th>\n"
    "        <th>accuracy of spec</th>\n"
    "      </tr>\n"
    "    </thead>\n"
    "    <tbody>\n"
    "~a\n"
    "    </tbody>\n"
    "  </table>\n"
    "</body>\n"
    "</html>\n")
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
 [("--output") path "Output HTML path."
  (output-path path)])

(with-handlers ([exn:fail?
                 (lambda (exn)
                   (error 'generate-table.rkt
                          (string-append
                           "Failed to activate growlibm platform. "
                           "Run `make compile-accelerators` first.\n"
                           (exn-message exn))))])
  (activate-platform! "growlibm"))

(set-seed! (seed))
(define rows (for/list ([acc (in-list (parse-accelerators growlibm-platform-path))])
               (compute-row acc)))

(call-with-output-file (output-path)
  (lambda (out)
    (display (render-html rows) out))
  #:exists 'replace)

(printf "Wrote ~a with ~a accelerators.\n" (output-path) (length rows))
