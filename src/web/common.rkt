#lang racket
(require (only-in xml write-xexpr xexpr?) 
         (only-in fpbench fpcore? supported-by-lang?
                          core->c core->fortran core->java core->python
                          core->julia core->matlab core->wls core->tex
                          expr->tex
                          [core-common-subexpr-elim core-cse]
                          *expr-cse-able?*))

(require "../common.rkt" "../syntax/read.rkt" "../programs.rkt"
         "../preprocess.rkt" "../syntax/types.rkt" "../syntax/sugar.rkt")

(provide render-menu render-warnings render-large render-comparison render-program
         render-bogosity render-help
         format-percent
         program->fpcore program->tex render-reproduction js-tex-include)

(define (program->fpcore expr ctx #:ident [ident #f])
  (define body (resugar-program expr (context-repr ctx) #:full #t))
  (if ident
      (list 'FPCore ident (context-vars ctx) body)
      (list 'FPCore (context-vars ctx) body)))

(define (fpcore-add-props core props)
  (match core
   [(list 'FPCore name args expr) `(FPCore ,name ,args ,@props ,expr)]
   [(list 'FPCore args expr) `(FPCore ,args ,@props ,expr)]))

(define (at-least-two-ops? expr)
  (match expr
   [(list op args ...) (ormap list? args)]
   [_ #f]))

(define (fpcore->string core)
  (define-values (ident args props expr)
    (match core
     [(list 'FPCore name (list args ...) props ... expr) (values name args props expr)]
     [(list 'FPCore (list args ...) props ... expr) (values #f args props expr)]))
  (define props*  ; make sure each property (name, value) gets put on the same line
    (for/list ([(prop name) (in-dict (apply dict-set* '() props))]) ; how to make a list of pairs from a list
      (format "~a ~a" prop name)))
  (define top (if ident (format "FPCore ~a ~a" ident args) (format "FPCore ~a" args)))
  (pretty-format `(,top ,@props* ,expr) #:mode 'display))

(define/contract (render-menu #:path [path "."] name links)
  (->* (string? (listof (cons/c string? string?)))
       (#:path string?)
       xexpr?)
  `(header
    (h1 ,name)
    (img ([src ,(string-append path "/" "logo-car.png")]))
    (nav
     (ul
      ,@(for/list ([(text url) (in-dict (filter identity links))])
         `(li (a ([href ,url]) ,text)))))))

(define/contract (render-warnings warnings)
  (-> (listof (list/c symbol? string? (listof any/c) (or/c string? #f) (listof string?))) xexpr?)
  (if (null? warnings)
      ""
      `(ul ([class "warnings"])
           ,@(for/list ([warning warnings])
               (match-define (list type message args url extra) warning)
               `(li (h2 ,(apply format message args)
                        ,(if url `(a ([href ,url]) " (more)") ""))
                    ,(if (null? extra)
                         ""
                         `(ol ([class "extra"])
                              ,@(for/list ([line extra])
                                  `(li ,line)))))))))

(define (render-large #:title [title #f] name . values)
  `(div ,name ": " (span ([class "number"]
                          ,@(if title `([title ,title]) '()))
                         ,@values)))

(define (render-comparison #:title [title #f] name a b )
  (render-large #:title title name a `(span ([class "unit"]) " → ") b))

(define (format-less-than-condition variables)
  (string-join
   (for/list ([a (in-list variables)]
              [b (in-list (cdr variables))])
     (format "~a < ~a" a b))
   " && "))

(define (preprocess->c preprocess)
  (match preprocess
    [(cons 'sort variables)
     (format "assert(~a);" (format-less-than-condition variables))]
    [(cons 'abs x)
     (format "~a = abs(~a);" x x)]))

(define (preprocess->java preprocess)
  (match preprocess
    [(cons 'sort variables)
     (format "assert ~a;" (format-less-than-condition variables))]
    [(cons 'abs x)
     (format "~a = Math.abs(~a);" x x)]))

(define (preprocess->python preprocess)
  (match preprocess
    [(cons 'sort variables)
     (define comma-joined (string-join (map ~a variables) ", "))
     (format "[~a] = sort([~a])" comma-joined comma-joined)]
    [(cons 'abs x)
     (format "~a = abs(~a)" x x)]))

(define (preprocess->julia preprocess)
  (match preprocess
    [(cons 'sort variables)
      (define comma-joined (string-join (map ~a variables) ", "))
      (format "~a = sort([~a])" comma-joined comma-joined)]
    [(cons 'abs x)
     (format "~a = abs(~a)" x x)]))

(define (preprocess->matlab preprocess)
  (match preprocess
    [(cons 'sort variables)
     (define comma-joined (string-join (map ~a variables) ", "))
     (format "~a = num2cell(sort([~a])){:}" comma-joined comma-joined)]
    [(cons 'abs x)
     (format "~a = abs(~a)" x x)]))

(define (preprocess->tex preprocess)
  (match preprocess
    [(cons 'sort variables)
     (define comma-joined (string-join (map ~a variables) ", "))
     (format "[~a] = \\mathsf{sort}([~a])\\\\" comma-joined comma-joined)]
    [(cons 'abs x)
     (format "~a = |~a|\\\\" x x)]))

(define (preprocess->default preprocess)
  (match preprocess
    [(cons 'sort (list a b))
     (format sort-note (format "~a and ~a" a b))]
    [(cons 'sort variables)
     (format
      sort-note
      (string-join (map ~a variables) ", " #:before-last ", and "))]
    [(cons 'abs x)
     (format abs-note x)]))

(define sort-note
  "NOTE: ~a should be sorted in increasing order before calling this function.")

(define abs-note "NOTE: ~a should be positive before calling this function")

(define languages
  `(("FPCore" "fpcore" ,(λ (c i) (fpcore->string c)) ,preprocess->default)
    ("C" "c" ,core->c ,preprocess->c)
    ("Fortran" "f03" ,core->fortran ,preprocess->default)
    ("Java" "java" ,core->java ,preprocess->java)
    ("Python" "py" ,core->python ,preprocess->python)
    ("Julia" "jl" ,core->julia ,preprocess->julia)
    ("MATLAB" "mat" ,core->matlab ,preprocess->matlab)
    ("Wolfram" "wl" ,core->wls ,preprocess->default)
    ("TeX" "tex" ,(λ (c i) (core->tex c)) ,preprocess->tex)))

(define (program->tex prog ctx #:loc [loc #f])
  (define prog* (program->fpcore prog ctx))
  (if (supported-by-lang? prog* "tex")
      (core->tex prog* #:loc (and loc (cons 2 loc)) #:color "blue")
      "ERROR"))

(define (add-preprocessing-tex preprocess-lines output)
  (format "\\begin{array}{l}\n~a\\\\\n~a\\end{array}\n"
          preprocess-lines output))

(define (render-program preprocesses expr ctx #:ident [identifier #f] #:pre [precondition '(TRUE)])
  (define output-repr (context-repr ctx))
  (define out-prog
    (parameterize ([*expr-cse-able?* at-least-two-ops?])
      (core-cse (program->fpcore expr ctx #:ident identifier))))

  (define output-prec (representation-name output-repr))
  (define out-prog* (fpcore-add-props out-prog (list ':precision output-prec)))

  (define versions
    (reap [sow]
      (for ([(lang record) (in-dict languages)])
        (match-define (list ext converter preprocess) record)
        (when (and (fpcore? out-prog*)
                   (or (equal? ext "fpcore") (supported-by-lang? out-prog* ext)))
          (define name (if identifier (symbol->string identifier) "code"))
          (define out (converter out-prog* name))
          (define preprocess-lines
            (string-join (map preprocess preprocesses) "\n" #:after-last "\n"))
          (define add-preprocessing
            (if (equal? lang "TeX") add-preprocessing-tex string-append))
          (sow (cons lang (add-preprocessing preprocess-lines out)))))))

  (define math-out
    (if (dict-has-key? versions "TeX")
        (let ([val (dict-ref versions "TeX")])
          val)
        ""))

  (define dropdown
    `(select
      (option "Math")
      ,@(for/list ([lang (in-dict-keys versions)])
          `(option ,lang))))

  (define body
    `(div
      ,(if (equal? precondition '(TRUE))
           ""
           `(div ([id "precondition"])
                 (div ([class "program math"])
                      "\\[" ,(expr->tex (resugar-program precondition output-repr)) "\\]")))
      (div ([class "implementation"] [data-language "Math"])
           (div ([class "program math"]) "\\[" ,math-out "\\]"))
      ,@(for/list ([(lang out) (in-dict versions)])
          `(div ([class "implementation"] [data-language ,lang])
                (pre ([class "program"]) ,out)))))
  
  (values dropdown body))

(define/contract (render-command-line)
  (-> string?)
  (format
   "herbie shell --seed ~a ~a"
   (if (vector? (get-seed)) (format "'~a'" (get-seed)) (get-seed))
   (string-join
    (for/list ([rec (changed-flags)])
      (match rec
        [(list 'enabled class flag) (format "+o ~a:~a" class flag)]
        [(list 'disabled class flag) (format "-o ~a:~a" class flag)]))
    " ")))

(define/contract (render-fpcore test)
  (-> test? string?)
  (define output-repr (test-output-repr test))
  (string-join
   (filter
    identity
    (list
     (if (test-identifier test)
         (format "(FPCore ~a ~a" (test-identifier test) (test-vars test))
         (format "(FPCore ~a" (test-vars test)))
     (format "  :name ~s" (test-name test))
     (format "  :precision ~s" (representation-name (test-output-repr test)))
     (if (equal? (test-pre test) '(TRUE))
         #f
         (format "  :pre ~a" (resugar-program (test-pre test) output-repr)))
     (if (equal? (test-expected test) #t)
         #f
         (format "  :herbie-expected ~a" (test-expected test)))
     (if (test-output test)
         ;; Extra newlines for clarity
         (format "\n  :herbie-target\n  ~a\n" (resugar-program (test-output test) output-repr))
         #f)
     (format "  ~a)" (resugar-program (test-input test) output-repr))))
   "\n"))

(define (format-percent num den)
  (string-append
   (if (zero? den)
       (cond [(positive? num) "+∞"] [(zero? num) "0"] [(negative? num) "-∞"])
       (~r (* (/ num den) 100) #:precision 1))
   "%"))

(define (render-bogosity domain-info)
  (define total (round (apply + (hash-values domain-info))))
  (define tags '(valid unknown infinite unsamplable invalid precondition))
  `(div ([class "bogosity"])
     ,@(for/list ([tag tags])
         (define pct (format-percent (hash-ref domain-info tag 0) total))
         `(div
           ([class ,(format "bogosity-~a" tag)]
            [data-id ,(format "bogosity-~a" tag)]
            [data-type ,(~a tag)]
            [data-timespan ,(~a (hash-ref domain-info tag 0))]
            [title ,(format "~a (~a)" tag pct)])))))

(define/contract (render-reproduction test #:bug? [bug? #f])
  (->* (test?) (#:bug? boolean?) xexpr?)

  `(section ([id "reproduce"])
    (details ,(if bug? '([open "open"]) "")
     (summary
      (h2 "Reproduce")
      (a ([class "help-button float"] 
          [href "/doc/latest/report.html#reproduction"] 
          [target "_blank"]) "?"))
     (pre ((class "shell"))
          (code
           ,(render-command-line) "\n"
           ,(render-fpcore test) "\n"))
     ,(if bug?
          `(p "Please file a "
              (a ((href "https://github.com/herbie-fp/herbie/issues")) "bug report")
              " with this information.")
          ""))))

(define (render-help url #:float [float? #t])
  `(a ([class ,(if float? "help-button float" "help-button")] 
       [href ,(format "/doc/latest/~a" url)] 
       [target "_blank"]) "?"))

(define js-tex-include
  '((link ([rel "stylesheet"] [href "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css"]
           [integrity "sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH"]
           [crossorigin "anonymous"]))
    (script ([src "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js"]
             [integrity "sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm"]
             [crossorigin "anonymous"]))
    (script ([src "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/contrib/auto-render.min.js"]
             [integrity "sha384-aGfk5kvhIq5x1x5YdvCp4upKZYnA8ckafviDpmWEKp4afOZEqOli7gqSnh8I6enH"]
             [crossorigin "anonymous"]))))
             
