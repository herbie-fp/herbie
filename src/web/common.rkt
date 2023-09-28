#lang racket
(require (only-in xml write-xexpr xexpr?) 
         (only-in fpbench fpcore? supported-by-lang?
                          core->c core->fortran core->java core->python
                          core->julia core->matlab core->wls core->tex
                          expr->tex
                          [core-common-subexpr-elim core-cse]
                          *expr-cse-able?*))

(require "../common.rkt" "../syntax/read.rkt" "../programs.rkt"
         "../syntax/types.rkt" "../syntax/sugar.rkt")

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

(define languages
  `(("FPCore" "fpcore" ,(λ (c i) (fpcore->string c)))
    ("C" "c" ,core->c)
    ("Fortran" "f03" ,core->fortran)
    ("Java" "java" ,core->java)
    ("Python" "py" ,core->python)
    ("Julia" "jl" ,core->julia)
    ("MATLAB" "mat" ,core->matlab)
    ("Wolfram" "wl" ,core->wls)
    ("TeX" "tex" ,(λ (c i) (core->tex c)))))

(define (program->tex prog ctx #:loc [loc #f])
  (define prog* (program->fpcore prog ctx))
  (if (supported-by-lang? prog* "tex")
      (core->tex prog* #:loc (and loc (cons 2 loc)) #:color "blue")
      "ERROR"))

(define (fpcore-instruction? i)
  (match (first i)
    [(or 'abs 'negabs) #t]
    ['sort #f]))

(define (combine-fpcore-instruction i e)
  (match i
    [(list 'abs x) (combine-abs x e)]
    [(list 'negabs x) (combine-negabs x e)]))

(define (combine-abs x e)
  (define x* (string->symbol (string-append (symbol->string x) "*")))
  (define e* (replace-vars (list (cons x x*)) e))
  `(let* ([,x* (abs ,x)])
     ,e))

(define (combine-negabs x e)
  (define x-string (symbol->string x))
  (define x-sign (string->symbol (string-append x-string "-sign")))
  (define x* (string->symbol (string-append x-string "*")))
  (define e* (replace-vars (list (cons x x*)) e))
  `(let* ([,x-sign `(copysign 1 ,x)]
          [,x* `(abs ,x)])
     (* ,x-sign ,e*)))

(define (format-prelude-instruction l i)
  (match (cons i l)
    [(cons (list 'sort vs ...) "C")
     (format "assert(~a);" (format-less-than-condition vs))]
    [(cons (list 'sort vs ...) "Java")
     (format "assert ~a;" (format-less-than-condition vs))]
    [(cons (list 'sort vs ...) "Python")
     (define comma-joined (comma-join vs))
     (format "[~a] = sort([~a])" comma-joined comma-joined)]
    [(cons (list 'sort vs ...) "Julia")
      (define comma-joined (comma-join vs))
      (format "~a = sort([~a])" comma-joined comma-joined)]
    [(cons (list 'sort vs ...) "MATLAB")
     (define comma-joined (comma-join vs))
     (format "~a = num2cell(sort([~a])){:}" comma-joined comma-joined)]
    [(cons (list 'sort vs ...) "TeX")
     (define comma-joined (comma-join vs))
     (format "[~a] = \\mathsf{sort}([~a])\\\\" comma-joined comma-joined)]
    [(cons (list 'sort x y) _) (format sort-note (format "~a and ~a" x y))]
    [(cons (list 'sort vs ...) _)
     (format
      sort-note
      (string-join (map ~a vs) ", "
                   ;; "Lil Jon, he always tells the truth"
                   #:before-last ", and "))]
    [_ #f]))

(define (format-less-than-condition variables)
  (string-join
   (for/list ([a (in-list variables)]
              [b (in-list (cdr variables))])
     (format "~a < ~a" a b))
   " && "))

(define (comma-join vs)
  (string-join (map ~a vs) ", "))

(define sort-note
  "NOTE: ~a should be sorted in increasing order before calling this function.")

(define (render-program expr ctx #:ident [identifier #f] #:pre [precondition '(TRUE)] #:instructions [instructions empty])
  (define output-repr (context-repr ctx))
  (define-values (fpcore-instructions prelude-instructions) (partition fpcore-instruction? instructions))
  (define out-prog
    (parameterize ([*expr-cse-able?* at-least-two-ops?])
      (define expr* (foldl combine-fpcore-instruction expr fpcore-instructions))
      (core-cse (program->fpcore expr* ctx #:ident identifier))))

  (define output-prec (representation-name output-repr))
  (define out-prog* (fpcore-add-props out-prog (list ':precision output-prec)))

  (define versions
    (reap [sow]
      (for ([(lang record) (in-dict languages)])
        (match-define (list ext converter) record)
        (when (and (fpcore? out-prog*)
                   (or (equal? ext "fpcore") (supported-by-lang? out-prog* ext)))
          (define name (if identifier (symbol->string identifier) "code"))
          (define out (converter out-prog* name))
          (define prelude-lines
            (string-join
             (filter-map (curry format-prelude-instruction lang) prelude-instructions)
             "\n" #:after-last "\n"))
          (sow
           (cons lang
                 ((if (equal? lang "TeX")
                      (curry format "\\begin{array}{l}\n~a\\\\\n~a\\end{array}\n")
                      string-append)
                  prelude-lines out)))))))

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
             
