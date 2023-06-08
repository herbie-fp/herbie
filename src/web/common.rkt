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

(provide render-menu render-warnings render-large render-program
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

(define/contract (render-menu sections links)
  (-> (listof (or/c (cons/c string? string?) #f)) (listof (cons/c string? string?)) xexpr?)
  `(nav ([id "links"])
    (div ([class "right"])
     ,@(for/list ([(text url) (in-dict (filter identity links))])
         `(a ([href ,url]) ,text)))
    (div
     ,@(for/list ([(text url) (in-dict (filter identity sections))])
         `(a ([href ,url]) ,text)))))

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
  
(define (preprocess-sort-core output sort-preprocesses)
  (string-append
   ";; Ensure these are sorted, for example in Racket, do\n"
   (apply
    string-append
    (for/list ([variables sort-preprocesses])
      (format "~a\n" `(match-define (list ,@variables) (sort ,variables <)))))
   "\n"
   output))

(define (preprocess-sort-c-like output sort-preprocesses #:assert-has-parentheses [assert-has-parentheses #f])
  ;; Stolen from https://github.com/codereport/racket-algorithms/blob/master/main.rkt#L88-L96
  (define (sliding lst size [step 1])
    (define (tail-call lst)
      (if (>= size (length lst))
          (list lst)
          (cons (take lst size)
                (tail-call (drop lst step)))))
    (if (>= step (length lst))
        (error "step has to be smaller then length of the list")
        (tail-call lst)))
  (string-append
   (apply
    string-append
    "// Ensure these are sorted\n" 
    (for/list ([variables sort-preprocesses])
      ;; TODO: Is the ordering right here?
      (define comparisons
        (map
         (match-lambda [(list a b) (format "~a < ~a" a b)])
         (sliding variables 2)))
      (format
       (if assert-has-parentheses
           "assert(~a);\n"
           "assert ~a;\n")
       (string-join comparisons " && "))))
   "\n"
   output))

(define (preprocess-sort-c output sort-preprocesses)
  (preprocess-sort-c-like output sort-preprocesses #:assert-has-parentheses #t))

;; (define (preprocess-sort-fortran output sort-preprocesses)
;;   (string-append
;;    (apply
;;     string-append
;;     (for/list ([variables sort-preprocesses])
;;       (format "// Reassign `~a` by sorting them\n" (string-join (map ~a variables) ", "))))
;;    output))A

(define (preprocess-sort-java output sort-preprocesses)
  (preprocess-sort-c-like output sort-preprocesses #:assert-has-parentheses #f))
  

(define (preprocess-sort-python output sort-preprocesses)
  (string-append
   (apply
    string-append
    (for/list ([variables sort-preprocesses])
      (define lst (format "[~a]" (string-join (map ~a variables) ", ")))
      (format "~a = sort(~a)\n" lst lst)))
   "\n"
   output))

(define (preprocess-sort-julia output sort-preprocesses)
  (string-append
   (apply
    string-append
    (for/list ([variables sort-preprocesses])
      (define list-without-brackets (string-join (map ~a variables) ", "))
      (define list-with-brackets (format "[~a]" list-without-brackets))
      (format "~a = sort(~a)\n" list-without-brackets list-with-brackets)))
   "\n"
   output))

(define (preprocess-sort-matlab output sort-preprocesses)
  (string-append
   (apply
    string-append
    (for/list ([variables sort-preprocesses])
      (define list-without-brackets (string-join (map ~a variables) ", "))
      (define list-with-brackets (format "[~a]" list-without-brackets))
      (format "~a = num2cell(sort(~a)){:}\n" list-without-brackets list-with-brackets)))
   "\n"
   output))

(define (preprocess-sort-tex output sort-preprocesses)
  (string-append
   "\\begin{array}{l}\n"
   (string-join
    (for/list ([variables sort-preprocesses])
      (define lst (format "[~a]" (string-join (map ~a variables) ", ")))
      (format "~a = \\mathsf{sort}(~a)" lst lst))
    " \\\\\n")
   "\\\\\n"
   "\\end{array}\n"
   output))

(define (preprocess-sort-default output sort-preprocesses)
  (string-append
   (apply
    string-append
    (for/list ([variables sort-preprocesses])
      (format
       "NOTE: ~a should be sorted in increasing order before calling this function.\n"
       (match variables
         [(list a b) (format "~a and ~a" a b)]
         [_ (string-join (map ~a variables) ", " #:before-last ", and ")]))))
   "\n"
   output))

(define languages
  `(("FPCore" "fpcore" ,(λ (c i) (fpcore->string c)) ,preprocess-sort-core)
    ("C" "c" ,core->c ,preprocess-sort-c)
    ("Fortran" "f03" ,core->fortran ,preprocess-sort-default)
    ("Java" "java" ,core->java ,preprocess-sort-java)
    ("Python" "py" ,core->python ,preprocess-sort-python)
    ("Julia" "jl" ,core->julia ,preprocess-sort-julia)
    ("MATLAB" "mat" ,core->matlab ,preprocess-sort-matlab)
    ("Wolfram" "wl" ,core->wls ,preprocess-sort-default)
    ("TeX" "tex" ,(λ (c i) (core->tex c)) ,preprocess-sort-tex)))

(define (program->tex prog ctx #:loc [loc #f])
  (define prog* (program->fpcore prog ctx))
  (if (supported-by-lang? prog* "tex")
      (core->tex prog* #:loc loc #:color "blue")
      "ERROR"))

(define (render-program #:to [result #f] preprocesses test)
  (define identifier (test-identifier test))
  (define ctx (test-context test))
  (define output-repr (test-output-repr test))

  (define in-prog (program->fpcore (test-input test) ctx #:ident identifier))
  (define out-prog
    (and result
         (parameterize ([*expr-cse-able?* at-least-two-ops?])
           (core-cse (program->fpcore result ctx #:ident identifier)))))

  (define output-prec (representation-name output-repr))
  (define in-prog* (fpcore-add-props in-prog (list ':precision output-prec)))
  (define out-prog* (and out-prog (fpcore-add-props out-prog (list ':precision output-prec))))

  (define versions
    (reap [sow]
      (for ([(lang record) (in-dict languages)])
        (match-define (list ext converter preprocess-sort) record)
        (when (and (fpcore? in-prog*)
                   (or (not out-prog*) (fpcore? out-prog*))
                   (or (equal? ext "fpcore")                           
                       (and (supported-by-lang? in-prog* ext) ; must be valid in a given language  
                            (or (not out-prog*)
                                (supported-by-lang? out-prog* ext)))))
          (define name (if identifier (symbol->string identifier) "code"))
          (define in (converter in-prog* name))
          (define out (and out-prog* (converter out-prog* name)))
          (define sort-preprocesses
            (filter-map
             (match-lambda
               [(list 'sort variables ...) variables]
               [_ #f])
             preprocesses))
          (define out-with-sort-preprocessing
            (if (empty? sort-preprocesses)
                out
                (preprocess-sort out sort-preprocesses)))
          (sow (cons lang (cons in out-with-sort-preprocessing)))))))

  (define-values (math-in math-out)
    (if (dict-has-key? versions "TeX")
        (let ([val (dict-ref versions "TeX")])
          (values (car val) (cdr val)))
        (values "" "")))

  `(section ([id "program"])
     (h1 (a (
          [class "help-button"] 
          [href "/doc/latest/report.html#programs"] 
          [target "_blank"] 
          ;[style "rotate: 270deg"]
          ) "?"))
     ,(if (equal? (test-pre test) '(TRUE))
          ""
          `(div ([id "precondition"])
             (div ([class "program math"])
                  "\\[" ,(expr->tex (resugar-program (test-pre test) output-repr)) "\\]")))
     (select ([id "language"])
       (option "Math")
       ,@(for/list ([lang (in-dict-keys versions)])
           `(option ,lang)))
     (div ([class "implementation"] [data-language "Math"])
       (div ([class "program math"]) "\\[" ,math-in "\\]")
       ,@(if result
             `((div ([class "arrow"]) "↓")
               (div ([class "program math"]) "\\[" ,math-out "\\]"))
             `()))
     ,@(for/list ([(lang outs) (in-dict versions)])
         (match-define (cons out-input out-output) outs)
         `(div ([class "implementation"] [data-language ,lang])
            (pre ([class "program"]) ,out-input) 
            ,@(if out-output   
                  `((div ([class "arrow"]) "↓")
                    (pre ([class "program"]) ,out-output))  
                  `())))))

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

(define/contract (render-reproduction test #:bug? [bug? #f])
  (->* (test?) (#:bug? boolean?) xexpr?)

  `(section ((id "reproduce"))
    (h1 "Reproduce" (a (
          [class "help-button"] 
          [href "/doc/latest/report.html#reproduction"] 
          [target "_blank"] 
          ;[style "rotate: 270deg"]
          ) "?"))
    ,(if bug?
         `(p "Please include this information when filing a "
             (a ((href "https://github.com/herbie-fp/herbie/issues")) "bug report") ":")
         "")
    (pre ((class "shell"))
         (code
          ,(render-command-line) "\n"
          ,(render-fpcore test) "\n"))))

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
             
