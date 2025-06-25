#lang racket
(require (only-in xml write-xexpr xexpr?)
         (only-in fpbench
                  fpcore?
                  supported-by-lang?
                  core->c
                  core->fortran
                  core->java
                  core->python
                  core->julia
                  core->matlab
                  core->wls
                  core->tex
                  expr->tex
                  core->js
                  compilers
                  [core-common-subexpr-elim core-cse]
                  *expr-cse-able?*))

(require "../utils/common.rkt"
         "../syntax/read.rkt"
         "../core/programs.rkt"
         "../syntax/types.rkt"
         "../syntax/sugar.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt")

(provide format-accuracy
         render-menu
         render-warnings
         render-large
         render-comparison
         render-specification
         render-program
         render-help
         render-reproduction
         format-percent
         write-html
         program->fpcore
         program->tex
         fpcore->string
         js-tex-include
         doc-url
         core->c
         core->fortran
         core->java
         core->python
         core->julia
         core->matlab
         core->wls
         core->tex
         expr->tex
         core->js)

(define (format-accuracy numerator repr #:sign [sign #f] #:unit [unit ""])
  (define denominator (representation-total-bits repr))
  (cond
    [(and numerator (positive? denominator))
     (define percent (~r (- 100 (* (/ numerator denominator) 100)) #:precision '(= 1)))
     (if (and (positive? numerator) sign)
         (format "+~a~a" percent unit)
         (format "~a~a" percent unit))]
    [else ""]))

(define (write-html xexpr out)
  (fprintf out "<!doctype html>\n")
  (write-xexpr xexpr out))

(define (program->fpcore expr ctx #:ident [ident #f])
  (define body (prog->fpcore expr ctx))
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
  (define props* ; make sure each property (name, value) gets put on the same line
    (for/list ([(prop name)
                (in-dict (apply dict-set* '() props))]) ; how to make a list of pairs from a list
      (format "~a ~a" prop name)))
  (define top
    (if ident
        (format "FPCore ~a ~a" ident args)
        (format "FPCore ~a" args)))
  (pretty-format `(,top ,@props* ,expr) #:mode 'display))

(define (doc-url page)
  (format "https://herbie.uwplse.org/doc/~a/~a" *herbie-version* page))

(define/contract (render-menu #:path [path "."] name links)
  (->* (string? (listof (cons/c string? string?))) (#:path string?) xexpr?)
  `(header (h1 ,name)
           (img ([src ,(string-append path "/" "logo-car.png")]))
           (nav (ul ,@(for/list ([(text url) (in-dict (filter identity links))])
                        `(li (a ([href ,url]) ,text)))))))

(define/contract (render-warnings warnings)
  (-> (listof (list/c string? string? (or/c string? #f) (listof string?))) xexpr?)
  (if (null? warnings)
      ""
      `(ul ((class "warnings"))
           ,@(for/list ([warning warnings])
               (match-define (list type message url extra) warning)
               `(li (h2 ,message
                        ,(if url
                             `(a ([href ,url]) " (more)")
                             ""))
                    ,(if (null? extra)
                         ""
                         `(ol ((class "extra"))
                              ,@(for/list ([line extra])
                                  `(li ,line)))))))))

(define (render-large #:title [title #f] name . values)
  `(div ,name
        ": "
        (span ((class "number") ,@(if title
                                      `([title ,title])
                                      '()))
              ,@values)))

(define (render-comparison #:title [title #f] name a b)
  (render-large #:title title name a `(span ((class "unit")) " → ") b))

(define (render-specification test)
  (define-values (dropdown body)
    (render-program (test-spec test)
                    (test-context test)
                    #:pre (test-pre test)
                    #:ident (test-identifier test)))
  `(section (details ([id "specification"] (class "programs"))
                     (summary (h2 "Specification")
                              ,dropdown
                              (a ((class "help-button float") [href ,(doc-url "report.html#spec")]
                                                              [target "_blank"])
                                 "?"))
                     ,body)))

(define languages
  `(("FPCore" "fpcore" ,(λ (c i) (fpcore->string c))) ("C" "c" ,core->c)
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

(define (render-program expr ctx #:ident [identifier #f] #:pre [precondition '(TRUE)])
  (define output-repr (context-repr ctx))
  (define out-prog
    (parameterize ([*expr-cse-able?* at-least-two-ops?])
      (core-cse (program->fpcore expr ctx #:ident identifier))))

  (define output-prec (representation-name output-repr))
  (define out-prog* (fpcore-add-props out-prog (list ':precision output-prec)))

  (define versions
    (reap [sow]
          (for ([(lang record) (in-dict languages)])
            (match-define (list ext converter) record)
            (when (and (fpcore? out-prog*)
                       (or (equal? ext "fpcore") (supported-by-lang? out-prog* ext)))
              (define name
                (if identifier
                    (symbol->string identifier)
                    "code"))
              (define out (converter out-prog* name))
              (sow (cons lang out))))))

  (define math-out (dict-ref versions "TeX" ""))

  (define dropdown
    `(select (option "Math")
             ,@(for/list ([lang (in-dict-keys versions)])
                 `(option ,lang))))

  (define body
    `(div
      ,(if (equal? precondition '(TRUE))
           ""
           `(div
             ([id "precondition"])
             (div ((class "program math")) "\\[" ,(expr->tex (prog->fpcore precondition ctx)) "\\]")))
      (div ((class "implementation") [data-language "Math"])
           (div ((class "program math")) "\\[" ,math-out "\\]"))
      ,@(for/list ([(lang out) (in-dict versions)])
          `(div ((class "implementation") [data-language ,lang]) (pre ((class "program")) ,out)))))

  (values dropdown body))

(define/contract (render-command-line)
  (-> string?)
  (format "herbie shell --seed ~a ~a"
          (if (vector? (get-seed))
              (format "'~a'" (get-seed))
              (get-seed))
          (string-join (for/list ([rec (changed-flags)])
                         (match rec
                           [(list 'enabled class flag) (format "+o ~a:~a" class flag)]
                           [(list 'disabled class flag) (format "-o ~a:~a" class flag)]))
                       " ")))

(define/contract (render-fpcore test)
  (-> test? string?)
  (define output-repr (test-output-repr test))
  (string-join
   (filter identity
           (list (if (test-identifier test)
                     (format "(FPCore ~a ~a" (test-identifier test) (test-vars test))
                     (format "(FPCore ~a" (test-vars test)))
                 (format "  :name ~s" (test-name test))
                 (format "  :precision ~s" (representation-name (test-output-repr test)))
                 (if (equal? (test-pre test) '(TRUE))
                     #f
                     (format "  :pre ~a" (prog->fpcore (test-pre test) (test-context test))))
                 (if (equal? (test-expected test) #t)
                     #f
                     (format "  :herbie-expected ~a" (test-expected test)))
                 (and (test-output test)
                      (not (null? (test-output test)))
                      (format "\n~a"
                              (string-join (map (lambda (exp) (format "  :alt\n  ~a\n" (car exp)))
                                                (test-output test))
                                           "\n")))
                 (format "  ~a)" (prog->fpcore (test-input test) (test-context test)))))
   "\n"))

(define (format-percent num den)
  (string-append (if (zero? den)
                     (cond
                       [(positive? num) "+∞"]
                       [(zero? num) "0"]
                       [(negative? num) "-∞"])
                     (~r (* (/ num den) 100) #:precision '(= 1)))
                 "%"))

(define/contract (render-reproduction test #:bug? [bug? #f])
  (->* (test?) (#:bug? boolean?) xexpr?)

  `(section
    ([id "reproduce"])
    (details ,(if bug?
                  '([open "open"])
                  "")
             (summary (h2 "Reproduce")
                      (a ((class "help-button float") [href ,(doc-url "report.html#reproduction")]
                                                      [target "_blank"])
                         "?"))
             (pre ((class "shell")) (code ,(render-command-line) "\n" ,(render-fpcore test) "\n"))
             ,(if bug?
                  `(p "Please file a "
                      (a ((href "https://github.com/herbie-fp/herbie/issues")) "bug report")
                      " with this information.")
                  ""))))

(define (render-help url #:float [float? #t])
  `(a ((class ,(if float? "help-button float" "help-button")) [href ,(doc-url url)] [target "_blank"])
      "?"))

(define js-tex-include
  '((link ([rel "stylesheet"]
           [href "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.css"]
           [integrity "sha384-5TcZemv2l/9On385z///+d7MSYlvIEw9FuZTIdZ14vJLqWphw7e7ZPuOiCHJcFCP"]
           [crossorigin "anonymous"]))
    (script ([defer ""] [src "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/katex.min.js"]
                        [integrity
                         "sha384-cMkvdD8LoxVzGF/RPUKAcvmm49FQ0oxwDF3BGKtDXcEc+T1b2N+teh/OJfpU0jr6"]
                        [crossorigin "anonymous"]))
    (script ([defer ""]
             [src "https://cdn.jsdelivr.net/npm/katex@0.16.22/dist/contrib/auto-render.min.js"]
             [integrity "sha384-hCXGrW6PitJEwbkoStFjeJxv+fSOOQKOPbJxSfM6G5sWZjAyWhXiTIIAmQqnlLlh"]
             [crossorigin "anonymous"]))))
