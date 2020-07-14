#lang racket
(require (only-in xml write-xexpr xexpr?) 
         (only-in fpbench fpcore? supported-by-lang? core->c core->tex expr->tex))
(require "../common.rkt" "../syntax/read.rkt" "../programs.rkt" "../interface.rkt")
(provide render-menu render-warnings render-large render-program program->fpcore render-reproduction js-tex-include)

(define (program->fpcore prog)
  (match-define (list _ args expr) prog)
  (list 'FPCore args expr))

(define (fpcore-add-props core props)
  (match-define (list 'FPCore args expr) core)
  `(FPCore ,args ,@props ,expr))

(define (fpcore->string core)
  (match-define (list 'FPCore args props ... expr) core)
  (define props*
    (for/list ([val (apply dict-set* '() props)])
      (format "~a ~a" (car val) (cdr val))))
  (with-output-to-string
    (λ () (pretty-display `(,(format "FPCore ~a" args) ,@props* ,expr)))))

(define/contract (render-menu sections links)
  (-> (listof (cons/c string? string?)) (listof (cons/c string? string?)) xexpr?)
  `(nav ([id "links"])
    (div
     ,@(for/list ([(text url) (in-dict links)])
         `(a ([href ,url]) ,text)))
    (div
     ,@(for/list ([(text url) (in-dict sections)])
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
  
(define languages
  `(("TeX" . ,core->tex)
    ("FPCore" . ,fpcore->string)
    ("C" . ,(curryr core->c "code"))))

(define (render-program #:to [result #f] test)
  (define output-prec (test-output-prec test))
  (define in-prog (program->fpcore (resugar-program (test-program test) output-prec)))
  (define out-prog (and result (program->fpcore (resugar-program result output-prec))))

  (define in-prog* (fpcore-add-props in-prog (list ':precision output-prec)))
  (define out-prog* (and out-prog (fpcore-add-props out-prog (list ':precision output-prec))))

  (define versions
    (reap [sow]
      (for ([(lang converter) (in-dict languages)])
        (let ([ext (string-downcase lang)]) ; FPBench organizes compilers by extension
          (when (and (fpcore? in-prog*) (or (not out-prog*) (fpcore? out-prog*))
                    (or (equal? ext "fpcore")                           
                        (and (supported-by-lang? in-prog ext) ; must be valid in a given language  
                             (or (not out-prog*) (supported-by-lang? out-prog* ext)))))
            (sow (cons lang (cons (converter in-prog*)
                                  (and out-prog* (converter out-prog*)))))
    )))))

  `(section ([id "program"])
     ,(if (equal? (program-body (test-precondition test)) 'TRUE)
          ""
          `(div ([id "precondition"])
             (div ([class "program math"])
                  "\\[" ,(expr->tex (resugar-program (test-precondition test) output-prec)) "\\]")))
     (select ([id "language"])
       (option "Math")
       ,@(for/list ([lang (in-dict-keys versions)])
           `(option ,lang)))
     (div ([class "implementation"] [data-language "Math"])
       (div ([class "program math"]) "\\[" ,(core->tex in-prog*) "\\]")
       ,@(if result
             `((div ([class "arrow"]) "↓")
               (div ([class "program math"]) "\\[" ,(core->tex out-prog*) "\\]"))
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
  (string-join
   (filter
    identity
    (list
     (format "(FPCore ~a" (test-vars test))
     (format "  :name ~s" (test-name test))
     (format "  :precision ~s" (test-output-prec test))
     (if (equal? (program-body (test-precondition test)) 'TRUE)
         #f
         (format "  :pre ~a" (resugar-program (program-body (test-precondition test))
                                              (test-output-prec test))))
     (if (equal? (test-expected test) #t)
         #f
         (format "  :herbie-expected ~a" (test-expected test)))
     (if (test-output test)
         ;; Extra newlines for clarity
         (format "\n  :herbie-target\n  ~a\n" (resugar-program (test-output test)
                                                               (test-output-prec test)))
         #f)
     (format "  ~a)" (resugar-program (test-input test) (test-output-prec test)))))
   "\n"))

(define/contract (render-reproduction test #:bug? [bug? #f])
  (->* (test?) (#:bug? boolean?) xexpr?)

  `(section ((id "reproduce"))
    (h1 "Reproduce")
    ,(if bug?
         `(p "Please include this information when filing a "
             (a ((href "https://github.com/uwplse/herbie/issues")) "bug report") ":")
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
             