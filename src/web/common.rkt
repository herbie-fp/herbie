#lang racket
(require (only-in xml write-xexpr xexpr?) (only-in fpbench fpcore? core->c))
(require "../common.rkt" "../syntax/read.rkt" "../programs.rkt" "../interface.rkt" "tex.rkt")
(provide render-menu render-warnings render-large render-program program->fpcore resugar-fpcore render-reproduction)

(define (program->fpcore prog [transform identity])
  (match-define (list _ args expr) prog)
  (list 'FPCore args (transform expr)))

(define (resugar-fpcore prog prec)
  (match-define (list 'FPCore args expr) prog)
  (list 'FPCore args (resugar-program expr prec)))

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

;; TODO: TeX does not want fpcores resugared while C does. It would be great to have tex match C
;; to make everything cleaner.
(define languages
  `(("TeX" . ,(λ (prog) (texify-prog prog)))
    ;; TODO(interface): currently program->c doesn't take the repr into account
    ("C" . ,(λ (prog) (core->c prog "code")))))

(define (render-program #:to [result #f] test)
  (define output-prec (test-output-prec test))
  (define output-repr (get-representation output-prec))

  (define in-prog (program->fpcore (test-program test) (curryr resugar-program output-prec)))
  (define out-prog (and result (program->fpcore result (curryr resugar-program output-prec))))

  (define versions
    (reap [sow]
      (for ([(lang converter) (in-dict languages)])
        (when (and (fpcore? in-prog) (or (not out-prog) (fpcore? out-prog)))
          (sow (cons lang (cons (converter in-prog)
                                (and out-prog (converter out-prog)))))))))

  `(section ([id "program"])
     ,(if (equal? (test-precondition test) 'TRUE)
          ""
          `(div ([id "precondition"])
             (div ([class "program math"])
                  "\\[" ,(texify-expr (test-precondition test)) "\\]")))
     (select ([id "language"])
       (option "Math")
       ,@(for/list ([lang (in-dict-keys versions)])
           `(option ,lang)))
     (div ([class "implementation"] [data-language "Math"])
       (div ([class "program math"]) "\\[" ,(texify-prog in-prog) "\\]")
       ,@(if result
             `((div ([class "arrow"]) "↓")
               (div ([class "program math"]) "\\[" ,(texify-prog out-prog) "\\]"))
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
     (if (equal? (test-precondition test) 'TRUE)
         #f
         (format "  :pre ~a" (resugar-program (test-precondition test)
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
