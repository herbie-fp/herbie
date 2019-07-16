#lang racket
(require (only-in xml write-xexpr xexpr?))
(require "../common.rkt" "../formats/test.rkt" "../sandbox.rkt")
(require "../formats/c.rkt" "../formats/tex.rkt" "../interface.rkt")
(provide render-menu render-warnings render-large render-program)

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
  `(("TeX" . ,(curryr texify-prog (*output-prec*)))
    ("C" . ,program->c)))

(define (render-program #:to [result #f] test)
  (define output-prec (test-output-prec test))
  `(section ([id "program"])
     ,(if (equal? (test-precondition test) 'TRUE)
          ""
          `(div ([id "precondition"])
             (div ([class "program math"])
                  "\\[" ,(texify-expr (test-precondition test) output-prec) "\\]")))
     (select ([id "language"])
       (option "Math")
       ,@(for/list ([(lang fn) (in-dict languages)])
           `(option ,lang)))
     (div ([class "implementation"] [data-language "Math"])
       (div ([class "program math"]) "\\[" ,(texify-prog
                                              (test-program test)
                                              output-prec) "\\]")
       ,@(if result
             `((div ([class "arrow"]) "↓")
               (div ([class "program math"]) "\\[" ,(texify-prog
                                                      result
                                                      output-prec) "\\]"))
             `()))
     ,@(for/list ([(lang fn) (in-dict languages)])
         `(div ([class "implementation"] [data-language ,lang])
            (pre ([class "program"]) ,(fn (test-program test)))
            ,@(if result
                  `((div ([class "arrow"]) "↓")
                    (pre ([class "program"]) ,(fn result)))
                  `())))))
