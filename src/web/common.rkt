#lang racket
(require (only-in xml write-xexpr xexpr?))
(require "../common.rkt" "../formats/test.rkt" "../sandbox.rkt")
(provide render-menu render-warnings render-large)

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
