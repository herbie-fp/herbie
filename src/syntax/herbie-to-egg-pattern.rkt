#lang racket
(require "./rules.rkt")
(provide herbie-pattern->rust-pattern)


(define (herbie-pattern->rust-pattern datum)
  (cond
    [(list? datum)
     (string-append
      "("
      (symbol->string (first datum))
     (foldr
      (lambda (sub-expr acc)
        (string-append " "
                       (herbie-pattern->rust-pattern sub-expr)
                       acc))
      ""
      (rest datum))
     ")")]
    [(symbol? datum)
     (if
      (set-member? fpconstants datum)
      (symbol->string datum)
      (string-append "?" (symbol->string datum)))]
    [(number? datum)
     (number->string datum)]
    [else
     (error "expected list, number, or symbol")]))

