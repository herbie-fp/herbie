#lang racket

(provide to-egg-pattern extract-operator)

(module+ test (require rackunit))

(define (extract-operator op)
  (if (symbol? op)
      (match (regexp-match #px"([^\\s^\\.]+)\\.([^\\s]+)" (~s op))
        [(list _ op* prec)  (list op* prec)]
        [#f (list (~s op) "real")])
      #f))

(define (to-egg-pattern datum)
  (cond
    [(list? datum)
     (string-join
      (cons
       (~s (first datum))
       (map (lambda (sub-expr) (to-egg-pattern sub-expr))
            (rest datum)))
      " "
      #:before-first "("
      #:after-last ")")]
    [(symbol? datum)
     (format "?~a" datum)]
    [(number? datum)
     (number->string datum)]
    [else
     (error "expected list, number, or symbol")]))

(module+ test
  (check-equal? (to-egg-pattern `(+ a b)) "(+ ?a ?b)")
  (check-equal? (to-egg-pattern `(/ c (- 2 a))) "(/ ?c (- real 2 ?a))")
  (check-equal? (to-egg-pattern `(cos f64 (PI f64))) "(cos ?f64 (PI ?f64))")
  (check-equal? (to-egg-pattern `(if (TRUE) x y)) "(if (TRUE) ?x ?y)"))
