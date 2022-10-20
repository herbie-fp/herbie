#lang racket

(provide to-egg-pattern extract-operator)

(module+ test (require rackunit))

(define (extract-operator op)
  (if (symbol? op)
      (match (regexp-match #px"([^\\s^\\.]+)\\.([^\\s]+)" (~s op))
        [(list _ op* prec)  (list op* prec)]
        [#f (list (~s op) "real")])
      #f))

(define (to-egg-pattern expr)
  (match expr
    [(list op prec args ...)
     (string-join
      (cons (~s op) (cons (~s prec) (map (λ (sub-expr) (to-egg-pattern sub-expr)) args)))
      " "
      #:before-first "("
      #:after-last ")")]
    [(? symbol?)
     (format "?~a" expr)]
    [(? number?)
     (number->string expr)]
    [_
     (error "expected list, number, or symbol: " expr)]))

(module+ test
  (check-equal? (to-egg-pattern `(+ a b)) "(+ ?a ?b)")
  (check-equal? (to-egg-pattern `(/ c (- 2 a))) "(/ ?c (- real 2 ?a))")
  (check-equal? (to-egg-pattern `(cos f64 (PI f64))) "(cos ?f64 (PI ?f64))")
  (check-equal? (to-egg-pattern `(if (TRUE) x y)) "(if (TRUE) ?x ?y)"))
