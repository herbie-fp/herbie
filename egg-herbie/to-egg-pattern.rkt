#lang racket

(provide to-egg-pattern extract-operator extract-symbol constant?)

(module+ test (require rackunit))

;; copy of constants from herbie/fpcore for egg-herbie
(define constants
  (append
    '(E LOG2E LOG10E LN2 LN10
        PI PI_2 PI_4 1_PI 2_PI 2_SQRTPI
        SQRT2 SQRT1_2 MAXFLOAT HUGE_VAL
        TRUE FALSE)))

(define (constant? x)
  (set-member? constants x))

(define (extract-operator op)
  (if (symbol? op)
      (match (regexp-match #px"([^\\s^\\.]+)\\.([^\\s]+)" (~s op))
        [(list _ op* prec)  (list op* prec)]
        [#f (list (~s op) "real")])
      #f))

(define (extract-symbol sym)
  (if (symbol? sym)
      (match (regexp-match #px"([^\\s^\\.]+)\\.([^\\s]+)" (~s sym))
        [(list _ constant prec) (~a (list constant prec))]
        [#f (format (if (constant? sym) "(~a real)" "?~a") sym)])
      #f))

(define (to-egg-pattern datum)
  (cond
    [(list? datum)
     (string-join
      (append
       (extract-operator (first datum))
       (map (lambda (sub-expr) (to-egg-pattern sub-expr))
            (rest datum)))
      " "
      #:before-first "("
      #:after-last ")")]
    [(symbol? datum)
     (extract-symbol datum)]
    [(number? datum)
     (number->string datum)]
    [else
     (error "expected list, number, or symbol")]))

(module+ test
  (check-equal? (to-egg-pattern `(+ a b)) "(+ real ?a ?b)")
  (check-equal? (to-egg-pattern `(/ c (- 2 a))) "(/ real ?c (- real 2 ?a))")
  (check-equal? (to-egg-pattern `(cos.f64 (PI.f64))) "(cos f64 (PI f64))")
  (check-equal? (to-egg-pattern `(if TRUE x y)) "(if real (TRUE real) ?x ?y)"))
