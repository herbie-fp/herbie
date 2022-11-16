#lang racket


(module+ test (require rackunit))


#;(module+ test
  (check-equal? (to-egg-pattern `(+ a b)) "(+ real ?a ?b)")
  (check-equal? (to-egg-pattern `(/ c (- 2 a))) "(/ real ?c (- real 2 ?a))")
  (check-equal? (to-egg-pattern `(cos.f64 (PI.f64))) "(cos f64 (PI f64))")
  (check-equal? (to-egg-pattern `(if (TRUE) x y)) "(if real (TRUE real) ?x ?y)"))
