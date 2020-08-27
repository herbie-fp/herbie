#lang racket

(require "rules.rkt")
(require "../core/simplify.rkt")

(module+ test (require rackunit))



(module+ test
  (define derivatives
          `(((d a a) . 1)
            ((d (+.f64 1 (*.f64 2 x)) x) . 2)
            ((d (+.f64 1 (*.f64 y x)) x) . y)
            ((d (pow.f64 x 3) x) . (* 3 (pow x 2)))
            ((subst (+.f64 a a) a 4) . 8)
            ((subst c a 4) . c)
            ((subst (subst (/.f64 c d) c 4) d 5) . 4/5)))
  
  #;(pretty-print (*differentiation-rules*))

  (for ([pair derivatives])
       (check-equal? (differentiate-expr (car pair)) (cdr pair))))