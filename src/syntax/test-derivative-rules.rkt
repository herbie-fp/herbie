#lang racket

(require "rules.rkt")
(require "../core/simplify.rkt")

(module+ test (require rackunit))



(module+ test
  (define derivatives
          `(((d a a) . 1)
            ((d (+.f64 1 (*.f64 2 x)) x) . 2)
            ((d (+.f64 1 (*.f64 y x)) x) . y)
            ((d (pow.f64 x 3) x) . (* 3 (pow x 2)))))

  (for ([pair derivatives])
       (check-equal? (differentiate-expr (car pair)) (cdr pair))))