#lang racket

(require "rules.rkt")
(require "../core/simplify.rkt")

(module+ test (require rackunit))



(module+ test
  (define derivatives
          `(((d a a) . 1)
            ((d y x) . 0)
            ((d (+.f64 1 (*.f64 2 x)) x) . 2)
            ((d (+.f64 1 (*.f64 y x)) x) . y)
            ((d (pow.f64 x 3) x) . (*.f64 x (*.f64 x 3)))
            ((d (-.f64 (pow.f64 x 3) (*.f64 7 (pow.f64 x 2))) x)
             . (*.f64 x (+.f64 (*.f64 3 x) -14)))
            ((subst (+.f64 a a) a 4) . 8)
            ((subst c a 4) . c)
            ((subst (subst (/.f64 c d) c 4) d 5) . 4/5)
            ((subst (d (sin.f64 x) x) x PI.f64) . -1)
            ((subst (d (-.f64 (pow.f64 x 3) (*.f64 7 (pow.f64 x 2))) x) x 1)
             . -11)
            ((subst (d (log.f64 (/.f64 (-.f64 x 1) (+.f64 x 1))) x) x 0)
             . -2)))

  (for ([pair derivatives])
       (check-equal? (differentiate-expr (car pair)) (cdr pair))))