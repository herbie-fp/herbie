#lang racket

(require "rules.rkt")
(require "../core/simplify.rkt")

(module+ test (require rackunit))



(module+ test
  (define derivatives
          `(((d a a) . 1)
            ((d a x) . 0)))
  
  (for ([pair derivatives])
       (check-equal? (differentiate-expr (car pair)) (cdr pair))))