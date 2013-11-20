#lang racket

(require casio/test)

(casio-test (x y)
  "Akshay's logistic regression example 2"

  (- (log (1+ (exp x))) (* x y))

  (if (<= x 0) ;;cases are flipped.
      (- (log (1+ (exp x))) (* x y))
      (- (log (1+ (exp (- x)))) (* (- x) (- 1 y)))))
