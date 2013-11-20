#lang racket

(require casio/test)

(casio-test (x y)
  "Akshay's logistic regression example 1"

  (- (/ 1 (+ 1 (exp (- x)))) y)

  (if (> x 0)
      (- (/ 1 (+ 1 (exp (- x)))) y)
      (- (- 1 y) (/ 1 (1+ (exp x))))))


