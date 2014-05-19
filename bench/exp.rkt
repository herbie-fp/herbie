#lang racket

(require casio/test)

(casio-test (x)
  "Kahan's exp quotient"
  (/ (- (exp x) 1) x)
  (/ (- (exp x) 1) (log (exp x))))
