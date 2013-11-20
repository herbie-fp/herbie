#lang racket

(require casio/test)

(casio-test (x)
  "Test Kahan's classic (exp(x) - 1) / x"
  (/ (- (exp x) 1) x)
  (/ (- (exp x) 1) (log (exp x))))
