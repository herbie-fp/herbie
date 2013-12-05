#lang racket
(require casio/test)

(casio-test (x)
  "Hamming (NMSE) problem 3.3.7, e^x - 2 + e^-x"
  (+ (- (exp x) 2) (exp (- x)))
  (* 4 (expt (sinh (/ x 2)) 2)))
