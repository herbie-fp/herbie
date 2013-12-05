#lang racket
(require casio/test)

(casio-test (x)
  "Hamming (NMSE) problem 3.3.4, ³√(x + 1) - ³√x"
  (- (expt (+ x 1) (/ 1 3)) (expt x (/ 1 3))))
