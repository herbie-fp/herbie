#lang racket
(require casio/test)

(casio-bench (x n)
  "Hamming (NMSE) problem 3.4.6, (x + 1)^(1/n) - x^(1/n)"
  (- (expt (+ x 1) (/ 1 n)) (expt x (/ 1 n))))
