#lang racket
(require casio/test)

(casio-bench (x)
  "Hamming (NMSE) problem 3.3.1, 1/(x + 1) - 1/x"
  (- (/ 1 (+ x 1)) (/ 1 x)))
