#lang racket
(require casio/test)

(casio-bench (x)
  "Hamming (NMSE) problem 3.4.1, (1 - (cos x)) / x^2"
  (/ (- 1 (cos x)) (expt x 2)))
