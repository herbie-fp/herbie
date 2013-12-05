#lang racket
(require casio/test)

(casio-bench (N)
  "Hamming (NMSE) problem 3.3.6, ln(N + 1) - ln(N)"
  (- (log (+ N 1)) (log N)))
