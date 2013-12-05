#lang racket
(require casio/test)

(casio-bench (N)
  "Hamming (NMSE) example 3.8, (N+1) ln(N+1) - N ln(N) - 1"
  (- (- (* (+ N 1) (log (+ N 1))) (* N (log N))) 1))
