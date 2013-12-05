#lang racket
(require casio/test)

(casio-bench (x)
  "Hamming (NMSE) example 3.10, ln(1 - x)/ln(1+x)"
  (/ (log (- 1 x)) (log (+ 1 x))))
