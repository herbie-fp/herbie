#lang racket
(require casio/test)

(casio-bench (x)
  "Hamming (NMSE) problem 3.4.3, log(1 - x / 1 + x)"
  (log (/ (- 1 x) (+ 1 x))))
