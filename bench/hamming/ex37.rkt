#lang racket
(require casio/test)

(casio-bench (x)
  "Hamming (NMSE) example 3.7, e^x - x"
  (- (exp x) x))
