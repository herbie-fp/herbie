#lang racket
(require casio/test)

(casio-bench (x)
  "Hamming (NMSE) example 3.9, 1/x - ctn(x)"
  (- (/ 1 x) (cot x)))
