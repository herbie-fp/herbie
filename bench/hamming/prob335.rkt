#lang racket
(require casio/test)

(casio-test (x eps)
  "Hamming (NMSE) problem 3.3.5, cos(x + ε) - cos(x)"
  (- (cos (+ x eps)) (cos x)))
