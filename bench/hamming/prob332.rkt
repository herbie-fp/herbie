#lang racket
(require casio/test)

(casio-test (x eps)
  "Hamming (NMSE) problem 3.3.2, tan(x + ε) - tan(x)"
  (- (tan (+ x eps)) (tan x))
  (/ (sin eps) (* (cos x) (cos (+ x eps)))))
