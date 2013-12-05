#lang racket

(require casio/test)

(casio-test (x)
  "Hamming (NMSE) example 3.4, (1 - cos(x)) / sin(x)"
  (/ (- 1 (cos x)) (sin x))
  (tan (/ x 2)))
