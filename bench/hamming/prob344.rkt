#lang racket
(require casio/test)

(casio-bench (x)
  "Hamming (NMSE) problem 3.4.4, √(e^2x - 1 / e^x - 1)"
  (sqrt (/ (- (exp (* 2 x)) 1) (- (exp x) 1))))
