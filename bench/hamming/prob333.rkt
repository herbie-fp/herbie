#lang racket
(require casio/test)

(casio-test (x)
  "Hamming (NMSE) problem 3.3.3, 1/(x + 1) - 2/x + 1/(x - 1)"
  (+ (- (/ 1 (+ x 1)) (/ 2 x)) (/ 1 (- x 1)))
  (/ 2 (* x (- (* x x) 1))))
