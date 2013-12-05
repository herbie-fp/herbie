#lang racket

(require casio/test)

(casio-test (x)
  "Hamming (NMSE) example 3.6, 1/√(x + 1) - 1/√x"
  (- (/ 1 (+ x 1)) (/ 1 x))
  (/ 1 (+ (* (+ x 1) (sqrt x)) (* x (sqrt (+ x 1))))))
