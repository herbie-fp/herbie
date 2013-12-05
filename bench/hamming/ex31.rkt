#lang racket

(require casio/test)

(casio-test (x)
  "Hamming (NMSE) example 3.1, √(x+1) - √x"
  (- (sqrt (+ x 1)) (sqrt x))
  (/ 1 (+ (sqrt (+ x 1)) (sqrt x))))
