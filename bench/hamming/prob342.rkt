#lang racket
(require casio/test)

(casio-bench (a b eps)
  "Hamming (NMSE) problem 3.4.2"
  (/ (* eps (- (exp (* (+ a b) eps)) 1))
     (* (- (exp (* a eps)) 1) (- (exp (* b eps)) 1))))
