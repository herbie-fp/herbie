#lang racket

(require casio/test)

(casio-test (x eps)
  "Hamming (NMSE) example 3.3, sin(x + ε) - sin(x)"
  (- (sin (+ x eps)) (sin x))
  (* 2 (* (cos (+ x (/ eps 2))) (sin (/ eps 2)))))
