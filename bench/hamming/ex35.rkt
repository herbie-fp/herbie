#lang racket

(require casio/test)

(casio-test (N)
  "Hamming (NMSE) example 3.5, atan(N + 1) - atan(N)"
  (- (atan (+ N 1)) (atan N))
  (atan (/ 1 (+ 1 (* N (- N 1))))))
