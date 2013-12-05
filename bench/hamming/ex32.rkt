#lang racket

(require casio/racket)

(casio-bench (x)
  "Hamming (NMSE) example 3.2, sin(x) / x"
  (if (= x 0)
      1
      (/ (sin x) x)))
