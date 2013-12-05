#lang racket
(require casio/test)

(casio-bench (x)
  "Hamming (NMSE) problem 3.4.5, (x - sin(x)) / (x - tan(x))"
  (/ (- x (sin x)) (- x (tan x))))
