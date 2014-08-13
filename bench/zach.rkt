#lang racket
(require casio/test)

(casio-test (x)
  "Sanity"
  x
  x)

(casio-bench (x)
  "sin(sqr(x))"
  (sin (sqr x)))
