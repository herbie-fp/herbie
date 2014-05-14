#lang racket
(require casio/test)

(casio-bench (a b c)
  "Mildly inspired by the FT_MulDiv function in Freetype"
  (/ (+ a (/ c 2)) b))
