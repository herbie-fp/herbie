#lang racket
(require casio/test)

(casio-bench (a b c)
  "Inspired by FT_MulDiv in Freetype"
  (/ (+ a (/ c 2)) b))
