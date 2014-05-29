#lang racket
(require casio/test)

(casio-bench (a b c)
  "A la Freetype's FT_MulDiv"
  (/ (+ a (/ c 2)) b))
