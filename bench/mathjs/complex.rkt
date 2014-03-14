#lang racket
(require casio/test)

; All code examples from mathjs
; https://github.com/josdejong/mathjs/blob/master/

(casio-bench (re im)
  "/lib/function/complex/arg.js math.arg for complex arguments"
  (atan2 im re))

