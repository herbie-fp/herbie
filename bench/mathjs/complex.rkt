#lang racket
(require casio/test)

; All code examples from mathjs
; https://github.com/josdejong/mathjs/blob/master/

(casio-test (re im)
  "math.arg on complex"
  (atan2 im re))

