#lang racket
(require casio/test)

; All code examples from mathjs
; https://github.com/josdejong/mathjs/blob/master/
; from files in
; /lib/function/probability/

(casio-test (u1 u2)
  "normal distribution"
  (+ (* (* (/ 1 6) (expt (* -2 (log u1)) 0.5))
        (cos (* (* 2 pi) u2))) 0.5))
