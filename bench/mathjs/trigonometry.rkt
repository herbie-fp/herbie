#lang racket
(require casio/test)

; All code examples from mathjs
; https://github.com/josdejong/mathjs/blob/master/

; TODO : The inverse functions acos, asin, and atan were too complex to expand fully.

(casio-bench (re im) ; real part
  "/lib/function/trigonometry/cos.js math.cos for complex arguments, real part"
  (* (* 0.5 (cos re)) (+ (exp (- im)) (exp im))))
(casio-bench (re im) ; imag part
  "/lib/function/trigonometry/cos.js math.cos for complex arguments, imaginary part"
  (* (* 0.5 (sin re)) (- (exp (- im)) (exp im))))

; TODO : The reciprocal functions cot, csc, sec, and tan were too complex to expand fully.

(casio-bench (re im) ; real part
  "/lib/function/trigonometry/sin.js math.sin for complex arguments, real part"
  (* (* 0.5 (sin re)) (+ (exp (- 0 im)) (exp im))))
(casio-bench (re im) ; imag part
  "/lib/function/trigonometry/sin.js math.sin for complex arguments, imaginary part"
  (* (* 0.5 (cos re)) (- (exp (- 0 im)) (exp im))))
