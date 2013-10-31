; All code examples from mathjs
; https://github.com/josdejong/mathjs/blob/master/

; TODO : The inverse functions acos, asin, and atan were too complex to expand fully.

; /lib/function/trigonometry/cos.js math.cos for complex arguments
(lambda (re im) ; real part
  (* (* 0.5 (cos re)) (+ (exp (- 0 im)) (exp im))))
(lambda (re im) ; imag part
  (* (* 0.5 (sin re)) (- (exp (- 0 im)) (exp im))))

; TODO : The reciprocal functions cot, csc, sec, and tan were too complex to expand fully.

; /lib/function/trigonometry/sin.js math.sin for complex arguments
(lambda (re im) ; real part
  (* (* 0.5 (sin re)) (+ (exp (- 0 im)) (exp im))))
(lambda (re im) ; imag part
  (* (* 0.5 (cos re)) (- (exp (- 0 im)) (exp im))))
