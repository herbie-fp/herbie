
; All code examples from mathjs
; https://github.com/josdejong/mathjs/blob/master/
; from files in
; /lib/function/trigonometry/

; TODO : The inverse functions acos, asin, and atan were too complex to expand fully.

(herbie-test (re im) ; real part
  "math.cos on complex, real part"
  (* (* 0.5 (cos re)) (+ (exp (- im)) (exp im))))

(herbie-test (re im) ; imag part
  "math.cos on complex, imaginary part"
  (* (* 0.5 (sin re)) (- (exp (- im)) (exp im)))
  (if (< (fabs im) 1)
      (- (* (sin re) (+ im (* 1/6 im im im) (* 1/120 im im im im im))))
      (* (* 0.5 (sin re)) (- (exp (- im)) (exp im)))))

; TODO : The reciprocal functions cot, csc, sec, and tan were too complex to expand fully.

(herbie-test (re im) ; real part
  "math.sin on complex, real part"
  (* (* 0.5 (sin re)) (+ (exp (- 0 im)) (exp im))))

(herbie-test (re im) ; imag part
  "math.sin on complex, imaginary part"
  (* (* 0.5 (cos re)) (- (exp (- 0 im)) (exp im)))
  (if (< (fabs im) 1)
      (- (* (cos re) (+ im (* 1/6 im im im) (* 1/120 im im im im im))))
      (* (* 0.5 (cos re)) (- (exp (- 0 im)) (exp im)))))
