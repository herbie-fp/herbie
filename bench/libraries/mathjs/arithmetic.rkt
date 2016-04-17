
; All code examples from mathjs
; https://github.com/josdejong/mathjs/blob/master/
; from files in
; /lib/function/arithmetic/

(herbie-test (re im)
  "math.abs on complex"
  (sqrt (+ (* re re) (* im im))))

(herbie-test (x)
  "math.cube on real"
  (* (* x x) x)
  (pow x 3))

; Manually expanded math.multiply
(herbie-test (x.re x.im) ; real part
  "math.cube on complex, real part"
  (- (* (- (* x.re x.re) (* x.im x.im)) ; x^2.re
        x.re)
     (* (+ (* x.re x.im) (* x.im x.re)) ; x^2.im
        x.im)))

(herbie-test (x.re x.im) ; imag part
  "math.cube on complex, imaginary part"
  (+ (* (- (* x.re x.re) (* x.im x.im)) ; x^2.re
        x.im)
     (* (+ (* x.re x.im) (* x.im x.re)) ; x^2.im
        x.re)))

(herbie-test (x.re x.im y.re y.im) ; real part
  "_divideComplex, real part"
  (/ (+ (* x.re y.re) (* x.im y.im))
     (+ (* y.re y.re) (* y.im y.im))))
(herbie-test (x.re x.im y.re y.im) ; imag part
  "_divideComplex, imaginary part"
  (/ (- (* x.im y.re) (* x.re y.im))
     (+ (* y.re y.re) (* y.im y.im))))

(herbie-test (re im) ; real part
  "math.exp on complex, real part"
  (* (exp re) (cos im)))
(herbie-test (re im) ; imag part
  "math.exp on complex, imaginary part"
  (* (exp re) (sin im)))

(herbie-test (re im) ; real part
  "math.log/1 on complex, real part"
  (log (sqrt (+ (* re re) (* im im)))))
(herbie-test (re im) ; imag part
  "math.log/1 on complex, imaginary part"
  (atan2 im re))

; Manually expanded math.divide
(herbie-test (re im base) ; real part
  "math.log/2 on complex, real part"
  (/ (+ (* (log (sqrt (+ (* re re) (* im im)))) (log base)) (* (atan2 im re) 0))
     (+ (* (log base) (log base)) (* 0 0))))
(herbie-test (re im base) ; imag part
  "math.log/2 on complex, imaginary part"
  (/ (- (* (atan2 im re) (log base)) (* (log (sqrt (+ (* re re) (* im im)))) 0))
     (+ (* (log base) (log base)) (* 0 0))))

(herbie-test (re im) ; real part
  "math.log10 on complex, real part"
  (/ (log (sqrt (+ (* re re) (* im im)))) (log 10)))
(herbie-test (re im) ; imag part
  "math.log10 on complex, imaginary part"
  (/ (atan2 im re) (log 10)))

(herbie-test (x.re x.im y.re y.im) ; real part
  "_multiplyComplex, real part"
  (- (* x.re y.re) (* x.im y.im)))
(herbie-test (x.re x.im y.re y.im) ; imag part
  "_multiplyComplex, imaginary part"
  (+ (* x.re y.im) (* x.im y.re)))

; Manually expanded math.log, math.multiply, math.exp
(herbie-test (x.re x.im y.re y.im) ; real part
  "powComplex, real part"
  (* (exp (- (* (log (sqrt (+ (* x.re x.re) (* x.im x.im)))) y.re) (* (atan2 x.im x.re) y.im)))
     (cos (+ (* (log (sqrt (+ (* x.re x.re) (* x.im x.im)))) y.im) (* (atan2 x.im x.re) y.re)))))
(herbie-test (x.re x.im y.re y.im) ; imag part
  "powComplex, imaginary part"
  (* (exp (- (* (log (sqrt (+ (* x.re x.re) (* x.im x.im)))) y.re) (* (atan2 x.im x.re) y.im)))
     (sin (+ (* (log (sqrt (+ (* x.re x.re) (* x.im x.im)))) y.im) (* (atan2 x.im x.re) y.re)))))

(herbie-test (re im) ; real part
  "math.sqrt on complex, real part"
  (* 0.5 (sqrt (* 2.0 (+ (sqrt (+ (* re re) (* im im))) re))))
  (if (< re 0)
      (* 0.5 (* (sqrt 2) (sqrt (/ (sqr im)
				  (- (sqrt (+ (sqr re) (sqr im))) re)))))
      (* 0.5 (sqrt (* 2.0 (+ (sqrt (+ (* re re) (* im im))) re))))))

(herbie-test (re im) ; imag part
  "math.sqrt on complex, imaginary part, im greater than 0 branch"
  (*  0.5 (sqrt (* 2.0 (+ (sqrt (- (* re re) (* im im))) re)))))

; Manually expanded math.multiply
(herbie-test (re im) ; real part
  "math.square on complex, real part"
  (- (* re re) (* im im)))
(herbie-test (re im) ; imag part
  "math.square on complex, imaginary part"
  (+ (* re im) (* im re)))

