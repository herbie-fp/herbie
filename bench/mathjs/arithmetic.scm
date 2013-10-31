; All code examples from mathjs
; https://github.com/josdejong/mathjs/blob/master/

; /lib/function/arithmetic/abs.js math.abs for complex arguments
(lambda (re im)
  (sqrt (+ (* re re) (* im im))))

; /lib/function/arithmetic/cube.js math.cube for real arguments
(lambda (x)
  (* (* x x) x))

; /lib/function/arithmetic/cube.js math.cube for complex arguments
; Manually expanded math.multiply
(lambda (re im) ; real part
  (- (* (- (* x.re x.re) (* x.im x.im)) ; x^2.re
        x.re)
     (* (+ (* x.re x.im) (* x.im x.re)) ; x^2.im
        x.im)))
(lambda (re im) ; imag part
  (+ (* (- (* x.re x.re) (* x.im x.im)) ; x^2.re
        x.im)
     (* (+ (* x.re x.im) (* x.im x.re)) ; x^2.im
        x.re)))

; /lib/function/arithmetic/divide.js _divideComplex
(lambda (x.re x.im y.re y.im) ; real part
  (/ (+ (* x.re y.re) (* x.im y.im))
     (+ (* y.re y.re) (* y.im y.im))))
(lambda (x.re x.im y.re y.im) ; imag part
  (/ (- (* x.im y.re) (* x.re y.im))
     (+ (* y.re y.re) (* y.im y.im))))

; /lib/function/arithmetic/exp.js math.exp for complex arguments
(lambda (re im) ; real part
  (* (exp re) (cos im)))
(lambda (re im) ; imag part
  (* (exp re) (sin im)))

; /lib/function/arithmetic/log.js math.log for complex arguments, with one argument
(lambda (re im) ; real part
  (log (sqrt (+ (* re re) (* im im)))))
(lambda (re im) ; imag part
  (atan2 im re))

; /lib/function/arithmetic/log.js math.log for complex arguments, with two arguments
; Manually expanded math.divide
(lambda (re im base) ; real part
  (/ (+ (* (log (sqrt (+ (* re re) (* im im)))) (log base)) (* (atan2 im re) 0))
     (+ (* (log base) (log base)) (* 0 0))))
(lambda (re im base) ; imag part
  (/ (- (* (atan2 im re) (log base)) (* (log (sqrt (+ (* re re) (* im im)))) 0))
     (+ (* (log base) (log base)) (* 0 0))))

; /lib/function/arithmetic/log10.js math.log10 for complex arguments, with one argument
(lambda (re im) ; real part
  (/ (log (sqrt (+ (* re re) (* im im)))) ln10))
(lambda (re im) ; imag part
  (/ (atan2 im re) ln10))

; /lib/function/arithmetic/multiply.js _multiplyComplex
(lambda (x.re x.im y.re y.im) ; real part
  (- (* x.re y.re) (* x.im y.im)))
(lambda (x.re x.im y.re y.im) ; imag part
  (+ (* x.re y.im) (* x.im y.re)))

; /lib/function/arithmetic/pow.js powComplex
; Manually expanded math.log, math.multiply, math.exp
(lambda (x.re x.im y.re y.im) ; real part
  (* (exp (- (* (log (sqrt (+ (* x.re x.re) (* x.im x.im)))) y.re) (* (atan2 x.im x.re) y.im)))
     (cos (+ (* (log (sqrt (+ (* x.re x.re) (* x.im x.im)))) y.im) (* (atan2 x.im x.re) y.re)))))
(lambda (x.re x.im y.re y.im) ; imag part
  (* (exp (- (* (log (sqrt (+ (* x.re x.re) (* x.im x.im)))) y.re) (* (atan2 x.im x.re) y.im)))
     (sin (+ (* (log (sqrt (+ (* x.re x.re) (* x.im x.im)))) y.im) (* (atan2 x.im x.re) y.re)))))

; /lib/function/arithmetic/sqrt.js math.sqrt for complex arguments
(lambda (re im) ; real part
  (* 0.5 (sqrt (* 2.0 (+ (sqrt (+ (* re re) (* im im))) re)))))
(lambda (re im) ; imag part
  (if (> im 0)
      (*  0.5 (sqrt (* 2.0 (+ (sqrt (- (* re re) (* im im))) re))))
      (* -0.5 (sqrt (* 2.0 (+ (sqrt (- (* re re) (* im im))) re))))))

; /lib/function/arithmetic/square.js math.square for complex arguments
; Manually expanded math.multiply
(lambda (re im) ; real part
  (- (* re re) (* im im)))
(lambda (re im) ; imag part
  (+ (* re im) (* im re)))

