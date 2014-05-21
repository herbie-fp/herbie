#lang racket
(require casio/test)

; All code examples from mathjs
; https://github.com/josdejong/mathjs/blob/master/

(casio-bench (re im)
  "/lib/function/arithmetic/abs.js math.abs for complex arguments"
  (sqrt (+ (* re re) (* im im))))

(casio-test (x)
  "math.cube on real"
  (* (* x x) x)
  (expt x 3))

; Manually expanded math.multiply
(casio-bench (re im) ; real part
  "/lib/function/arithmetic/cube.js math.cube for complex arguments, real part"
  (- (* (- (* x.re x.re) (* x.im x.im)) ; x^2.re
        x.re)
     (* (+ (* x.re x.im) (* x.im x.re)) ; x^2.im
        x.im)))

(casio-bench (re im) ; imag part
  "/lib/function/arithmetic/cube.js math.cube for complex arguments, imaginary part"
  (+ (* (- (* x.re x.re) (* x.im x.im)) ; x^2.re
        x.im)
     (* (+ (* x.re x.im) (* x.im x.re)) ; x^2.im
        x.re)))

(casio-bench (x.re x.im y.re y.im) ; real part
  "/lib/function/arithmetic/divide.js _divideComplex, real part"
  (/ (+ (* x.re y.re) (* x.im y.im))
     (+ (* y.re y.re) (* y.im y.im))))
(casio-bench (x.re x.im y.re y.im) ; imag part
  "/lib/function/arithmetic/divide.js _divideComplex, imaginary part"
  (/ (- (* x.im y.re) (* x.re y.im))
     (+ (* y.re y.re) (* y.im y.im))))

(casio-bench (re im) ; real part
  "/lib/function/arithmetic/exp.js math.exp for complex arguments, real part"
  (* (exp re) (cos im)))
(casio-bench (re im) ; imag part
  "/lib/function/arithmetic/exp.js math.exp for complex arguments, imaginary part"
  (* (exp re) (sin im)))

(casio-bench (re im) ; real part
  "/lib/function/arithmetic/log.js math.log for complex arguments, with one argument, real part"
  (log (sqrt (+ (* re re) (* im im)))))
(casio-bench (re im) ; imag part
  "/lib/function/arithmetic/log.js math.log for complex arguments, with one argument, imaginary part"
  (atan2 im re))

; Manually expanded math.divide
(casio-bench (re im base) ; real part
  "/lib/function/arithmetic/log.js math.log for complex arguments, with two arguments, real part"
  (/ (+ (* (log (sqrt (+ (* re re) (* im im)))) (log base)) (* (atan2 im re) 0))
     (+ (* (log base) (log base)) (* 0 0))))
(casio-bench (re im base) ; imag part
  "/lib/function/arithmetic/log.js math.log for complex arguments, with two arguments, imaginary part"
  (/ (- (* (atan2 im re) (log base)) (* (log (sqrt (+ (* re re) (* im im)))) 0))
     (+ (* (log base) (log base)) (* 0 0))))

(casio-bench (re im) ; real part
  "/lib/function/arithmetic/log10.js math.log10 for complex arguments, with one argument, real part"
  (/ (log (sqrt (+ (* re re) (* im im)))) ln10))
(casio-bench (re im) ; imag part
  "/lib/function/arithmetic/log10.js math.log10 for complex arguments, with one argument, imaginary part"
  (/ (atan2 im re) ln10))

(casio-bench (x.re x.im y.re y.im) ; real part
  "/lib/function/arithmetic/multiply.js _multiplyComplex, real part"
  (- (* x.re y.re) (* x.im y.im)))
(casio-bench (x.re x.im y.re y.im) ; imag part
  "/lib/function/arithmetic/multiply.js _multiplyComplex, imaginary part"
  (+ (* x.re y.im) (* x.im y.re)))

; Manually expanded math.log, math.multiply, math.exp
(casio-bench (x.re x.im y.re y.im) ; real part
  "/lib/function/arithmetic/pow.js powComplex, real part"
  (* (exp (- (* (log (sqrt (+ (* x.re x.re) (* x.im x.im)))) y.re) (* (atan2 x.im x.re) y.im)))
     (cos (+ (* (log (sqrt (+ (* x.re x.re) (* x.im x.im)))) y.im) (* (atan2 x.im x.re) y.re)))))
(casio-bench (x.re x.im y.re y.im) ; imag part
  "/lib/function/arithmetic/pow.js powComplex, imaginary part"
  (* (exp (- (* (log (sqrt (+ (* x.re x.re) (* x.im x.im)))) y.re) (* (atan2 x.im x.re) y.im)))
     (sin (+ (* (log (sqrt (+ (* x.re x.re) (* x.im x.im)))) y.im) (* (atan2 x.im x.re) y.re)))))

(casio-bench (re im) ; real part
  "/lib/function/arithmetic/sqrt.js math.sqrt for complex arguments, real part"
  (* 0.5 (sqrt (* 2.0 (+ (sqrt (+ (* re re) (* im im))) re)))))
(casio-bench (re im) ; imag part
  "/lib/function/arithmetic/sqrt.js math.sqrt for complex arguments, imaginary part"
  (if (> im 0)
      (*  0.5 (sqrt (* 2.0 (+ (sqrt (- (* re re) (* im im))) re))))
      (* -0.5 (sqrt (* 2.0 (+ (sqrt (- (* re re) (* im im))) re))))))

; Manually expanded math.multiply
(casio-bench (re im) ; real part
  "/lib/function/arithmetic/square.js math.square for complex arguments, real part"
  (- (* re re) (* im im)))
(casio-bench (re im) ; imag part
  "/lib/function/arithmetic/square.js math.square for complex arguments, imaginary part"
  (+ (* re im) (* im re)))

