#lang s-exp "../syntax/platform-language.rkt"

;; Julia platform

(require math/flonum)

(define 64bit-move-cost 1.000)
(define 32bit-move-cost 1.000)
(define boolean-move-cost 0.100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost boolean-move-cost)

(define-operations ()
  <bool>
  [TRUE #:spec (TRUE) #:impl (const true) #:fpcore TRUE #:cost boolean-move-cost]
  [FALSE #:spec (FALSE) #:impl (const false) #:fpcore FALSE #:cost boolean-move-cost])

(define-operations ([x <bool>] [y <bool>])
  <bool>
  [and #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost boolean-move-cost]
  [or #:spec (or x y) #:impl (lambda v (ormap values v)) #:cost boolean-move-cost])

(define-operation (not [x <bool>]) <bool> #:spec (not x) #:impl not #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary32> #:cost 32bit-move-cost)

(define-operation (if.f32 [c <bool>] [t <binary32>] [f <binary32>])
                  <binary32>
                  #:spec (if c t f)
                  #:impl if-impl
                  #:cost (if-cost boolean-move-cost))

(define-operation (neg.f32 [x <binary32>])
                  <binary32>
                  #:spec (neg x)
                  #:impl (compose flsingle -)
                  #:fpcore (! :precision binary32 (- x))
                  #:cost 1.0)

;; Comparison operations
(define-operations ([x <binary32>] [y <binary32>])
  <bool>
  [==.f32 #:spec (== x y) #:impl = #:cost boolean-move-cost]
  [!=.f32 #:spec (!= x y) #:impl (negate =) #:cost boolean-move-cost]
  [<.f32 #:spec (< x y) #:impl < #:cost boolean-move-cost]
  [>.f32 #:spec (> x y) #:impl > #:cost boolean-move-cost]
  [<=.f32 #:spec (<= x y) #:impl <= #:cost boolean-move-cost]
  [>=.f32 #:spec (>= x y) #:impl >= #:cost boolean-move-cost])

;; Constants
(define-operations ()
  <binary32>
  #:fpcore (! :precision binary32 _)
  [PI.f32 #:spec (PI) #:impl (const (flsingle pi)) #:fpcore PI #:cost 32bit-move-cost]
  [E.f32 #:spec (E) #:impl (const (flsingle (exp 1))) #:fpcore E #:cost 32bit-move-cost]
  [INFINITY.f32 #:spec (INFINITY) #:impl (const +inf.0) #:fpcore INFINITY #:cost 32bit-move-cost]
  [NAN.f32 #:spec (NAN) #:impl (const +nan.0) #:fpcore NAN #:cost 32bit-move-cost])

;; Binary operations
(define-operations ([x <binary32>] [y <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [+.f32 #:spec (+ x y) #:impl (compose flsingle +) #:cost 1.0]
  [-.f32 #:spec (- x y) #:impl (compose flsingle -) #:cost 1.0]
  [*.f32 #:spec (* x y) #:impl (compose flsingle *) #:cost 1.0]
  [/.f32 #:spec (/ x y) #:impl (compose flsingle /) #:cost 1.0]
  [fmax.f32 #:spec (fmax x y) #:impl (from-libm 'fmaxf) #:cost 1.0]
  [fmin.f32 #:spec (fmin x y) #:impl (from-libm 'fminf) #:cost 1.0]
  [hypot.f32
   #:spec (sqrt (+ (* x x) (* y y)))
   #:impl (from-libm 'hypotf)
   #:cost 4.0
   #:fpcore (hypot x y)]
  [pow.f32 #:spec (pow x y) #:impl (from-libm 'powf) #:cost 15.5]
  [copysign.f32 #:spec (copysign x y) #:impl (from-libm 'copysignf) #:cost 1.0])

;; Unary operations
(define-operations ([x <binary32>])
  <binary32>
  #:fpcore (! :precision binary32 _)
  [fabs.f32 #:spec (fabs x) #:impl (from-libm 'fabsf) #:cost 1.0]
  [sqrt.f32 #:spec (sqrt x) #:impl (from-libm 'sqrtf) #:cost 1.0]
  [exp.f32 #:spec (exp x) #:impl (from-libm 'expf) #:cost 3.5]
  [exp2.f32 #:spec (exp2 x) #:impl (from-libm 'exp2f) #:cost 3.5]
  [exp10.f32 #:spec (pow 10 x) #:impl (from-rival) #:cost 3.5]
  [log.f32 #:spec (log x) #:impl (from-libm 'logf) #:cost 4.5]
  [log10.f32 #:spec (log10 x) #:impl (from-libm 'log10f) #:cost 5.0]
  [log2.f32 #:spec (log2 x) #:impl (from-libm 'log2f) #:cost 5.5]
  [sin.f32 #:spec (sin x) #:impl (from-libm 'sinf) #:cost 3.5]
  [cos.f32 #:spec (cos x) #:impl (from-libm 'cosf) #:cost 4.0]
  [tan.f32 #:spec (tan x) #:impl (from-libm 'tanf) #:cost 6.0]
  [sinh.f32 #:spec (sinh x) #:impl (from-libm 'sinhf) #:cost 4.5]
  [cosh.f32 #:spec (cosh x) #:impl (from-libm 'coshf) #:cost 4.5]
  [tanh.f32 #:spec (tanh x) #:impl (from-libm 'tanhf) #:cost 5.0]
  [asin.f32 #:spec (asin x) #:impl (from-libm 'asinf) #:cost 4.0]
  [acos.f32 #:spec (acos x) #:impl (from-libm 'acosf) #:cost 4.0]
  [atan.f32 #:spec (atan x) #:impl (from-libm 'atanf) #:cost 6.0]
  [atanh.f32 #:spec (atanh x) #:impl (from-libm 'atanhf) #:cost 6.5]
  [asinh.f32 #:spec (asinh x) #:impl (from-libm 'asinhf) #:cost 9.5]
  [acosh.f32 #:spec (acosh x) #:impl (from-libm 'acoshf) #:cost 7.5]
  [cbrt.f32 #:spec (cbrt x) #:impl (from-libm 'cbrtf) #:cost 4.5]
  [log1p.f32 #:spec (log (+ 1 x)) #:impl (from-libm 'log1pf) #:cost 5.5 #:fpcore (log1p x)]
  [expm1.f32 #:spec (- (exp x) 1) #:impl (from-libm 'expm1f) #:cost 5.5 #:fpcore (expm1 x)]
  [deg2rad.f32 #:spec (* x (/ (PI) 180)) #:impl (from-rival) #:cost 1.0 #:fpcore (deg2rad x)]
  [rad2deg.f32 #:spec (* x (/ 180 (PI))) #:impl (from-rival) #:cost 1.0 #:fpcore (rad2deg x)]
  [abs2.f32 #:spec (* (fabs x) (fabs x)) #:impl (from-rival) #:cost 1.0 #:fpcore (abs2 x)]
  [sec.f32 #:spec (/ 1 (cos x)) #:impl (from-rival) #:cost 4.0 #:fpcore (sec x)]
  [csc.f32 #:spec (/ 1 (sin x)) #:impl (from-rival) #:cost 3.5 #:fpcore (csc x)]
  [cot.f32 #:spec (/ 1 (tan x)) #:impl (from-rival) #:cost 7.5 #:fpcore (cot x)]
  [sech.f32 #:spec (/ 1 (cosh x)) #:impl (from-rival) #:cost 8.0 #:fpcore (sech x)]
  [csch.f32 #:spec (/ 1 (sinh x)) #:impl (from-rival) #:cost 5.0 #:fpcore (csch x)]
  [coth.f32 #:spec (/ (cosh x) (sinh x)) #:impl (from-rival) #:cost 8.0 #:fpcore (coth x)]
  [asec.f32 #:spec (acos (/ 1 x)) #:impl (from-rival) #:cost 4.5 #:fpcore (asec x)]
  [acsc.f32 #:spec (asin (/ 1 x)) #:impl (from-rival) #:cost 6.5 #:fpcore (acsc x)]
  [acot.f32 #:spec (atan (/ 1 x)) #:impl (from-rival) #:cost 7.5 #:fpcore (acot x)]
  [asech.f32 #:spec (acosh (/ 1 x)) #:impl (from-rival) #:cost 8.5 #:fpcore (asech x)]
  [acsch.f32 #:spec (asinh (/ 1 x)) #:impl (from-rival) #:cost 10.0 #:fpcore (acsch x)]
  [acoth.f32 #:spec (atanh (/ 1 x)) #:impl (from-rival) #:cost 7.0 #:fpcore (acoth x)]
  [sind.f32 #:spec (sin (* x (/ (PI) 180))) #:impl (from-rival) #:cost 6.0 #:fpcore (sind x)]
  [cosd.f32 #:spec (cos (* x (/ (PI) 180))) #:impl (from-rival) #:cost 6.5 #:fpcore (cosd x)]
  [tand.f32 #:spec (tan (* x (/ (PI) 180))) #:impl (from-rival) #:cost 13.0 #:fpcore (tand x)]
  [cotd.f32 #:spec (/ 1 (tan (* x (/ (PI) 180)))) #:impl (from-rival) #:cost 14.0 #:fpcore (cotd x)]
  [asind.f32 #:spec (* (asin x) (/ 180 (PI))) #:impl (from-rival) #:cost 4.5 #:fpcore (asind x)]
  [acosd.f32 #:spec (* (acos x) (/ 180 (PI))) #:impl (from-rival) #:cost 4.0 #:fpcore (acosd x)]
  [atand.f32 #:spec (* (atan x) (/ 180 (PI))) #:impl (from-rival) #:cost 6.0 #:fpcore (atand x)]
  [acotd.f32 #:spec (* (atan (/ 1 x)) (/ 180 (PI))) #:impl (from-rival) #:cost 8.0 #:fpcore (acotd x)]
  [asecd.f32 #:spec (* (acos (/ 1 x)) (/ 180 (PI))) #:impl (from-rival) #:cost 5.0 #:fpcore (asecd x)]
  [acscd.f32 #:spec (* (asin (/ 1 x)) (/ 180 (PI))) #:impl (from-rival) #:cost 6.5 #:fpcore (acscd x)]
  [secd.f32 #:spec (/ 1 (cos (* x (/ (PI) 180)))) #:impl (from-rival) #:cost 8.0 #:fpcore (secd x)]
  [cscd.f32 #:spec (/ 1 (sin (* x (/ (PI) 180)))) #:impl (from-rival) #:cost 6.5 #:fpcore (cscd x)]
  [sinpi.f32
   #:spec (sin (* (* x (PI)) (/ (PI) 180)))
   #:impl (from-rival)
   #:cost 5.0
   #:fpcore (sinpi x)]
  [cospi.f32
   #:spec (cos (* (* x (PI)) (/ (PI) 180)))
   #:impl (from-rival)
   #:cost 5.0
   #:fpcore (cospi x)])

;; Ternary fused multiply-add operation
(define-operation (fma.f32 [x <binary32>] [y <binary32>] [z <binary32>])
                  <binary32>
                  #:spec (+ (* x y) z)
                  #:impl (from-libm 'fmaf)
                  #:fpcore (! :precision binary32 (fma x y z))
                  #:cost 1.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost 64bit-move-cost)

(define-operation (if.f64 [c <bool>] [t <binary64>] [f <binary64>])
                  <binary64>
                  #:spec (if c t f)
                  #:impl if-impl
                  #:cost (if-cost boolean-move-cost))

(define-operation (neg.f64 [x <binary64>])
                  <binary64>
                  #:spec (neg x)
                  #:impl -
                  #:fpcore (! :precision binary64 (- x))
                  #:cost 1.0)

;; Comparison operations
(define-operations ([x <binary64>] [y <binary64>])
  <bool>
  [==.f64 #:spec (== x y) #:impl = #:cost boolean-move-cost]
  [!=.f64 #:spec (!= x y) #:impl (negate =) #:cost boolean-move-cost]
  [<.f64 #:spec (< x y) #:impl < #:cost boolean-move-cost]
  [>.f64 #:spec (> x y) #:impl > #:cost boolean-move-cost]
  [<=.f64 #:spec (<= x y) #:impl <= #:cost boolean-move-cost]
  [>=.f64 #:spec (>= x y) #:impl >= #:cost boolean-move-cost])

;; Constants
(define-operations ()
  <binary64>
  #:fpcore (! :precision binary64 _)
  [PI.f64 #:spec (PI) #:impl (const pi) #:fpcore PI #:cost 64bit-move-cost]
  [E.f64 #:spec (E) #:impl (const (exp 1)) #:fpcore E #:cost 64bit-move-cost]
  [INFINITY #:spec (INFINITY) #:impl (const +inf.0) #:fpcore INFINITY #:cost 64bit-move-cost]
  [NAN.f64 #:spec (NAN) #:impl (const +nan.0) #:fpcore NAN #:cost 64bit-move-cost])

;; Binary operations
(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [+.f64 #:spec (+ x y) #:impl + #:cost 1.0]
  [-.f64 #:spec (- x y) #:impl - #:cost 1.0]
  [*.f64 #:spec (* x y) #:impl * #:cost 1.0]
  [/.f64 #:spec (/ x y) #:impl / #:cost 1.0]
  [fmax.f64 #:spec (fmax x y) #:impl (from-libm 'fmax) #:cost 1.0]
  [fmin.f64 #:spec (fmin x y) #:impl (from-libm 'fmin) #:cost 1.0]
  [hypot.f64
   #:spec (sqrt (+ (* x x) (* y y)))
   #:impl (from-libm 'hypot)
   #:cost 4.0
   #:fpcore (hypot x y)]
  [pow.f64 #:spec (pow x y) #:impl (from-libm 'pow) #:cost 15.5]
  [copysign.f64 #:spec (copysign x y) #:impl (from-libm 'copysign) #:cost 1.0])

;; Unary operations
(define-operations ([x <binary64>])
  <binary64>
  #:fpcore (! :precision binary64 _)
  [fabs.f64 #:spec (fabs x) #:impl (from-libm 'fabs) #:cost 1.0]
  [sqrt.f64 #:spec (sqrt x) #:impl (from-libm 'sqrt) #:cost 1.0]
  [exp.f64 #:spec (exp x) #:impl (from-libm 'exp) #:cost 3.5]
  [exp2.f64 #:spec (exp2 x) #:impl (from-libm 'exp2) #:cost 3.5]
  [exp10.f64 #:spec (pow 10 x) #:impl (from-rival) #:cost 3.5]
  [log.f64 #:spec (log x) #:impl (from-libm 'log) #:cost 4.5]
  [log10.f64 #:spec (log10 x) #:impl (from-libm 'log10) #:cost 5.0]
  [log2.f64 #:spec (log2 x) #:impl (from-libm 'log2) #:cost 5.5]
  [sin.f64 #:spec (sin x) #:impl (from-libm 'sin) #:cost 3.5]
  [cos.f64 #:spec (cos x) #:impl (from-libm 'cos) #:cost 4.0]
  [tan.f64 #:spec (tan x) #:impl (from-libm 'tan) #:cost 6.0]
  [sinh.f64 #:spec (sinh x) #:impl (from-libm 'sinh) #:cost 4.5]
  [cosh.f64 #:spec (cosh x) #:impl (from-libm 'cosh) #:cost 4.5]
  [tanh.f64 #:spec (tanh x) #:impl (from-libm 'tanh) #:cost 5.0]
  [asin.f64 #:spec (asin x) #:impl (from-libm 'asin) #:cost 4.0]
  [acos.f64 #:spec (acos x) #:impl (from-libm 'acos) #:cost 4.0]
  [atan.f64 #:spec (atan x) #:impl (from-libm 'atan) #:cost 6.0]
  [atanh.f64 #:spec (atanh x) #:impl (from-libm 'atanh) #:cost 6.5]
  [asinh.f64 #:spec (asinh x) #:impl (from-libm 'asinh) #:cost 9.5]
  [acosh.f64 #:spec (acosh x) #:impl (from-libm 'acosh) #:cost 7.5]
  [cbrt.f64 #:spec (cbrt x) #:impl (from-libm 'cbrt) #:cost 4.5]
  [log1p.f64 #:spec (log (+ 1 x)) #:impl (from-libm 'log1p) #:cost 5.5 #:fpcore (log1p x)]
  [expm1.f64 #:spec (- (exp x) 1) #:impl (from-libm 'expm1) #:cost 5.5 #:fpcore (expm1 x)]
  [deg2rad.f64 #:spec (* x (/ (PI) 180)) #:impl (from-rival) #:cost 1.0 #:fpcore (deg2rad x)]
  [rad2deg.f64 #:spec (* x (/ 180 (PI))) #:impl (from-rival) #:cost 1.0 #:fpcore (rad2deg x)]
  [abs2.f64 #:spec (* (fabs x) (fabs x)) #:impl (from-rival) #:cost 1.0 #:fpcore (abs2 x)]
  [sec.f64 #:spec (/ 1 (cos x)) #:impl (from-rival) #:cost 4.0 #:fpcore (sec x)]
  [csc.f64 #:spec (/ 1 (sin x)) #:impl (from-rival) #:cost 3.5 #:fpcore (csc x)]
  [cot.f64 #:spec (/ 1 (tan x)) #:impl (from-rival) #:cost 7.5 #:fpcore (cot x)]
  [sech.f64 #:spec (/ 1 (cosh x)) #:impl (from-rival) #:cost 8.0 #:fpcore (sech x)]
  [csch.f64 #:spec (/ 1 (sinh x)) #:impl (from-rival) #:cost 5.0 #:fpcore (csch x)]
  [coth.f64 #:spec (/ (cosh x) (sinh x)) #:impl (from-rival) #:cost 8.0 #:fpcore (coth x)]
  [asec.f64 #:spec (acos (/ 1 x)) #:impl (from-rival) #:cost 4.5 #:fpcore (asec x)]
  [acsc.f64 #:spec (asin (/ 1 x)) #:impl (from-rival) #:cost 6.5 #:fpcore (acsc x)]
  [acot.f64 #:spec (atan (/ 1 x)) #:impl (from-rival) #:cost 7.5 #:fpcore (acot x)]
  [asech.f64 #:spec (acosh (/ 1 x)) #:impl (from-rival) #:cost 8.5 #:fpcore (asech x)]
  [acsch.f64 #:spec (asinh (/ 1 x)) #:impl (from-rival) #:cost 10.0 #:fpcore (acsch x)]
  [acoth.f64 #:spec (atanh (/ 1 x)) #:impl (from-rival) #:cost 7.0 #:fpcore (acoth x)]
  [sind.f64 #:spec (sin (* x (/ (PI) 180))) #:impl (from-rival) #:cost 6.0 #:fpcore (sind x)]
  [cosd.f64 #:spec (cos (* x (/ (PI) 180))) #:impl (from-rival) #:cost 6.5 #:fpcore (cosd x)]
  [tand.f64 #:spec (tan (* x (/ (PI) 180))) #:impl (from-rival) #:cost 13.0 #:fpcore (tand x)]
  [cotd.f64 #:spec (/ 1 (tan (* x (/ (PI) 180)))) #:impl (from-rival) #:cost 14.0 #:fpcore (cotd x)]
  [asind.f64 #:spec (* (asin x) (/ 180 (PI))) #:impl (from-rival) #:cost 4.5 #:fpcore (asind x)]
  [acosd.f64 #:spec (* (acos x) (/ 180 (PI))) #:impl (from-rival) #:cost 4.0 #:fpcore (acosd x)]
  [atand.f64 #:spec (* (atan x) (/ 180 (PI))) #:impl (from-rival) #:cost 6.0 #:fpcore (atand x)]
  [acotd.f64 #:spec (* (atan (/ 1 x)) (/ 180 (PI))) #:impl (from-rival) #:cost 8.0 #:fpcore (acotd x)]
  [asecd.f64 #:spec (* (acos (/ 1 x)) (/ 180 (PI))) #:impl (from-rival) #:cost 5.0 #:fpcore (asecd x)]
  [acscd.f64 #:spec (* (asin (/ 1 x)) (/ 180 (PI))) #:impl (from-rival) #:cost 6.5 #:fpcore (acscd x)]
  [secd.f64 #:spec (/ 1 (cos (* x (/ (PI) 180)))) #:impl (from-rival) #:cost 8.0 #:fpcore (secd x)]
  [cscd.f64 #:spec (/ 1 (sin (* x (/ (PI) 180)))) #:impl (from-rival) #:cost 6.5 #:fpcore (cscd x)]
  [sinpi.f64
   #:spec (sin (* (* x (PI)) (/ (PI) 180)))
   #:impl (from-rival)
   #:cost 5.0
   #:fpcore (sinpi x)]
  [cospi.f64
   #:spec (cos (* (* x (PI)) (/ (PI) 180)))
   #:impl (from-rival)
   #:cost 5.0
   #:fpcore (cospi x)])

;; Ternary fused multiply-add operation
(define-operation (fma.f64 [x <binary64>] [y <binary64>] [z <binary64>])
                  <binary64>
                  #:spec (+ (* x y) z)
                  #:impl (from-libm 'fma)
                  #:fpcore (! :precision binary64 (fma x y z))
                  #:cost 1.0)
