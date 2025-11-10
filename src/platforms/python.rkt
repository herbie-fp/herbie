#lang herbie/platform

(require math/flonum)

(define 64bit-move-cost 0.125)
(define boolean-move-cost 0.100)

(define-representation <bool> #:cost boolean-move-cost)
(define-representation <binary64> #:cost 64bit-move-cost)

(define-operations ()
  <bool>
  [true #:spec (TRUE) #:impl (const true) #:fpcore TRUE #:cost boolean-move-cost]
  [false #:spec (FALSE) #:impl (const false) #:fpcore FALSE #:cost boolean-move-cost])

(define-operations ([x <bool>] [y <bool>])
  <bool>
  [and #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost boolean-move-cost]
  [or #:spec (or x y) #:impl (lambda v (ormap values v)) #:cost boolean-move-cost])

(define-operation (not [x <bool>]) <bool> #:spec (not x) #:impl not #:cost boolean-move-cost)

(define-operation (if.py [c <bool>] [t <binary64>] [f <binary64>])
                  <binary64>
                  #:spec (if c t f)
                  #:impl if-impl
                  #:cost (if-cost boolean-move-cost))

(define-operations ([x <binary64>] [y <binary64>])
  <bool>
  [== #:spec (== x y) #:impl = #:cost 64bit-move-cost]
  [!= #:spec (!= x y) #:impl (negate =) #:cost 64bit-move-cost]
  [< #:spec (< x y) #:impl < #:cost 64bit-move-cost]
  [> #:spec (> x y) #:impl > #:cost 64bit-move-cost]
  [<= #:spec (<= x y) #:impl <= #:cost 64bit-move-cost]
  [>= #:spec (>= x y) #:impl >= #:cost 64bit-move-cost])

(define-operations ()
  <binary64>
  [pi #:spec (PI) #:impl (const (flsingle pi)) #:fpcore PI #:cost 64bit-move-cost]
  [e #:spec (E) #:impl (const (flsingle (exp 1))) #:fpcore E #:cost 64bit-move-cost]
  [inf #:spec (INFINITY) #:impl (const +inf.0) #:fpcore INFINITY #:cost 64bit-move-cost]
  [nan #:spec (NAN) #:impl (const +nan.0) #:fpcore NAN #:cost 64bit-move-cost]
  [tau #:spec (* 2 (PI)) #:impl (const (* 2 pi)) #:fpcore (* 2 PI) #:cost 64bit-move-cost])

(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  [+.py #:spec (+ x y) #:impl (compose flsingle +) #:cost 0.200]
  [-.py #:spec (- x y) #:impl (compose flsingle -) #:cost 0.200]
  [*.py #:spec (* x y) #:impl (compose flsingle *) #:cost 0.250]
  [/.py #:spec (/ x y) #:impl (compose flsingle /) #:cost 0.350])

(define-operations ([x <binary64>])
  <binary64>
  [degrees.py #:spec (* x (/ (PI) 180.0)) #:impl radians->degrees #:fpcore (degrees x) #:cost 1]
  [radians.py #:spec (* x (/ 180.0 (PI))) #:impl degrees->radians #:fpcore (radians x) #:cost 1]
  [neg.py #:spec (neg x) #:impl - #:cost 0.125]
  [fabs.py #:spec (fabs x) #:impl (from-libm 'fabs) #:cost 1]
  [sin.py #:spec (sin x) #:impl (from-libm 'sin) #:cost 1]
  [sinh.py #:spec (sinh x) #:impl (from-libm 'sinh) #:cost 1]
  [asin.py #:spec (asin x) #:impl (from-libm 'asin) #:cost 1]
  [asinh.py #:spec (asinh x) #:impl (from-libm 'asinh) #:cost 1]
  [cos.py #:spec (cos x) #:impl (from-libm 'cos) #:cost 1]
  [cosh.py #:spec (cosh x) #:impl (from-libm 'cosh) #:cost 1]
  [acos.py #:spec (acos x) #:impl (from-libm 'acos) #:cost 1]
  [acosh.py #:spec (acosh x) #:impl (from-libm 'acosh) #:cost 1]
  [tan.py #:spec (tan x) #:impl (from-libm 'tan) #:cost 1]
  [atan.py #:spec (atan x) #:impl (from-libm 'atan) #:cost 1]
  [atanh.py #:spec (atanh x) #:impl (from-libm 'atanh) #:cost 1]
  [tanh.py #:spec (tanh x) #:impl (from-libm 'tanh) #:cost 1]
  [sqrt.py #:spec (sqrt x) #:impl (from-libm 'sqrt) #:cost 1]
  [cbrt.py #:spec (cbrt x) #:impl (from-libm 'cbrt) #:cost 1]
  [ceil.py #:spec (ceil x) #:impl (from-libm 'ceil) #:cost 1]
  [floor.py #:spec (floor x) #:impl (from-libm 'floor) #:cost 1]
  [erf.py #:spec (erf x) #:impl (from-libm 'erf) #:cost 1]
  [exp.py #:spec (exp x) #:impl (from-libm 'exp) #:cost 1]
  [exp2.py #:spec (exp2 x) #:impl (from-libm 'exp2) #:cost 1]
  [gamma.py #:spec (tgamma x) #:impl (from-libm 'tgamma) #:cost 1]
  [lgamma.py #:spec (lgamma x) #:impl (from-libm 'lgamma) #:cost 1]
  [log.1var #:spec (log x) #:impl (from-libm 'log) #:cost 1]
  [log10.py #:spec (log10 x) #:impl (from-libm 'log10) #:cost 1]
  [log2.py #:spec (log2 x) #:impl (from-libm 'log2) #:cost 1]
  [rint.py #:spec (rint x) #:impl (from-libm 'rint) #:cost 1]
  [round.py #:spec (round x) #:impl (from-libm 'round) #:cost 1]
  [trunc.py #:spec (trunc x) #:impl (from-libm 'trunc) #:cost 1])

(define-operations ([x <binary64>])
  <binary64>
  [erfc.py #:spec (- 1 (erf x)) #:impl (from-libm 'erfc) #:fpcore (erfc x) #:cost 1]
  [expm1.py #:spec (- (exp x) 1) #:impl (from-libm 'expm1) #:fpcore (expm1 x) #:cost 1]
  [log1p.py #:spec (log (+ 1 x)) #:impl (from-libm 'log1p) #:fpcore (log1p x) #:cost 1])

(define-operations ([x <binary64>] [y <binary64>])
  <binary64>
  [pow.py #:spec (pow x y) #:impl (from-libm 'pow) #:cost 1]
  [atan2.py #:spec (atan2 x y) #:impl (from-libm 'atan2) #:cost 1]
  [copysign.py #:spec (copysign x y) #:impl (from-libm 'copysign) #:cost 1]
  [fdim.py #:spec (fdim x y) #:impl (from-libm 'fdim) #:cost 1]
  [fmax.py #:spec (fmax x y) #:impl (from-libm 'fmax) #:cost 1]
  [fmin.py #:spec (fmin x y) #:impl (from-libm 'fmin) #:cost 1]
  [fmod.py #:spec (fmod x y) #:impl (from-libm 'fmod) #:cost 1]
  [remainder.py #:spec (remainder x y) #:impl (from-libm 'remainder) #:cost 1])

(define-operations ([x <binary64>] [y <binary64>] [w <binary64>])
  <binary64>
  [fsum.3var #:spec (+ (+ x y) w) #:impl (curry apply flsum) #:fpcore (fsum x y w) #:cost 1]
  [prod.3var #:spec (* (* x y) w) #:impl (from-rival) #:fpcore (prod x y w) #:cost 1])

(define-operations ([x <binary64>] [y <binary64>] [w <binary64>] [z <binary64>])
  <binary64>
  [fsum.4var #:spec (+ (+ (+ x y) z) w) #:impl (curry apply flsum) #:fpcore (fsum x y w z) #:cost 1]
  [prod.4var #:spec (* (* (* x y) w) z) #:impl (from-rival) #:fpcore (prod x y w z) #:cost 1])

(define-operation (dist.2D [x <binary64>] [y <binary64>] [z <binary64>] [w <binary64>])
                  <binary64>
                  #:spec (sqrt (+ (pow (- y x) 2) (pow (- w z) 2)))
                  #:impl (from-rival)
                  #:fpcore (dist x y z w)
                  #:cost 1)

(define-operation
 (dist.3D [x <binary64>] [y <binary64>] [z <binary64>] [w <binary64>] [u <binary64>] [v <binary64>])
 <binary64>
 #:spec (sqrt (+ (+ (pow (- y x) 2) (pow (- w z) 2)) (pow (- v u) 2)))
 #:impl (from-rival)
 #:fpcore (dist x y z w u v)
 #:cost 1)

(define-operation (sumprod.2D [x <binary64>] [y <binary64>] [z <binary64>] [w <binary64>])
                  <binary64>
                  #:spec (+ (* x y) (* z w))
                  #:impl (from-rival)
                  #:fpcore (sumprod x y z w)
                  #:cost 1)

(define-operation (sumprod.3D [x <binary64>]
                              [y <binary64>]
                              [z <binary64>]
                              [w <binary64>]
                              [u <binary64>]
                              [v <binary64>])
                  <binary64>
                  #:spec (+ (+ (* x y) (* z w)) (* u v))
                  #:impl (from-rival)
                  #:fpcore (sumprod x y z w u v)
                  #:cost 1)

(define-operation (hypot.2D [x <binary64>] [y <binary64>])
                  <binary64>
                  #:spec (sqrt (+ (* x x) (* y y)))
                  #:impl (from-libm 'hypot)
                  #:fpcore (hypot x y)
                  #:cost 1)

(define-operation (hypot.3D [x <binary64>] [y <binary64>] [z <binary64>])
                  <binary64>
                  #:spec (sqrt (+ (+ (* x x) (* y y)) (* z z)))
                  #:impl (from-rival)
                  #:fpcore (hypot x y z)
                  #:cost 1)

(define-operation (fma.py [x <binary64>] [y <binary64>] [z <binary64>])
                  <binary64>
                  #:spec (+ (* x y) z)
                  #:impl (from-libm 'fma)
                  #:fpcore (fma x y z)
                  #:cost 1)
