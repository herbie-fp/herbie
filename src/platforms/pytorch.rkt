#lang s-exp "../syntax/platform-language.rkt"

;; PyTorch platform

(require math/flonum)

(define 64bit-move-cost   1.25)
(define 32bit-move-cost   1.0)
(define boolean-move-cost 0.75)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost boolean-move-cost)

(define-operations () <bool>
    [true  #:spec (TRUE)  #:impl (const true)  #:fpcore TRUE  #:cost boolean-move-cost]
    [false #:spec (FALSE) #:impl (const false) #:fpcore FALSE #:cost boolean-move-cost])

(define-operations ([x <bool>] [y <bool>]) <bool>
    [and #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost 1.50]
    [or  #:spec (or x y)  #:impl (lambda v (ormap values v))  #:cost 1.25])

(define-operation (not [x <bool>]) <bool>
    #:spec (not x) #:impl not #:cost 1.25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary32> #:cost 32bit-move-cost)

(define-operation (where.f32 [c <bool>] [t <binary32>] [f <binary32>]) <binary32>
    #:spec (if c t f) #:impl if-impl #:cost 2.5)

(define-operation (neg.f32 [x <binary32>]) <binary32>
    #:spec (neg x) #:impl (compose flsingle -)
    #:fpcore (! :precision binary32 (- x)) #:cost 1.25)

(define-operations ([x <binary32>] [y <binary32>]) <bool>
    [eq.f32 #:spec (== x y) #:impl =          #:cost 2]
    [ne.f32 #:spec (!= x y) #:impl (negate =) #:cost 2]
    [lt.f32 #:spec (< x y)  #:impl <          #:cost 2]
    [gt.f32 #:spec (> x y)  #:impl >          #:cost 2]
    [le.f32 #:spec (<= x y) #:impl <=         #:cost 2]
    [ge.f32 #:spec (>= x y) #:impl >=         #:cost 2])

(define-operations () <binary32> #:fpcore (! :precision binary32 _)
    [PI.f32       #:spec (PI)       #:impl (const (flsingle pi))      #:fpcore PI       #:cost 32bit-move-cost]
    [E.f32        #:spec (E)        #:impl (const (flsingle (exp 1))) #:fpcore E        #:cost 32bit-move-cost]
    [INFINITY.f32 #:spec (INFINITY) #:impl (const +inf.0)             #:fpcore INFINITY #:cost 32bit-move-cost]
    [NAN.f32      #:spec (NAN)      #:impl (const +nan.0)             #:fpcore NAN      #:cost 32bit-move-cost])

(define-operations ([x <binary32>] [y <binary32>]) <binary32> #:fpcore (! :precision binary32 _)
    [add.f32      #:spec (+ x y)        #:impl (compose flsingle +)   #:cost 1.25]
    [sub.f32      #:spec (- x y)        #:impl (compose flsingle -)   #:cost 1.5]
    [mul.f32      #:spec (* x y)        #:impl (compose flsingle *)   #:cost 1.25]
    [div.f32      #:spec (/ x y)        #:impl (compose flsingle /)   #:cost 1.5]
    [pow.f32      #:spec (pow x y)      #:impl (from-libm 'powf)      #:cost 15.5]
    [copysign.f32 #:spec (copysign x y) #:impl (from-libm 'copysignf) #:cost 1.75]
    [fmax.f32     #:spec (fmax x y)     #:impl (from-libm 'fmaxf)     #:cost 2]
    [fmin.f32     #:spec (fmin x y)     #:impl (from-libm 'fminf)     #:cost 2]
    [fmod.f32     #:spec (fmod x y)     #:impl (from-libm 'fmodf)     #:cost 4.25])

(define-operation (atan2.f32 [y <binary32>] [x <binary32>]) <binary32>
    #:spec (atan2 y x) #:impl (from-libm 'atan2f)
    #:fpcore (! :precision binary32 (atan2 y x)) #:cost 7.5)

;; Derived binary operations
(define-operations ([x <binary32>] [y <binary32>]) <binary32>
    [logaddexp.f32    #:spec (log (+ (exp x) (exp y)))     #:impl (from-rival) #:cost 9  #:fpcore (! :precision binary32 (logaddexp x y))]
    [logaddexp2.f32   #:spec (log2 (+ (exp2 x) (exp2 y)))  #:impl (from-rival) #:cost 8  #:fpcore (! :precision binary32 (logaddexp2 x y))]
    [xlogy.f32        #:spec (if (== x 0) 0 (* x (log y))) #:impl (from-rival) #:cost 6.25  #:fpcore (! :precision binary32 (xlogy x y))]
    [floor_divide.f32 #:spec (floor (/ x y))               #:impl (from-rival) #:cost 6.75  #:fpcore (! :precision binary32 (floor_divide x y))]
    [remainder.f32    #:spec (- x (* y (floor (/ x y))))   #:impl (from-rival) #:cost 4.75  #:fpcore (! :precision binary32 (remainder x y))])

(define-operations ([x <binary32>]) <binary32> #:fpcore (! :precision binary32 _)
    [abs.f32    #:spec (fabs x)   #:impl (from-libm 'fabsf)          #:cost 1.5]
    [acos.f32   #:spec (acos x)   #:impl (from-libm 'acosf)          #:cost 5]
    [acosh.f32  #:spec (acosh x)  #:impl (from-libm 'acoshf)         #:cost 5.5]
    [asin.f32   #:spec (asin x)   #:impl (from-libm 'asinf)          #:cost 4]
    [asinh.f32  #:spec (asinh x)  #:impl (from-libm 'asinhf)         #:cost 6.5]
    [atan.f32   #:spec (atan x)   #:impl (from-libm 'atanf)          #:cost 5.5]
    [atanh.f32  #:spec (atanh x)  #:impl (from-libm 'atanhf)         #:cost 7]
    [ceil.f32   #:spec (ceil x)   #:impl (from-libm 'ceilf)          #:cost 1.25]
    [cos.f32    #:spec (cos x)    #:impl (from-libm 'cosf)           #:cost 2.75]
    [cosh.f32   #:spec (cosh x)   #:impl (from-libm 'coshf)          #:cost 4.75]
    [erf.f32    #:spec (erf x)    #:impl (from-libm 'erff)           #:cost 2.75]
    [exp.f32    #:spec (exp x)    #:impl (from-libm 'expf)           #:cost 3]
    [exp2.f32   #:spec (exp2 x)   #:impl (from-libm 'exp2f)          #:cost 2.5]
    [floor.f32  #:spec (floor x)  #:impl (from-libm 'floorf)         #:cost 1.25]
    [lgamma.f32 #:spec (lgamma x) #:impl (from-libm 'lgammaf)        #:cost 29.25]
    [log.f32    #:spec (log x)    #:impl (from-libm 'logf)           #:cost 4.5]
    [log10.f32  #:spec (log10 x)  #:impl (from-libm 'log10f)         #:cost 4.75]
    [log2.f32   #:spec (log2 x)   #:impl (from-libm 'log2f)          #:cost 4.75]
    [round.f32  #:spec (round x)  #:impl (compose flsingle round)    #:cost 1.25]
    [sin.f32    #:spec (sin x)    #:impl (from-libm 'sinf)           #:cost 2.5]
    [sinh.f32   #:spec (sinh x)   #:impl (from-libm 'sinhf)          #:cost 4.25]
    [sqrt.f32   #:spec (sqrt x)   #:impl (from-libm 'sqrtf)          #:cost 1.5]
    [tan.f32    #:spec (tan x)    #:impl (from-libm 'tanf)           #:cost 5.75]
    [tanh.f32   #:spec (tanh x)   #:impl (from-libm 'tanhf)          #:cost 7.5]
    [trunc.f32  #:spec (trunc x)  #:impl (from-libm 'truncf)         #:cost 1.25])

(define-operations ([x <binary32>]) <binary32>
    [rsqrt.f32      #:spec (/ 1 (sqrt x))                                      #:impl (from-rival)                               #:cost 1.5  #:fpcore (! :precision binary32 (rsqrt x))]
    [square.f32     #:spec (* x x)                                             #:impl (compose flsingle sqr)                     #:cost 1.5  #:fpcore (! :precision binary32 (sqr x))]
    [reciprocal.f32 #:spec (/ 1 x)                                             #:impl (compose flsingle (lambda (x) (/ 1.0 x)))  #:cost 1.25  #:fpcore (! :precision binary32 (reciprocal x))]
    [sinc.f32       #:spec (if (== x 0) 1 (/ (sin (* (PI) x)) (* (PI) x)))     #:impl (from-rival)                               #:cost 7  #:fpcore (! :precision binary32 (sinc x))]
    [sigmoid.f32    #:spec (/ 1 (+ 1 (exp (neg x))))                           #:impl (from-rival)                               #:cost 3.25  #:fpcore (! :precision binary32 (sigmoid x))]
    [frac.f32       #:spec (- x (trunc x))                                     #:impl (from-rival)                               #:cost 1.25  #:fpcore (! :precision binary32 (frac x))]
    [sign.f32       #:spec (if (< x 0) -1 (if (> x 0) 1 0))                    #:impl (from-rival)                               #:cost 1.25  #:fpcore (! :precision binary32 (sign x))]
    [rad2deg.f32    #:spec (* x (/ 180 (PI)))                                  #:impl (from-rival)                               #:cost 2.75  #:fpcore (! :precision binary32 (rad2deg x))]
    [deg2rad.f32    #:spec (* x (/ (PI) 180))                                  #:impl (from-rival)                               #:cost 2.75  #:fpcore (! :precision binary32 (deg2rad x))]
    [erfc.f32       #:spec (- 1 (erf x))                                       #:impl (from-libm 'erfcf)                         #:cost 18.5  #:fpcore (! :precision binary32 (erfc x))]
    [expm1.f32      #:spec (- (exp x) 1)                                       #:impl (from-libm 'expm1f)                        #:cost 6.75  #:fpcore (! :precision binary32 (expm1 x))]
    [log1p.f32      #:spec (log (+ 1 x))                                       #:impl (from-libm 'log1pf)                        #:cost 4.5  #:fpcore (! :precision binary32 (log1p x))])

(define-operations ([input <binary32>] [x <binary32>] [y <binary32>]) <binary32>
    [addcmul.f32 #:spec (+ input (* x y)) #:impl (from-rival) #:cost 1.75  #:fpcore (! :precision binary32 (addcmul input x y))]
    [addcdiv.f32 #:spec (+ input (/ x y)) #:impl (from-rival) #:cost 1.75  #:fpcore (! :precision binary32 (addcdiv input x y))])

(define-operation (lerp.f32 [start <binary32>] [end <binary32>] [weight <binary32>]) <binary32>
    #:spec (+ start (* weight (- end start))) #:impl (from-rival)
    #:fpcore (! :precision binary32 (lerp start end weight)) #:cost 1.75)

(define-operation (hypot.f32 [x <binary32>] [y <binary32>]) <binary32>
    #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-libm 'hypotf)
    #:fpcore (! :precision binary32 (hypot x y)) #:cost 5.5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost 64bit-move-cost)

(define-operation (where.f64 [c <bool>] [t <binary64>] [f <binary64>]) <binary64>
    #:spec (if c t f) #:impl if-impl #:cost 2.5)

(define-operation (neg.f64 [x <binary64>]) <binary64>
    #:spec (neg x) #:impl -
    #:fpcore (! :precision binary64 (- x)) #:cost 1.25)

(define-operations ([x <binary64>] [y <binary64>]) <bool>
    [eq.f64 #:spec (== x y) #:impl =          #:cost 2]
    [ne.f64 #:spec (!= x y) #:impl (negate =) #:cost 2]
    [lt.f64 #:spec (< x y)  #:impl <          #:cost 2]
    [gt.f64 #:spec (> x y)  #:impl >          #:cost 2]
    [le.f64 #:spec (<= x y) #:impl <=         #:cost 2]
    [ge.f64 #:spec (>= x y) #:impl >=         #:cost 2])

(define-operations () <binary64> #:fpcore (! :precision binary64 _)
    [PI.f64   #:spec (PI)       #:impl (const pi)      #:fpcore PI       #:cost 64bit-move-cost]
    [E.f64    #:spec (E)        #:impl (const (exp 1)) #:fpcore E        #:cost 64bit-move-cost]
    [INFINITY #:spec (INFINITY) #:impl (const +inf.0)  #:fpcore INFINITY #:cost 64bit-move-cost]
    [NAN.f64  #:spec (NAN)      #:impl (const +nan.0)  #:fpcore NAN      #:cost 64bit-move-cost])

(define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
    [add.f64      #:spec (+ x y)        #:impl +                     #:cost 1.5]
    [sub.f64      #:spec (- x y)        #:impl -                     #:cost 1.75]
    [mul.f64      #:spec (* x y)        #:impl *                     #:cost 1.5]
    [div.f64      #:spec (/ x y)        #:impl /                     #:cost 1.75]
    [pow.f64      #:spec (pow x y)      #:impl (from-libm 'pow)      #:cost 31]
    [copysign.f64 #:spec (copysign x y) #:impl (from-libm 'copysign) #:cost 2.25]
    [fmax.f64     #:spec (fmax x y)     #:impl (from-libm 'fmax)     #:cost 2]
    [fmin.f64     #:spec (fmin x y)     #:impl (from-libm 'fmin)     #:cost 2]
    [fmod.f64     #:spec (fmod x y)     #:impl (from-libm 'fmod)     #:cost 7.25])

(define-operation (atan2.f64 [y <binary64>] [x <binary64>]) <binary64>
    #:spec (atan2 y x) #:impl (from-libm 'atan2)
    #:fpcore (! :precision binary64 (atan2 y x)) #:cost 16.75)

(define-operations ([x <binary64>] [y <binary64>]) <binary64>
    [logaddexp.f64    #:spec (log (+ (exp x) (exp y)))     #:impl (from-rival) #:cost 22  #:fpcore (! :precision binary64 (logaddexp x y))]
    [logaddexp2.f64   #:spec (log2 (+ (exp2 x) (exp2 y)))  #:impl (from-rival) #:cost 21  #:fpcore (! :precision binary64 (logaddexp2 x y))]
    [xlogy.f64        #:spec (if (== x 0) 0 (* x (log y))) #:impl (from-rival) #:cost 7.25  #:fpcore (! :precision binary64 (xlogy x y))]
    [floor_divide.f64 #:spec (floor (/ x y))               #:impl (from-rival) #:cost 12.75  #:fpcore (! :precision binary64 (floor_divide x y))]
    [remainder.f64    #:spec (- x (* y (floor (/ x y))))   #:impl (from-rival) #:cost 8.25  #:fpcore (! :precision binary64 (remainder x y))])

(define-operations ([x <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
    [abs.f64    #:spec (fabs x)   #:impl (from-libm 'fabs)   #:cost 1.5]
    [acos.f64   #:spec (acos x)   #:impl (from-libm 'acos)   #:cost 10]
    [acosh.f64  #:spec (acosh x)  #:impl (from-libm 'acosh)  #:cost 10.75]
    [asin.f64   #:spec (asin x)   #:impl (from-libm 'asin)   #:cost 8.5]
    [asinh.f64  #:spec (asinh x)  #:impl (from-libm 'asinh)  #:cost 18.5]
    [atan.f64   #:spec (atan x)   #:impl (from-libm 'atan)   #:cost 12.25]
    [atanh.f64  #:spec (atanh x)  #:impl (from-libm 'atanh)  #:cost 16.25]
    [ceil.f64   #:spec (ceil x)   #:impl (from-libm 'ceil)   #:cost 1.5]
    [cos.f64    #:spec (cos x)    #:impl (from-libm 'cos)    #:cost 8.25]
    [cosh.f64   #:spec (cosh x)   #:impl (from-libm 'cosh)   #:cost 13]
    [erf.f64    #:spec (erf x)    #:impl (from-libm 'erf)    #:cost 17.25]
    [exp.f64    #:spec (exp x)    #:impl (from-libm 'exp)    #:cost 6.5]
    [exp2.f64   #:spec (exp2 x)   #:impl (from-libm 'exp2)   #:cost 5.75]
    [floor.f64  #:spec (floor x)  #:impl (from-libm 'floor)  #:cost 1.5]
    [lgamma.f64 #:spec (lgamma x) #:impl (from-libm 'lgamma) #:cost 85.75]
    [log.f64    #:spec (log x)    #:impl (from-libm 'log)    #:cost 9.75]
    [log10.f64  #:spec (log10 x)  #:impl (from-libm 'log10)  #:cost 10.25]
    [log2.f64   #:spec (log2 x)   #:impl (from-libm 'log2)   #:cost 10.25]
    [round.f64  #:spec (round x)  #:impl round               #:cost 1.5]
    [sin.f64    #:spec (sin x)    #:impl (from-libm 'sin)    #:cost 7.25]
    [sinh.f64   #:spec (sinh x)   #:impl (from-libm 'sinh)   #:cost 13.25]
    [sqrt.f64   #:spec (sqrt x)   #:impl (from-libm 'sqrt)   #:cost 1.75]
    [tan.f64    #:spec (tan x)    #:impl (from-libm 'tan)    #:cost 10.25]
    [tanh.f64   #:spec (tanh x)   #:impl (from-libm 'tanh)   #:cost 15.25]
    [trunc.f64  #:spec (trunc x)  #:impl (from-libm 'trunc)  #:cost 1.5])

(define-operations ([x <binary64>]) <binary64>
    [rsqrt.f64      #:spec (/ 1 (sqrt x))                                      #:impl (from-rival)            #:cost 2  #:fpcore (! :precision binary64 (rsqrt x))]
    [square.f64     #:spec (* x x)                                             #:impl sqr                     #:cost 1.5  #:fpcore (! :precision binary64 (sqr x))]
    [reciprocal.f64 #:spec (/ 1 x)                                             #:impl (lambda (x) (/ 1.0 x))  #:cost 1.5  #:fpcore (! :precision binary64 (reciprocal x))]
    [sinc.f64       #:spec (if (== x 0) 1 (/ (sin (* (PI) x)) (* (PI) x)))     #:impl (from-rival)            #:cost 7.75  #:fpcore (! :precision binary64 (sinc x))]
    [sigmoid.f64    #:spec (/ 1 (+ 1 (exp (neg x))))                           #:impl (from-rival)            #:cost 7  #:fpcore (! :precision binary64 (sigmoid x))]
    [frac.f64       #:spec (- x (trunc x))                                     #:impl (from-rival)            #:cost 1.25  #:fpcore (! :precision binary64 (frac x))]
    [sign.f64       #:spec (if (< x 0) -1 (if (> x 0) 1 0))                    #:impl (from-rival)            #:cost 1.5  #:fpcore (! :precision binary64 (sign x))]
    [rad2deg.f64    #:spec (* x (/ 180 (PI)))                                  #:impl (from-rival)            #:cost 2  #:fpcore (! :precision binary64 (rad2deg x))]
    [deg2rad.f64    #:spec (* x (/ (PI) 180))                                  #:impl (from-rival)            #:cost 2  #:fpcore (! :precision binary64 (deg2rad x))]
    [erfc.f64       #:spec (- 1 (erf x))                                       #:impl (from-libm 'erfc)       #:cost 59.5  #:fpcore (! :precision binary64 (erfc x))]
    [expm1.f64      #:spec (- (exp x) 1)                                       #:impl (from-libm 'expm1)      #:cost 13  #:fpcore (! :precision binary64 (expm1 x))]
    [log1p.f64      #:spec (log (+ 1 x))                                       #:impl (from-libm 'log1p)      #:cost 10.25  #:fpcore (! :precision binary64 (log1p x))])

(define-operations ([input <binary64>] [x <binary64>] [y <binary64>]) <binary64>
    [addcmul.f64 #:spec (+ input (* x y)) #:impl (from-rival) #:cost 2  #:fpcore (! :precision binary64 (addcmul input x y))]
    [addcdiv.f64 #:spec (+ input (/ x y)) #:impl (from-rival) #:cost 2  #:fpcore (! :precision binary64 (addcdiv input x y))])

(define-operation (lerp.f64 [start <binary64>] [end <binary64>] [weight <binary64>]) <binary64>
    #:spec (+ start (* weight (- end start))) #:impl (from-rival)
    #:fpcore (! :precision binary64 (lerp start end weight)) #:cost 2)

(define-operation (hypot.f64 [x <binary64>] [y <binary64>]) <binary64>
    #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-libm 'hypot)
    #:fpcore (! :precision binary64 (hypot x y)) #:cost 9.75)
