#lang s-exp "../syntax/platform-language.rkt"

;; PyTorch platform

(require math/flonum)

(define operation-cost 1.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost operation-cost)

(define-operations () <bool>
    [true  #:spec (TRUE)  #:impl (const true)  #:fpcore TRUE  #:cost operation-cost]
    [false #:spec (FALSE) #:impl (const false) #:fpcore FALSE #:cost operation-cost])

(define-operations ([x <bool>] [y <bool>]) <bool>
    [and #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost operation-cost]
    [or  #:spec (or x y)  #:impl (lambda v (ormap values v))  #:cost operation-cost])

(define-operation (not [x <bool>]) <bool>
    #:spec (not x) #:impl not #:cost operation-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary32> #:cost operation-cost)

(define-operation (where.f32 [c <bool>] [t <binary32>] [f <binary32>]) <binary32>
    #:spec (if c t f) #:impl if-impl #:cost operation-cost)

(define-operation (neg.f32 [x <binary32>]) <binary32>
    #:spec (neg x) #:impl (compose flsingle -)
    #:fpcore (! :precision binary32 (- x)) #:cost operation-cost)

(define-operations ([x <binary32>] [y <binary32>]) <bool>
    [eq.f32 #:spec (== x y) #:impl =          #:cost operation-cost]
    [ne.f32 #:spec (!= x y) #:impl (negate =) #:cost operation-cost]
    [lt.f32 #:spec (< x y)  #:impl <          #:cost operation-cost]
    [gt.f32 #:spec (> x y)  #:impl >          #:cost operation-cost]
    [le.f32 #:spec (<= x y) #:impl <=         #:cost operation-cost]
    [ge.f32 #:spec (>= x y) #:impl >=         #:cost operation-cost])

(define-operations () <binary32> #:fpcore (! :precision binary32 _)
    [PI.f32       #:spec (PI)       #:impl (const (flsingle pi))      #:fpcore PI       #:cost operation-cost]
    [E.f32        #:spec (E)        #:impl (const (flsingle (exp 1))) #:fpcore E        #:cost operation-cost]
    [INFINITY.f32 #:spec (INFINITY) #:impl (const +inf.0)             #:fpcore INFINITY #:cost operation-cost]
    [NAN.f32      #:spec (NAN)      #:impl (const +nan.0)             #:fpcore NAN      #:cost operation-cost])

(define-operations ([x <binary32>] [y <binary32>]) <binary32> #:fpcore (! :precision binary32 _)
    [add.f32      #:spec (+ x y)        #:impl (compose flsingle +)   #:cost operation-cost]
    [sub.f32      #:spec (- x y)        #:impl (compose flsingle -)   #:cost operation-cost]
    [mul.f32      #:spec (* x y)        #:impl (compose flsingle *)   #:cost operation-cost]
    [div.f32      #:spec (/ x y)        #:impl (compose flsingle /)   #:cost operation-cost]
    [pow.f32      #:spec (pow x y)      #:impl (from-libm 'powf)      #:cost operation-cost]
    [copysign.f32 #:spec (copysign x y) #:impl (from-libm 'copysignf) #:cost operation-cost]
    [fmax.f32     #:spec (fmax x y)     #:impl (from-libm 'fmaxf)     #:cost operation-cost]
    [fmin.f32     #:spec (fmin x y)     #:impl (from-libm 'fminf)     #:cost operation-cost]
    [fmod.f32     #:spec (fmod x y)     #:impl (from-libm 'fmodf)     #:cost operation-cost])

(define-operation (atan2.f32 [y <binary32>] [x <binary32>]) <binary32>
    #:spec (atan2 y x) #:impl (from-libm 'atan2f)
    #:fpcore (! :precision binary32 (atan2 y x)) #:cost operation-cost)

;; Derived binary operations
(define-operations ([x <binary32>] [y <binary32>]) <binary32>
    [logaddexp.f32    #:spec (log (+ (exp x) (exp y)))     #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary32 (logaddexp x y))]
    [logaddexp2.f32   #:spec (log2 (+ (exp2 x) (exp2 y)))  #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary32 (logaddexp2 x y))]
    [xlogy.f32        #:spec (if (== x 0) 0 (* x (log y))) #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary32 (xlogy x y))]
    [floor_divide.f32 #:spec (floor (/ x y))               #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary32 (floor_divide x y))]
    [remainder.f32    #:spec (- x (* y (floor (/ x y))))   #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary32 (remainder x y))])

(define-operations ([x <binary32>]) <binary32> #:fpcore (! :precision binary32 _)
    [abs.f32    #:spec (fabs x)   #:impl (from-libm 'fabsf)          #:cost operation-cost]
    [acos.f32   #:spec (acos x)   #:impl (from-libm 'acosf)          #:cost operation-cost]
    [acosh.f32  #:spec (acosh x)  #:impl (from-libm 'acoshf)         #:cost operation-cost]
    [asin.f32   #:spec (asin x)   #:impl (from-libm 'asinf)          #:cost operation-cost]
    [asinh.f32  #:spec (asinh x)  #:impl (from-libm 'asinhf)         #:cost operation-cost]
    [atan.f32   #:spec (atan x)   #:impl (from-libm 'atanf)          #:cost operation-cost]
    [atanh.f32  #:spec (atanh x)  #:impl (from-libm 'atanhf)         #:cost operation-cost]
    [ceil.f32   #:spec (ceil x)   #:impl (from-libm 'ceilf)          #:cost operation-cost]
    [cos.f32    #:spec (cos x)    #:impl (from-libm 'cosf)           #:cost operation-cost]
    [cosh.f32   #:spec (cosh x)   #:impl (from-libm 'coshf)          #:cost operation-cost]
    [erf.f32    #:spec (erf x)    #:impl (from-libm 'erff)           #:cost operation-cost]
    [exp.f32    #:spec (exp x)    #:impl (from-libm 'expf)           #:cost operation-cost]
    [exp2.f32   #:spec (exp2 x)   #:impl (from-libm 'exp2f)          #:cost operation-cost]
    [floor.f32  #:spec (floor x)  #:impl (from-libm 'floorf)         #:cost operation-cost]
    [lgamma.f32 #:spec (lgamma x) #:impl (from-libm 'lgammaf)        #:cost operation-cost]
    [log.f32    #:spec (log x)    #:impl (from-libm 'logf)           #:cost operation-cost]
    [log10.f32  #:spec (log10 x)  #:impl (from-libm 'log10f)         #:cost operation-cost]
    [log2.f32   #:spec (log2 x)   #:impl (from-libm 'log2f)          #:cost operation-cost]
    [round.f32  #:spec (round x)  #:impl (compose flsingle round)    #:cost operation-cost]
    [sin.f32    #:spec (sin x)    #:impl (from-libm 'sinf)           #:cost operation-cost]
    [sinh.f32   #:spec (sinh x)   #:impl (from-libm 'sinhf)          #:cost operation-cost]
    [sqrt.f32   #:spec (sqrt x)   #:impl (from-libm 'sqrtf)          #:cost operation-cost]
    [tan.f32    #:spec (tan x)    #:impl (from-libm 'tanf)           #:cost operation-cost]
    [tanh.f32   #:spec (tanh x)   #:impl (from-libm 'tanhf)          #:cost operation-cost]
    [trunc.f32  #:spec (trunc x)  #:impl (from-libm 'truncf)         #:cost operation-cost])

(define-operations ([x <binary32>]) <binary32>
    [rsqrt.f32      #:spec (/ 1 (sqrt x))                                      #:impl (from-rival)                               #:cost operation-cost #:fpcore (! :precision binary32 (rsqrt x))]
    [square.f32     #:spec (* x x)                                             #:impl (compose flsingle sqr)                     #:cost operation-cost #:fpcore (! :precision binary32 (sqr x))]
    [reciprocal.f32 #:spec (/ 1 x)                                             #:impl (compose flsingle (lambda (x) (/ 1.0 x)))  #:cost operation-cost #:fpcore (! :precision binary32 (reciprocal x))]
    [sinc.f32       #:spec (if (== x 0) 1 (/ (sin (* (PI) x)) (* (PI) x)))     #:impl (from-rival)                               #:cost operation-cost #:fpcore (! :precision binary32 (sinc x))]
    [sigmoid.f32    #:spec (/ 1 (+ 1 (exp (neg x))))                           #:impl (from-rival)                               #:cost operation-cost #:fpcore (! :precision binary32 (sigmoid x))]
    [frac.f32       #:spec (- x (trunc x))                                     #:impl (from-rival)                               #:cost operation-cost #:fpcore (! :precision binary32 (frac x))]
    [sign.f32       #:spec (if (< x 0) -1 (if (> x 0) 1 0))                    #:impl (from-rival)                               #:cost operation-cost #:fpcore (! :precision binary32 (sign x))]
    [rad2deg.f32    #:spec (* x (/ 180 (PI)))                                  #:impl (from-rival)                               #:cost operation-cost #:fpcore (! :precision binary32 (rad2deg x))]
    [deg2rad.f32    #:spec (* x (/ (PI) 180))                                  #:impl (from-rival)                               #:cost operation-cost #:fpcore (! :precision binary32 (deg2rad x))]
    [erfc.f32       #:spec (- 1 (erf x))                                       #:impl (from-libm 'erfcf)                         #:cost operation-cost #:fpcore (! :precision binary32 (erfc x))]
    [expm1.f32      #:spec (- (exp x) 1)                                       #:impl (from-libm 'expm1f)                        #:cost operation-cost #:fpcore (! :precision binary32 (expm1 x))]
    [log1p.f32      #:spec (log (+ 1 x))                                       #:impl (from-libm 'log1pf)                        #:cost operation-cost #:fpcore (! :precision binary32 (log1p x))])

(define-operations ([input <binary32>] [tensor1 <binary32>] [tensor2 <binary32>]) <binary32>
    [addcmul.f32 #:spec (+ input (* tensor1 tensor2)) #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary32 (addcmul input tensor1 tensor2))]
    [addcdiv.f32 #:spec (+ input (/ tensor1 tensor2)) #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary32 (addcdiv input tensor1 tensor2))])

(define-operation (lerp.f32 [start <binary32>] [end <binary32>] [weight <binary32>]) <binary32>
    #:spec (+ start (* weight (- end start))) #:impl (from-rival)
    #:fpcore (! :precision binary32 (lerp start end weight)) #:cost operation-cost)

(define-operation (hypot.f32 [x <binary32>] [y <binary32>]) <binary32>
    #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-libm 'hypotf)
    #:fpcore (! :precision binary32 (hypot x y)) #:cost operation-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost operation-cost)

(define-operation (where.f64 [c <bool>] [t <binary64>] [f <binary64>]) <binary64>
    #:spec (if c t f) #:impl if-impl #:cost operation-cost)

(define-operation (neg.f64 [x <binary64>]) <binary64>
    #:spec (neg x) #:impl -
    #:fpcore (! :precision binary64 (- x)) #:cost operation-cost)

(define-operations ([x <binary64>] [y <binary64>]) <bool>
    [eq.f64 #:spec (== x y) #:impl =          #:cost operation-cost]
    [ne.f64 #:spec (!= x y) #:impl (negate =) #:cost operation-cost]
    [lt.f64 #:spec (< x y)  #:impl <          #:cost operation-cost]
    [gt.f64 #:spec (> x y)  #:impl >          #:cost operation-cost]
    [le.f64 #:spec (<= x y) #:impl <=         #:cost operation-cost]
    [ge.f64 #:spec (>= x y) #:impl >=         #:cost operation-cost])

(define-operations () <binary64> #:fpcore (! :precision binary64 _)
    [PI.f64   #:spec (PI)       #:impl (const pi)      #:fpcore PI       #:cost operation-cost]
    [E.f64    #:spec (E)        #:impl (const (exp 1)) #:fpcore E        #:cost operation-cost]
    [INFINITY #:spec (INFINITY) #:impl (const +inf.0)  #:fpcore INFINITY #:cost operation-cost]
    [NAN.f64  #:spec (NAN)      #:impl (const +nan.0)  #:fpcore NAN      #:cost operation-cost])

(define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
    [add.f64      #:spec (+ x y)        #:impl +                     #:cost operation-cost]
    [sub.f64      #:spec (- x y)        #:impl -                     #:cost operation-cost]
    [mul.f64      #:spec (* x y)        #:impl *                     #:cost operation-cost]
    [div.f64      #:spec (/ x y)        #:impl /                     #:cost operation-cost]
    [pow.f64      #:spec (pow x y)      #:impl (from-libm 'pow)      #:cost operation-cost]
    [copysign.f64 #:spec (copysign x y) #:impl (from-libm 'copysign) #:cost operation-cost]
    [fmax.f64     #:spec (fmax x y)     #:impl (from-libm 'fmax)     #:cost operation-cost]
    [fmin.f64     #:spec (fmin x y)     #:impl (from-libm 'fmin)     #:cost operation-cost]
    [fmod.f64     #:spec (fmod x y)     #:impl (from-libm 'fmod)     #:cost operation-cost])

(define-operation (atan2.f64 [y <binary64>] [x <binary64>]) <binary64>
    #:spec (atan2 y x) #:impl (from-libm 'atan2)
    #:fpcore (! :precision binary64 (atan2 y x)) #:cost operation-cost)

(define-operations ([x <binary64>] [y <binary64>]) <binary64>
    [logaddexp.f64    #:spec (log (+ (exp x) (exp y)))     #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary64 (logaddexp x y))]
    [logaddexp2.f64   #:spec (log2 (+ (exp2 x) (exp2 y)))  #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary64 (logaddexp2 x y))]
    [xlogy.f64        #:spec (if (== x 0) 0 (* x (log y))) #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary64 (xlogy x y))]
    [floor_divide.f64 #:spec (floor (/ x y))               #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary64 (floor_divide x y))]
    [remainder.f64    #:spec (- x (* y (floor (/ x y))))   #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary64 (remainder x y))])

(define-operations ([x <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
    [abs.f64    #:spec (fabs x)   #:impl (from-libm 'fabs)   #:cost operation-cost]
    [acos.f64   #:spec (acos x)   #:impl (from-libm 'acos)   #:cost operation-cost]
    [acosh.f64  #:spec (acosh x)  #:impl (from-libm 'acosh)  #:cost operation-cost]
    [asin.f64   #:spec (asin x)   #:impl (from-libm 'asin)   #:cost operation-cost]
    [asinh.f64  #:spec (asinh x)  #:impl (from-libm 'asinh)  #:cost operation-cost]
    [atan.f64   #:spec (atan x)   #:impl (from-libm 'atan)   #:cost operation-cost]
    [atanh.f64  #:spec (atanh x)  #:impl (from-libm 'atanh)  #:cost operation-cost]
    [ceil.f64   #:spec (ceil x)   #:impl (from-libm 'ceil)   #:cost operation-cost]
    [cos.f64    #:spec (cos x)    #:impl (from-libm 'cos)    #:cost operation-cost]
    [cosh.f64   #:spec (cosh x)   #:impl (from-libm 'cosh)   #:cost operation-cost]
    [erf.f64    #:spec (erf x)    #:impl (from-libm 'erf)    #:cost operation-cost]
    [exp.f64    #:spec (exp x)    #:impl (from-libm 'exp)    #:cost operation-cost]
    [exp2.f64   #:spec (exp2 x)   #:impl (from-libm 'exp2)   #:cost operation-cost]
    [floor.f64  #:spec (floor x)  #:impl (from-libm 'floor)  #:cost operation-cost]
    [lgamma.f64 #:spec (lgamma x) #:impl (from-libm 'lgamma) #:cost operation-cost]
    [log.f64    #:spec (log x)    #:impl (from-libm 'log)    #:cost operation-cost]
    [log10.f64  #:spec (log10 x)  #:impl (from-libm 'log10)  #:cost operation-cost]
    [log2.f64   #:spec (log2 x)   #:impl (from-libm 'log2)   #:cost operation-cost]
    [round.f64  #:spec (round x)  #:impl round               #:cost operation-cost]
    [sin.f64    #:spec (sin x)    #:impl (from-libm 'sin)    #:cost operation-cost]
    [sinh.f64   #:spec (sinh x)   #:impl (from-libm 'sinh)   #:cost operation-cost]
    [sqrt.f64   #:spec (sqrt x)   #:impl (from-libm 'sqrt)   #:cost operation-cost]
    [tan.f64    #:spec (tan x)    #:impl (from-libm 'tan)    #:cost operation-cost]
    [tanh.f64   #:spec (tanh x)   #:impl (from-libm 'tanh)   #:cost operation-cost]
    [trunc.f64  #:spec (trunc x)  #:impl (from-libm 'trunc)  #:cost operation-cost])

(define-operations ([x <binary64>]) <binary64>
    [rsqrt.f64      #:spec (/ 1 (sqrt x))                                      #:impl (from-rival)            #:cost operation-cost #:fpcore (! :precision binary64 (rsqrt x))]
    [square.f64     #:spec (* x x)                                             #:impl sqr                     #:cost operation-cost #:fpcore (! :precision binary64 (sqr x))]
    [reciprocal.f64 #:spec (/ 1 x)                                             #:impl (lambda (x) (/ 1.0 x))  #:cost operation-cost #:fpcore (! :precision binary64 (reciprocal x))]
    [sinc.f64       #:spec (if (== x 0) 1 (/ (sin (* (PI) x)) (* (PI) x)))     #:impl (from-rival)            #:cost operation-cost #:fpcore (! :precision binary64 (sinc x))]
    [sigmoid.f64    #:spec (/ 1 (+ 1 (exp (neg x))))                           #:impl (from-rival)            #:cost operation-cost #:fpcore (! :precision binary64 (sigmoid x))]
    [frac.f64       #:spec (- x (trunc x))                                     #:impl (from-rival)            #:cost operation-cost #:fpcore (! :precision binary64 (frac x))]
    [sign.f64       #:spec (if (< x 0) -1 (if (> x 0) 1 0))                    #:impl (from-rival)            #:cost operation-cost #:fpcore (! :precision binary64 (sign x))]
    [rad2deg.f64    #:spec (* x (/ 180 (PI)))                                  #:impl (from-rival)            #:cost operation-cost #:fpcore (! :precision binary64 (rad2deg x))]
    [deg2rad.f64    #:spec (* x (/ (PI) 180))                                  #:impl (from-rival)            #:cost operation-cost #:fpcore (! :precision binary64 (deg2rad x))]
    [erfc.f64       #:spec (- 1 (erf x))                                       #:impl (from-libm 'erfc)       #:cost operation-cost #:fpcore (! :precision binary64 (erfc x))]
    [expm1.f64      #:spec (- (exp x) 1)                                       #:impl (from-libm 'expm1)      #:cost operation-cost #:fpcore (! :precision binary64 (expm1 x))]
    [log1p.f64      #:spec (log (+ 1 x))                                       #:impl (from-libm 'log1p)      #:cost operation-cost #:fpcore (! :precision binary64 (log1p x))])

(define-operations ([input <binary64>] [tensor1 <binary64>] [tensor2 <binary64>]) <binary64>
    [addcmul.f64 #:spec (+ input (* tensor1 tensor2)) #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary64 (addcmul input tensor1 tensor2))]
    [addcdiv.f64 #:spec (+ input (/ tensor1 tensor2)) #:impl (from-rival) #:cost operation-cost #:fpcore (! :precision binary64 (addcdiv input tensor1 tensor2))])

(define-operation (lerp.f64 [start <binary64>] [end <binary64>] [weight <binary64>]) <binary64>
    #:spec (+ start (* weight (- end start))) #:impl (from-rival)
    #:fpcore (! :precision binary64 (lerp start end weight)) #:cost operation-cost)

(define-operation (hypot.f64 [x <binary64>] [y <binary64>]) <binary64>
    #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-libm 'hypot)
    #:fpcore (! :precision binary64 (hypot x y)) #:cost operation-cost)
