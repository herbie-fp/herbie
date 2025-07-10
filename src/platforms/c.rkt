#lang s-exp "../platform.rkt"

;; C/C++ platform with a full libm

(require math/flonum)

(define 64bit-move-cost   0.12538399999999972)
(define 32bit-move-cost   0.12961999999999974)
(define boolean-move-cost 0.1)

(define-if #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost boolean-move-cost)

(define-operations () <bool>
  [TRUE  #:spec (TRUE)  #:impl (const true)  #:fpcore TRUE  #:cost boolean-move-cost]
  [FALSE #:spec (FALSE) #:impl (const false) #:fpcore FALSE #:cost boolean-move-cost])

(define-operations ([x <bool>] [y <bool>]) <bool>
  [and #:spec (and x y) #:impl (curry andmap values) #:fpcore (and x y) #:cost boolean-move-cost]
  [or  #:spec (or x y)  #:impl (curry ormap values)  #:fpcore (or x y)  #:cost boolean-move-cost])

(define-operation (not [x <bool>]) <bool>
  #:spec (not x) #:impl not #:fpcore (not x) #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary32> #:cost 32bit-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pi.f32 (flsingle pi))
(define e.f32 (flsingle (exp 1)))

(define-operations () <binary32>
  [PI.f32       #:spec (PI)       #:impl (const pi.f32) #:fpcore (! :precision binary32 PI)       #:cost 32bit-move-cost]
  [E.f32        #:spec (E)        #:impl (const e.f32)  #:fpcore (! :precision binary32 E)        #:cost 32bit-move-cost]
  [INFINITY.f32 #:spec (INFINITY) #:impl (const +inf.0) #:fpcore (! :precision binary32 INFINITY) #:cost 32bit-move-cost]
  [NAN.f32      #:spec (NAN)      #:impl (const +nan.0) #:fpcore (! :precision binary32 NAN)      #:cost 32bit-move-cost])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operation (neg.f32 [x <binary32>]) <binary32>
  #:spec (neg x) #:impl (compose flsingle -) #:fpcore (! :precision binary32 (- x)) #:cost 0.11567699999999992)

(define-operations ([x <binary32>] [y <binary32>]) <binary32>
  [+.f32 #:spec (+ x y) #:impl (compose flsingle +) #:fpcore (! :precision binary32 (+ x y)) #:cost 0.200445]
  [-.f32 #:spec (- x y) #:impl (compose flsingle -) #:fpcore (! :precision binary32 (- x y)) #:cost 0.19106800000000014]
  [*.f32 #:spec (* x y) #:impl (compose flsingle *) #:fpcore (! :precision binary32 (* x y)) #:cost 0.256602]
  [/.f32 #:spec (/ x y) #:impl (compose flsingle /) #:fpcore (! :precision binary32 (/ x y)) #:cost 0.3465330000000001])

(define-operations ([x <binary32>] [y <binary32>]) <bool>
  [==.f32 #:spec (== x y) #:impl =          #:fpcore (== x y) #:cost 32bit-move-cost]
  [!=.f32 #:spec (!= x y) #:impl (negate =) #:fpcore (!= x y) #:cost 32bit-move-cost]
  [<.f32  #:spec (< x y)  #:impl <          #:fpcore (< x y)  #:cost 32bit-move-cost]
  [>.f32  #:spec (> x y)  #:impl >          #:fpcore (> x y)  #:cost 32bit-move-cost]
  [<=.f32 #:spec (<= x y) #:impl <=         #:fpcore (<= x y) #:cost 32bit-move-cost]
  [>=.f32 #:spec (>= x y) #:impl >=         #:fpcore (>= x y) #:cost 32bit-move-cost])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

(define-operations ([x <binary32>]) <binary32>
  [fabs.f32   #:spec (fabs x)   #:impl (from-libm 'fabsf)     #:fpcore (! :precision binary32 (fabs x))   #:cost 0.12464599999999992]
  [sin.f32    #:spec (sin x)    #:impl (from-libm 'sinf)      #:fpcore (! :precision binary32 (sin x))    #:cost 4.2185290000000003]
  [cos.f32    #:spec (cos x)    #:impl (from-libm 'cosf)      #:fpcore (! :precision binary32 (cos x))    #:cost 4.2738829999999994]
  [tan.f32    #:spec (tan x)    #:impl (from-libm 'tanf)      #:fpcore (! :precision binary32 (tan x))    #:cost 4.669173000000001]
  [sinh.f32   #:spec (sinh x)   #:impl (from-libm 'sinhf)     #:fpcore (! :precision binary32 (sinh x))   #:cost 1.7463029999999996]
  [cosh.f32   #:spec (cosh x)   #:impl (from-libm 'coshf)     #:fpcore (! :precision binary32 (cosh x))   #:cost 1.32949]
  [acos.f32   #:spec (acos x)   #:impl (from-libm 'acosf)     #:fpcore (! :precision binary32 (acos x))   #:cost 0.5213330000000001]
  [acosh.f32  #:spec (acosh x)  #:impl (from-libm 'acoshf)    #:fpcore (! :precision binary32 (acosh x))  #:cost 0.847705]
  [asin.f32   #:spec (asin x)   #:impl (from-libm 'asinf)     #:fpcore (! :precision binary32 (asin x))   #:cost 0.4802670000000002]
  [asinh.f32  #:spec (asinh x)  #:impl (from-libm 'asinhf)    #:fpcore (! :precision binary32 (asinh x))  #:cost 1.1207249999999999]
  [atan.f32   #:spec (atan x)   #:impl (from-libm 'atanf)     #:fpcore (! :precision binary32 (atan x))   #:cost 1.1015179999999995]
  [atanh.f32  #:spec (atanh x)  #:impl (from-libm 'atanhf)    #:fpcore (! :precision binary32 (atanh x))  #:cost 0.4652830000000002]
  [cbrt.f32   #:spec (cbrt x)   #:impl (from-libm 'cbrtf)     #:fpcore (! :precision binary32 (cbrt x))   #:cost 1.946652]
  [ceil.f32   #:spec (ceil x)   #:impl (from-libm 'ceilf)     #:fpcore (! :precision binary32 (ceil x))   #:cost 0.287118]
  [erf.f32    #:spec (erf x)    #:impl (from-libm 'erff)      #:fpcore (! :precision binary32 (erf x))    #:cost 1.1211180000000001]
  [exp.f32    #:spec (exp x)    #:impl (from-libm 'expf)      #:fpcore (! :precision binary32 (exp x))    #:cost 1.3860560000000002]
  [exp2.f32   #:spec (exp2 x)   #:impl (from-libm 'exp2f)     #:fpcore (! :precision binary32 (exp2 x))   #:cost 1.1716009999999999]
  [floor.f32  #:spec (floor x)  #:impl (from-libm 'floorf)    #:fpcore (! :precision binary32 (floor x))  #:cost 0.2959660000000004]
  [lgamma.f32 #:spec (lgamma x) #:impl (from-libm 'lgammaf)   #:fpcore (! :precision binary32 (lgamma x)) #:cost 2.2203209999999998]
  [log.f32    #:spec (log x)    #:impl (from-libm 'logf)      #:fpcore (! :precision binary32 (log x))    #:cost 0.778197]
  [log10.f32  #:spec (log10 x)  #:impl (from-libm 'log10f)    #:fpcore (! :precision binary32 (log10 x))  #:cost 1.1793579999999997]
  [log2.f32   #:spec (log2 x)   #:impl (from-libm 'log2f)     #:fpcore (! :precision binary32 (log2 x))   #:cost 0.8644160000000004]
  [logb.f32   #:spec (logb x)   #:impl (from-libm 'logbf)     #:fpcore (! :precision binary32 (logb x))   #:cost 0.36221]
  [rint.f32   #:spec (rint x)   #:impl (from-libm 'rintf)     #:fpcore (! :precision binary32 (rint x))   #:cost 0.29316799999999997]
  [round.f32  #:spec (round x)  #:impl (from-libm 'roundf)    #:fpcore (! :precision binary32 (round x))  #:cost 0.8677139999999997]
  [sqrt.f32   #:spec (sqrt x)   #:impl (from-libm 'sqrtf)     #:fpcore (! :precision binary32 (sqrt x))   #:cost 0.25464700000000007]
  [tanh.f32   #:spec (tanh x)   #:impl (from-libm 'tanhf)     #:fpcore (! :precision binary32 (tanh x))   #:cost 1.0567220000000002]
  [tgamma.f32 #:spec (tgamma x) #:impl (from-libm 'tgammaf)   #:fpcore (! :precision binary32 (tgamma x)) #:cost 2.6280819999999994]
  [trunc.f32  #:spec (trunc x)  #:impl (from-libm 'truncf)    #:fpcore (! :precision binary32 (trunc x))  #:cost 0.28765399999999997])

(define-operations ([x <binary32>] [y <binary32>]) <binary32>
  [pow.f32       #:spec (pow x y)       #:impl (from-libm 'powf)       #:fpcore (! :precision binary32 (pow x y))       #:cost 2.028296]
  [atan2.f32     #:spec (atan2 x y)     #:impl (from-libm 'atan2f)     #:fpcore (! :precision binary32 (atan2 x y))     #:cost 1.9559129999999997]
  [copysign.f32  #:spec (copysign x y)  #:impl (from-libm 'copysignf)  #:fpcore (! :precision binary32 (copysign x y))  #:cost 0.2042799999999998]
  [fdim.f32      #:spec (fdim x y)      #:impl (from-libm 'fdimf)      #:fpcore (! :precision binary32 (fdim x y))      #:cost 0.7635619999999999]
  [fmax.f32      #:spec (fmax x y)      #:impl (from-libm 'fmaxf)      #:fpcore (! :precision binary32 (fmax x y))      #:cost 0.23636500000000005]
  [fmin.f32      #:spec (fmin x y)      #:impl (from-libm 'fminf)      #:fpcore (! :precision binary32 (fmin x y))      #:cost 0.24126899999999996]
  [fmod.f32      #:spec (fmod x y)      #:impl (from-libm 'fmodf)      #:fpcore (! :precision binary32 (fmod x y))      #:cost 1.7182470000000002]
  [remainder.f32 #:spec (remainder x y) #:impl (from-libm 'remainderf) #:fpcore (! :precision binary32 (remainder x y)) #:cost 1.030245])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

(define-operations ([x <binary32>]) <binary32>
  [erfc.f32  #:spec (- 1 (erf x)) #:impl (from-libm 'erfcf)  #:fpcore (! :precision binary32 (erfc x))  #:cost 0.907758]
  [expm1.f32 #:spec (- (exp x) 1) #:impl (from-libm 'expm1f) #:fpcore (! :precision binary32 (expm1 x)) #:cost 0.906484]
  [log1p.f32 #:spec (log (+ 1 x)) #:impl (from-libm 'log1pf) #:fpcore (! :precision binary32 (log1p x)) #:cost 1.302969])

(define-operation (hypot.f32 [x <binary32>] [y <binary32>]) <binary32>
  #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-libm 'hypotf)
  #:fpcore (! :precision binary32 (hypot x y)) #:cost 1.6816069999999997)

(define-operation (fma.f32 [x <binary32>] [y <binary32>] [z <binary32>]) <binary32>
  #:spec (+ (* x y) z) #:impl (from-libm 'fmaf)
  #:fpcore (! :precision binary32 (fma x y z)) #:cost 0.38934)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost 64bit-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operations () <binary64>
  [PI.f64   #:spec (PI)       #:impl (const pi)      #:fpcore (! :precision binary64 PI)       #:cost 64bit-move-cost]
  [E.f64    #:spec (E)        #:impl (const (exp 1)) #:fpcore (! :precision binary64 E)        #:cost 64bit-move-cost]
  [INFINITY #:spec (INFINITY) #:impl (const +inf.0)  #:fpcore (! :precision binary64 INFINITY) #:cost 64bit-move-cost]
  [NAN.f64  #:spec (NAN)      #:impl (const +nan.0)  #:fpcore (! :precision binary64 NAN)      #:cost 64bit-move-cost])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operation (neg.f64 [x <binary64>]) <binary64>
  #:spec (neg x) #:impl - #:fpcore (! :precision binary64 (- x)) #:cost 0.11567699999999992)

(define-operations ([x <binary64>] [y <binary64>]) <binary64>
  [+.f64 #:spec (+ x y) #:impl + #:fpcore (! :precision binary64 (+ x y)) #:cost 0.200445]
  [-.f64 #:spec (- x y) #:impl - #:fpcore (! :precision binary64 (- x y)) #:cost 0.19106800000000014]
  [*.f64 #:spec (* x y) #:impl * #:fpcore (! :precision binary64 (* x y)) #:cost 0.256602]
  [/.f64 #:spec (/ x y) #:impl / #:fpcore (! :precision binary64 (/ x y)) #:cost 0.3465330000000001])

(define-operations ([x <binary64>] [y <binary64>]) <bool>
  [==.f64 #:spec (== x y) #:impl =          #:fpcore (== x y) #:cost 64bit-move-cost]
  [!=.f64 #:spec (!= x y) #:impl (negate =) #:fpcore (!= x y) #:cost 64bit-move-cost]
  [<.f64  #:spec (< x y)  #:impl <          #:fpcore (< x y)  #:cost 64bit-move-cost]
  [>.f64  #:spec (> x y)  #:impl >          #:fpcore (> x y)  #:cost 64bit-move-cost]
  [<=.f64 #:spec (<= x y) #:impl <=         #:fpcore (<= x y) #:cost 64bit-move-cost]
  [>=.f64 #:spec (>= x y) #:impl >=         #:fpcore (>= x y) #:cost 64bit-move-cost])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

(define-operations ([x <binary64>]) <binary64>
  [fabs.f64   #:spec (fabs x)   #:impl (from-libm 'fabs)     #:fpcore (! :precision binary64 (fabs x))   #:cost 0.12464599999999992]
  [sin.f64    #:spec (sin x)    #:impl (from-libm 'sin)      #:fpcore (! :precision binary64 (sin x))    #:cost 4.2185290000000003]
  [cos.f64    #:spec (cos x)    #:impl (from-libm 'cos)      #:fpcore (! :precision binary64 (cos x))    #:cost 4.2738829999999994]
  [tan.f64    #:spec (tan x)    #:impl (from-libm 'tan)      #:fpcore (! :precision binary64 (tan x))    #:cost 4.669173000000001]
  [sinh.f64   #:spec (sinh x)   #:impl (from-libm 'sinh)     #:fpcore (! :precision binary64 (sinh x))   #:cost 1.7463029999999996]
  [cosh.f64   #:spec (cosh x)   #:impl (from-libm 'cosh)     #:fpcore (! :precision binary64 (cosh x))   #:cost 1.64949]
  [acos.f64   #:spec (acos x)   #:impl (from-libm 'acos)     #:fpcore (! :precision binary64 (acos x))   #:cost 0.5213330000000001]
  [acosh.f64  #:spec (acosh x)  #:impl (from-libm 'acosh)    #:fpcore (! :precision binary64 (acosh x))  #:cost 0.847705]
  [asin.f64   #:spec (asin x)   #:impl (from-libm 'asin)     #:fpcore (! :precision binary64 (asin x))   #:cost 0.4802670000000002]
  [asinh.f64  #:spec (asinh x)  #:impl (from-libm 'asinh)    #:fpcore (! :precision binary64 (asinh x))  #:cost 1.1207249999999999]
  [atan.f64   #:spec (atan x)   #:impl (from-libm 'atan)     #:fpcore (! :precision binary64 (atan x))   #:cost 1.1015179999999995]
  [atanh.f64  #:spec (atanh x)  #:impl (from-libm 'atanh)    #:fpcore (! :precision binary64 (atanh x))  #:cost 0.4652830000000002]
  [cbrt.f64   #:spec (cbrt x)   #:impl (from-libm 'cbrt)     #:fpcore (! :precision binary64 (cbrt x))   #:cost 1.946652]
  [ceil.f64   #:spec (ceil x)   #:impl (from-libm 'ceil)     #:fpcore (! :precision binary64 (ceil x))   #:cost 0.287118]
  [erf.f64    #:spec (erf x)    #:impl (from-libm 'erf)      #:fpcore (! :precision binary64 (erf x))    #:cost 1.1211180000000001]
  [exp.f64    #:spec (exp x)    #:impl (from-libm 'exp)      #:fpcore (! :precision binary64 (exp x))    #:cost 1.3860560000000002]
  [exp2.f64   #:spec (exp2 x)   #:impl (from-libm 'exp2)     #:fpcore (! :precision binary64 (exp2 x))   #:cost 1.1716009999999999]
  [floor.f64  #:spec (floor x)  #:impl (from-libm 'floor)    #:fpcore (! :precision binary64 (floor x))  #:cost 0.2959660000000004]
  [lgamma.f64 #:spec (lgamma x) #:impl (from-libm 'lgamma)   #:fpcore (! :precision binary64 (lgamma x)) #:cost 2.2206409999999998]
  [log.f64    #:spec (log x)    #:impl (from-libm 'log)      #:fpcore (! :precision binary64 (log x))    #:cost 0.778197]
  [log10.f64  #:spec (log10 x)  #:impl (from-libm 'log10)    #:fpcore (! :precision binary64 (log10 x))  #:cost 1.1793579999999997]
  [log2.f64   #:spec (log2 x)   #:impl (from-libm 'log2)     #:fpcore (! :precision binary64 (log2 x))   #:cost 0.8644160000000004]
  [logb.f64   #:spec (logb x)   #:impl (from-libm 'logb)     #:fpcore (! :precision binary64 (logb x))   #:cost 0.36221]
  [rint.f64   #:spec (rint x)   #:impl (from-libm 'rint)     #:fpcore (! :precision binary64 (rint x))   #:cost 0.29316799999999997]
  [round.f64  #:spec (round x)  #:impl (from-libm 'round)    #:fpcore (! :precision binary64 (round x))  #:cost 0.8677139999999997]
  [sqrt.f64   #:spec (sqrt x)   #:impl (from-libm 'sqrt)     #:fpcore (! :precision binary64 (sqrt x))   #:cost 0.25464700000000007]
  [tanh.f64   #:spec (tanh x)   #:impl (from-libm 'tanh)     #:fpcore (! :precision binary64 (tanh x))   #:cost 1.0567220000000002]
  [tgamma.f64 #:spec (tgamma x) #:impl (from-libm 'tgamma)   #:fpcore (! :precision binary64 (tgamma x)) #:cost 2.6280819999999994]
  [trunc.f64  #:spec (trunc x)  #:impl (from-libm 'trunc)    #:fpcore (! :precision binary64 (trunc x))  #:cost 0.28765399999999997])

(define-operations ([x <binary64>] [y <binary64>]) <binary64>
  [pow.f64       #:spec (pow x y)       #:impl (from-libm 'pow)       #:fpcore (! :precision binary64 (pow x y))       #:cost 2.028296]
  [atan2.f64     #:spec (atan2 x y)     #:impl (from-libm 'atan2)     #:fpcore (! :precision binary64 (atan2 x y))     #:cost 1.9559129999999997]
  [copysign.f64  #:spec (copysign x y)  #:impl (from-libm 'copysign)  #:fpcore (! :precision binary64 (copysign x y))  #:cost 0.2042799999999998]
  [fdim.f64      #:spec (fdim x y)      #:impl (from-libm 'fdim)      #:fpcore (! :precision binary64 (fdim x y))      #:cost 0.7635619999999999]
  [fmax.f64      #:spec (fmax x y)      #:impl (from-libm 'fmax)      #:fpcore (! :precision binary64 (fmax x y))      #:cost 0.23636500000000005]
  [fmin.f64      #:spec (fmin x y)      #:impl (from-libm 'fmin)      #:fpcore (! :precision binary64 (fmin x y))      #:cost 0.24126899999999996]
  [fmod.f64      #:spec (fmod x y)      #:impl (from-libm 'fmod)      #:fpcore (! :precision binary64 (fmod x y))      #:cost 1.7182470000000002]
  [remainder.f64 #:spec (remainder x y) #:impl (from-libm 'remainder) #:fpcore (! :precision binary64 (remainder x y)) #:cost 1.030245])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

(define-operations ([x <binary64>]) <binary64>
  [erfc.f64  #:spec (- 1 (erf x)) #:impl (from-libm 'erfc)  #:fpcore (! :precision binary64 (erfc x))  #:cost 0.907758]
  [expm1.f64 #:spec (- (exp x) 1) #:impl (from-libm 'expm1) #:fpcore (! :precision binary64 (expm1 x)) #:cost 0.906484]
  [log1p.f64 #:spec (log (+ 1 x)) #:impl (from-libm 'log1p) #:fpcore (! :precision binary64 (log1p x)) #:cost 1.302969])

(define-operation (hypot.f64 [x <binary64>] [y <binary64>]) <binary64>
  #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-libm 'hypot)
  #:fpcore (! :precision binary64 (hypot x y)) #:cost 1.6816069999999997)

(define-operation (fma.f64 [x <binary64>] [y <binary64>] [z <binary64>]) <binary64>
  #:spec (+ (* x y) z) #:impl (from-libm 'fma)
  #:fpcore (! :precision binary64 (fma x y z)) #:cost 0.38934)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; additional converters ;;;;;;;;;;;;;;;;;

(define-operation (binary64->binary32 [x <binary64>]) <binary32>
  #:spec x #:fpcore (! :precision binary32 (cast x)) #:impl flsingle #:cost 32bit-move-cost)

(define-operation (binary32->binary64 [x <binary32>]) <binary64>
  #:spec x #:fpcore (! :precision binary32 (cast x)) #:impl identity #:cost 64bit-move-cost)
