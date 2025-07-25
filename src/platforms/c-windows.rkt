#lang s-exp "../platform.rkt"

;; C/C++ on Windows platform with a full libm

(require math/flonum)

(define 64bit-move-cost   0.125)
(define 32bit-move-cost   0.125)
(define boolean-move-cost 0.100)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost boolean-move-cost)

(define-operations () <bool>
  [TRUE  #:spec (TRUE)  #:impl (const true)  #:fpcore TRUE  #:cost boolean-move-cost]
  [FALSE #:spec (FALSE) #:impl (const false) #:fpcore FALSE #:cost boolean-move-cost])

(define-operations ([x <bool>] [y <bool>]) <bool>
  [and #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost boolean-move-cost]
  [or  #:spec (or x y)  #:impl (lambda v (ormap values v))  #:cost boolean-move-cost])

(define-operation (not [x <bool>]) <bool>
  #:spec (not x) #:impl not #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary32> #:cost 32bit-move-cost)

(define-operation (if.f32 [c <bool>] [t <binary32>] [f <binary32>]) <binary32>
  #:spec (if c t f) #:impl if-impl
  #:cost boolean-move-cost #:aggregate if-cost)

(define-operations ([x <binary32>] [y <binary32>]) <bool>
  [==.f32 #:spec (== x y) #:impl =          #:cost 32bit-move-cost]
  [!=.f32 #:spec (!= x y) #:impl (negate =) #:cost 32bit-move-cost]
  [<.f32  #:spec (< x y)  #:impl <          #:cost 32bit-move-cost]
  [>.f32  #:spec (> x y)  #:impl >          #:cost 32bit-move-cost]
  [<=.f32 #:spec (<= x y) #:impl <=         #:cost 32bit-move-cost]
  [>=.f32 #:spec (>= x y) #:impl >=         #:cost 32bit-move-cost])

(define-operations () <binary32> #:fpcore (! :precision binary32)
  [PI.f32       #:spec (PI)       #:impl (const (flsingle pi))       #:fpcore PI       #:cost 32bit-move-cost]
  [E.f32        #:spec (E)        #:impl (const (flsingle (exp 1)))  #:fpcore E        #:cost 32bit-move-cost]
  [INFINITY.f32 #:spec (INFINITY) #:impl (const +inf.0)              #:fpcore INFINITY #:cost 32bit-move-cost]
  [NAN.f32      #:spec (NAN)      #:impl (const +nan.0)              #:fpcore NAN      #:cost 32bit-move-cost])

(define-operation (neg.f32 [x <binary32>]) <binary32>
  #:spec (neg x) #:impl (compose flsingle -)
  #:fpcore (! :precision binary32 (- x)) #:cost 0.125)

(define-operations ([x <binary32>] [y <binary32>]) <binary32> #:fpcore (! :precision binary32)
  [+.f32 #:spec (+ x y) #:impl (compose flsingle +) #:cost 0.200]
  [-.f32 #:spec (- x y) #:impl (compose flsingle -) #:cost 0.200]
  [*.f32 #:spec (* x y) #:impl (compose flsingle *) #:cost 0.250]
  [/.f32 #:spec (/ x y) #:impl (compose flsingle /) #:cost 0.350])

(define-operations ([x <binary32>]) <binary32> #:fpcore (! :precision binary32)
  [sin.f32    #:spec (sin x)    #:impl (from-libm 'sinf)    #:cost 4.250]
  [cos.f32    #:spec (cos x)    #:impl (from-libm 'cosf)    #:cost 4.250]
  [tan.f32    #:spec (tan x)    #:impl (from-libm 'tanf)    #:cost 4.750]
  [sinh.f32   #:spec (sinh x)   #:impl (from-libm 'sinhf)   #:cost 1.750]
  [cosh.f32   #:spec (cosh x)   #:impl (from-libm 'coshf)   #:cost 1.250]
  [acos.f32   #:spec (acos x)   #:impl (from-libm 'acosf)   #:cost 0.500]
  [acosh.f32  #:spec (acosh x)  #:impl (from-libm 'acoshf)  #:cost 0.850]
  [asin.f32   #:spec (asin x)   #:impl (from-libm 'asinf)   #:cost 0.500]
  [asinh.f32  #:spec (asinh x)  #:impl (from-libm 'asinhf)  #:cost 1.125]
  [atan.f32   #:spec (atan x)   #:impl (from-libm 'atanf)   #:cost 1.100]
  [atanh.f32  #:spec (atanh x)  #:impl (from-libm 'atanhf)  #:cost 0.500]
  [cbrt.f32   #:spec (cbrt x)   #:impl (from-libm 'cbrtf)   #:cost 2.000]
  [ceil.f32   #:spec (ceil x)   #:impl (from-libm 'ceilf)   #:cost 0.250]
  [erf.f32    #:spec (erf x)    #:impl (from-libm 'erff)    #:cost 1.125]
  [exp.f32    #:spec (exp x)    #:impl (from-libm 'expf)    #:cost 1.375]
  [exp2.f32   #:spec (exp2 x)   #:impl (from-libm 'exp2f)   #:cost 1.175]
  [floor.f32  #:spec (floor x)  #:impl (from-libm 'floorf)  #:cost 0.250]
  [lgamma.f32 #:spec (lgamma x) #:impl (from-libm 'lgammaf) #:cost 2.250]
  [log.f32    #:spec (log x)    #:impl (from-libm 'logf)    #:cost 0.750]
  [log10.f32  #:spec (log10 x)  #:impl (from-libm 'log10f)  #:cost 1.175]
  [log2.f32   #:spec (log2 x)   #:impl (from-libm 'log2f)   #:cost 0.875]
  [logb.f32   #:spec (logb x)   #:impl (from-libm 'logbf)   #:cost 0.375]
  [rint.f32   #:spec (rint x)   #:impl (from-libm 'rintf)   #:cost 0.300]
  [round.f32  #:spec (round x)  #:impl (from-libm 'roundf)  #:cost 0.875]
  [sqrt.f32   #:spec (sqrt x)   #:impl (from-libm 'sqrtf)   #:cost 0.250]
  [tanh.f32   #:spec (tanh x)   #:impl (from-libm 'tanhf)   #:cost 1.000]
  [tgamma.f32 #:spec (tgamma x) #:impl (from-libm 'tgammaf) #:cost 2.625]
  [trunc.f32  #:spec (trunc x)  #:impl (from-libm 'truncf)  #:cost 0.275])

(define-operations ([x <binary32>] [y <binary32>]) <binary32> #:fpcore (! :precision binary32)
  [pow.f32       #:spec (pow x y)       #:impl (from-libm 'powf)       #:cost 2.000]
  [atan2.f32     #:spec (atan2 x y)     #:impl (from-libm 'atan2f)     #:cost 2.000]
  [copysign.f32  #:spec (copysign x y)  #:impl (from-libm 'copysignf)  #:cost 0.200]
  [fdim.f32      #:spec (fdim x y)      #:impl (from-libm 'fdimf)      #:cost 0.750]
  [fmax.f32      #:spec (fmax x y)      #:impl (from-libm 'fmaxf)      #:cost 0.250]
  [fmin.f32      #:spec (fmin x y)      #:impl (from-libm 'fminf)      #:cost 0.250]
  [fmod.f32      #:spec (fmod x y)      #:impl (from-libm 'fmodf)      #:cost 1.750]
  [remainder.f32 #:spec (remainder x y) #:impl (from-libm 'remainderf) #:cost 1.000])

(define-operations ([x <binary32>]) <binary32> #:fpcore (! :precision binary32)
  [erfc.f32  #:spec (- 1 (erf x)) #:impl (from-libm 'erfcf)  #:fpcore (erfc x)  #:cost 0.900]
  [expm1.f32 #:spec (- (exp x) 1) #:impl (from-libm 'expm1f) #:fpcore (expm1 x) #:cost 0.900]
  [log1p.f32 #:spec (log (+ 1 x)) #:impl (from-libm 'log1pf) #:fpcore (log1p x) #:cost 1.300])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost 64bit-move-cost)

(define-operation (if.f64 [c <bool>] [t <binary64>] [f <binary64>]) <binary64>
  #:spec (if c t f) #:impl if-impl
  #:cost boolean-move-cost #:aggregate if-cost)

(define-operations ([x <binary64>] [y <binary64>]) <binary64>
  [+.f64 #:spec (+ x y) #:impl + #:cost 0.200]
  [-.f64 #:spec (- x y) #:impl - #:cost 0.200]
  [*.f64 #:spec (* x y) #:impl * #:cost 0.250]
  [/.f64 #:spec (/ x y) #:impl / #:cost 0.350])

(define-operations () <binary64> #:fpcore (! :precision binary64)
  [PI.f64   #:spec (PI)       #:impl (const pi)      #:fpcore PI       #:cost 64bit-move-cost]
  [E.f64    #:spec (E)        #:impl (const (exp 1)) #:fpcore E        #:cost 64bit-move-cost]
  [INFINITY #:spec (INFINITY) #:impl (const +inf.0)  #:fpcore INFINITY #:cost 64bit-move-cost]
  [NAN.f64  #:spec (NAN)      #:impl (const +nan.0)  #:fpcore NAN      #:cost 64bit-move-cost])

(define-operation (neg.f64 [x <binary64>]) <binary64>
  #:spec (neg x) #:impl -
  #:fpcore (! :precision binary64 (- x)) #:cost 0.125)

(define-operations ([x <binary64>] [y <binary64>]) <bool>
  [==.f64 #:spec (== x y) #:impl =          #:cost 64bit-move-cost]
  [!=.f64 #:spec (!= x y) #:impl (negate =) #:cost 64bit-move-cost]
  [<.f64  #:spec (< x y)  #:impl <          #:cost 64bit-move-cost]
  [>.f64  #:spec (> x y)  #:impl >          #:cost 64bit-move-cost]
  [<=.f64 #:spec (<= x y) #:impl <=         #:cost 64bit-move-cost]
  [>=.f64 #:spec (>= x y) #:impl >=         #:cost 64bit-move-cost])

(define-operations ([x <binary64>]) <binary64> #:fpcore (! :precision binary64)
  [fabs.f64   #:spec (fabs x)   #:impl (from-libm 'fabs)      #:cost 0.125]
  [sin.f64    #:spec (sin x)    #:impl (from-libm 'sin)       #:cost 4.200]
  [cos.f64    #:spec (cos x)    #:impl (from-libm 'cos)       #:cost 4.200]
  [tan.f64    #:spec (tan x)    #:impl (from-libm 'tan)       #:cost 4.650]
  [sinh.f64   #:spec (sinh x)   #:impl (from-libm 'sinh)      #:cost 1.750]
  [cosh.f64   #:spec (cosh x)   #:impl (from-libm 'cosh)      #:cost 1.650]
  [acos.f64   #:spec (acos x)   #:impl (from-libm 'acos)      #:cost 0.500]
  [acosh.f64  #:spec (acosh x)  #:impl (from-libm 'acosh)     #:cost 0.850]
  [asin.f64   #:spec (asin x)   #:impl (from-libm 'asin)      #:cost 0.500]
  [asinh.f64  #:spec (asinh x)  #:impl (from-libm 'asinh)     #:cost 1.125]
  [atan.f64   #:spec (atan x)   #:impl (from-libm 'atan)      #:cost 1.100]
  [atanh.f64  #:spec (atanh x)  #:impl (from-libm 'atanh)     #:cost 0.450]
  [cbrt.f64   #:spec (cbrt x)   #:impl (from-libm 'cbrt)      #:cost 2.000]
  [ceil.f64   #:spec (ceil x)   #:impl (from-libm 'ceil)      #:cost 0.250]
  [erf.f64    #:spec (erf x)    #:impl (from-libm 'erf)       #:cost 1.125]
  [exp.f64    #:spec (exp x)    #:impl (from-libm 'exp)       #:cost 1.375]
  [exp2.f64   #:spec (exp2 x)   #:impl (from-libm 'exp2)      #:cost 1.175]
  [floor.f64  #:spec (floor x)  #:impl (from-libm 'floor)     #:cost 0.300]
  [lgamma.f64 #:spec (lgamma x) #:impl (from-libm 'lgamma)    #:cost 2.250]
  [log.f64    #:spec (log x)    #:impl (from-libm 'log)       #:cost 0.750]
  [log10.f64  #:spec (log10 x)  #:impl (from-libm 'log10)     #:cost 1.175]
  [log2.f64   #:spec (log2 x)   #:impl (from-libm 'log2)      #:cost 0.850]
  [logb.f64   #:spec (logb x)   #:impl (from-libm 'logb)      #:cost 0.350]
  [rint.f64   #:spec (rint x)   #:impl (from-libm 'rint)      #:cost 0.300]
  [round.f64  #:spec (round x)  #:impl (from-libm 'round)     #:cost 0.850]
  [sqrt.f64   #:spec (sqrt x)   #:impl (from-libm 'sqrt)      #:cost 0.250]
  [tanh.f64   #:spec (tanh x)   #:impl (from-libm 'tanh)      #:cost 1.000]
  [tgamma.f64 #:spec (tgamma x) #:impl (from-libm 'tgamma)    #:cost 2.625]
  [trunc.f64  #:spec (trunc x)  #:impl (from-libm 'trunc)     #:cost 0.250])

(define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (! :precision binary64)
  [pow.f64       #:spec (pow x y)       #:impl (from-libm 'pow)       #:cost 2.000]
  [atan2.f64     #:spec (atan2 x y)     #:impl (from-libm 'atan2)     #:cost 2.000]
  [copysign.f64  #:spec (copysign x y)  #:impl (from-libm 'copysign)  #:cost 0.200]
  [fdim.f64      #:spec (fdim x y)      #:impl (from-libm 'fdim)      #:cost 0.750]
  [fmax.f64      #:spec (fmax x y)      #:impl (from-libm 'fmax)      #:cost 0.250]
  [fmin.f64      #:spec (fmin x y)      #:impl (from-libm 'fmin)      #:cost 0.250]
  [fmod.f64      #:spec (fmod x y)      #:impl (from-libm 'fmod)      #:cost 1.750]
  [remainder.f64 #:spec (remainder x y) #:impl (from-libm 'remainder) #:cost 1.000])

(define-operations ([x <binary64>]) <binary64> #:fpcore (! :precision binary64)
  [erfc.f64  #:spec (- 1 (erf x)) #:impl (from-libm 'erfc)  #:fpcore (erfc x)  #:cost 0.900]
  [expm1.f64 #:spec (- (exp x) 1) #:impl (from-libm 'expm1) #:fpcore (expm1 x) #:cost 0.900]
  [log1p.f64 #:spec (log (+ 1 x)) #:impl (from-libm 'log1p) #:fpcore (log1p x) #:cost 1.300])

(define-operation (hypot.f64 [x <binary64>] [y <binary64>]) <binary64>
  #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-libm 'hypot)
  #:fpcore (! :precision binary64 (hypot x y)) #:cost 1.700)

(define-operation (fma.f64 [x <binary64>] [y <binary64>] [z <binary64>]) <binary64>
  #:spec (+ (* x y) z) #:impl (from-libm 'fma)
  #:fpcore (! :precision binary64 (fma x y z)) #:cost 0.375)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CASTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operation (binary64->binary32 [x <binary64>]) <binary32>
  #:spec x #:fpcore (! :precision binary32 (cast x)) #:impl flsingle #:cost 32bit-move-cost)

(define-operation (binary32->binary64 [x <binary32>]) <binary64>
  #:spec x #:fpcore (! :precision binary64 (cast x)) #:impl identity #:cost 64bit-move-cost)
