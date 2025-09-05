#lang s-exp "../syntax/platform-language.rkt"

;; Herbie 1.0 platform. Based on the C Windows platform, but with
;; every operation having cost 0, so as to emulate no-pareto mode.

(require math/flonum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost 0)

(define-operations () <bool>
  [TRUE  #:spec (TRUE)  #:impl (const true)  #:fpcore TRUE  #:cost 0]
  [FALSE #:spec (FALSE) #:impl (const false) #:fpcore FALSE #:cost 0])

(define-operations ([x <bool>] [y <bool>]) <bool>
  [and #:spec (and x y) #:impl (lambda v (andmap values v)) #:cost 0]
  [or  #:spec (or x y)  #:impl (lambda v (ormap values v))  #:cost 0])

(define-operation (not [x <bool>]) <bool>
  #:spec (not x) #:impl not #:cost 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary32> #:cost 0)

(define-operation (if.f32 [c <bool>] [t <binary32>] [f <binary32>]) <binary32>
  #:spec (if c t f) #:impl if-impl
  #:cost (if-cost 0))

(define-operations ([x <binary32>] [y <binary32>]) <bool>
  [==.f32 #:spec (== x y) #:impl =          #:cost 0]
  [!=.f32 #:spec (!= x y) #:impl (negate =) #:cost 0]
  [<.f32  #:spec (< x y)  #:impl <          #:cost 0]
  [>.f32  #:spec (> x y)  #:impl >          #:cost 0]
  [<=.f32 #:spec (<= x y) #:impl <=         #:cost 0]
  [>=.f32 #:spec (>= x y) #:impl >=         #:cost 0])

(define-operations () <binary32> #:fpcore (! :precision binary32 _)
  [PI.f32       #:spec (PI)       #:impl (const (flsingle pi))       #:fpcore PI       #:cost 0]
  [E.f32        #:spec (E)        #:impl (const (flsingle (exp 1)))  #:fpcore E        #:cost 0]
  [INFINITY.f32 #:spec (INFINITY) #:impl (const +inf.0)              #:fpcore INFINITY #:cost 0]
  [NAN.f32      #:spec (NAN)      #:impl (const +nan.0)              #:fpcore NAN      #:cost 0])

(define-operation (neg.f32 [x <binary32>]) <binary32>
  #:spec (neg x) #:impl (compose flsingle -)
  #:fpcore (! :precision binary32 (- x)) #:cost 0)
  
(define-operations ([x <binary32>] [y <binary32>]) <binary32> #:fpcore (! :precision binary32 _)
  [+.f32 #:spec (+ x y) #:impl (compose flsingle +) #:cost 0]
  [-.f32 #:spec (- x y) #:impl (compose flsingle -) #:cost 0]
  [*.f32 #:spec (* x y) #:impl (compose flsingle *) #:cost 0]
  [/.f32 #:spec (/ x y) #:impl (compose flsingle /) #:cost 0])

(define-operations ([x <binary32>]) <binary32> #:fpcore (! :precision binary32 _)
  [fabs.f32   #:spec (fabs x)   #:impl (from-libm 'fabsf)   #:cost 0]
  [sin.f32    #:spec (sin x)    #:impl (from-libm 'sinf)    #:cost 0]
  [cos.f32    #:spec (cos x)    #:impl (from-libm 'cosf)    #:cost 0]
  [tan.f32    #:spec (tan x)    #:impl (from-libm 'tanf)    #:cost 0]
  [sinh.f32   #:spec (sinh x)   #:impl (from-libm 'sinhf)   #:cost 0]
  [cosh.f32   #:spec (cosh x)   #:impl (from-libm 'coshf)   #:cost 0]
  [acos.f32   #:spec (acos x)   #:impl (from-libm 'acosf)   #:cost 0]
  [acosh.f32  #:spec (acosh x)  #:impl (from-libm 'acoshf)  #:cost 0]
  [asin.f32   #:spec (asin x)   #:impl (from-libm 'asinf)   #:cost 0]
  [asinh.f32  #:spec (asinh x)  #:impl (from-libm 'asinhf)  #:cost 0]
  [atan.f32   #:spec (atan x)   #:impl (from-libm 'atanf)   #:cost 0]
  [atanh.f32  #:spec (atanh x)  #:impl (from-libm 'atanhf)  #:cost 0]
  [cbrt.f32   #:spec (cbrt x)   #:impl (from-libm 'cbrtf)   #:cost 0]
  [ceil.f32   #:spec (ceil x)   #:impl (from-libm 'ceilf)   #:cost 0]
  [erf.f32    #:spec (erf x)    #:impl (from-libm 'erff)    #:cost 0]
  [exp.f32    #:spec (exp x)    #:impl (from-libm 'expf)    #:cost 0]
  [exp2.f32   #:spec (exp2 x)   #:impl (from-libm 'exp2f)   #:cost 0]
  [floor.f32  #:spec (floor x)  #:impl (from-libm 'floorf)  #:cost 0]
  [lgamma.f32 #:spec (lgamma x) #:impl (from-libm 'lgammaf) #:cost 0]
  [log.f32    #:spec (log x)    #:impl (from-libm 'logf)    #:cost 0]
  [log10.f32  #:spec (log10 x)  #:impl (from-libm 'log10f)  #:cost 0]
  [log2.f32   #:spec (log2 x)   #:impl (from-libm 'log2f)   #:cost 0]
  [logb.f32   #:spec (logb x)   #:impl (from-libm 'logbf)   #:cost 0]
  [rint.f32   #:spec (rint x)   #:impl (from-libm 'rintf)   #:cost 0]
  [round.f32  #:spec (round x)  #:impl (from-libm 'roundf)  #:cost 0]
  [sqrt.f32   #:spec (sqrt x)   #:impl (from-libm 'sqrtf)   #:cost 0]
  [tanh.f32   #:spec (tanh x)   #:impl (from-libm 'tanhf)   #:cost 0]
  [tgamma.f32 #:spec (tgamma x) #:impl (from-libm 'tgammaf) #:cost 0]
  [trunc.f32  #:spec (trunc x)  #:impl (from-libm 'truncf)  #:cost 0])
  
(define-operations ([x <binary32>] [y <binary32>]) <binary32> #:fpcore (! :precision binary32 _)
  [pow.f32       #:spec (pow x y)       #:impl (from-libm 'powf)       #:cost 0]
  [atan2.f32     #:spec (atan2 x y)     #:impl (from-libm 'atan2f)     #:cost 0]
  [copysign.f32  #:spec (copysign x y)  #:impl (from-libm 'copysignf)  #:cost 0]
  [fdim.f32      #:spec (fdim x y)      #:impl (from-libm 'fdimf)      #:cost 0]
  [fmax.f32      #:spec (fmax x y)      #:impl (from-libm 'fmaxf)      #:cost 0]
  [fmin.f32      #:spec (fmin x y)      #:impl (from-libm 'fminf)      #:cost 0]
  [fmod.f32      #:spec (fmod x y)      #:impl (from-libm 'fmodf)      #:cost 0]
  [remainder.f32 #:spec (remainder x y) #:impl (from-libm 'remainderf) #:cost 0])
  
(define-operations ([x <binary32>]) <binary32> #:fpcore (! :precision binary32 _)
  [erfc.f32  #:spec (- 1 (erf x)) #:impl (from-libm 'erfcf)  #:fpcore (erfc x)  #:cost 0]
  [expm1.f32 #:spec (- (exp x) 1) #:impl (from-libm 'expm1f) #:fpcore (expm1 x) #:cost 0]
  [log1p.f32 #:spec (log (+ 1 x)) #:impl (from-libm 'log1pf) #:fpcore (log1p x) #:cost 0])
  
(define-operation (hypot.f32 [x <binary32>] [y <binary32>]) <binary32>
  #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-libm 'hypotf)
  #:fpcore (! :precision binary32 (hypot x y)) #:cost 0)

(define-operation (fma.f32 [x <binary32>] [y <binary32>] [z <binary32>]) <binary32>
  #:spec (+ (* x y) z) #:impl (from-libm 'fmaf)
  #:fpcore (! :precision binary32 (fma x y z)) #:cost 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost 0)

(define-operation (if.f64 [c <bool>] [t <binary64>] [f <binary64>]) <binary64>
  #:spec (if c t f) #:impl if-impl
  #:cost (if-cost 0))

(define-operations ([x <binary64>] [y <binary64>]) <bool>
  [==.f64 #:spec (== x y) #:impl =          #:cost 0]
  [!=.f64 #:spec (!= x y) #:impl (negate =) #:cost 0]
  [<.f64  #:spec (< x y)  #:impl <          #:cost 0]
  [>.f64  #:spec (> x y)  #:impl >          #:cost 0]
  [<=.f64 #:spec (<= x y) #:impl <=         #:cost 0]
  [>=.f64 #:spec (>= x y) #:impl >=         #:cost 0])

(define-operations () <binary64> #:fpcore (! :precision binary64 _)
  [PI.f64   #:spec (PI)       #:impl (const pi)      #:fpcore PI       #:cost 0]
  [E.f64    #:spec (E)        #:impl (const (exp 1)) #:fpcore E        #:cost 0]
  [INFINITY #:spec (INFINITY) #:impl (const +inf.0)  #:fpcore INFINITY #:cost 0]
  [NAN.f64  #:spec (NAN)      #:impl (const +nan.0)  #:fpcore NAN      #:cost 0])

 (define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
   [+.f64 #:spec (+ x y) #:impl + #:cost 0]
   [-.f64 #:spec (- x y) #:impl - #:cost 0]
   [*.f64 #:spec (* x y) #:impl * #:cost 0]
   [/.f64 #:spec (/ x y) #:impl / #:cost 0])

 (define-operation (neg.f64 [x <binary64>]) <binary64>
   #:spec (neg x) #:impl -
   #:fpcore (! :precision binary64 (- x)) #:cost 0)
  
 (define-operations ([x <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
   [fabs.f64   #:spec (fabs x)   #:impl (from-libm 'fabs)      #:cost 0]
   [sin.f64    #:spec (sin x)    #:impl (from-libm 'sin)       #:cost 0]
   [cos.f64    #:spec (cos x)    #:impl (from-libm 'cos)       #:cost 0]
   [tan.f64    #:spec (tan x)    #:impl (from-libm 'tan)       #:cost 0]
   [sinh.f64   #:spec (sinh x)   #:impl (from-libm 'sinh)      #:cost 0]
   [cosh.f64   #:spec (cosh x)   #:impl (from-libm 'cosh)      #:cost 0]
   [acos.f64   #:spec (acos x)   #:impl (from-libm 'acos)      #:cost 0]
   [acosh.f64  #:spec (acosh x)  #:impl (from-libm 'acosh)     #:cost 0]
   [asin.f64   #:spec (asin x)   #:impl (from-libm 'asin)      #:cost 0]
   [asinh.f64  #:spec (asinh x)  #:impl (from-libm 'asinh)     #:cost 0]
   [atan.f64   #:spec (atan x)   #:impl (from-libm 'atan)      #:cost 0]
   [atanh.f64  #:spec (atanh x)  #:impl (from-libm 'atanh)     #:cost 0]
   [cbrt.f64   #:spec (cbrt x)   #:impl (from-libm 'cbrt)      #:cost 0]
   [ceil.f64   #:spec (ceil x)   #:impl (from-libm 'ceil)      #:cost 0]
   [erf.f64    #:spec (erf x)    #:impl (from-libm 'erf)       #:cost 0]
   [exp.f64    #:spec (exp x)    #:impl (from-libm 'exp)       #:cost 0]
   [exp2.f64   #:spec (exp2 x)   #:impl (from-libm 'exp2)      #:cost 0]
   [floor.f64  #:spec (floor x)  #:impl (from-libm 'floor)     #:cost 0]
   [lgamma.f64 #:spec (lgamma x) #:impl (from-libm 'lgamma)    #:cost 0]
   [log.f64    #:spec (log x)    #:impl (from-libm 'log)       #:cost 0]
   [log10.f64  #:spec (log10 x)  #:impl (from-libm 'log10)     #:cost 0]
   [log2.f64   #:spec (log2 x)   #:impl (from-libm 'log2)      #:cost 0]
   [logb.f64   #:spec (logb x)   #:impl (from-libm 'logb)      #:cost 0]
   [rint.f64   #:spec (rint x)   #:impl (from-libm 'rint)      #:cost 0]
   [round.f64  #:spec (round x)  #:impl (from-libm 'round)     #:cost 0]
   [sqrt.f64   #:spec (sqrt x)   #:impl (from-libm 'sqrt)      #:cost 0]
   [tanh.f64   #:spec (tanh x)   #:impl (from-libm 'tanh)      #:cost 0]
   [tgamma.f64 #:spec (tgamma x) #:impl (from-libm 'tgamma)    #:cost 0]
   [trunc.f64  #:spec (trunc x)  #:impl (from-libm 'trunc)     #:cost 0])
  
 (define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
   [pow.f64       #:spec (pow x y)       #:impl (from-libm 'pow)       #:cost 0]
   [atan2.f64     #:spec (atan2 x y)     #:impl (from-libm 'atan2)     #:cost 0]
   [copysign.f64  #:spec (copysign x y)  #:impl (from-libm 'copysign)  #:cost 0]
   [fdim.f64      #:spec (fdim x y)      #:impl (from-libm 'fdim)      #:cost 0]
   [fmax.f64      #:spec (fmax x y)      #:impl (from-libm 'fmax)      #:cost 0]
   [fmin.f64      #:spec (fmin x y)      #:impl (from-libm 'fmin)      #:cost 0]
   [fmod.f64      #:spec (fmod x y)      #:impl (from-libm 'fmod)      #:cost 0]
   [remainder.f64 #:spec (remainder x y) #:impl (from-libm 'remainder) #:cost 0])
  
 (define-operations ([x <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
   [erfc.f64  #:spec (- 1 (erf x)) #:impl (from-libm 'erfc)  #:fpcore (erfc x)  #:cost 0]
   [expm1.f64 #:spec (- (exp x) 1) #:impl (from-libm 'expm1) #:fpcore (expm1 x) #:cost 0]
   [log1p.f64 #:spec (log (+ 1 x)) #:impl (from-libm 'log1p) #:fpcore (log1p x) #:cost 0])

 (define-operation (hypot.f64 [x <binary64>] [y <binary64>]) <binary64>
   #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-libm 'hypot)
   #:fpcore (! :precision binary64 (hypot x y)) #:cost 0)

 (define-operation (fma.f64 [x <binary64>] [y <binary64>] [z <binary64>]) <binary64>
   #:spec (+ (* x y) z) #:impl (from-libm 'fma)
   #:fpcore (! :precision binary64 (fma x y z)) #:cost 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CASTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operation (binary64->binary32 [x <binary64>]) <binary32>
  #:spec x #:fpcore (! :precision binary32 (cast x)) #:impl flsingle #:cost 0)

(define-operation (binary32->binary64 [x <binary32>]) <binary64>
  #:spec x #:fpcore (! :precision binary64 (cast x)) #:impl identity #:cost 0)
