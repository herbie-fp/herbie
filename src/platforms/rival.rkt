#lang s-exp "../platform.rkt"

;;; Rival correctly-rounded platform

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <bool> #:cost 1)

(define-operations () <bool>
  [TRUE  #:spec (TRUE)  #:impl (from-rival) #:fpcore TRUE  #:cost 1]
  [FALSE #:spec (FALSE) #:impl (from-rival) #:fpcore FALSE #:cost 1])

(define-operations ([x <bool>] [y <bool>]) <bool>
  [and #:spec (and x y) #:impl (from-rival) #:cost 1]
  [or  #:spec (or x y)  #:impl (from-rival) #:cost 1])

(define-operation (not [x <bool>]) <bool>
  #:spec (not x) #:impl not #:cost 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary32> #:cost 1)

(define-operation (if.f32 [c <bool>] [t <binary32>] [f <binary32>]) <binary32>
  #:spec (if c t f) #:impl (from-rival) #:cost 1 #:aggregate if-cost)

(define-operations ([x <binary32>] [y <binary32>]) <bool>
  [==.f32 #:spec (== x y) #:impl (from-rival) #:cost 1]
  [!=.f32 #:spec (!= x y) #:impl (from-rival) #:cost 1]
  [<.f32  #:spec (< x y)  #:impl (from-rival) #:cost 1]
  [>.f32  #:spec (> x y)  #:impl (from-rival) #:cost 1]
  [<=.f32 #:spec (<= x y) #:impl (from-rival) #:cost 1]
  [>=.f32 #:spec (>= x y) #:impl (from-rival) #:cost 1])

(parameterize ([fpcore-context '(:precision binary32)])
  (define-operations () <binary32>
    [PI.f32 #:spec (PI) #:impl (from-rival) #:fpcore PI #:cost 1]
    [E.f32  #:spec (E)  #:impl (from-rival) #:fpcore E  #:cost 1])
  
  (define-operation (neg.f32 [x <binary32>]) <binary32>
    #:spec (neg x) #:impl (from-rival) #:fpcore (- x) #:cost 1)
  
  (define-operations ([x <binary32>] [y <binary32>]) <binary32>
    [+.f32 #:spec (+ x y) #:impl (from-rival) #:cost 1]
    [-.f32 #:spec (- x y) #:impl (from-rival) #:cost 1]
    [*.f32 #:spec (* x y) #:impl (from-rival) #:cost 1]
    [/.f32 #:spec (/ x y) #:impl (from-rival) #:cost 1])
  
  (define-operations ([x <binary32>]) <binary32>
    [fabs.f32   #:spec (fabs x)   #:impl (from-rival) #:cost 1]
    [sin.f32    #:spec (sin x)    #:impl (from-rival) #:cost 1]
    [cos.f32    #:spec (cos x)    #:impl (from-rival) #:cost 1]
    [tan.f32    #:spec (tan x)    #:impl (from-rival) #:cost 1]
    [sinh.f32   #:spec (sinh x)   #:impl (from-rival) #:cost 1]
    [cosh.f32   #:spec (cosh x)   #:impl (from-rival) #:cost 1]
    [acos.f32   #:spec (acos x)   #:impl (from-rival) #:cost 1]
    [acosh.f32  #:spec (acosh x)  #:impl (from-rival) #:cost 1]
    [asin.f32   #:spec (asin x)   #:impl (from-rival) #:cost 1]
    [asinh.f32  #:spec (asinh x)  #:impl (from-rival) #:cost 1]
    [atan.f32   #:spec (atan x)   #:impl (from-rival) #:cost 1]
    [atanh.f32  #:spec (atanh x)  #:impl (from-rival) #:cost 1]
    [cbrt.f32   #:spec (cbrt x)   #:impl (from-rival) #:cost 1]
    [ceil.f32   #:spec (ceil x)   #:impl (from-rival) #:cost 1]
    [erf.f32    #:spec (erf x)    #:impl (from-rival) #:cost 1]
    [exp.f32    #:spec (exp x)    #:impl (from-rival) #:cost 1]
    [exp2.f32   #:spec (exp2 x)   #:impl (from-rival) #:cost 1]
    [floor.f32  #:spec (floor x)  #:impl (from-rival) #:cost 1]
    [lgamma.f32 #:spec (lgamma x) #:impl (from-rival) #:cost 1]
    [log.f32    #:spec (log x)    #:impl (from-rival) #:cost 1]
    [log10.f32  #:spec (log10 x)  #:impl (from-rival) #:cost 1]
    [log2.f32   #:spec (log2 x)   #:impl (from-rival) #:cost 1]
    [logb.f32   #:spec (logb x)   #:impl (from-rival) #:cost 1]
    [rint.f32   #:spec (rint x)   #:impl (from-rival) #:cost 1]
    [round.f32  #:spec (round x)  #:impl (from-rival) #:cost 1]
    [sqrt.f32   #:spec (sqrt x)   #:impl (from-rival) #:cost 1]
    [tanh.f32   #:spec (tanh x)   #:impl (from-rival) #:cost 1]
    [tgamma.f32 #:spec (tgamma x) #:impl (from-rival) #:cost 1]
    [trunc.f32  #:spec (trunc x)  #:impl (from-rival) #:cost 1])
  
  (define-operations ([x <binary32>] [y <binary32>]) <binary32>
    [pow.f32       #:spec (pow x y)       #:impl (from-rival) #:cost 1]
    [atan2.f32     #:spec (atan2 x y)     #:impl (from-rival) #:cost 1]
    [copysign.f32  #:spec (copysign x y)  #:impl (from-rival) #:cost 1]
    [fdim.f32      #:spec (fdim x y)      #:impl (from-rival) #:cost 1]
    [fmax.f32      #:spec (fmax x y)      #:impl (from-rival) #:cost 1]
    [fmin.f32      #:spec (fmin x y)      #:impl (from-rival) #:cost 1]
    [fmod.f32      #:spec (fmod x y)      #:impl (from-rival) #:cost 1]
    [remainder.f32 #:spec (remainder x y) #:impl (from-rival) #:cost 1])
  
  (define-operations ([x <binary32>]) <binary32>
    [erfc.f32  #:spec (- 1 (erf x)) #:impl (from-rival) #:fpcore (erfc x)  #:cost 1]
    [expm1.f32 #:spec (- (exp x) 1) #:impl (from-rival) #:fpcore (expm1 x) #:cost 1]
    [log1p.f32 #:spec (log (+ 1 x)) #:impl (from-rival) #:fpcore (log1p x) #:cost 1])
  
  (define-operation (hypot.f32 [x <binary32>] [y <binary32>]) <binary32>
    #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-rival) #:fpcore (hypot x y) #:cost 1)
  
  (define-operation (fma.f32 [x <binary32>] [y <binary32>] [z <binary32>]) <binary32>
    #:spec (+ (* x y) z) #:impl (from-rival) #:fpcore (fma x y z) #:cost 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost 1)

(define-operation (if.f64 [c <bool>] [t <binary64>] [f <binary64>]) <binary64>
  #:spec (if c t f) #:impl (from-rival) #:cost 1 #:aggregate if-cost)

(define-operations ([x <binary64>] [y <binary64>]) <bool>
  [==.f64 #:spec (== x y) #:impl (from-rival) #:cost 1]
  [!=.f64 #:spec (!= x y) #:impl (from-rival) #:cost 1]
  [<.f64  #:spec (< x y)  #:impl (from-rival) #:cost 1]
  [>.f64  #:spec (> x y)  #:impl (from-rival) #:cost 1]
  [<=.f64 #:spec (<= x y) #:impl (from-rival) #:cost 1]
  [>=.f64 #:spec (>= x y) #:impl (from-rival) #:cost 1])

(parameterize ([fpcore-context '(:precision binary64)])
  (define-operations () <binary64>
    [PI.f64   #:spec (PI)       #:impl (from-rival) #:cost 1]
    [E.f64    #:spec (E)        #:impl (from-rival) #:cost 1]
    [INFINITY #:spec (INFINITY) #:impl (from-rival) #:cost 1]
    [NAN.f64  #:spec (NAN)      #:impl (from-rival) #:cost 1])
  
  (define-operation (neg.f64 [x <binary64>]) <binary64>
    #:spec (neg x) #:impl (from-rival) #:fpcore (- x) #:cost 1)
  
  (define-operations ([x <binary64>] [y <binary64>]) <binary64>
    [+.f64 #:spec (+ x y) #:impl (from-rival) #:cost 1]
    [-.f64 #:spec (- x y) #:impl (from-rival) #:cost 1]
    [*.f64 #:spec (* x y) #:impl (from-rival) #:cost 1]
    [/.f64 #:spec (/ x y) #:impl (from-rival) #:cost 1])
  
  (define-operations ([x <binary64>]) <binary64>
    [fabs.f64   #:spec (fabs x)   #:impl (from-rival) #:cost 1]
    [sin.f64    #:spec (sin x)    #:impl (from-rival) #:cost 1]
    [cos.f64    #:spec (cos x)    #:impl (from-rival) #:cost 1]
    [tan.f64    #:spec (tan x)    #:impl (from-rival) #:cost 1]
    [sinh.f64   #:spec (sinh x)   #:impl (from-rival) #:cost 1]
    [cosh.f64   #:spec (cosh x)   #:impl (from-rival) #:cost 1]
    [acos.f64   #:spec (acos x)   #:impl (from-rival) #:cost 1]
    [acosh.f64  #:spec (acosh x)  #:impl (from-rival) #:cost 1]
    [asin.f64   #:spec (asin x)   #:impl (from-rival) #:cost 1]
    [asinh.f64  #:spec (asinh x)  #:impl (from-rival) #:cost 1]
    [atan.f64   #:spec (atan x)   #:impl (from-rival) #:cost 1]
    [atanh.f64  #:spec (atanh x)  #:impl (from-rival) #:cost 1]
    [cbrt.f64   #:spec (cbrt x)   #:impl (from-rival) #:cost 1]
    [ceil.f64   #:spec (ceil x)   #:impl (from-rival) #:cost 1]
    [erf.f64    #:spec (erf x)    #:impl (from-rival) #:cost 1]
    [exp.f64    #:spec (exp x)    #:impl (from-rival) #:cost 1]
    [exp2.f64   #:spec (exp2 x)   #:impl (from-rival) #:cost 1]
    [floor.f64  #:spec (floor x)  #:impl (from-rival) #:cost 1]
    [lgamma.f64 #:spec (lgamma x) #:impl (from-rival) #:cost 1]
    [log.f64    #:spec (log x)    #:impl (from-rival) #:cost 1]
    [log10.f64  #:spec (log10 x)  #:impl (from-rival) #:cost 1]
    [log2.f64   #:spec (log2 x)   #:impl (from-rival) #:cost 1]
    [logb.f64   #:spec (logb x)   #:impl (from-rival) #:cost 1]
    [rint.f64   #:spec (rint x)   #:impl (from-rival) #:cost 1]
    [round.f64  #:spec (round x)  #:impl (from-rival) #:cost 1]
    [sqrt.f64   #:spec (sqrt x)   #:impl (from-rival) #:cost 1]
    [tanh.f64   #:spec (tanh x)   #:impl (from-rival) #:cost 1]
    [tgamma.f64 #:spec (tgamma x) #:impl (from-rival) #:cost 1]
    [trunc.f64  #:spec (trunc x)  #:impl (from-rival) #:cost 1])
  
  (define-operations ([x <binary64>] [y <binary64>]) <binary64>
    [pow.f64       #:spec (pow x y)       #:impl (from-rival) #:cost 1]
    [atan2.f64     #:spec (atan2 x y)     #:impl (from-rival) #:cost 1]
    [copysign.f64  #:spec (copysign x y)  #:impl (from-rival) #:cost 1]
    [fdim.f64      #:spec (fdim x y)      #:impl (from-rival) #:cost 1]
    [fmax.f64      #:spec (fmax x y)      #:impl (from-rival) #:cost 1]
    [fmin.f64      #:spec (fmin x y)      #:impl (from-rival) #:cost 1]
    [fmod.f64      #:spec (fmod x y)      #:impl (from-rival) #:cost 1]
    [remainder.f64 #:spec (remainder x y) #:impl (from-rival) #:cost 1])
  
  (define-operations ([x <binary64>]) <binary64>
    [erfc.f64  #:spec (- 1 (erf x)) #:impl (from-rival) #:fpcore (erfc x)  #:cost 1]
    [expm1.f64 #:spec (- (exp x) 1) #:impl (from-rival) #:fpcore (expm1 x) #:cost 1]
    [log1p.f64 #:spec (log (+ 1 x)) #:impl (from-rival) #:fpcore (log1p x) #:cost 1])
  
  (define-operation (hypot.f64 [x <binary64>] [y <binary64>]) <binary64>
    #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-rival) #:fpcore (hypot x y) #:cost 1)
  
  (define-operation (fma.f64 [x <binary64>] [y <binary64>] [z <binary64>]) <binary64>
    #:spec (+ (* x y) z) #:impl (from-rival) #:fpcore (fma x y z) #:cost 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CASTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operation (binary64->binary32 [x <binary64>]) <binary32>
  #:spec x #:fpcore (! :precision binary32 (cast x)) #:impl (from-rival) #:cost 1)

(define-operation (binary32->binary64 [x <binary32>]) <binary64>
  #:spec x #:fpcore (! :precision binary64 (cast x)) #:impl (from-rival) #:cost 1)
