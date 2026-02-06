#lang s-exp "../syntax/platform-language.rkt"

;; C/C++ platform with a full libm

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
  #:cost (if-cost boolean-move-cost))

(define-operations ([x <binary32>] [y <binary32>]) <bool>
  [==.f32 #:spec (== x y) #:impl =          #:cost 32bit-move-cost]
  [!=.f32 #:spec (!= x y) #:impl (negate =) #:cost 32bit-move-cost]
  [<.f32  #:spec (< x y)  #:impl <          #:cost 32bit-move-cost]
  [>.f32  #:spec (> x y)  #:impl >          #:cost 32bit-move-cost]
  [<=.f32 #:spec (<= x y) #:impl <=         #:cost 32bit-move-cost]
  [>=.f32 #:spec (>= x y) #:impl >=         #:cost 32bit-move-cost])

(define-operations () <binary32> #:fpcore (! :precision binary32 _)
  [PI.f32       #:spec (PI)       #:impl (const (flsingle pi))       #:fpcore PI       #:cost 32bit-move-cost]
  [E.f32        #:spec (E)        #:impl (const (flsingle (exp 1)))  #:fpcore E        #:cost 32bit-move-cost])

(define-operation (neg.f32 [x <binary32>]) <binary32>
  #:spec (neg x) #:impl (compose flsingle -)
  #:fpcore (! :precision binary32 (- x)) #:cost 0.125)

(define-operations ([x <binary32>] [y <binary32>]) <binary32> #:fpcore (! :precision binary32 _)
  [+.f32 #:spec (+ x y) #:impl (compose flsingle +) #:cost 0.200]
  [-.f32 #:spec (- x y) #:impl (compose flsingle -) #:cost 0.200]
  [*.f32 #:spec (* x y) #:impl (compose flsingle *) #:cost 0.250]
  [/.f32 #:spec (/ x y) #:impl (compose flsingle /) #:cost 0.350])

(define-operations ([x <binary32>]) <binary32> #:fpcore (! :precision binary32 _)
  [fabs.f32   #:spec (fabs x)   #:impl (from-libm 'fabsf)   #:cost 0.125]
  [sin.f32    #:spec (sin x)    #:impl (from-libm 'sinf)    #:cost 4.250]
  [cos.f32    #:spec (cos x)    #:impl (from-libm 'cosf)    #:cost 4.250]
  [tan.f32    #:spec (tan x)    #:impl (from-libm 'tanf)    #:cost 4.750]
  [acos.f32   #:spec (acos x)   #:impl (from-libm 'acosf)   #:cost 0.500]
  [asin.f32   #:spec (asin x)   #:impl (from-libm 'asinf)   #:cost 0.500]
  [atan.f32   #:spec (atan x)   #:impl (from-libm 'atanf)   #:cost 1.100]
  [exp.f32    #:spec (exp x)    #:impl (from-libm 'expf)    #:cost 1.375]
  [log.f32    #:spec (log x)    #:impl (from-libm 'logf)    #:cost 0.750]
  [sqrt.f32   #:spec (sqrt x)   #:impl (from-libm 'sqrtf)   #:cost 0.250])

(define-operations ([x <binary32>] [y <binary32>]) <binary32> #:fpcore (! :precision binary32 _)
  [pow.f32       #:spec (pow x y)       #:impl (from-libm 'powf)       #:cost 2.000]
  [fmax.f32      #:spec (fmax x y)      #:impl (from-libm 'fmaxf)      #:cost 0.250]
  [fmin.f32      #:spec (fmin x y)      #:impl (from-libm 'fminf)      #:cost 0.250])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation <binary64> #:cost 64bit-move-cost)

(define-operation (if.f64 [c <bool>] [t <binary64>] [f <binary64>]) <binary64>
  #:spec (if c t f) #:impl if-impl
  #:cost (if-cost boolean-move-cost))

(define-operations ([x <binary64>] [y <binary64>]) <bool>
  [==.f64 #:spec (== x y) #:impl =          #:cost 64bit-move-cost]
  [!=.f64 #:spec (!= x y) #:impl (negate =) #:cost 64bit-move-cost]
  [<.f64  #:spec (< x y)  #:impl <          #:cost 64bit-move-cost]
  [>.f64  #:spec (> x y)  #:impl >          #:cost 64bit-move-cost]
  [<=.f64 #:spec (<= x y) #:impl <=         #:cost 64bit-move-cost]
  [>=.f64 #:spec (>= x y) #:impl >=         #:cost 64bit-move-cost])

(define-operations () <binary64> #:fpcore (! :precision binary64 _)
  [PI.f64   #:spec (PI)       #:impl (const pi)      #:fpcore PI       #:cost 64bit-move-cost]
  [E.f64    #:spec (E)        #:impl (const (exp 1)) #:fpcore E        #:cost 64bit-move-cost])

(define-operation (neg.f64 [x <binary64>]) <binary64>
  #:spec (neg x) #:impl - #:fpcore (! :precision binary64 (- x)) #:cost 0.125)

(define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
  [+.f64 #:spec (+ x y) #:impl + #:cost 0.200]
  [-.f64 #:spec (- x y) #:impl - #:cost 0.200]
  [*.f64 #:spec (* x y) #:impl * #:cost 0.250]
  [/.f64 #:spec (/ x y) #:impl / #:cost 0.350])

(define-operations ([x <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
  [fabs.f64   #:spec (fabs x)   #:impl (from-libm 'fabs)      #:cost 0.125]
  [sin.f64    #:spec (sin x)    #:impl (from-libm 'sin)       #:cost 4.200]
  [cos.f64    #:spec (cos x)    #:impl (from-libm 'cos)       #:cost 4.200]
  [tan.f64    #:spec (tan x)    #:impl (from-libm 'tan)       #:cost 4.650]
  [acos.f64   #:spec (acos x)   #:impl (from-libm 'acos)      #:cost 0.500]
  [asin.f64   #:spec (asin x)   #:impl (from-libm 'asin)      #:cost 0.500]
  [atan.f64   #:spec (atan x)   #:impl (from-libm 'atan)      #:cost 1.100]
  [exp.f64    #:spec (exp x)    #:impl (from-libm 'exp)       #:cost 1.375]
  [log.f64    #:spec (log x)    #:impl (from-libm 'log)       #:cost 0.750]
  [sqrt.f64   #:spec (sqrt x)   #:impl (from-libm 'sqrt)      #:cost 0.250])

(define-operations ([x <binary64>] [y <binary64>]) <binary64> #:fpcore (! :precision binary64 _)
  [pow.f64       #:spec (pow x y)       #:impl (from-libm 'pow)       #:cost 2.000]
  [fmax.f64      #:spec (fmax x y)      #:impl (from-libm 'fmax)      #:cost 0.250]
  [fmin.f64      #:spec (fmin x y)      #:impl (from-libm 'fmin)      #:cost 0.250])
