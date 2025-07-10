#lang s-exp "../platform.rkt"

;; C/C++ platform with a full libm

(require math/flonum)

(define-syntax-rule (define-operation (name [arg irepr] ...) orepr
                      flags ...)
  (let ([impl (make-operator-impl (name [arg : irepr] ...) orepr
                                  flags ...)])
    (platform-register-implementation! platform impl)))

(define-syntax-rule (define-operations ([arg irepr] ...) orepr
                      [name flags ...] ...)
  (begin
    (define-operation (name [arg irepr] ...) orepr flags ...) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMPTY PLATFORM ;;;;;;;;;;;;;;;;;;;;;;;;

(define 64bit-move-cost   0.12538399999999972)
(define 32bit-move-cost   0.12961999999999974)
(define boolean-move-cost 0.1)

(platform-register-if-cost! platform #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bool <bool>)

(platform-register-representation! platform #:repr bool #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operations () <bool>
  [TRUE  #:spec (TRUE)  #:fl (const true)  #:fpcore TRUE  #:cost boolean-move-cost]
  [FALSE #:spec (FALSE) #:fl (const false) #:fpcore FALSE #:cost boolean-move-cost])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(define-operations ([x <bool>] [y <bool>]) <bool>
  [and #:spec (and x y) #:fl and-fn #:fpcore (and x y) #:cost boolean-move-cost]
  [or  #:spec (or x y)  #:fl or-fn  #:fpcore (or x y)  #:cost boolean-move-cost])

(define-operation (not [x <bool>]) <bool>
  #:spec (not x) #:fl not #:fpcore (not x) #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define binary32 <binary32>)

(platform-register-representation! platform #:repr binary32 #:cost 32bit-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pi.f32 (flsingle pi))
(define e.f32 (flsingle (exp 1)))

(define-operations () <binary32>
  [PI.f32   #:spec (PI)       #:fl (const pi.f32) #:fpcore (! :precision binary32 PI)       #:cost 32bit-move-cost]
  [E.f32    #:spec (E)        #:fl (const e.f32)  #:fpcore (! :precision binary32 E)        #:cost 32bit-move-cost]
  [INFINITY #:spec (INFINITY) #:fl (const +inf.0) #:fpcore (! :precision binary32 INFINITY) #:cost 32bit-move-cost]
  [NAN.f32  #:spec (NAN)      #:fl (const +nan.0) #:fpcore (! :precision binary32 NAN)      #:cost 32bit-move-cost])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operation (neg.f32 [x <binary32>]) <binary32>
  #:spec (neg x) #:fl (compose flsingle -) #:fpcore (! :precision binary32 (- x)) #:cost 0.11567699999999992)

(define-operations ([x <binary32>] [y <binary32>]) <binary32>
  [+.f32 #:spec (+ x y) #:fl (compose flsingle +) #:fpcore (! :precision binary32 (+ x y)) #:cost 0.200445]
  [-.f32 #:spec (- x y) #:fl (compose flsingle -) #:fpcore (! :precision binary32 (- x y)) #:cost 0.19106800000000014]
  [*.f32 #:spec (* x y) #:fl (compose flsingle *) #:fpcore (! :precision binary32 (* x y)) #:cost 0.256602]
  [/.f32 #:spec (/ x y) #:fl (compose flsingle /) #:fpcore (! :precision binary32 (/ x y)) #:cost 0.3465330000000001])

(define-operations ([x <binary32>] [y <binary32>]) <bool>
  [==.f32 #:spec (== x y) #:fl =          #:fpcore (== x y) #:cost 32bit-move-cost]
  [!=.f32 #:spec (!= x y) #:fl (negate =) #:fpcore (!= x y) #:cost 32bit-move-cost]
  [<.f32  #:spec (< x y)  #:fl <          #:fpcore (< x y)  #:cost 32bit-move-cost]
  [>.f32  #:spec (> x y)  #:fl >          #:fpcore (> x y)  #:cost 32bit-move-cost]
  [<=.f32 #:spec (<= x y) #:fl <=         #:fpcore (<= x y) #:cost 32bit-move-cost]
  [>=.f32 #:spec (>= x y) #:fl >=         #:fpcore (>= x y) #:cost 32bit-move-cost])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

(define-operations ([x <binary32>]) <binary32>
  [fabs.f32   #:spec (fabs x)   #:fl (from-libm 'fabsf)     #:fpcore (! :precision binary32 (fabs x))   #:cost 0.12464599999999992]
  [sin.f32    #:spec (sin x)    #:fl (from-libm 'sinf)      #:fpcore (! :precision binary32 (sin x))    #:cost 4.2185290000000003]
  [cos.f32    #:spec (cos x)    #:fl (from-libm 'cosf)      #:fpcore (! :precision binary32 (cos x))    #:cost 4.2738829999999994]
  [tan.f32    #:spec (tan x)    #:fl (from-libm 'tanf)      #:fpcore (! :precision binary32 (tan x))    #:cost 4.669173000000001]
  [sinh.f32   #:spec (sinh x)   #:fl (from-libm 'sinhf)     #:fpcore (! :precision binary32 (sinh x))   #:cost 1.7463029999999996]
  [cosh.f32   #:spec (cosh x)   #:fl (from-libm 'coshf)     #:fpcore (! :precision binary32 (cosh x))   #:cost 1.32949]
  [acos.f32   #:spec (acos x)   #:fl (from-libm 'acosf)     #:fpcore (! :precision binary32 (acos x))   #:cost 0.5213330000000001]
  [acosh.f32  #:spec (acosh x)  #:fl (from-libm 'acoshf)    #:fpcore (! :precision binary32 (acosh x))  #:cost 0.847705]
  [asin.f32   #:spec (asin x)   #:fl (from-libm 'asinf)     #:fpcore (! :precision binary32 (asin x))   #:cost 0.4802670000000002]
  [asinh.f32  #:spec (asinh x)  #:fl (from-libm 'asinhf)    #:fpcore (! :precision binary32 (asinh x))  #:cost 1.1207249999999999]
  [atan.f32   #:spec (atan x)   #:fl (from-libm 'atanf)     #:fpcore (! :precision binary32 (atan x))   #:cost 1.1015179999999995]
  [atanh.f32  #:spec (atanh x)  #:fl (from-libm 'atanhf)    #:fpcore (! :precision binary32 (atanh x))  #:cost 0.4652830000000002]
  [cbrt.f32   #:spec (cbrt x)   #:fl (from-libm 'cbrtf)     #:fpcore (! :precision binary32 (cbrt x))   #:cost 1.946652]
  [ceil.f32   #:spec (ceil x)   #:fl (from-libm 'ceilf)     #:fpcore (! :precision binary32 (ceil x))   #:cost 0.287118]
  [erf.f32    #:spec (erf x)    #:fl (from-libm 'erff)      #:fpcore (! :precision binary32 (erf x))    #:cost 1.1211180000000001]
  [exp.f32    #:spec (exp x)    #:fl (from-libm 'expf)      #:fpcore (! :precision binary32 (exp x))    #:cost 1.3860560000000002]
  [exp2.f32   #:spec (exp2 x)   #:fl (from-libm 'exp2f)     #:fpcore (! :precision binary32 (exp2 x))   #:cost 1.1716009999999999]
  [floor.f32  #:spec (floor x)  #:fl (from-libm 'floorf)    #:fpcore (! :precision binary32 (floor x))  #:cost 0.2959660000000004]
  [lgamma.f32 #:spec (lgamma x) #:fl (from-libm 'lgammaf)   #:fpcore (! :precision binary32 (lgamma x)) #:cost 2.2203209999999998]
  [log.f32    #:spec (log x)    #:fl (from-libm 'logf)      #:fpcore (! :precision binary32 (log x))    #:cost 0.778197]
  [log10.f32  #:spec (log10 x)  #:fl (from-libm 'log10f)    #:fpcore (! :precision binary32 (log10 x))  #:cost 1.1793579999999997]
  [log2.f32   #:spec (log2 x)   #:fl (from-libm 'log2f)     #:fpcore (! :precision binary32 (log2 x))   #:cost 0.8644160000000004]
  [logb.f32   #:spec (logb x)   #:fl (from-libm 'logbf)     #:fpcore (! :precision binary32 (logb x))   #:cost 0.36221]
  [rint.f32   #:spec (rint x)   #:fl (from-libm 'rintf)     #:fpcore (! :precision binary32 (rint x))   #:cost 0.29316799999999997]
  [round.f32  #:spec (round x)  #:fl (from-libm 'roundf)    #:fpcore (! :precision binary32 (round x))  #:cost 0.8677139999999997]
  [sqrt.f32   #:spec (sqrt x)   #:fl (from-libm 'sqrtf)     #:fpcore (! :precision binary32 (sqrt x))   #:cost 0.25464700000000007]
  [tanh.f32   #:spec (tanh x)   #:fl (from-libm 'tanhf)     #:fpcore (! :precision binary32 (tanh x))   #:cost 1.0567220000000002]
  [tgamma.f32 #:spec (tgamma x) #:fl (from-libm 'tgammaf)   #:fpcore (! :precision binary32 (tgamma x)) #:cost 2.6280819999999994]
  [trunc.f32  #:spec (trunc x)  #:fl (from-libm 'truncf)    #:fpcore (! :precision binary32 (trunc x))  #:cost 0.28765399999999997])

(define-operations ([x <binary32>] [y <binary32>]) <binary32>
  [pow.f32       #:spec (pow x y)       #:fl (from-libm 'powf)       #:fpcore (! :precision binary32 (pow x y))       #:cost 2.028296]
  [atan2.f32     #:spec (atan2 x y)     #:fl (from-libm 'atan2f)     #:fpcore (! :precision binary32 (atan2 x y))     #:cost 1.9559129999999997]
  [copysign.f32  #:spec (copysign x y)  #:fl (from-libm 'copysignf)  #:fpcore (! :precision binary32 (copysign x y))  #:cost 0.2042799999999998]
  [fdim.f32      #:spec (fdim x y)      #:fl (from-libm 'fdimf)      #:fpcore (! :precision binary32 (fdim x y))      #:cost 0.7635619999999999]
  [fmax.f32      #:spec (fmax x y)      #:fl (from-libm 'fmaxf)      #:fpcore (! :precision binary32 (fmax x y))      #:cost 0.23636500000000005]
  [fmin.f32      #:spec (fmin x y)      #:fl (from-libm 'fminf)      #:fpcore (! :precision binary32 (fmin x y))      #:cost 0.24126899999999996]
  [fmod.f32      #:spec (fmod x y)      #:fl (from-libm 'fmodf)      #:fpcore (! :precision binary32 (fmod x y))      #:cost 1.7182470000000002]
  [remainder.f32 #:spec (remainder x y) #:fl (from-libm 'remainderf) #:fpcore (! :precision binary32 (remainder x y)) #:cost 1.030245]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([erfc.f32  ([x : binary32])                               binary32 (- 1 (erf x))              (from-libm 'erfcf)  (! :precision binary32 (erfc x))    0.907758]
  [expm1.f32 ([x : binary32])                               binary32 (- (exp x) 1)              (from-libm 'expm1f) (! :precision binary32 (expm1 x))   0.906484]
  [log1p.f32 ([x : binary32])                               binary32 (log (+ 1 x))              (from-libm 'log1pf) (! :precision binary32 (log1p x))   1.302969]
  [hypot.f32 ([x : binary32] [y : binary32])                binary32 (sqrt (+ (* x x) (* y y))) (from-libm 'hypotf) (! :precision binary32 (hypot x y)) 1.6816069999999997]
  [fma.f32   ([x : binary32] [y : binary32] [z : binary32]) binary32 (+ (* x y) z)              (from-libm 'fmaf)   (! :precision binary32 (fma x y z)) 0.38934]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define binary64 <binary64>)

(platform-register-representation! platform #:repr binary64 #:cost 64bit-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([PI.f64       () binary64 (PI)       (const pi)        (! :precision binary64 PI)       64bit-move-cost]
  [E.f64        () binary64 (E)        (const (exp 1.0)) (! :precision binary64 E)        64bit-move-cost]
  [INFINITY.f64 () binary64 (INFINITY) (const +inf.0)    (! :precision binary64 INFINITY) 64bit-move-cost]
  [NAN.f64      () binary64 (NAN)      (const +nan.0)    (! :precision binary64 NAN)      64bit-move-cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([neg.f64 ([x : binary64])                binary64 (neg x)  -          (! :precision binary64 (- x))   0.12114199999999964]
  [+.f64   ([x : binary64] [y : binary64]) binary64 (+ x y)  +          (! :precision binary64 (+ x y)) 0.2174189999999998]
  [-.f64   ([x : binary64] [y : binary64]) binary64 (- x y)  -          (! :precision binary64 (- x y)) 0.20265700000000008]
  [*.f64   ([x : binary64] [y : binary64]) binary64 (* x y)  *          (! :precision binary64 (* x y)) 0.24512299999999976]
  [/.f64   ([x : binary64] [y : binary64]) binary64 (/ x y)  /          (! :precision binary64 (/ x y)) 0.2962459999999998]
  [==.f64  ([x : binary64] [y : binary64]) bool     (== x y) =          (== x y)                        64bit-move-cost]
  [!=.f64  ([x : binary64] [y : binary64]) bool     (!= x y) (negate =) (!= x y)                        64bit-move-cost]
  [<.f64   ([x : binary64] [y : binary64]) bool     (< x y)  <          (< x y)                         64bit-move-cost]
  [>.f64   ([x : binary64] [y : binary64]) bool     (> x y)  >          (> x y)                         64bit-move-cost]
  [<=.f64  ([x : binary64] [y : binary64]) bool     (<= x y) <=         (<= x y)                        64bit-move-cost]
  [>=.f64  ([x : binary64] [y : binary64]) bool     (>= x y) >=         (>= x y)                        64bit-move-cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

; ([name         ([var : repr] ...)              otype    spec            fl    fpcore                                  cost])
(platform-register-implementations!
 platform
 (; Unary libm operators
  [fabs.f64      ([x : binary64])                binary64 (fabs x)        (from-libm 'fabs)      (! :precision binary64 (fabs x))        0.14942199999999971]
  [sin.f64       ([x : binary64])                binary64 (sin x)         (from-libm 'sin)       (! :precision binary64 (sin x))         3.979444]
  [cos.f64       ([x : binary64])                binary64 (cos x)         (from-libm 'cos)       (! :precision binary64 (cos x))         3.9988340000000006]
  [tan.f64       ([x : binary64])                binary64 (tan x)         (from-libm 'tan)       (! :precision binary64 (tan x))         4.3931]
  [sinh.f64      ([x : binary64])                binary64 (sinh x)        (from-libm 'sinh)      (! :precision binary64 (sinh x))        1.5345409999999998]
  [cosh.f64      ([x : binary64])                binary64 (cosh x)        (from-libm 'cosh)      (! :precision binary64 (cosh x))        1.3327939999999996]
  [acos.f64      ([x : binary64])                binary64 (acos x)        (from-libm 'acos)      (! :precision binary64 (acos x))        0.5283240000000002]
  [acosh.f64     ([x : binary64])                binary64 (acosh x)       (from-libm 'acosh)     (! :precision binary64 (acosh x))       0.929629]
  [asin.f64      ([x : binary64])                binary64 (asin x)        (from-libm 'asin)      (! :precision binary64 (asin x))        0.592536]
  [asinh.f64     ([x : binary64])                binary64 (asinh x)       (from-libm 'asinh)     (! :precision binary64 (asinh x))       1.120752]
  [atan.f64      ([x : binary64])                binary64 (atan x)        (from-libm 'atan)      (! :precision binary64 (atan x))        1.1491229999999997]
  [atanh.f64     ([x : binary64])                binary64 (atanh x)       (from-libm 'atanh)     (! :precision binary64 (atanh x))       0.4773569999999998]
  [cbrt.f64      ([x : binary64])                binary64 (cbrt x)        (from-libm 'cbrt)      (! :precision binary64 (cbrt x))        1.8992809999999996]
  [ceil.f64      ([x : binary64])                binary64 (ceil x)        (from-libm 'ceil)      (! :precision binary64 (ceil x))        0.2699039999999999]
  [erf.f64       ([x : binary64])                binary64 (erf x)         (from-libm 'erf)       (! :precision binary64 (erf x))         1.0642519999999996]
  [exp.f64       ([x : binary64])                binary64 (exp x)         (from-libm 'exp)       (! :precision binary64 (exp x))         1.270031]
  [exp2.f64      ([x : binary64])                binary64 (exp2 x)        (from-libm 'exp2)      (! :precision binary64 (exp2 x))        1.0709029999999997]
  [floor.f64     ([x : binary64])                binary64 (floor x)       (from-libm 'floor)     (! :precision binary64 (floor x))       0.27001899999999957]
  [lgamma.f64    ([x : binary64])                binary64 (lgamma x)      (from-libm 'lgamma)    (! :precision binary64 (lgamma x))      2.1097169999999998]
  [log.f64       ([x : binary64])                binary64 (log x)         (from-libm 'log)       (! :precision binary64 (log x))         0.719641]
  [log10.f64     ([x : binary64])                binary64 (log10 x)       (from-libm 'log10)     (! :precision binary64 (log10 x))       1.0994940000000002]
  [log2.f64      ([x : binary64])                binary64 (log2 x)        (from-libm 'log2)      (! :precision binary64 (log2 x))        0.8050260000000001]
  [logb.f64      ([x : binary64])                binary64 (logb x)        (from-libm 'logb)      (! :precision binary64 (logb x))        0.33284199999999986]
  [rint.f64      ([x : binary64])                binary64 (rint x)        (from-libm 'rint)      (! :precision binary64 (rint x))        0.3101590000000003]
  [round.f64     ([x : binary64])                binary64 (round x)       (from-libm 'round)     (! :precision binary64 (round x))       0.7762879999999997]
  [sqrt.f64      ([x : binary64])                binary64 (sqrt x)        (from-libm 'sqrt)      (! :precision binary64 (sqrt x))        0.2225119999999998]
  [tanh.f64      ([x : binary64])                binary64 (tanh x)        (from-libm 'tanh)      (! :precision binary64 (tanh x))        0.9856559999999996]
  [tgamma.f64    ([x : binary64])                binary64 (tgamma x)      (from-libm 'tgamma)    (! :precision binary64 (tgamma x))      2.4006499999999997]
  [trunc.f64     ([x : binary64])                binary64 (trunc x)       (from-libm 'trunc)     (! :precision binary64 (trunc x))       0.2714409999999996]
  ; Binary libm operators
  [pow.f64       ([x : binary64] [y : binary64]) binary64 (pow x y)       (from-libm 'pow)       (! :precision binary64 (pow x y))       1.860082]
  [atan2.f64     ([x : binary64] [y : binary64]) binary64 (atan2 x y)     (from-libm 'atan2)     (! :precision binary64 (atan2 x y))     1.8138300000000002]
  [copysign.f64  ([x : binary64] [y : binary64]) binary64 (copysign x y)  (from-libm 'copysign)  (! :precision binary64 (copysign x y))  0.23862799999999962]
  [fdim.f64      ([x : binary64] [y : binary64]) binary64 (fdim x y)      (from-libm 'fdim)      (! :precision binary64 (fdim x y))      0.7254930000000001]
  [fmax.f64      ([x : binary64] [y : binary64]) binary64 (fmax x y)      (from-libm 'fmax)      (! :precision binary64 (fmax x y))      0.2480699999999998]
  [fmin.f64      ([x : binary64] [y : binary64]) binary64 (fmin x y)      (from-libm 'fmin)      (! :precision binary64 (fmin x y))      0.26680799999999994]
  [fmod.f64      ([x : binary64] [y : binary64]) binary64 (fmod x y)      (from-libm 'fmod)      (! :precision binary64 (fmod x y))      1.5324479999999996]
  [remainder.f64 ([x : binary64] [y : binary64]) binary64 (remainder x y) (from-libm 'remainder) (! :precision binary64 (remainder x y)) 0.9494380000000005]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

; ([name     ([var : repr] ...)                             otype    spec                       fl    fpcore                              cost])
(platform-register-implementations!
 platform
 ([erfc.f64  ([x : binary64])                               binary64 (- 1 (erf x))              (from-libm 'erfc)  (! :precision binary64 (erfc x))    0.8588620000000002]
  [expm1.f64 ([x : binary64])                               binary64 (- (exp x) 1)              (from-libm 'expm1) (! :precision binary64 (expm1 x))   0.8483490000000002]
  [log1p.f64 ([x : binary64])                               binary64 (log (+ 1 x))              (from-libm 'log1p) (! :precision binary64 (log1p x))   1.2416829999999997]
  [hypot.f64 ([x : binary64] [y : binary64])                binary64 (sqrt (+ (* x x) (* y y))) (from-libm 'hypot) (! :precision binary64 (hypot x y)) 1.498331]
  [fma.f64   ([x : binary64] [y : binary64] [z : binary64]) binary64 (+ (* x y) z)              (from-libm 'fma)   (! :precision binary64 (fma x y z)) 0.37174700000000026]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; additional converters ;;;;;;;;;;;;;;;;;

(define-operation (binary64->binary32 [x <binary64>]) <binary32>
  #:spec x #:fpcore (! :precision binary32 (cast x)) #:fl flsingle #:cost 32bit-move-cost)

(define-operation (binary32->binary64 [x <binary32>]) <binary64>
  #:spec x #:fpcore (! :precision binary32 (cast x)) #:fl identity #:cost 64bit-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REGISTER PLATFORM ;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (display-platform platform))

;; Do not run this file during testing
(module test racket/base
  )
