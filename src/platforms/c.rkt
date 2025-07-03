#lang racket

;;; C platform:
;;; C/C++ on Linux with a full libm

(require math/bigfloat
         math/flonum
         "runtime/libm.rkt"    ; libm wrapper
         "../utils/float.rkt"  ; for shift/unshift
         "../syntax/platform.rkt")
(provide platform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMPTY PLATFORM ;;;;;;;;;;;;;;;;;;;;;;;;

(define 64bit-move-cost   0.12538399999999972)
(define 32bit-move-cost   0.12961999999999974)
(define boolean-move-cost 0.1)
(define gen (from-libm))

(define platform
  (make-empty-platform 'c #:if-cost boolean-move-cost))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define bool <bool>)

(platform-register-representation! platform #:repr bool #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([TRUE  () bool (TRUE)  (const true)  (! TRUE)  boolean-move-cost]
  [FALSE () bool (FALSE) (const false) (! FALSE) boolean-move-cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(platform-register-implementations!
 platform
 ([not ([x : bool])            bool (not x)   not    (not x)   boolean-move-cost]
  [and ([x : bool] [y : bool]) bool (and x y) and-fn (and x y) boolean-move-cost]
  [or  ([x : bool] [y : bool]) bool (or x y)  or-fn  (or x y)  boolean-move-cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define binary32 <binary32>)

(platform-register-representation! platform #:repr binary32 #:cost 32bit-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([PI.f32       () binary32 (PI)       (const (flsingle pi))        (! :precision binary32 PI)       32bit-move-cost]
  [E.f32        () binary32 (E)        (const (flsingle (exp 1.0))) (! :precision binary32 E)        32bit-move-cost]
  [INFINITY.f32 () binary32 (INFINITY) (const +inf.0)               (! :precision binary32 INFINITY) 32bit-move-cost]
  [NAN.f32      () binary32 (NAN)      (const +nan.0)               (! :precision binary32 NAN)      32bit-move-cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ([name   ([var : repr] ...)              otype    spec     fl         fpcore                          cost])
(platform-register-implementations!
 platform
 ([neg.f32 ([x : binary32])                binary32 (neg x)  fl32-      (! :precision binary32 (- x))   0.11567699999999992]
  [+.f32   ([x : binary32] [y : binary32]) binary32 (+ x y)  fl32+      (! :precision binary32 (+ x y)) 0.200445]
  [-.f32   ([x : binary32] [y : binary32]) binary32 (- x y)  fl32-      (! :precision binary32 (- x y)) 0.19106800000000014]
  [*.f32   ([x : binary32] [y : binary32]) binary32 (* x y)  fl32*      (! :precision binary32 (* x y)) 0.256602]
  [/.f32   ([x : binary32] [y : binary32]) binary32 (/ x y)  fl32/      (! :precision binary32 (/ x y)) 0.3465330000000001]
  [==.f32  ([x : binary32] [y : binary32]) bool     (== x y) =          (== x y)                        32bit-move-cost]
  [!=.f32  ([x : binary32] [y : binary32]) bool     (!= x y) (negate =) (!= x y)                        32bit-move-cost]
  [<.f32   ([x : binary32] [y : binary32]) bool     (< x y)  <          (< x y)                         32bit-move-cost]
  [>.f32   ([x : binary32] [y : binary32]) bool     (> x y)  >          (> x y)                         32bit-move-cost]
  [<=.f32  ([x : binary32] [y : binary32]) bool     (<= x y) <=         (<= x y)                        32bit-move-cost]
  [>=.f32  ([x : binary32] [y : binary32]) bool     (>= x y) >=         (>= x y)                        32bit-move-cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

; ([name         ([var : repr] ...)              otype    spec            fl    fpcore                                  cost])
(platform-register-implementations!
 platform
 (; Unary libm operators
  [fabs.f32      ([x : binary32])                binary32 (fabs x)        gen (! :precision binary32 (fabs x))        0.12464599999999992]
  [sin.f32       ([x : binary32])                binary32 (sin x)         gen (! :precision binary32 (sin x))         4.2185290000000003]
  [cos.f32       ([x : binary32])                binary32 (cos x)         gen (! :precision binary32 (cos x))         4.2738829999999994]
  [tan.f32       ([x : binary32])                binary32 (tan x)         gen (! :precision binary32 (tan x))         4.669173000000001]
  [sinh.f32      ([x : binary32])                binary32 (sinh x)        gen (! :precision binary32 (sinh x))        1.7463029999999996]
  [cosh.f32      ([x : binary32])                binary32 (cosh x)        gen (! :precision binary32 (cosh x))        1.32949]
  [acos.f32      ([x : binary32])                binary32 (acos x)        gen (! :precision binary32 (acos x))        0.5213330000000001]
  [acosh.f32     ([x : binary32])                binary32 (acosh x)       gen (! :precision binary32 (acosh x))       0.847705]
  [asin.f32      ([x : binary32])                binary32 (asin x)        gen (! :precision binary32 (asin x))        0.4802670000000002]
  [asinh.f32     ([x : binary32])                binary32 (asinh x)       gen (! :precision binary32 (asinh x))       1.1207249999999999]
  [atan.f32      ([x : binary32])                binary32 (atan x)        gen (! :precision binary32 (atan x))        1.1015179999999995]
  [atanh.f32     ([x : binary32])                binary32 (atanh x)       gen (! :precision binary32 (atanh x))       0.4652830000000002]
  [cbrt.f32      ([x : binary32])                binary32 (cbrt x)        gen (! :precision binary32 (cbrt x))        1.946652]
  [ceil.f32      ([x : binary32])                binary32 (ceil x)        gen (! :precision binary32 (ceil x))        0.287118]
  [erf.f32       ([x : binary32])                binary32 (erf x)         gen (! :precision binary32 (erf x))         1.1211180000000001]
  [exp.f32       ([x : binary32])                binary32 (exp x)         gen (! :precision binary32 (exp x))         1.3860560000000002]
  [exp2.f32      ([x : binary32])                binary32 (exp2 x)        gen (! :precision binary32 (exp2 x))        1.1716009999999999]
  [floor.f32     ([x : binary32])                binary32 (floor x)       gen (! :precision binary32 (floor x))       0.2959660000000004]
  [lgamma.f32    ([x : binary32])                binary32 (lgamma x)      gen (! :precision binary32 (lgamma x))      2.2203209999999998]
  [log.f32       ([x : binary32])                binary32 (log x)         gen (! :precision binary32 (log x))         0.778197]
  [log10.f32     ([x : binary32])                binary32 (log10 x)       gen (! :precision binary32 (log10 x))       1.1793579999999997]
  [log2.f32      ([x : binary32])                binary32 (log2 x)        gen (! :precision binary32 (log2 x))        0.8644160000000004]
  [logb.f32      ([x : binary32])                binary32 (logb x)        gen (! :precision binary32 (logb x))        0.36221]
  [rint.f32      ([x : binary32])                binary32 (rint x)        gen (! :precision binary32 (rint x))        0.29316799999999997]
  [round.f32     ([x : binary32])                binary32 (round x)       gen (! :precision binary32 (round x))       0.8677139999999997]
  [sqrt.f32      ([x : binary32])                binary32 (sqrt x)        gen (! :precision binary32 (sqrt x))        0.25464700000000007]
  [tanh.f32      ([x : binary32])                binary32 (tanh x)        gen (! :precision binary32 (tanh x))        1.0567220000000002]
  [tgamma.f32    ([x : binary32])                binary32 (tgamma x)      gen (! :precision binary32 (tgamma x))      2.6280819999999994]
  [trunc.f32     ([x : binary32])                binary32 (trunc x)       gen (! :precision binary32 (trunc x))       0.28765399999999997]
  ; Binary libm operators
  [pow.f32       ([x : binary32] [y : binary32]) binary32 (pow x y)       gen (! :precision binary32 (pow x y))       2.028296]
  [atan2.f32     ([x : binary32] [y : binary32]) binary32 (atan2 x y)     gen (! :precision binary32 (atan2 x y))     1.9559129999999997]
  [copysign.f32  ([x : binary32] [y : binary32]) binary32 (copysign x y)  gen (! :precision binary32 (copysign x y))  0.2042799999999998]
  [fdim.f32      ([x : binary32] [y : binary32]) binary32 (fdim x y)      gen (! :precision binary32 (fdim x y))      0.7635619999999999]
  [fmax.f32      ([x : binary32] [y : binary32]) binary32 (fmax x y)      gen (! :precision binary32 (fmax x y))      0.23636500000000005]
  [fmin.f32      ([x : binary32] [y : binary32]) binary32 (fmin x y)      gen (! :precision binary32 (fmin x y))      0.24126899999999996]
  [fmod.f32      ([x : binary32] [y : binary32]) binary32 (fmod x y)      gen (! :precision binary32 (fmod x y))      1.7182470000000002]
  [remainder.f32 ([x : binary32] [y : binary32]) binary32 (remainder x y) gen (! :precision binary32 (remainder x y)) 1.030245]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

; ([name     ([var : repr] ...)                             otype    spec                       fl      fpcore                               cost])
(platform-register-implementations!
 platform
 ([erfc.f32  ([x : binary32])                               binary32 (- 1 (erf x))              gen (! :precision binary32 (erfc x))    0.907758]
  [expm1.f32 ([x : binary32])                               binary32 (- (exp x) 1)              gen (! :precision binary32 (expm1 x))   0.906484]
  [log1p.f32 ([x : binary32])                               binary32 (log (+ 1 x))              gen (! :precision binary32 (log1p x))   1.302969]
  [hypot.f32 ([x : binary32] [y : binary32])                binary32 (sqrt (+ (* x x) (* y y))) gen (! :precision binary32 (hypot x y)) 1.6816069999999997]
  [fma.f32   ([x : binary32] [y : binary32] [z : binary32]) binary32 (+ (* x y) z)              gen (! :precision binary32 (fma x y z)) 0.38934]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

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
  [fabs.f64      ([x : binary64])                binary64 (fabs x)        gen (! :precision binary64 (fabs x))        0.14942199999999971]
  [sin.f64       ([x : binary64])                binary64 (sin x)         gen (! :precision binary64 (sin x))         3.979444]
  [cos.f64       ([x : binary64])                binary64 (cos x)         gen (! :precision binary64 (cos x))         3.9988340000000006]
  [tan.f64       ([x : binary64])                binary64 (tan x)         gen (! :precision binary64 (tan x))         4.3931]
  [sinh.f64      ([x : binary64])                binary64 (sinh x)        gen (! :precision binary64 (sinh x))        1.5345409999999998]
  [cosh.f64      ([x : binary64])                binary64 (cosh x)        gen (! :precision binary64 (cosh x))        1.3327939999999996]
  [acos.f64      ([x : binary64])                binary64 (acos x)        gen (! :precision binary64 (acos x))        0.5283240000000002]
  [acosh.f64     ([x : binary64])                binary64 (acosh x)       gen (! :precision binary64 (acosh x))       0.929629]
  [asin.f64      ([x : binary64])                binary64 (asin x)        gen (! :precision binary64 (asin x))        0.592536]
  [asinh.f64     ([x : binary64])                binary64 (asinh x)       gen (! :precision binary64 (asinh x))       1.120752]
  [atan.f64      ([x : binary64])                binary64 (atan x)        gen (! :precision binary64 (atan x))        1.1491229999999997]
  [atanh.f64     ([x : binary64])                binary64 (atanh x)       gen (! :precision binary64 (atanh x))       0.4773569999999998]
  [cbrt.f64      ([x : binary64])                binary64 (cbrt x)        gen (! :precision binary64 (cbrt x))        1.8992809999999996]
  [ceil.f64      ([x : binary64])                binary64 (ceil x)        gen (! :precision binary64 (ceil x))        0.2699039999999999]
  [erf.f64       ([x : binary64])                binary64 (erf x)         gen (! :precision binary64 (erf x))         1.0642519999999996]
  [exp.f64       ([x : binary64])                binary64 (exp x)         gen (! :precision binary64 (exp x))         1.270031]
  [exp2.f64      ([x : binary64])                binary64 (exp2 x)        gen (! :precision binary64 (exp2 x))        1.0709029999999997]
  [floor.f64     ([x : binary64])                binary64 (floor x)       gen (! :precision binary64 (floor x))       0.27001899999999957]
  [lgamma.f64    ([x : binary64])                binary64 (lgamma x)      gen (! :precision binary64 (lgamma x))      2.1097169999999998]
  [log.f64       ([x : binary64])                binary64 (log x)         gen (! :precision binary64 (log x))         0.719641]
  [log10.f64     ([x : binary64])                binary64 (log10 x)       gen (! :precision binary64 (log10 x))       1.0994940000000002]
  [log2.f64      ([x : binary64])                binary64 (log2 x)        gen (! :precision binary64 (log2 x))        0.8050260000000001]
  [logb.f64      ([x : binary64])                binary64 (logb x)        gen (! :precision binary64 (logb x))        0.33284199999999986]
  [rint.f64      ([x : binary64])                binary64 (rint x)        gen (! :precision binary64 (rint x))        0.3101590000000003]
  [round.f64     ([x : binary64])                binary64 (round x)       gen (! :precision binary64 (round x))       0.7762879999999997]
  [sqrt.f64      ([x : binary64])                binary64 (sqrt x)        gen (! :precision binary64 (sqrt x))        0.2225119999999998]
  [tanh.f64      ([x : binary64])                binary64 (tanh x)        gen (! :precision binary64 (tanh x))        0.9856559999999996]
  [tgamma.f64    ([x : binary64])                binary64 (tgamma x)      gen (! :precision binary64 (tgamma x))      2.4006499999999997]
  [trunc.f64     ([x : binary64])                binary64 (trunc x)       gen (! :precision binary64 (trunc x))       0.2714409999999996]
  ; Binary libm operators
  [pow.f64       ([x : binary64] [y : binary64]) binary64 (pow x y)       gen (! :precision binary64 (pow x y))       1.860082]
  [atan2.f64     ([x : binary64] [y : binary64]) binary64 (atan2 x y)     gen (! :precision binary64 (atan2 x y))     1.8138300000000002]
  [copysign.f64  ([x : binary64] [y : binary64]) binary64 (copysign x y)  gen (! :precision binary64 (copysign x y))  0.23862799999999962]
  [fdim.f64      ([x : binary64] [y : binary64]) binary64 (fdim x y)      gen (! :precision binary64 (fdim x y))      0.7254930000000001]
  [fmax.f64      ([x : binary64] [y : binary64]) binary64 (fmax x y)      gen (! :precision binary64 (fmax x y))      0.2480699999999998]
  [fmin.f64      ([x : binary64] [y : binary64]) binary64 (fmin x y)      gen (! :precision binary64 (fmin x y))      0.26680799999999994]
  [fmod.f64      ([x : binary64] [y : binary64]) binary64 (fmod x y)      gen (! :precision binary64 (fmod x y))      1.5324479999999996]
  [remainder.f64 ([x : binary64] [y : binary64]) binary64 (remainder x y) gen (! :precision binary64 (remainder x y)) 0.9494380000000005]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

; ([name     ([var : repr] ...)                             otype    spec                       fl    fpcore                              cost])
(platform-register-implementations!
 platform
 ([erfc.f64  ([x : binary64])                               binary64 (- 1 (erf x))              gen (! :precision binary64 (erfc x))    0.8588620000000002]
  [expm1.f64 ([x : binary64])                               binary64 (- (exp x) 1)              gen (! :precision binary64 (expm1 x))   0.8483490000000002]
  [log1p.f64 ([x : binary64])                               binary64 (log (+ 1 x))              gen (! :precision binary64 (log1p x))   1.2416829999999997]
  [hypot.f64 ([x : binary64] [y : binary64])                binary64 (sqrt (+ (* x x) (* y y))) gen (! :precision binary64 (hypot x y)) 1.498331]
  [fma.f64   ([x : binary64] [y : binary64] [z : binary64]) binary64 (+ (* x y) z)              gen (! :precision binary64 (fma x y z)) 0.37174700000000026]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; additional converters ;;;;;;;;;;;;;;;;;

#;(platform-register-implementation! platform
                                     (make-operator-impl (binary64->binary32 [x : binary64])
                                                         binary32
                                                         #:spec x
                                                         #:fpcore (! :precision binary32 (cast x))
                                                         #:fl flsingle
                                                         #:cost 32bit-move-cost))

#;(platform-register-implementation! platform
                                     (make-operator-impl (binary32->binary64 [x : binary32])
                                                         binary64
                                                         #:spec x
                                                         #:fpcore (! :precision binary64 (cast x))
                                                         #:fl identity
                                                         #:cost 64bit-move-cost))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REGISTER PLATFORM ;;;;;;;;;;;;;;;;;;;;;

(register-platform! platform)

(module+ main
  (display-platform platform))

;; Do not run this file during testing
(module test racket/base
  )
