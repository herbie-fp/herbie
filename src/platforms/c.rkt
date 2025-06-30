#lang s-exp "../platform.rkt"

;; C/C++ platform with a full libm

(require math/flonum         ; for flsingle
         "runtime/libm.rkt") ; libm wrapper

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMPTY PLATFORM ;;;;;;;;;;;;;;;;;;;;;;;;

(define 64bit-move-cost   0.12538399999999972)
(define 32bit-move-cost   0.12961999999999974)
(define boolean-move-cost 0.1)

(platform-register-if-cost! platform #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bool <bool>)

(platform-register-representation! platform #:repr bool #:cost boolean-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([TRUE  () bool (TRUE)  (const true)  TRUE  boolean-move-cost]
  [FALSE () bool (FALSE) (const false) FALSE boolean-move-cost]))

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
 ([neg.f32 ([x : binary32])                binary32 (neg x)  (compose flsingle -)      (! :precision binary32 (- x))   0.11567699999999992]
  [+.f32   ([x : binary32] [y : binary32]) binary32 (+ x y)  (compose flsingle +)      (! :precision binary32 (+ x y)) 0.200445]
  [-.f32   ([x : binary32] [y : binary32]) binary32 (- x y)  (compose flsingle -)      (! :precision binary32 (- x y)) 0.19106800000000014]
  [*.f32   ([x : binary32] [y : binary32]) binary32 (* x y)  (compose flsingle *)      (! :precision binary32 (* x y)) 0.256602]
  [/.f32   ([x : binary32] [y : binary32]) binary32 (/ x y)  (compose flsingle /)      (! :precision binary32 (/ x y)) 0.3465330000000001]
  [==.f32  ([x : binary32] [y : binary32]) bool     (== x y) =          (== x y)                        32bit-move-cost]
  [!=.f32  ([x : binary32] [y : binary32]) bool     (!= x y) (negate =) (!= x y)                        32bit-move-cost]
  [<.f32   ([x : binary32] [y : binary32]) bool     (< x y)  <          (< x y)                         32bit-move-cost]
  [>.f32   ([x : binary32] [y : binary32]) bool     (> x y)  >          (> x y)                         32bit-move-cost]
  [<=.f32  ([x : binary32] [y : binary32]) bool     (<= x y) <=         (<= x y)                        32bit-move-cost]
  [>=.f32  ([x : binary32] [y : binary32]) bool     (>= x y) >=         (>= x y)                        32bit-move-cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

(define libm-impls.f32
  (make-libm-impls/binary32
   [(binary32 binary32)
    ([fabs      0.12464599999999992]
     [sin       4.2185290000000003]
     [cos       4.2738829999999994]
     [tan       4.669173000000001]
     [sinh      1.7463029999999996]
     [cosh      1.32949]
     [acos      0.5213330000000001]
     [acosh     0.847705]
     [asin      0.4802670000000002]
     [asinh     1.1207249999999999]
     [atan      1.1015179999999995]
     [atanh     0.4652830000000002]
     [cbrt      1.946652]
     [ceil      0.287118]
     [erf       1.1211180000000001]
     [exp       1.3860560000000002]
     [exp2      1.1716009999999999]
     [floor     0.2959660000000004]
     [lgamma    2.2203209999999998]
     [log       0.778197]
     [log10     1.1793579999999997]
     [log2      0.8644160000000004]
     [logb      0.36221]
     [rint      0.29316799999999997]
     [round     0.8677139999999997]
     [sqrt      0.25464700000000007]
     [tanh      1.0567220000000002]
     [tgamma    2.6280819999999994]
     [trunc     0.28765399999999997])]
   [(binary32 binary32 binary32)
    ([pow       2.028296]
     [atan2     1.9559129999999997]
     [copysign  0.2042799999999998]
     [fdim      0.7635619999999999]
     [fmax      0.23636500000000005]
     [fmin      0.24126899999999996]
     [fmod      1.7182470000000002]
     [remainder 1.030245])]))

(for ([libm-impl.f32 (in-list libm-impls.f32)])
  (platform-register-implementation! platform libm-impl.f32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

(define c_erfcf  (make-libm (erfcf  float float)))
(define c_expm1f (make-libm (expm1f float float)))
(define c_log1pf (make-libm (log1pf float float)))
(define c_hypotf (make-libm (hypotf float float float)))
(define c_fmaf   (make-libm (fmaf   float float float float)))

; ([name     ([var : repr] ...)                             otype    spec                       fl      fpcore                               cost])
(platform-register-implementations!
 platform
 ([erfc.f32  ([x : binary32])                               binary32 (- 1 (erf x))              c_erfcf  (! :precision binary32 (erfc x))    0.907758]
  [expm1.f32 ([x : binary32])                               binary32 (- (exp x) 1)              c_expm1f (! :precision binary32 (expm1 x))   0.906484]
  [log1p.f32 ([x : binary32])                               binary32 (log (+ 1 x))              c_log1pf (! :precision binary32 (log1p x))   1.302969]
  [hypot.f32 ([x : binary32] [y : binary32])                binary32 (sqrt (+ (* x x) (* y y))) c_hypotf (! :precision binary32 (hypot x y)) 1.6816069999999997]
  [fma.f32   ([x : binary32] [y : binary32] [z : binary32]) binary32 (+ (* x y) z)              c_fmaf   (! :precision binary32 (fma x y z)) 0.38934]))

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

(define libm-impls.f64
  (make-libm-impls/binary64
   [(binary64 binary64)
    ([fabs      0.14942199999999971]
     [sin       3.979444]
     [cos       3.9988340000000006]
     [tan       4.3931]
     [sinh      1.5345409999999998]
     [cosh      1.3327939999999996]
     [acos      0.5283240000000002]
     [acosh     0.929629]
     [asin      0.592536]
     [asinh     1.120752]
     [atan      1.1491229999999997]
     [atanh     0.4773569999999998]
     [cbrt      1.8992809999999996]
     [ceil      0.2699039999999999]
     [erf       1.0642519999999996]
     [exp       1.270031]
     [exp2      1.0709029999999997]
     [floor     0.27001899999999957]
     [lgamma    2.1097169999999998]
     [log       0.719641]
     [log10     1.0994940000000002]
     [log2      0.8050260000000001]
     [logb      0.33284199999999986]
     [rint      0.3101590000000003]
     [round     0.7762879999999997]
     [sqrt      0.2225119999999998]
     [tanh      0.9856559999999996]
     [tgamma    2.4006499999999997]
     [trunc     0.2714409999999996])]
   [(binary64 binary64 binary64)
    ([pow       1.860082]
     [atan2     1.8138300000000002]
     [copysign  0.23862799999999962]
     [fdim      0.7254930000000001]
     [fmax      0.2480699999999998]
     [fmin      0.26680799999999994]
     [fmod      1.5324479999999996]
     [remainder 0.9494380000000005])]))

(for ([libm-impl.f64 (in-list libm-impls.f64)])
  (platform-register-implementation! platform libm-impl.f64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

(define c_erfc  (make-libm (erfc  double double)))
(define c_expm1 (make-libm (expm1 double double)))
(define c_log1p (make-libm (log1p double double)))
(define c_hypot (make-libm (hypot double double double)))
(define c_fma   (make-libm (fma   double double double double)))

; ([name     ([var : repr] ...)                             otype    spec                       fl      fpcore                              cost])
(platform-register-implementations!
 platform
 ([erfc.f64  ([x : binary64])                               binary64 (- 1 (erf x))              c_erfc  (! :precision binary64 (erfc x))    0.8588620000000002]
  [expm1.f64 ([x : binary64])                               binary64 (- (exp x) 1)              c_expm1 (! :precision binary64 (expm1 x))   0.8483490000000002]
  [log1p.f64 ([x : binary64])                               binary64 (log (+ 1 x))              c_log1p (! :precision binary64 (log1p x))   1.2416829999999997]
  [hypot.f64 ([x : binary64] [y : binary64])                binary64 (sqrt (+ (* x x) (* y y))) c_hypot (! :precision binary64 (hypot x y)) 1.498331]
  [fma.f64   ([x : binary64] [y : binary64] [z : binary64]) binary64 (+ (* x y) z)              c_fma   (! :precision binary64 (fma x y z)) 0.37174700000000026]))

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
