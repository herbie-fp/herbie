#lang racket

;;; Herbie10 platform:
;;; C/C++ on Linux with a full libm

(require math/bigfloat
         math/flonum
         "runtime/float32.rkt" ; float representation helper functions
         "runtime/libm.rkt"    ; libm wrapper
         "../utils/float.rkt"  ; for shift/unshift
         (submod "../syntax/platform.rkt" internals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMPTY PLATFORM ;;;;;;;;;;;;;;;;;;;;;;;;

(define herbie10-platform (make-empty-platform 'herbie10 #:if-cost 0 #:default-cost 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define bool
  (make-representation #:name 'bool
                       #:type 'bool
                       #:repr? boolean?
                       #:bf->repr identity
                       #:repr->bf identity
                       #:ordinal->repr (λ (x) (= x 0))
                       #:repr->ordinal (λ (x) (if x 1 0))
                       #:total-bits 1
                       #:special-value? (const #f)
                       #:cost 0))

(platform-register-representation! herbie10-platform bool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 herbie10-platform
 ([TRUE  () bool (TRUE)  (const true)  (! TRUE)  0]
  [FALSE () bool (FALSE) (const false) (! FALSE) 0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(platform-register-implementations!
 herbie10-platform
 ([not ([x : bool])            bool (not x)   not    (not x)   0]
  [and ([x : bool] [y : bool]) bool (and x y) and-fn (and x y) 0]
  [or  ([x : bool] [y : bool]) bool (or x y)  or-fn  (or x y)  0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define binary32
  (make-representation #:name 'binary32
                       #:type 'real
                       #:repr? flonum?
                       #:bf->repr bigfloat->float32
                       #:repr->bf bf
                       #:ordinal->repr (shift 31 ordinal->float32)
                       #:repr->ordinal (unshift 31 float32->ordinal)
                       #:total-bits 32
                       #:special-value? nan?
                       #:cost 0))

(platform-register-representation! herbie10-platform binary32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 herbie10-platform
 ([PI.f32       () binary32 (PI)       (const (flsingle pi))        (! :precision binary32 (PI))       0]
  [E.f32        () binary32 (E)        (const (flsingle (exp 1.0))) (! :precision binary32 (E))        0]
  [INFINITY.f32 () binary32 (INFINITY) (const +inf.0)               (! :precision binary32 (INFINITY)) 0]
  [NAN.f32      () binary32 (NAN)      (const +nan.0)               (! :precision binary32 (NAN))      0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ([name   ([var : repr] ...)              otype    spec     fl         fpcore                         cost])
(platform-register-implementations!
 herbie10-platform
 ([neg.f32 ([x : binary32])                binary32 (neg x)  fl32-      (! :precision binary32 (- x))   0]
  [+.f32   ([x : binary32] [y : binary32]) binary32 (+ x y)  fl32+      (! :precision binary32 (+ x y)) 0]
  [-.f32   ([x : binary32] [y : binary32]) binary32 (- x y)  fl32-      (! :precision binary32 (- x y)) 0]
  [*.f32   ([x : binary32] [y : binary32]) binary32 (* x y)  fl32*      (! :precision binary32 (* x y)) 0]
  [/.f32   ([x : binary32] [y : binary32]) binary32 (/ x y)  fl32/      (! :precision binary32 (/ x y)) 0]
  [==.f32  ([x : binary32] [y : binary32]) bool     (== x y) =          (== x y)                        0]
  [!=.f32  ([x : binary32] [y : binary32]) bool     (!= x y) (negate =) (!= x y)                        0]
  [<.f32   ([x : binary32] [y : binary32]) bool     (< x y)  <          (< x y)                         0]
  [>.f32   ([x : binary32] [y : binary32]) bool     (> x y)  >          (> x y)                         0]
  [<=.f32  ([x : binary32] [y : binary32]) bool     (<= x y) <=         (<= x y)                        0]
  [>=.f32  ([x : binary32] [y : binary32]) bool     (>= x y) >=         (>= x y)                        0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

(define libm-impls.f32
  (make-libm-impls/binary32
   [(binary32 binary32)
    ([fabs      0]
     [sin       0]
     [cos       0]
     [tan       0]
     [sinh      0]
     [cosh      0]
     [acos      0]
     [acosh     0]
     [asin      0]
     [asinh     0]
     [atan      0]
     [atanh     0]
     [cbrt      0]
     [ceil      0]
     [erf       0]
     [exp       0]
     [exp2      0]
     [floor     0]
     [lgamma    0]
     [log       0]
     [log10     0]
     [log2      0]
     [logb      0]
     [rint      0]
     [round     0]
     [sqrt      0]
     [tanh      0]
     [tgamma    0]
     [trunc     0])]
   [(binary32 binary32 binary32)
    ([pow       0]
     [atan2     0]
     [copysign  0]
     [fdim      0]
     [fmax      0]
     [fmin      0]
     [fmod      0]
     [remainder 0])]))

(for ([libm-impl.f32 (in-list libm-impls.f32)])
  (when libm-impl.f32
    (platform-register-implementation! herbie10-platform libm-impl.f32)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

(define c_erfcf  (make-libm (erfcf  float float)))
(define c_expm1f (make-libm (expm1f float float)))
(define c_log1pf (make-libm (log1pf float float)))
(define c_hypotf (make-libm (hypotf float float float)))
(define c_fmaf   (make-libm (fmaf   float float float float)))

; ([name     ([var : repr] ...)                             otype    spec                       fl       fpcore                           cost])
(platform-register-implementations!
 herbie10-platform
 ([erfc.f32  ([x : binary32])                               binary32 (- 1 (erf x))              c_erfcf  (! :precision binary32 (erfc x))    0]
  [expm1.f32 ([x : binary32])                               binary32 (- (exp x) 1)              c_expm1f (! :precision binary32 (expm1 x))   0]
  [log1p.f32 ([x : binary32])                               binary32 (log (+ 1 x))              c_log1pf (! :precision binary32 (log1p x))   0]
  [hypot.f32 ([x : binary32] [y : binary32])                binary32 (sqrt (+ (* x x) (* y y))) c_hypotf (! :precision binary32 (hypot x y)) 0]
  [fma.f32   ([x : binary32] [y : binary32] [z : binary32]) binary32 (+ (* x y) z)              c_fmaf   (! :precision binary32 (fma x y z)) 0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define binary64
  (make-representation #:name 'binary64
                       #:type 'real
                       #:repr? flonum?
                       #:bf->repr bigfloat->flonum
                       #:repr->bf bf
                       #:ordinal->repr (shift 63 ordinal->flonum)
                       #:repr->ordinal (unshift 63 flonum->ordinal)
                       #:total-bits 64
                       #:special-value? nan?
                       #:cost 0))

(platform-register-representation! herbie10-platform binary64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 herbie10-platform
 ([PI.f64       () binary64 (PI)       (const pi)        (! :precision binary64 (PI))       0]
  [E.f64        () binary64 (E)        (const (exp 1.0)) (! :precision binary64 (E))        0]
  [INFINITY.f64 () binary64 (INFINITY) (const +inf.0)    (! :precision binary64 (INFINITY)) 0]
  [NAN.f64      () binary64 (NAN)      (const +nan.0)    (! :precision binary64 (NAN))      0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 herbie10-platform
 ([neg.f64 ([x : binary64])                binary64 (neg x)  -          (! :precision binary64 (- x))   0]
  [+.f64   ([x : binary64] [y : binary64]) binary64 (+ x y)  +          (! :precision binary64 (+ x y)) 0]
  [-.f64   ([x : binary64] [y : binary64]) binary64 (- x y)  -          (! :precision binary64 (- x y)) 0]
  [*.f64   ([x : binary64] [y : binary64]) binary64 (* x y)  *          (! :precision binary64 (* x y)) 0]
  [/.f64   ([x : binary64] [y : binary64]) binary64 (/ x y)  /          (! :precision binary64 (/ x y)) 0]
  [==.f64  ([x : binary64] [y : binary64]) bool     (== x y) =          (== x y)                        0]
  [!=.f64  ([x : binary64] [y : binary64]) bool     (!= x y) (negate =) (!= x y)                        0]
  [<.f64   ([x : binary64] [y : binary64]) bool     (< x y)  <          (< x y)                         0]
  [>.f64   ([x : binary64] [y : binary64]) bool     (> x y)  >          (> x y)                         0]
  [<=.f64  ([x : binary64] [y : binary64]) bool     (<= x y) <=         (<= x y)                        0]
  [>=.f64  ([x : binary64] [y : binary64]) bool     (>= x y) >=         (>= x y)                        0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

(define libm-impls.f64
  (make-libm-impls/binary64
   [(binary64 binary64)
    ([fabs      0]
     [sin       0]
     [cos       0]
     [tan       0]
     [sinh      0]
     [cosh      0]
     [acos      0]
     [acosh     0]
     [asin      0]
     [asinh     0]
     [atan      0]
     [atanh     0]
     [cbrt      0]
     [ceil      0]
     [erf       0]
     [exp       0]
     [exp2      0]
     [floor     0]
     [lgamma    0]
     [log       0]
     [log10     0]
     [log2      0]
     [logb      0]
     [rint      0]
     [round     0]
     [sqrt      0]
     [tanh      0]
     [tgamma    0]
     [trunc     0])]
   [(binary64 binary64 binary64)
    ([pow       0]
     [atan2     0]
     [copysign  0]
     [fdim      0]
     [fmax      0]
     [fmin      0]
     [fmod      0]
     [remainder 0])]))

(for ([libm-impl.f64 (in-list libm-impls.f64)])
  (when libm-impl.f64
    (platform-register-implementation! herbie10-platform libm-impl.f64)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

(define c_erfc  (make-libm (erfc  double double)))
(define c_expm1 (make-libm (expm1 double double)))
(define c_log1p (make-libm (log1p double double)))
(define c_hypot (make-libm (hypot double double double)))
(define c_fma   (make-libm (fma   double double double double)))

; ([name     ([var : repr] ...)                             otype    spec                       fl      fpcore                          cost])
(platform-register-implementations!
 herbie10-platform
 ([erfc.f64  ([x : binary64])                               binary64 (- 1 (erf x))              c_erfc  (! :precision binary64 (erfc x))    0]
  [expm1.f64 ([x : binary64])                               binary64 (- (exp x) 1)              c_expm1 (! :precision binary64 (expm1 x))   0]
  [log1p.f64 ([x : binary64])                               binary64 (log (+ 1 x))              c_log1p (! :precision binary64 (log1p x))   0]
  [hypot.f64 ([x : binary64] [y : binary64])                binary64 (sqrt (+ (* x x) (* y y))) c_hypot (! :precision binary64 (hypot x y)) 0]
  [fma.f64   ([x : binary64] [y : binary64] [z : binary64]) binary64 (+ (* x y) z)              c_fma   (! :precision binary64 (fma x y z)) 0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; additional converters ;;;;;;;;;;;;;;;;;

#;(platform-register-implementation! herbie10-platform
                                     (make-operator-impl (binary64->binary32 [x : binary64])
                                                         binary32
                                                         #:spec x
                                                         #:fpcore (! :precision binary32 (cast x))
                                                         #:fl flsingle))

#;(platform-register-implementation! herbie10-platform
                                     (make-operator-impl (binary32->binary64 [x : binary32])
                                                         binary64
                                                         #:spec x
                                                         #:fpcore (! :precision binary64 (cast x))
                                                         #:fl identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REGISTER PLATFORM ;;;;;;;;;;;;;;;;;;;;;

(register-platform! herbie10-platform)

(module+ main
  (display-platform herbie10-platform))

;; Do not run this file during testing
(module test racket/base
  )
