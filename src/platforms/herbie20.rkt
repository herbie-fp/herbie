#lang racket

;;; The default platform:
;;; C/C++ on Linux with a full libm

(require math/bigfloat
         math/flonum
         "runtime/libm.rkt"    ; libm wrapper
         "../syntax/types.rkt"  ; for shift/unshift
         "../syntax/platform.rkt")
(provide platform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMPTY PLATFORM ;;;;;;;;;;;;;;;;;;;;;;;;

(define platform (make-empty-platform 'herbie20))

(platform-register-if-cost! platform #:if-cost 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define bool <bool>)

(platform-register-representation! platform #:repr bool #:cost 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([TRUE  () bool (TRUE)  (const true)  (! TRUE)  1]
  [FALSE () bool (FALSE) (const false) (! FALSE) 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(platform-register-implementations!
 platform
 ([not ([x : bool])            bool (not x)   not    (not x)   1]
  [and ([x : bool] [y : bool]) bool (and x y) and-fn (and x y) 1]
  [or  ([x : bool] [y : bool]) bool (or x y)  or-fn  (or x y)  1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define binary32 <binary32>)

(platform-register-representation! platform #:repr binary32 #:cost 32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([PI.f32       () binary32 (PI)       (const (flsingle pi))        (! :precision binary32 PI)       32]
  [E.f32        () binary32 (E)        (const (flsingle (exp 1.0))) (! :precision binary32 E)        32]
  [INFINITY.f32 () binary32 (INFINITY) (const +inf.0)               (! :precision binary32 INFINITY) 32]
  [NAN.f32      () binary32 (NAN)      (const +nan.0)               (! :precision binary32 NAN)      32]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ([name   ([var : repr] ...)              otype    spec     fl         fpcore                         cost])
(platform-register-implementations!
 platform
 ([neg.f32 ([x : binary32])                binary32 (neg x)  (compose flsingle -)      (! :precision binary32 (- x))   64]
  [+.f32   ([x : binary32] [y : binary32]) binary32 (+ x y)  (compose flsingle +)      (! :precision binary32 (+ x y)) 64]
  [-.f32   ([x : binary32] [y : binary32]) binary32 (- x y)  (compose flsingle -)      (! :precision binary32 (- x y)) 64]
  [*.f32   ([x : binary32] [y : binary32]) binary32 (* x y)  (compose flsingle *)      (! :precision binary32 (* x y)) 128]
  [/.f32   ([x : binary32] [y : binary32]) binary32 (/ x y)  (compose flsingle /)      (! :precision binary32 (/ x y)) 320]
  [==.f32  ([x : binary32] [y : binary32]) bool     (== x y) =          (== x y)                        128]
  [!=.f32  ([x : binary32] [y : binary32]) bool     (!= x y) (negate =) (!= x y)                        128]
  [<.f32   ([x : binary32] [y : binary32]) bool     (< x y)  <          (< x y)                         128]
  [>.f32   ([x : binary32] [y : binary32]) bool     (> x y)  >          (> x y)                         128]
  [<=.f32  ([x : binary32] [y : binary32]) bool     (<= x y) <=         (<= x y)                        128]
  [>=.f32  ([x : binary32] [y : binary32]) bool     (>= x y) >=         (>= x y)                        128]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

(define libm-impls.f32
  (make-libm-impls/binary32
   [(binary32 binary32)
    ([fabs      64]
     [sin       3200]
     [cos       3200]
     [tan       3200]
     [sinh      3200]
     [cosh      3200]
     [acos      3200]
     [acosh     3200]
     [asin      3200]
     [asinh     3200]
     [atan      3200]
     [atanh     3200]
     [cbrt      3200]
     [ceil      3200]
     [erf       3200]
     [exp       3200]
     [exp2      3200]
     [floor     3200]
     [lgamma    3200]
     [log       3200]
     [log10     3200]
     [log2      3200]
     [logb      3200]
     [rint      3200]
     [round     3200]
     [sqrt      320] ; not a typo, it is 320
     [tanh      3200]
     [tgamma    3200]
     [trunc     3200])]
   [(binary32 binary32 binary32)
    ([pow       3200]
     [atan2     3200]
     [copysign  3200]
     [fdim      3200]
     [fmax      3200]
     [fmin      3200]
     [fmod      3200]
     [remainder 3200])]))

(for ([libm-impl.f32 (in-list libm-impls.f32)])
  (platform-register-implementation! platform libm-impl.f32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

(define c_erfcf  (make-libm (erfcf  float float)))
(define c_expm1f (make-libm (expm1f float float)))
(define c_log1pf (make-libm (log1pf float float)))
(define c_hypotf (make-libm (hypotf float float float)))
(define c_fmaf   (make-libm (fmaf   float float float float)))

; ([name     ([var : repr] ...)                             otype    spec                       fl       fpcore                              cost])
(platform-register-implementations!
 platform
 ([erfc.f32  ([x : binary32])                               binary32 (- 1 (erf x))              c_erfcf  (! :precision binary32 (erfc x))    3200]
  [expm1.f32 ([x : binary32])                               binary32 (- (exp x) 1)              c_expm1f (! :precision binary32 (expm1 x))   3200]
  [log1p.f32 ([x : binary32])                               binary32 (log (+ 1 x))              c_log1pf (! :precision binary32 (log1p x))   3200]
  [hypot.f32 ([x : binary32] [y : binary32])                binary32 (sqrt (+ (* x x) (* y y))) c_hypotf (! :precision binary32 (hypot x y)) 3200]
  [fma.f32   ([x : binary32] [y : binary32] [z : binary32]) binary32 (+ (* x y) z)              c_fmaf   (! :precision binary32 (fma x y z)) 128]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define binary64 <binary64>)

(platform-register-representation! platform #:repr binary64 #:cost 64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([PI.f64       () binary64 (PI)       (const pi)        (! :precision binary64 PI)       64]
  [E.f64        () binary64 (E)        (const (exp 1.0)) (! :precision binary64 E)        64]
  [INFINITY.f64 () binary64 (INFINITY) (const +inf.0)    (! :precision binary64 INFINITY) 64]
  [NAN.f64      () binary64 (NAN)      (const +nan.0)    (! :precision binary64 NAN)      64]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([neg.f64 ([x : binary64])                binary64 (neg x)  -          (! :precision binary64 (- x))   128]
  [+.f64   ([x : binary64] [y : binary64]) binary64 (+ x y)  +          (! :precision binary64 (+ x y)) 128]
  [-.f64   ([x : binary64] [y : binary64]) binary64 (- x y)  -          (! :precision binary64 (- x y)) 128]
  [*.f64   ([x : binary64] [y : binary64]) binary64 (* x y)  *          (! :precision binary64 (* x y)) 256]
  [/.f64   ([x : binary64] [y : binary64]) binary64 (/ x y)  /          (! :precision binary64 (/ x y)) 640]
  [==.f64  ([x : binary64] [y : binary64]) bool     (== x y) =          (== x y)                        256]
  [!=.f64  ([x : binary64] [y : binary64]) bool     (!= x y) (negate =) (!= x y)                        256]
  [<.f64   ([x : binary64] [y : binary64]) bool     (< x y)  <          (< x y)                         256]
  [>.f64   ([x : binary64] [y : binary64]) bool     (> x y)  >          (> x y)                         256]
  [<=.f64  ([x : binary64] [y : binary64]) bool     (<= x y) <=         (<= x y)                        256]
  [>=.f64  ([x : binary64] [y : binary64]) bool     (>= x y) >=         (>= x y)                        256]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

(define libm-impls.f64
  (make-libm-impls/binary64
   [(binary64 binary64)
    ([fabs      128]
     [sin       6400]
     [cos       6400]
     [tan       6400]
     [sinh      6400]
     [cosh      6400]
     [acos      6400]
     [acosh     6400]
     [asin      6400]
     [asinh     6400]
     [atan      6400]
     [atanh     6400]
     [cbrt      6400]
     [ceil      6400]
     [erf       6400]
     [exp       6400]
     [exp2      6400]
     [floor     6400]
     [lgamma    6400]
     [log       6400]
     [log10     6400]
     [log2      6400]
     [logb      6400]
     [rint      6400]
     [round     6400]
     [sqrt      640] ; not a typo
     [tanh      6400]
     [tgamma    6400]
     [trunc     6400])]
   [(binary64 binary64 binary64)
    ([pow       6400]
     [atan2     6400]
     [copysign  6400]
     [fdim      6400]
     [fmax      6400]
     [fmin      6400]
     [fmod      6400]
     [remainder 6400])]))

(for ([libm-impl.f64 (in-list libm-impls.f64)])
  (platform-register-implementation! platform libm-impl.f64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

(define c_erfc  (make-libm (erfc  double double)))
(define c_expm1 (make-libm (expm1 double double)))
(define c_log1p (make-libm (log1p double double)))
(define c_hypot (make-libm (hypot double double double)))
(define c_fma   (make-libm (fma   double double double double)))

; ([name     ([var : repr] ...)                             otype    spec                       fl      fpcore                           cost])
(platform-register-implementations!
 platform
 ([erfc.f64  ([x : binary64])                               binary64 (- 1 (erf x))              c_erfc  (! :precision binary64 (erfc x))    6400]
  [expm1.f64 ([x : binary64])                               binary64 (- (exp x) 1)              c_expm1 (! :precision binary64 (expm1 x))   6400]
  [log1p.f64 ([x : binary64])                               binary64 (log (+ 1 x))              c_log1p (! :precision binary64 (log1p x))   6400]
  [hypot.f64 ([x : binary64] [y : binary64])                binary64 (sqrt (+ (* x x) (* y y))) c_hypot (! :precision binary64 (hypot x y)) 6400]
  [fma.f64   ([x : binary64] [y : binary64] [z : binary64]) binary64 (+ (* x y) z)              c_fma   (! :precision binary64 (fma x y z)) 256]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; additional converters ;;;;;;;;;;;;;;;;;

#;(platform-register-implementation! platform
                                     (make-operator-impl (binary64->binary32 [x : binary64])
                                                         binary32
                                                         #:spec x
                                                         #:fpcore (! :precision binary32 (cast x))
                                                         #:fl flsingle
                                                         #:cost 64))

#;(platform-register-implementation! platform
                                     (make-operator-impl (binary32->binary64 [x : binary32])
                                                         binary64
                                                         #:spec x
                                                         #:fpcore (! :precision binary64 (cast x))
                                                         #:fl identity
                                                         #:cost 64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REGISTER PLATFORM ;;;;;;;;;;;;;;;;;;;;;

(register-platform! platform)

(module+ main
  (display-platform platform))

;; Do not run this file during testing
(module test racket/base
  )
