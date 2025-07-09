#lang racket

;;; The default platform:
;;; C/C++ on Linux with a full libm

(require math/bigfloat
         math/flonum
         "../utils/float.rkt"  ; for shift/unshift
         "../syntax/platform.rkt")
(provide platform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMPTY PLATFORM ;;;;;;;;;;;;;;;;;;;;;;;;

(define platform (make-empty-platform 'herbie20 #:if-cost 1))

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
                       #:special-value? (const #f)))

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
 ([neg.f32 ([x : binary32])                binary32 (neg x)  fl32-      (! :precision binary32 (- x))   64]
  [+.f32   ([x : binary32] [y : binary32]) binary32 (+ x y)  fl32+      (! :precision binary32 (+ x y)) 64]
  [-.f32   ([x : binary32] [y : binary32]) binary32 (- x y)  fl32-      (! :precision binary32 (- x y)) 64]
  [*.f32   ([x : binary32] [y : binary32]) binary32 (* x y)  fl32*      (! :precision binary32 (* x y)) 128]
  [/.f32   ([x : binary32] [y : binary32]) binary32 (/ x y)  fl32/      (! :precision binary32 (/ x y)) 320]
  [==.f32  ([x : binary32] [y : binary32]) bool     (== x y) =          (== x y)                        128]
  [!=.f32  ([x : binary32] [y : binary32]) bool     (!= x y) (negate =) (!= x y)                        128]
  [<.f32   ([x : binary32] [y : binary32]) bool     (< x y)  <          (< x y)                         128]
  [>.f32   ([x : binary32] [y : binary32]) bool     (> x y)  >          (> x y)                         128]
  [<=.f32  ([x : binary32] [y : binary32]) bool     (<= x y) <=         (<= x y)                        128]
  [>=.f32  ([x : binary32] [y : binary32]) bool     (>= x y) >=         (>= x y)                        128]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

; ([name         ([var : repr] ...)              otype    spec            fl    fpcore                                  cost])
(platform-register-implementations!
 platform
 (; Unary libm operators
  [fabs.f32      ([x : binary32])                binary32 (fabs x)        (from-libm 'fabsf)      (! :precision binary32 (fabs x))        64]
  [sin.f32       ([x : binary32])                binary32 (sin x)         (from-libm 'sinf)       (! :precision binary32 (sin x))         3200]
  [cos.f32       ([x : binary32])                binary32 (cos x)         (from-libm 'cosf)       (! :precision binary32 (cos x))         3200]
  [tan.f32       ([x : binary32])                binary32 (tan x)         (from-libm 'tanf)       (! :precision binary32 (tan x))         3200]
  [sinh.f32      ([x : binary32])                binary32 (sinh x)        (from-libm 'sinhf)      (! :precision binary32 (sinh x))        3200]
  [cosh.f32      ([x : binary32])                binary32 (cosh x)        (from-libm 'coshf)      (! :precision binary32 (cosh x))        3200]
  [acos.f32      ([x : binary32])                binary32 (acos x)        (from-libm 'acosf)      (! :precision binary32 (acos x))        3200]
  [acosh.f32     ([x : binary32])                binary32 (acosh x)       (from-libm 'acoshf)     (! :precision binary32 (acosh x))       3200]
  [asin.f32      ([x : binary32])                binary32 (asin x)        (from-libm 'asinf)      (! :precision binary32 (asin x))        3200]
  [asinh.f32     ([x : binary32])                binary32 (asinh x)       (from-libm 'asinhf)     (! :precision binary32 (asinh x))       3200]
  [atan.f32      ([x : binary32])                binary32 (atan x)        (from-libm 'atanf)      (! :precision binary32 (atan x))        3200]
  [atanh.f32     ([x : binary32])                binary32 (atanh x)       (from-libm 'atanhf)     (! :precision binary32 (atanh x))       3200]
  [cbrt.f32      ([x : binary32])                binary32 (cbrt x)        (from-libm 'cbrtf)      (! :precision binary32 (cbrt x))        3200]
  [ceil.f32      ([x : binary32])                binary32 (ceil x)        (from-libm 'ceilf)      (! :precision binary32 (ceil x))        3200]
  [erf.f32       ([x : binary32])                binary32 (erf x)         (from-libm 'erff)       (! :precision binary32 (erf x))         3200]
  [exp.f32       ([x : binary32])                binary32 (exp x)         (from-libm 'expf)       (! :precision binary32 (exp x))         3200]
  [exp2.f32      ([x : binary32])                binary32 (exp2 x)        (from-libm 'exp2f)      (! :precision binary32 (exp2 x))        3200]
  [floor.f32     ([x : binary32])                binary32 (floor x)       (from-libm 'floorf)     (! :precision binary32 (floor x))       3200]
  [lgamma.f32    ([x : binary32])                binary32 (lgamma x)      (from-libm 'lgammaf)    (! :precision binary32 (lgamma x))      3200]
  [log.f32       ([x : binary32])                binary32 (log x)         (from-libm 'logf)       (! :precision binary32 (log x))         3200]
  [log10.f32     ([x : binary32])                binary32 (log10 x)       (from-libm 'log10f)     (! :precision binary32 (log10 x))       3200]
  [log2.f32      ([x : binary32])                binary32 (log2 x)        (from-libm 'log2f)      (! :precision binary32 (log2 x))        3200]
  [logb.f32      ([x : binary32])                binary32 (logb x)        (from-libm 'logbf)      (! :precision binary32 (logb x))        3200]
  [rint.f32      ([x : binary32])                binary32 (rint x)        (from-libm 'rintf)      (! :precision binary32 (rint x))        3200]
  [round.f32     ([x : binary32])                binary32 (round x)       (from-libm 'roundf)     (! :precision binary32 (round x))       3200]
  [sqrt.f32      ([x : binary32])                binary32 (sqrt x)        (from-libm 'sqrtf)      (! :precision binary32 (sqrt x))        320]
  [tanh.f32      ([x : binary32])                binary32 (tanh x)        (from-libm 'tanhf)      (! :precision binary32 (tanh x))        3200]
  [tgamma.f32    ([x : binary32])                binary32 (tgamma x)      (from-libm 'tgammaf)    (! :precision binary32 (tgamma x))      3200]
  [trunc.f32     ([x : binary32])                binary32 (trunc x)       (from-libm 'truncf)     (! :precision binary32 (trunc x))       3200]
  ; Binary libm operators
  [pow.f32       ([x : binary32] [y : binary32]) binary32 (pow x y)       (from-libm 'powf)       (! :precision binary32 (pow x y))       3200]
  [atan2.f32     ([x : binary32] [y : binary32]) binary32 (atan2 x y)     (from-libm 'atan2f)     (! :precision binary32 (atan2 x y))     3200]
  [copysign.f32  ([x : binary32] [y : binary32]) binary32 (copysign x y)  (from-libm 'copysignf)  (! :precision binary32 (copysign x y))  3200]
  [fdim.f32      ([x : binary32] [y : binary32]) binary32 (fdim x y)      (from-libm 'fdimf)      (! :precision binary32 (fdim x y))      3200]
  [fmax.f32      ([x : binary32] [y : binary32]) binary32 (fmax x y)      (from-libm 'fmaxf)      (! :precision binary32 (fmax x y))      3200]
  [fmin.f32      ([x : binary32] [y : binary32]) binary32 (fmin x y)      (from-libm 'fminf)      (! :precision binary32 (fmin x y))      3200]
  [fmod.f32      ([x : binary32] [y : binary32]) binary32 (fmod x y)      (from-libm 'fmodf)      (! :precision binary32 (fmod x y))      3200]
  [remainder.f32 ([x : binary32] [y : binary32]) binary32 (remainder x y) (from-libm 'remainderf) (! :precision binary32 (remainder x y)) 3200]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

; ([name     ([var : repr] ...)                             otype    spec                       fl      fpcore                               cost])
(platform-register-implementations!
 platform
 ([erfc.f32  ([x : binary32])                               binary32 (- 1 (erf x))              (from-libm 'erfcf)  (! :precision binary32 (erfc x))    3200]
  [expm1.f32 ([x : binary32])                               binary32 (- (exp x) 1)              (from-libm 'expm1f) (! :precision binary32 (expm1 x))   3200]
  [log1p.f32 ([x : binary32])                               binary32 (log (+ 1 x))              (from-libm 'log1pf) (! :precision binary32 (log1p x))   3200]
  [hypot.f32 ([x : binary32] [y : binary32])                binary32 (sqrt (+ (* x x) (* y y))) (from-libm 'hypotf) (! :precision binary32 (hypot x y)) 3200]
  [fma.f32   ([x : binary32] [y : binary32] [z : binary32]) binary32 (+ (* x y) z)              (from-libm 'fmaf)   (! :precision binary32 (fma x y z)) 128]))

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
                       #:special-value? nan?))

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

; ([name         ([var : repr] ...)              otype    spec            fl    fpcore                                  cost])
(platform-register-implementations!
 platform
 (; Unary libm operators
  [fabs.f64      ([x : binary64])                binary64 (fabs x)        (from-libm 'fabs)      (! :precision binary64 (fabs x))        128]
  [sin.f64       ([x : binary64])                binary64 (sin x)         (from-libm 'sin)       (! :precision binary64 (sin x))         6400]
  [cos.f64       ([x : binary64])                binary64 (cos x)         (from-libm 'cos)       (! :precision binary64 (cos x))         6400]
  [tan.f64       ([x : binary64])                binary64 (tan x)         (from-libm 'tan)       (! :precision binary64 (tan x))         6400]
  [sinh.f64      ([x : binary64])                binary64 (sinh x)        (from-libm 'sinh)      (! :precision binary64 (sinh x))        6400]
  [cosh.f64      ([x : binary64])                binary64 (cosh x)        (from-libm 'cosh)      (! :precision binary64 (cosh x))        6400]
  [acos.f64      ([x : binary64])                binary64 (acos x)        (from-libm 'acos)      (! :precision binary64 (acos x))        6400]
  [acosh.f64     ([x : binary64])                binary64 (acosh x)       (from-libm 'acosh)     (! :precision binary64 (acosh x))       6400]
  [asin.f64      ([x : binary64])                binary64 (asin x)        (from-libm 'asin)      (! :precision binary64 (asin x))        6400]
  [asinh.f64     ([x : binary64])                binary64 (asinh x)       (from-libm 'asinh)     (! :precision binary64 (asinh x))       6400]
  [atan.f64      ([x : binary64])                binary64 (atan x)        (from-libm 'atan)      (! :precision binary64 (atan x))        6400]
  [atanh.f64     ([x : binary64])                binary64 (atanh x)       (from-libm 'atanh)     (! :precision binary64 (atanh x))       6400]
  [cbrt.f64      ([x : binary64])                binary64 (cbrt x)        (from-libm 'cbrt)      (! :precision binary64 (cbrt x))        6400]
  [ceil.f64      ([x : binary64])                binary64 (ceil x)        (from-libm 'ceil)      (! :precision binary64 (ceil x))        6400]
  [erf.f64       ([x : binary64])                binary64 (erf x)         (from-libm 'erf)       (! :precision binary64 (erf x))         6400]
  [exp.f64       ([x : binary64])                binary64 (exp x)         (from-libm 'exp)       (! :precision binary64 (exp x))         6400]
  [exp2.f64      ([x : binary64])                binary64 (exp2 x)        (from-libm 'exp2)      (! :precision binary64 (exp2 x))        6400]
  [floor.f64     ([x : binary64])                binary64 (floor x)       (from-libm 'floor)     (! :precision binary64 (floor x))       6400]
  [lgamma.f64    ([x : binary64])                binary64 (lgamma x)      (from-libm 'lgamma)    (! :precision binary64 (lgamma x))      6400]
  [log.f64       ([x : binary64])                binary64 (log x)         (from-libm 'log)       (! :precision binary64 (log x))         6400]
  [log10.f64     ([x : binary64])                binary64 (log10 x)       (from-libm 'log10)     (! :precision binary64 (log10 x))       6400]
  [log2.f64      ([x : binary64])                binary64 (log2 x)        (from-libm 'log2)      (! :precision binary64 (log2 x))        6400]
  [logb.f64      ([x : binary64])                binary64 (logb x)        (from-libm 'logb)      (! :precision binary64 (logb x))        6400]
  [rint.f64      ([x : binary64])                binary64 (rint x)        (from-libm 'rint)      (! :precision binary64 (rint x))        6400]
  [round.f64     ([x : binary64])                binary64 (round x)       (from-libm 'round)     (! :precision binary64 (round x))       6400]
  [sqrt.f64      ([x : binary64])                binary64 (sqrt x)        (from-libm 'sqrt)      (! :precision binary64 (sqrt x))        640]
  [tanh.f64      ([x : binary64])                binary64 (tanh x)        (from-libm 'tanh)      (! :precision binary64 (tanh x))        6400]
  [tgamma.f64    ([x : binary64])                binary64 (tgamma x)      (from-libm 'tgamma)    (! :precision binary64 (tgamma x))      6400]
  [trunc.f64     ([x : binary64])                binary64 (trunc x)       (from-libm 'trunc)     (! :precision binary64 (trunc x))       6400]
  ; Binary libm operators
  [pow.f64       ([x : binary64] [y : binary64]) binary64 (pow x y)       (from-libm 'pow)       (! :precision binary64 (pow x y))       6400]
  [atan2.f64     ([x : binary64] [y : binary64]) binary64 (atan2 x y)     (from-libm 'atan2)     (! :precision binary64 (atan2 x y))     6400]
  [copysign.f64  ([x : binary64] [y : binary64]) binary64 (copysign x y)  (from-libm 'copysign)  (! :precision binary64 (copysign x y))  6400]
  [fdim.f64      ([x : binary64] [y : binary64]) binary64 (fdim x y)      (from-libm 'fdim)      (! :precision binary64 (fdim x y))      6400]
  [fmax.f64      ([x : binary64] [y : binary64]) binary64 (fmax x y)      (from-libm 'fmax)      (! :precision binary64 (fmax x y))      6400]
  [fmin.f64      ([x : binary64] [y : binary64]) binary64 (fmin x y)      (from-libm 'fmin)      (! :precision binary64 (fmin x y))      6400]
  [fmod.f64      ([x : binary64] [y : binary64]) binary64 (fmod x y)      (from-libm 'fmod)      (! :precision binary64 (fmod x y))      6400]
  [remainder.f64 ([x : binary64] [y : binary64]) binary64 (remainder x y) (from-libm 'remainder) (! :precision binary64 (remainder x y)) 6400]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

; ([name     ([var : repr] ...)                             otype    spec                       fl    fpcore                              cost])
(platform-register-implementations!
 platform
 ([erfc.f64  ([x : binary64])                               binary64 (- 1 (erf x))              (from-libm 'erfc)  (! :precision binary64 (erfc x))    6400]
  [expm1.f64 ([x : binary64])                               binary64 (- (exp x) 1)              (from-libm 'expm1) (! :precision binary64 (expm1 x))   6400]
  [log1p.f64 ([x : binary64])                               binary64 (log (+ 1 x))              (from-libm 'log1p) (! :precision binary64 (log1p x))   6400]
  [hypot.f64 ([x : binary64] [y : binary64])                binary64 (sqrt (+ (* x x) (* y y))) (from-libm 'hypot) (! :precision binary64 (hypot x y)) 6400]
  [fma.f64   ([x : binary64] [y : binary64] [z : binary64]) binary64 (+ (* x y) z)              (from-libm 'fma)   (! :precision binary64 (fma x y z)) 256]))

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

(module+ main
  (display-platform platform))

;; Do not run this file during testing
(module test racket/base
  )
