#lang racket

;;; Herbie10 platform:
;;; C/C++ on Linux with a full libm

(require math/bigfloat
         math/flonum
         "runtime/libm.rkt"    ; libm wrapper
         "../utils/float.rkt"  ; for shift/unshift
         "../syntax/platform.rkt")
(provide platform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMPTY PLATFORM ;;;;;;;;;;;;;;;;;;;;;;;;

(define platform (make-empty-platform 'herbie10 #:if-cost 0))

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

(platform-register-representation! platform #:repr bool #:cost 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([TRUE  () bool (TRUE)  (const true)  (! TRUE)  0]
  [FALSE () bool (FALSE) (const false) (! FALSE) 0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(platform-register-implementations!
 platform
 ([not ([x : bool])            bool (not x)   not    (not x)   0]
  [and ([x : bool] [y : bool]) bool (and x y) and-fn (and x y) 0]
  [or  ([x : bool] [y : bool]) bool (or x y)  or-fn  (or x y)  0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define binary32 <binary32>)

(platform-register-representation! platform #:repr binary32 #:cost 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([PI.f32       () binary32 (PI)       (const (flsingle pi))        (! :precision binary32 PI)       0]
  [E.f32        () binary32 (E)        (const (flsingle (exp 1.0))) (! :precision binary32 E)        0]
  [INFINITY.f32 () binary32 (INFINITY) (const +inf.0)               (! :precision binary32 INFINITY) 0]
  [NAN.f32      () binary32 (NAN)      (const +nan.0)               (! :precision binary32 NAN)      0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ([name   ([var : repr] ...)              otype    spec     fl         fpcore                         cost])
(platform-register-implementations!
 platform
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

; ([name        ([var : repr] ...)              otype    spec            fl    fpcore                                  cost])
(platform-register-implementations!
 platform
 (; Unary libm operators
  [fabs.32      ([x : binary32])                binary32 (fabs x)        'libm (! :precision binary32 (fabs x))        0]
  [sin.32       ([x : binary32])                binary32 (sin x)         'libm (! :precision binary32 (sin x))         0]
  [cos.32       ([x : binary32])                binary32 (cos x)         'libm (! :precision binary32 (cos x))         0]
  [tan.32       ([x : binary32])                binary32 (tan x)         'libm (! :precision binary32 (tan x))         0]
  [sinh.32      ([x : binary32])                binary32 (sinh x)        'libm (! :precision binary32 (sinh x))        0]
  [cosh.32      ([x : binary32])                binary32 (cosh x)        'libm (! :precision binary32 (cosh x))        0]
  [acos.32      ([x : binary32])                binary32 (acos x)        'libm (! :precision binary32 (acos x))        0]
  [acosh.32     ([x : binary32])                binary32 (acosh x)       'libm (! :precision binary32 (acosh x))       0]
  [asin.32      ([x : binary32])                binary32 (asin x)        'libm (! :precision binary32 (asin x))        0]
  [asinh.32     ([x : binary32])                binary32 (asinh x)       'libm (! :precision binary32 (asinh x))       0]
  [atan.32      ([x : binary32])                binary32 (atan x)        'libm (! :precision binary32 (atan x))        0]
  [atanh.32     ([x : binary32])                binary32 (atanh x)       'libm (! :precision binary32 (atanh x))       0]
  [cbrt.32      ([x : binary32])                binary32 (cbrt x)        'libm (! :precision binary32 (cbrt x))        0]
  [ceil.32      ([x : binary32])                binary32 (ceil x)        'libm (! :precision binary32 (ceil x))        0]
  [erf.32       ([x : binary32])                binary32 (erf x)         'libm (! :precision binary32 (erf x))         0]
  [exp.32       ([x : binary32])                binary32 (exp x)         'libm (! :precision binary32 (exp x))         0]
  [exp2.32      ([x : binary32])                binary32 (exp2 x)        'libm (! :precision binary32 (exp2 x))        0]
  [floor.32     ([x : binary32])                binary32 (floor x)       'libm (! :precision binary32 (floor x))       0]
  [lgamma.32    ([x : binary32])                binary32 (lgamma x)      'libm (! :precision binary32 (lgamma x))      0]
  [log.32       ([x : binary32])                binary32 (log x)         'libm (! :precision binary32 (log x))         0]
  [log10.32     ([x : binary32])                binary32 (log10 x)       'libm (! :precision binary32 (log10 x))       0]
  [log2.32      ([x : binary32])                binary32 (log2 x)        'libm (! :precision binary32 (log2 x))        0]
  [logb.32      ([x : binary32])                binary32 (logb x)        'libm (! :precision binary32 (logb x))        0]
  [rint.32      ([x : binary32])                binary32 (rint x)        'libm (! :precision binary32 (rint x))        0]
  [round.32     ([x : binary32])                binary32 (round x)       'libm (! :precision binary32 (round x))       0]
  [sqrt.32      ([x : binary32])                binary32 (sqrt x)        'libm (! :precision binary32 (sqrt x))        0]
  [tanh.32      ([x : binary32])                binary32 (tanh x)        'libm (! :precision binary32 (tanh x))        0]
  [tgamma.32    ([x : binary32])                binary32 (tgamma x)      'libm (! :precision binary32 (tgamma x))      0]
  [trunc.32     ([x : binary32])                binary32 (trunc x)       'libm (! :precision binary32 (trunc x))       0]
  ; Binary libm operators
  [pow.32       ([x : binary32] [y : binary32]) binary32 (pow x y)       'libm (! :precision binary32 (pow x y))       0]
  [atan2.32     ([x : binary32] [y : binary32]) binary32 (atan2 x y)     'libm (! :precision binary32 (atan2 x y))     0]
  [copysign.32  ([x : binary32] [y : binary32]) binary32 (copysign x y)  'libm (! :precision binary32 (copysign x y))  0]
  [fdim.32      ([x : binary32] [y : binary32]) binary32 (fdim x y)      'libm (! :precision binary32 (fdim x y))      0]
  [fmax.32      ([x : binary32] [y : binary32]) binary32 (fmax x y)      'libm (! :precision binary32 (fmax x y))      0]
  [fmin.32      ([x : binary32] [y : binary32]) binary32 (fmin x y)      'libm (! :precision binary32 (fmin x y))      0]
  [fmod.32      ([x : binary32] [y : binary32]) binary32 (fmod x y)      'libm (! :precision binary32 (fmod x y))      0]
  [remainder.32 ([x : binary32] [y : binary32]) binary32 (remainder x y) 'libm (! :precision binary32 (remainder x y)) 0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

; ([name     ([var : repr] ...)                             otype    spec                       fl      fpcore                               cost])
(platform-register-implementations!
 platform
 ([erfc.f32  ([x : binary32])                               binary32 (- 1 (erf x))              'libm (! :precision binary32 (erfc x))    0]
  [expm1.f32 ([x : binary32])                               binary32 (- (exp x) 1)              'libm (! :precision binary32 (expm1 x))   0]
  [log1p.f32 ([x : binary32])                               binary32 (log (+ 1 x))              'libm (! :precision binary32 (log1p x))   0]
  [hypot.f32 ([x : binary32] [y : binary32])                binary32 (sqrt (+ (* x x) (* y y))) 'libm (! :precision binary32 (hypot x y)) 0]
  [fma.f32   ([x : binary32] [y : binary32] [z : binary32]) binary32 (+ (* x y) z)              'libm (! :precision binary32 (fma x y z)) 0]))

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

(platform-register-representation! platform #:repr binary64 #:cost 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([PI.f64       () binary64 (PI)       (const pi)        (! :precision binary64 PI)       0]
  [E.f64        () binary64 (E)        (const (exp 1.0)) (! :precision binary64 E)        0]
  [INFINITY.f64 () binary64 (INFINITY) (const +inf.0)    (! :precision binary64 INFINITY) 0]
  [NAN.f64      () binary64 (NAN)      (const +nan.0)    (! :precision binary64 NAN)      0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
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

; ([name        ([var : repr] ...)              otype    spec            fl    fpcore                                  cost])
(platform-register-implementations!
 platform
 (; Unary libm operators
  [fabs.64      ([x : binary64])                binary64 (fabs x)        'libm (! :precision binary64 (fabs x))        0]
  [sin.64       ([x : binary64])                binary64 (sin x)         'libm (! :precision binary64 (sin x))         0]
  [cos.64       ([x : binary64])                binary64 (cos x)         'libm (! :precision binary64 (cos x))         0]
  [tan.64       ([x : binary64])                binary64 (tan x)         'libm (! :precision binary64 (tan x))         0]
  [sinh.64      ([x : binary64])                binary64 (sinh x)        'libm (! :precision binary64 (sinh x))        0]
  [cosh.64      ([x : binary64])                binary64 (cosh x)        'libm (! :precision binary64 (cosh x))        0]
  [acos.64      ([x : binary64])                binary64 (acos x)        'libm (! :precision binary64 (acos x))        0]
  [acosh.64     ([x : binary64])                binary64 (acosh x)       'libm (! :precision binary64 (acosh x))       0]
  [asin.64      ([x : binary64])                binary64 (asin x)        'libm (! :precision binary64 (asin x))        0]
  [asinh.64     ([x : binary64])                binary64 (asinh x)       'libm (! :precision binary64 (asinh x))       0]
  [atan.64      ([x : binary64])                binary64 (atan x)        'libm (! :precision binary64 (atan x))        0]
  [atanh.64     ([x : binary64])                binary64 (atanh x)       'libm (! :precision binary64 (atanh x))       0]
  [cbrt.64      ([x : binary64])                binary64 (cbrt x)        'libm (! :precision binary64 (cbrt x))        0]
  [ceil.64      ([x : binary64])                binary64 (ceil x)        'libm (! :precision binary64 (ceil x))        0]
  [erf.64       ([x : binary64])                binary64 (erf x)         'libm (! :precision binary64 (erf x))         0]
  [exp.64       ([x : binary64])                binary64 (exp x)         'libm (! :precision binary64 (exp x))         0]
  [exp2.64      ([x : binary64])                binary64 (exp2 x)        'libm (! :precision binary64 (exp2 x))        0]
  [floor.64     ([x : binary64])                binary64 (floor x)       'libm (! :precision binary64 (floor x))       0]
  [lgamma.64    ([x : binary64])                binary64 (lgamma x)      'libm (! :precision binary64 (lgamma x))      0]
  [log.64       ([x : binary64])                binary64 (log x)         'libm (! :precision binary64 (log x))         0]
  [log10.64     ([x : binary64])                binary64 (log10 x)       'libm (! :precision binary64 (log10 x))       0]
  [log2.64      ([x : binary64])                binary64 (log2 x)        'libm (! :precision binary64 (log2 x))        0]
  [logb.64      ([x : binary64])                binary64 (logb x)        'libm (! :precision binary64 (logb x))        0]
  [rint.64      ([x : binary64])                binary64 (rint x)        'libm (! :precision binary64 (rint x))        0]
  [round.64     ([x : binary64])                binary64 (round x)       'libm (! :precision binary64 (round x))       0]
  [sqrt.64      ([x : binary64])                binary64 (sqrt x)        'libm (! :precision binary64 (sqrt x))        0]
  [tanh.64      ([x : binary64])                binary64 (tanh x)        'libm (! :precision binary64 (tanh x))        0]
  [tgamma.64    ([x : binary64])                binary64 (tgamma x)      'libm (! :precision binary64 (tgamma x))      0]
  [trunc.64     ([x : binary64])                binary64 (trunc x)       'libm (! :precision binary64 (trunc x))       0]
  ; Binary libm operators
  [pow.64       ([x : binary64] [y : binary64]) binary64 (pow x y)       'libm (! :precision binary64 (pow x y))       0]
  [atan2.64     ([x : binary64] [y : binary64]) binary64 (atan2 x y)     'libm (! :precision binary64 (atan2 x y))     0]
  [copysign.64  ([x : binary64] [y : binary64]) binary64 (copysign x y)  'libm (! :precision binary64 (copysign x y))  0]
  [fdim.64      ([x : binary64] [y : binary64]) binary64 (fdim x y)      'libm (! :precision binary64 (fdim x y))      0]
  [fmax.64      ([x : binary64] [y : binary64]) binary64 (fmax x y)      'libm (! :precision binary64 (fmax x y))      0]
  [fmin.64      ([x : binary64] [y : binary64]) binary64 (fmin x y)      'libm (! :precision binary64 (fmin x y))      0]
  [fmod.64      ([x : binary64] [y : binary64]) binary64 (fmod x y)      'libm (! :precision binary64 (fmod x y))      0]
  [remainder.64 ([x : binary64] [y : binary64]) binary64 (remainder x y) 'libm (! :precision binary64 (remainder x y)) 0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

; ([name     ([var : repr] ...)                             otype    spec                       fl    fpcore                              cost])
(platform-register-implementations!
 platform
 ([erfc.f64  ([x : binary64])                               binary64 (- 1 (erf x))              'libm (! :precision binary64 (erfc x))    0]
  [expm1.f64 ([x : binary64])                               binary64 (- (exp x) 1)              'libm (! :precision binary64 (expm1 x))   0]
  [log1p.f64 ([x : binary64])                               binary64 (log (+ 1 x))              'libm (! :precision binary64 (log1p x))   0]
  [hypot.f64 ([x : binary64] [y : binary64])                binary64 (sqrt (+ (* x x) (* y y))) 'libm (! :precision binary64 (hypot x y)) 0]
  [fma.f64   ([x : binary64] [y : binary64] [z : binary64]) binary64 (+ (* x y) z)              'libm (! :precision binary64 (fma x y z)) 0]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; additional converters ;;;;;;;;;;;;;;;;;

#;(platform-register-implementation! platform
                                     (make-operator-impl (binary64->binary32 [x : binary64])
                                                         binary32
                                                         #:spec x
                                                         #:fpcore (! :precision binary32 (cast x))
                                                         #:fl flsingle))

#;(platform-register-implementation! platform
                                     (make-operator-impl (binary32->binary64 [x : binary32])
                                                         binary64
                                                         #:spec x
                                                         #:fpcore (! :precision binary64 (cast x))
                                                         #:fl identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REGISTER PLATFORM ;;;;;;;;;;;;;;;;;;;;;

(register-platform! platform)

(module+ main
  (display-platform platform))

;; Do not run this file during testing
(module test racket/base
  )
