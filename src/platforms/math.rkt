#lang racket

;;; C/C++ on Linux with reduced libm
;;; We use textbook mathematical operators, i.e.,
;;; no special numbers functions

(require math/bigfloat
         math/flonum
         "../utils/float.rkt" ; for shift/unshift
         "../syntax/platform.rkt")
(provide platform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMPTY PLATFORM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define move-cost    0.02333600000000001)
(define fl-move-cost (* move-cost 4))

(define platform (make-empty-platform 'math #:if-cost move-cost))

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

(platform-register-representation! platform #:repr bool #:cost move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([TRUE  () bool (TRUE)  (const true)  (! TRUE)  move-cost]
  [FALSE () bool (FALSE) (const false) (! FALSE) move-cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(platform-register-implementations!
 platform
 ([not ([x : bool])            bool (not x)   not    (not x)   move-cost]
  [and ([x : bool] [y : bool]) bool (and x y) and-fn (and x y) move-cost]
  [or  ([x : bool] [y : bool]) bool (or x y)  or-fn  (or x y)  move-cost]))

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

(platform-register-representation! platform #:repr binary64 #:cost fl-move-cost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([PI.f64       () binary64 (PI)       (const pi)        (! :precision binary64 PI)       fl-move-cost]
  [E.f64        () binary64 (E)        (const (exp 1.0)) (! :precision binary64 E)        fl-move-cost]
  [INFINITY.f64 () binary64 (INFINITY) (const +inf.0)    (! :precision binary64 INFINITY) fl-move-cost]
  [NAN.f64      () binary64 (NAN)      (const +nan.0)    (! :precision binary64 NAN)      fl-move-cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ([name   ([var : repr] ...)              otype    spec     fl         fpcore                         cost])
(platform-register-implementations!
 platform
 ([neg.f64 ([x : binary64])                binary64 (neg x)  -          (! :precision binary64 (- x))   0.096592]
  [+.f64   ([x : binary64] [y : binary64]) binary64 (+ x y)  +          (! :precision binary64 (+ x y)) 0.164604]
  [-.f64   ([x : binary64] [y : binary64]) binary64 (- x y)  -          (! :precision binary64 (- x y)) 0.15163999999999997]
  [*.f64   ([x : binary64] [y : binary64]) binary64 (* x y)  *          (! :precision binary64 (* x y)) 0.20874800000000002]
  [/.f64   ([x : binary64] [y : binary64]) binary64 (/ x y)  /          (! :precision binary64 (/ x y)) 0.26615199999999994]
  [==.f64  ([x : binary64] [y : binary64]) bool     (== x y) =          (== x y)                        fl-move-cost]
  [!=.f64  ([x : binary64] [y : binary64]) bool     (!= x y) (negate =) (!= x y)                        fl-move-cost]
  [<.f64   ([x : binary64] [y : binary64]) bool     (< x y)  <          (< x y)                         fl-move-cost]
  [>.f64   ([x : binary64] [y : binary64]) bool     (> x y)  >          (> x y)                         fl-move-cost]
  [<=.f64  ([x : binary64] [y : binary64]) bool     (<= x y) <=         (<= x y)                        fl-move-cost]
  [>=.f64  ([x : binary64] [y : binary64]) bool     (>= x y) >=         (>= x y)                        fl-move-cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

; ([name         ([var : repr] ...)              otype    spec            fl    fpcore                                  cost])
(platform-register-implementations!
 platform
 (; Unary libm operators
  [fabs.f64      ([x : binary64])                binary64 (fabs x)        (from-libm 'fabs)      (! :precision binary64 (fabs x))        0.10162]
  [sin.f64       ([x : binary64])                binary64 (sin x)         (from-libm 'sin)       (! :precision binary64 (sin x))         3.318128]
  [cos.f64       ([x : binary64])                binary64 (cos x)         (from-libm 'cos)       (! :precision binary64 (cos x))         3.32288]
  [tan.f64       ([x : binary64])                binary64 (tan x)         (from-libm 'tan)       (! :precision binary64 (tan x))         3.710904]
  [sinh.f64      ([x : binary64])                binary64 (sinh x)        (from-libm 'sinh)      (! :precision binary64 (sinh x))        1.20954]
  [cosh.f64      ([x : binary64])                binary64 (cosh x)        (from-libm 'cosh)      (! :precision binary64 (cosh x))        0.953896]
  [acos.f64      ([x : binary64])                binary64 (acos x)        (from-libm 'acos)      (! :precision binary64 (acos x))        0.357748]
  [acosh.f64     ([x : binary64])                binary64 (acosh x)       (from-libm 'acosh)     (! :precision binary64 (acosh x))       0.659472]
  [asin.f64      ([x : binary64])                binary64 (asin x)        (from-libm 'asin)      (! :precision binary64 (asin x))        0.389788]
  [asinh.f64     ([x : binary64])                binary64 (asinh x)       (from-libm 'asinh)     (! :precision binary64 (asinh x))       0.835028]
  [atan.f64      ([x : binary64])                binary64 (atan x)        (from-libm 'atan)      (! :precision binary64 (atan x))        0.83752]
  [atanh.f64     ([x : binary64])                binary64 (atanh x)       (from-libm 'atanh)     (! :precision binary64 (atanh x))       0.36238]
  [cbrt.f64      ([x : binary64])                binary64 (cbrt x)        (from-libm 'cbrt)      (! :precision binary64 (cbrt x))        1.565176]
  [ceil.f64      ([x : binary64])                binary64 (ceil x)        (from-libm 'ceil)      (! :precision binary64 (ceil x))        0.47299]
  [erf.f64       ([x : binary64])                binary64 (erf x)         (from-libm 'erf)       (! :precision binary64 (erf x))         0.806436]
  [exp.f64       ([x : binary64])                binary64 (exp x)         (from-libm 'exp)       (! :precision binary64 (exp x))         1.0806]
  [exp2.f64      ([x : binary64])                binary64 (exp2 x)        (from-libm 'exp2)      (! :precision binary64 (exp2 x))        0.825484]
  [floor.f64     ([x : binary64])                binary64 (floor x)       (from-libm 'floor)     (! :precision binary64 (floor x))       0.468568]
  [lgamma.f64    ([x : binary64])                binary64 (lgamma x)      (from-libm 'lgamma)    (! :precision binary64 (lgamma x))      1.568012]
  [log.f64       ([x : binary64])                binary64 (log x)         (from-libm 'log)       (! :precision binary64 (log x))         0.505724]
  [log10.f64     ([x : binary64])                binary64 (log10 x)       (from-libm 'log10)     (! :precision binary64 (log10 x))       0.868856]
  [log2.f64      ([x : binary64])                binary64 (log2 x)        (from-libm 'log2)      (! :precision binary64 (log2 x))        0.681276]
  [logb.f64      ([x : binary64])                binary64 (logb x)        (from-libm 'logb)      (! :precision binary64 (logb x))        0.220656]
  [rint.f64      ([x : binary64])                binary64 (rint x)        (from-libm 'rint)      (! :precision binary64 (rint x))        0.121864]
  [round.f64     ([x : binary64])                binary64 (round x)       (from-libm 'round)     (! :precision binary64 (round x))       0.658564]
  [sqrt.f64      ([x : binary64])                binary64 (sqrt x)        (from-libm 'sqrt)      (! :precision binary64 (sqrt x))        0.191872]
  [tanh.f64      ([x : binary64])                binary64 (tanh x)        (from-libm 'tanh)      (! :precision binary64 (tanh x))        0.824016]
  [tgamma.f64    ([x : binary64])                binary64 (tgamma x)      (from-libm 'tgamma)    (! :precision binary64 (tgamma x))      1.882576]
  [trunc.f64     ([x : binary64])                binary64 (trunc x)       (from-libm 'trunc)     (! :precision binary64 (trunc x))       0.463644]
  ; Binary libm operators
  [pow.f64       ([x : binary64] [y : binary64]) binary64 (pow x y)       (from-libm 'pow)       (! :precision binary64 (pow x y))       1.52482]
  [atan2.f64     ([x : binary64] [y : binary64]) binary64 (atan2 x y)     (from-libm 'atan2)     (! :precision binary64 (atan2 x y))     1.492804]
  [copysign.f64  ([x : binary64] [y : binary64]) binary64 (copysign x y)  (from-libm 'copysign)  (! :precision binary64 (copysign x y))  0.200452]
  [fdim.f64      ([x : binary64] [y : binary64]) binary64 (fdim x y)      (from-libm 'fdim)      (! :precision binary64 (fdim x y))      0.592576]
  [fmax.f64      ([x : binary64] [y : binary64]) binary64 (fmax x y)      (from-libm 'fmax)      (! :precision binary64 (fmax x y))      0.3106]
  [fmin.f64      ([x : binary64] [y : binary64]) binary64 (fmin x y)      (from-libm 'fmin)      (! :precision binary64 (fmin x y))      0.289256]
  [fmod.f64      ([x : binary64] [y : binary64]) binary64 (fmod x y)      (from-libm 'fmod)      (! :precision binary64 (fmod x y))      94.277144]
  [remainder.f64 ([x : binary64] [y : binary64]) binary64 (remainder x y) (from-libm 'remainder) (! :precision binary64 (remainder x y)) 16.165012]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation! platform
                                   (make-operator-impl (erfc.f64 [x : binary64])
                                                       binary64
                                                       #:spec (- 1 (erf x))
                                                       #:fpcore (! :precision binary64 (erfc x))
                                                       #:fl (from-libm 'erfc)
                                                       #:cost 0.816512))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REGISTER PLATFORM ;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (display-platform platform))

;; Do not run this file during testing
(module test racket/base
  )
