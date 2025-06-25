#lang racket

;;; C/C++ on Linux with reduced libm
;;; We use textbook mathematical operators, i.e.,
;;; no special numbers functions

(require math/bigfloat
         math/flonum
         "runtime/libm.rkt" ; libm wrapper in Racket
         "../utils/float.rkt" ; for shift/unshift
         (submod "../syntax/platform.rkt" internals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMPTY PLATFORM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define move-cost    0.02333600000000001)
(define fl-move-cost (* move-cost 4))

(define math-platform (make-empty-platform 'math #:if-cost move-cost #:default-cost fl-move-cost))

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
                       #:cost move-cost))

(platform-register-representation! math-platform bool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 math-platform
 ([TRUE  () bool (TRUE)  (const true)  (! TRUE)  move-cost]
  [FALSE () bool (FALSE) (const false) (! FALSE) move-cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(platform-register-implementations!
 math-platform
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
                       #:special-value? nan?
                       #:cost fl-move-cost))

(platform-register-representation! math-platform binary64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 math-platform
 ([PI.f64       () binary64 (PI)       (const pi)        (! :precision binary64 (PI))       fl-move-cost]
  [E.f64        () binary64 (E)        (const (exp 1.0)) (! :precision binary64 (E))        fl-move-cost]
  [INFINITY.f64 () binary64 (INFINITY) (const +inf.0)    (! :precision binary64 (INFINITY)) fl-move-cost]
  [NAN.f64      () binary64 (NAN)      (const +nan.0)    (! :precision binary64 (NAN))      fl-move-cost]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ([name   ([var : repr] ...)              otype    spec     fl         fpcore                         cost])
(platform-register-implementations!
 math-platform
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

(define libm-impls.f64
  (make-libm-impls/binary64
   [(binary64 binary64)
    ([fabs      0.10162]
     [sin       3.318128]
     [cos       3.32288]
     [tan       3.710904]
     [sinh      1.20954]
     [cosh      0.953896]
     [acos      0.357748]
     [acosh     0.659472]
     [asin      0.389788]
     [asinh     0.835028]
     [atan      0.83752]
     [atanh     0.36238]
     [cbrt      1.565176]
     [ceil      0.47299]
     [erf       0.806436]
     [exp       1.0806]
     [exp2      0.825484]
     [floor     0.468568]
     [lgamma    1.568012]
     [log       0.505724]
     [log10     0.868856]
     [log2      0.681276]
     [logb      0.220656]
     [rint      0.121864]
     [round     0.658564]
     [sqrt      0.191872]
     [tanh      0.824016]
     [tgamma    1.882576]
     [trunc     0.463644])]
   [(binary64 binary64 binary64)
    ([pow       1.52482]
     [atan2     1.492804]
     [copysign  0.200452]
     [fdim      0.592576]
     [fmax      0.3106]
     [fmin      0.289256]
     [fmod      94.277144]
     [remainder 16.165012])]))

(for ([libm-impl.f64 (in-list libm-impls.f64)])
  (when libm-impl.f64
    (platform-register-implementation! math-platform libm-impl.f64)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm accelerators ;;;;;;;;;;;;;;;;;;;;;

(define c_erfc (make-libm (erfc double double)))

(when c_erfc
  (platform-register-implementation! math-platform
                                     (make-operator-impl (erfc.f64 [x : binary64])
                                                         binary64
                                                         #:spec (- 1 (erf x))
                                                         #:fpcore (! :precision binary64 (erfc x))
                                                         #:fl c_erfc
                                                         #:cost 0.816512)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REGISTER PLATFORM ;;;;;;;;;;;;;;;;;;;;;

(register-platform! math-platform)

(module+ main
  (display-platform math-platform))

;; Do not run this file during testing
(module test racket/base
  )
