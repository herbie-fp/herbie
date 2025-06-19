#lang racket

;;; C/C++ on Linux with reduced libm
;;; We use textbook mathematical operators, i.e.,
;;; no special numbers functions

(require math/bigfloat
         math/flonum)

(require "runtime/libm.rkt") ; libm wrapper in Racket

(require "../utils/float.rkt" ; for shift/unshift
         (submod "../syntax/platform.rkt" internals))

(define move-cost 0.02333600000000001)
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

(platform-register-implementation!
 math-platform
 (make-operator-impl (TRUE) bool #:spec (TRUE) #:fl (const true) #:fpcore (! TRUE) #:cost move-cost))

(platform-register-implementation! math-platform
                                   (make-operator-impl (FALSE)
                                                       bool
                                                       #:spec (FALSE)
                                                       #:fl (const false)
                                                       #:fpcore (! FALSE)
                                                       #:cost move-cost))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(platform-register-implementation!
 math-platform
 (make-operator-impl (not [x : bool]) bool #:spec (not x) #:fpcore (not x) #:fl not #:cost move-cost))

(platform-register-implementation! math-platform
                                   (make-operator-impl (and [x : bool] [y : bool])
                                                       bool
                                                       #:spec (and x y)
                                                       #:fpcore (and x y)
                                                       #:fl and-fn
                                                       #:cost move-cost))

(platform-register-implementation! math-platform
                                   (make-operator-impl (or [x : bool] [y : bool])
                                                       bool
                                                       #:spec (or x y)
                                                       #:fpcore (or x y)
                                                       #:fl or-fn
                                                       #:cost move-cost))

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

(platform-register-implementation! math-platform
                                   (make-operator-impl (PI.f64)
                                                       binary64
                                                       #:spec (PI)
                                                       #:fl (const pi)
                                                       #:fpcore (! :precision binary64 (PI))
                                                       #:cost fl-move-cost))
(platform-register-implementation! math-platform
                                   (make-operator-impl (E.f64)
                                                       binary64
                                                       #:spec (E)
                                                       #:fl (const (exp 1.0))
                                                       #:fpcore (! :precision binary64 (E))
                                                       #:cost fl-move-cost))
(platform-register-implementation! math-platform
                                   (make-operator-impl (INFINITY.f64)
                                                       binary64
                                                       #:spec (INFINITY)
                                                       #:fl (const +inf.0)
                                                       #:fpcore (! :precision binary64 (INFINITY))
                                                       #:cost fl-move-cost))
(platform-register-implementation! math-platform
                                   (make-operator-impl (NAN.f64)
                                                       binary64
                                                       #:spec (NAN)
                                                       #:fl (const +nan.0)
                                                       #:fpcore (! :precision binary64 (NAN))
                                                       #:cost fl-move-cost))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation! math-platform
                                   (make-operator-impl (neg.f64 [x : binary64])
                                                       binary64
                                                       #:spec (neg x)
                                                       #:fpcore (! :precision binary64 (- x))
                                                       #:fl -
                                                       #:cost 0.096592))

(platform-register-implementation! math-platform
                                   (make-operator-impl (+.f64 [x : binary64] [y : binary64])
                                                       binary64
                                                       #:spec (+ x y)
                                                       #:fpcore (! :precision binary64 (+ x y))
                                                       #:fl +
                                                       #:cost 0.164604))

(platform-register-implementation! math-platform
                                   (make-operator-impl (-.f64 [x : binary64] [y : binary64])
                                                       binary64
                                                       #:spec (- x y)
                                                       #:fpcore (! :precision binary64 (- x y))
                                                       #:fl -
                                                       #:cost 0.15163999999999997))

(platform-register-implementation! math-platform
                                   (make-operator-impl (*.f64 [x : binary64] [y : binary64])
                                                       binary64
                                                       #:spec (* x y)
                                                       #:fpcore (! :precision binary64 (* x y))
                                                       #:fl *
                                                       #:cost 0.20874800000000002))

(platform-register-implementation! math-platform
                                   (make-operator-impl (/.f64 [x : binary64] [y : binary64])
                                                       binary64
                                                       #:spec (/ x y)
                                                       #:fpcore (! :precision binary64 (/ x y))
                                                       #:fl /
                                                       #:cost 0.26615199999999994))

(platform-register-implementation! math-platform
                                   (make-operator-impl (==.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (== x y)
                                                       #:fpcore (== x y)
                                                       #:fl =
                                                       #:cost fl-move-cost))

(platform-register-implementation! math-platform
                                   (make-operator-impl (!=.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (!= x y)
                                                       #:fpcore (!= x y)
                                                       #:fl (negate =)
                                                       #:cost fl-move-cost))

(platform-register-implementation! math-platform
                                   (make-operator-impl (<.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (< x y)
                                                       #:fpcore (< x y)
                                                       #:fl <
                                                       #:cost fl-move-cost))

(platform-register-implementation! math-platform
                                   (make-operator-impl (>.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (> x y)
                                                       #:fpcore (> x y)
                                                       #:fl >
                                                       #:cost fl-move-cost))

(platform-register-implementation! math-platform
                                   (make-operator-impl (<=.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (<= x y)
                                                       #:fpcore (<= x y)
                                                       #:fl <=
                                                       #:cost fl-move-cost))

(platform-register-implementation! math-platform
                                   (make-operator-impl (>=.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (>= x y)
                                                       #:fpcore (>= x y)
                                                       #:fl >=
                                                       #:cost fl-move-cost))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (make-libm-impl/binary64 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype cost attrib ...)
     (with-syntax ([impl (string->symbol (format "~a.f64" (syntax->datum #'op)))])
       #'(make-libm-impl op (op impl itype ...) otype cost attrib ...))]))

(define-syntax-rule (make-libm-impls/binary64* (itype ... otype) [name cost] ...)
  (list (make-libm-impl/binary64 name (itype ...) otype cost) ...))

(define-syntax-rule (make-libm-impls/binary64 [(itype ... otype) ([name cost] ...)] ...)
  (apply append (list (make-libm-impls/binary64* (itype ... otype) [name cost] ...) ...)))

(define libm-impls.f64
  (make-libm-impls/binary64 [(binary64 binary64)
                             ([fabs 0.10162] [sin 3.318128]
                                             [cos 3.32288]
                                             [tan 3.710904]
                                             [sinh 1.20954]
                                             [cosh 0.953896]
                                             [acos 0.357748]
                                             [acosh 0.659472]
                                             [asin 0.389788]
                                             [asinh 0.835028]
                                             [atan 0.83752]
                                             [atanh 0.36238]
                                             [cbrt 1.565176]
                                             [ceil 0.47299]
                                             [erf 0.806436]
                                             [exp 1.0806]
                                             [exp2 0.825484]
                                             [floor 0.468568]
                                             [lgamma 1.568012]
                                             [log 0.505724]
                                             [log10 0.868856]
                                             [log2 0.681276]
                                             [logb 0.220656]
                                             [rint 0.121864]
                                             [round 0.658564]
                                             [sqrt 0.191872]
                                             [tanh 0.824016]
                                             [tgamma 1.882576]
                                             [trunc 0.463644])]
                            [(binary64 binary64 binary64)
                             ([pow 1.52482] [atan2 1.492804]
                                            [copysign 0.200452]
                                            [fdim 0.592576]
                                            [fmax 0.3106]
                                            [fmin 0.289256]
                                            [fmod 94.277144]
                                            [remainder 16.165012])]))

(for ([libm-impl.f64 (in-list libm-impls.f64)])
  (when libm-impl.f64
    (platform-register-implementation! math-platform libm-impl.f64)))

(define c_erfc (make-libm (erfc double double)))
(define c_expm1 (make-libm (expm1 double double)))
(define c_log1p (make-libm (log1p double double)))
(define c_hypot (make-libm (hypot double double double)))
(define c_fma (make-libm (fma double double double double)))

(when c_erfc
  (platform-register-implementation! math-platform
                                     (make-operator-impl (erfc.f64 [x : binary64])
                                                         binary64
                                                         #:spec (- 1 (erf x))
                                                         #:fpcore (! :precision binary64 (erfc x))
                                                         #:fl c_erfc
                                                         #:cost 0.816512)))

(register-platform! math-platform)

;; Do not run this file during testing
(module test racket/base
  )
