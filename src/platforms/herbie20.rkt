#lang racket

;;; The default platform:
;;; C/C++ on Linux with a full libm

(require math/bigfloat
         math/flonum)

(require "runtime/float32.rkt" ; float representation helper functions
         "runtime/libm.rkt") ; libm wrapper

(require "../utils/float.rkt" ; for shift/unshift
         (submod "../syntax/platform.rkt" internals))

(define herbie20-platform (make-empty-platform 'herbie20 #:if-cost 1 #:default-cost 1))

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
                       #:cost 1))

(platform-register-representation! herbie20-platform bool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation!
 herbie20-platform
 (make-operator-impl (TRUE) bool #:spec (TRUE) #:fl (const true) #:fpcore (! TRUE) #:cost 1))

(platform-register-implementation!
 herbie20-platform
 (make-operator-impl (FALSE) bool #:spec (FALSE) #:fl (const false) #:fpcore (! FALSE) #:cost 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(platform-register-implementation!
 herbie20-platform
 (make-operator-impl (not [x : bool]) bool #:spec (not x) #:fpcore (not x) #:fl not #:cost 1))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (and [x : bool] [y : bool])
                                                       bool
                                                       #:spec (and x y)
                                                       #:fpcore (and x y)
                                                       #:fl and-fn
                                                       #:cost 1))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (or [x : bool] [y : bool])
                                                       bool
                                                       #:spec (or x y)
                                                       #:fpcore (or x y)
                                                       #:fl or-fn
                                                       #:cost 1))

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
                       #:cost 32))

(platform-register-representation! herbie20-platform binary32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (PI.f32)
                                                       binary32
                                                       #:spec (PI)
                                                       #:fl (const (flsingle pi))
                                                       #:fpcore (! :precision binary32 (PI))
                                                       #:cost 32))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (E.f32)
                                                       binary32
                                                       #:spec (E)
                                                       #:fl (const (flsingle (exp 1.0)))
                                                       #:fpcore (! :precision binary32 (E))
                                                       #:cost 32))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (INFINITY.f32)
                                                       binary32
                                                       #:spec (INFINITY)
                                                       #:fl (const +inf.0)
                                                       #:fpcore (! :precision binary32 (INFINITY))
                                                       #:cost 32))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (NAN.f32)
                                                       binary32
                                                       #:spec (NAN)
                                                       #:fl (const +nan.0)
                                                       #:fpcore (! :precision binary32 (NAN))
                                                       #:cost 32))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Wrapping arithmetic operations with `flsingle` introduces a
;; possible double-rounding problem but, perhaps surprisingly, this
;; double-rounding problem never actually causes error; see:
;;
;;   https://hal.science/hal-01091186/document

(define fl32+ (compose flsingle +))
(define fl32- (compose flsingle -))
(define fl32* (compose flsingle *))
(define fl32/ (compose flsingle /))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (neg.f32 [x : binary32])
                                                       binary32
                                                       #:spec (neg x)
                                                       #:fpcore (! :precision binary32 (- x))
                                                       #:fl fl32-
                                                       #:cost 64))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (+.f32 [x : binary32] [y : binary32])
                                                       binary32
                                                       #:spec (+ x y)
                                                       #:fpcore (! :precision binary32 (+ x y))
                                                       #:fl fl32+
                                                       #:cost 64))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (-.f32 [x : binary32] [y : binary32])
                                                       binary32
                                                       #:spec (- x y)
                                                       #:fpcore (! :precision binary32 (- x y))
                                                       #:fl fl32-
                                                       #:cost 64))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (*.f32 [x : binary32] [y : binary32])
                                                       binary32
                                                       #:spec (* x y)
                                                       #:fpcore (! :precision binary32 (* x y))
                                                       #:fl fl32*
                                                       #:cost 128))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (/.f32 [x : binary32] [y : binary32])
                                                       binary32
                                                       #:spec (/ x y)
                                                       #:fpcore (! :precision binary32 (/ x y))
                                                       #:fl fl32/
                                                       #:cost 320))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (==.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (== x y)
                                                       #:fpcore (== x y)
                                                       #:fl =
                                                       #:cost 128))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (!=.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (!= x y)
                                                       #:fpcore (!= x y)
                                                       #:fl (negate =)
                                                       #:cost 128))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (<.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (< x y)
                                                       #:fpcore (< x y)
                                                       #:fl <
                                                       #:cost 128))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (>.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (> x y)
                                                       #:fpcore (> x y)
                                                       #:fl >
                                                       #:cost 128))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (<=.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (<= x y)
                                                       #:fpcore (<= x y)
                                                       #:fl <=
                                                       #:cost 128))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (>=.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (>= x y)
                                                       #:fpcore (>= x y)
                                                       #:fl >=
                                                       #:cost 128))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

; Same as
; (define fabsf.libm (make-libm fabsf.libm (fabsf float float)))
; (define fabs.f32
;   (make-operator-impl (fabs.f32 [x : binary32]) binary32 #:spec (fabs x) #:fl fabsf.libm #:cost 1))
; (platform-register-implementation! herbie20-platform fabs.f32)

(define-syntax (make-libm-impl/binary32 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype cost attrib ...)
     (with-syntax ([impl (string->symbol (format "~a.f32" (syntax->datum #'op)))]
                   [cname (string->symbol (format "~af" (syntax->datum #'op)))])
       #'(make-libm-impl cname (op impl itype ...) otype cost attrib ...))]))

(define-syntax-rule (make-libm-impls/binary32* (itype ... otype) [name cost] ...)
  (list (make-libm-impl/binary32 name (itype ...) otype cost) ...))

(define-syntax-rule (make-libm-impls/binary32 [(itype ... otype) ([name cost] ...)] ...)
  (apply append (list (make-libm-impls/binary32* (itype ... otype) [name cost] ...) ...)))

(define libm-impls.f32
  (make-libm-impls/binary32 [(binary32 binary32)
                             ([fabs 64] [sin 3200]
                                        [cos 3200]
                                        [tan 3200]
                                        [sinh 3200]
                                        [cosh 3200]
                                        [acos 3200]
                                        [acosh 3200]
                                        [asin 3200]
                                        [asinh 3200]
                                        [atan 3200]
                                        [atanh 3200]
                                        [cbrt 3200]
                                        [ceil 3200]
                                        [erf 3200]
                                        [exp 3200]
                                        [exp2 3200]
                                        [floor 3200]
                                        [lgamma 3200]
                                        [log 3200]
                                        [log10 3200]
                                        [log2 3200]
                                        [logb 3200]
                                        [rint 3200]
                                        [round 3200]
                                        [sqrt 320] ; not a typo, it is 320
                                        [tanh 3200]
                                        [tgamma 3200]
                                        [trunc 3200])]
                            [(binary32 binary32 binary32)
                             ([pow 3200] [atan2 3200]
                                         [copysign 3200]
                                         [fdim 3200]
                                         [fmax 3200]
                                         [fmin 3200]
                                         [fmod 3200]
                                         [remainder 3200])]))

(for ([libm-impl.f32 (in-list libm-impls.f32)])
  (when libm-impl.f32
    (platform-register-implementation! herbie20-platform libm-impl.f32)))

(define c_erfcf (make-libm (erfcf float float)))
(define c_expm1f (make-libm (expm1f float float)))
(define c_log1pf (make-libm (log1pf float float)))
(define c_hypotf (make-libm (hypotf float float float)))
(define c_fmaf (make-libm (fmaf float float float float)))

(when c_erfcf
  (platform-register-implementation! herbie20-platform
                                     (make-operator-impl (erfc.f32 [x : binary32])
                                                         binary32
                                                         #:spec (- 1 (erf x))
                                                         #:fpcore (! :precision binary32 (erfc x))
                                                         #:fl c_erfcf
                                                         #:cost 3200)))

(when c_expm1f
  (platform-register-implementation! herbie20-platform
                                     (make-operator-impl (expm1.f32 [x : binary32])
                                                         binary32
                                                         #:spec (- (exp x) 1)
                                                         #:fpcore (! :precision binary32 (expm1 x))
                                                         #:fl c_expm1f
                                                         #:cost 3200)))

(when c_log1pf
  (platform-register-implementation! herbie20-platform
                                     (make-operator-impl (log1p.f32 [x : binary32])
                                                         binary32
                                                         #:spec (log (+ 1 x))
                                                         #:fpcore (! :precision binary32 (log1p x))
                                                         #:fl c_log1pf
                                                         #:cost 3200)))

(when c_hypotf
  (platform-register-implementation! herbie20-platform
                                     (make-operator-impl (hypot.f32 [x : binary32] [y : binary32])
                                                         binary32
                                                         #:spec (sqrt (+ (* x x) (* y y)))
                                                         #:fpcore (! :precision binary32 (hypot x y))
                                                         #:fl c_hypotf
                                                         #:cost 3200)))

(when c_fmaf
  (platform-register-implementation!
   herbie20-platform
   (make-operator-impl (fma.f32 [x : binary32] [y : binary32] [z : binary32])
                       binary32
                       #:spec (+ (* x y) z)
                       #:fpcore (! :precision binary32 (fma x y z))
                       #:fl c_fmaf
                       #:cost 128)))

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
                       #:cost 64))

(platform-register-representation! herbie20-platform binary64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (PI.f64)
                                                       binary64
                                                       #:spec (PI)
                                                       #:fl (const pi)
                                                       #:fpcore (! :precision binary64 (PI))
                                                       #:cost 64))
(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (E.f64)
                                                       binary64
                                                       #:spec (E)
                                                       #:fl (const (exp 1.0))
                                                       #:fpcore (! :precision binary64 (E))
                                                       #:cost 64))
(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (INFINITY.f64)
                                                       binary64
                                                       #:spec (INFINITY)
                                                       #:fl (const +inf.0)
                                                       #:fpcore (! :precision binary64 (INFINITY))
                                                       #:cost 64))
(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (NAN.f64)
                                                       binary64
                                                       #:spec (NAN)
                                                       #:fl (const +nan.0)
                                                       #:fpcore (! :precision binary64 (NAN))
                                                       #:cost 64))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (neg.f64 [x : binary64])
                                                       binary64
                                                       #:spec (neg x)
                                                       #:fpcore (! :precision binary64 (- x))
                                                       #:fl -
                                                       #:cost 128))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (+.f64 [x : binary64] [y : binary64])
                                                       binary64
                                                       #:spec (+ x y)
                                                       #:fpcore (! :precision binary64 (+ x y))
                                                       #:fl +
                                                       #:cost 128))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (-.f64 [x : binary64] [y : binary64])
                                                       binary64
                                                       #:spec (- x y)
                                                       #:fpcore (! :precision binary64 (- x y))
                                                       #:fl -
                                                       #:cost 128))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (*.f64 [x : binary64] [y : binary64])
                                                       binary64
                                                       #:spec (* x y)
                                                       #:fpcore (! :precision binary64 (* x y))
                                                       #:fl *
                                                       #:cost 256))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (/.f64 [x : binary64] [y : binary64])
                                                       binary64
                                                       #:spec (/ x y)
                                                       #:fpcore (! :precision binary64 (/ x y))
                                                       #:fl /
                                                       #:cost 640))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (==.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (== x y)
                                                       #:fpcore (== x y)
                                                       #:fl =
                                                       #:cost 256))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (!=.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (!= x y)
                                                       #:fpcore (!= x y)
                                                       #:fl (negate =)
                                                       #:cost 256))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (<.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (< x y)
                                                       #:fpcore (< x y)
                                                       #:fl <
                                                       #:cost 256))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (>.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (> x y)
                                                       #:fpcore (> x y)
                                                       #:fl >
                                                       #:cost 256))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (<=.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (<= x y)
                                                       #:fpcore (<= x y)
                                                       #:fl <=
                                                       #:cost 256))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (>=.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (>= x y)
                                                       #:fpcore (>= x y)
                                                       #:fl >=
                                                       #:cost 256))

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
                             ([fabs 128] [sin 6400]
                                         [cos 6400]
                                         [tan 6400]
                                         [sinh 6400]
                                         [cosh 6400]
                                         [acos 6400]
                                         [acosh 6400]
                                         [asin 6400]
                                         [asinh 6400]
                                         [atan 6400]
                                         [atanh 6400]
                                         [cbrt 6400]
                                         [ceil 6400]
                                         [erf 6400]
                                         [exp 6400]
                                         [exp2 6400]
                                         [floor 6400]
                                         [lgamma 6400]
                                         [log 6400]
                                         [log10 6400]
                                         [log2 6400]
                                         [logb 6400]
                                         [rint 6400]
                                         [round 6400]
                                         [sqrt 640] ; not a typo
                                         [tanh 6400]
                                         [tgamma 6400]
                                         [trunc 6400])]
                            [(binary64 binary64 binary64)
                             ([pow 6400] [atan2 6400]
                                         [copysign 6400]
                                         [fdim 6400]
                                         [fmax 6400]
                                         [fmin 6400]
                                         [fmod 6400]
                                         [remainder 6400])]))

(for ([libm-impl.f64 (in-list libm-impls.f64)])
  (when libm-impl.f64
    (platform-register-implementation! herbie20-platform libm-impl.f64)))

(define c_erfc (make-libm (erfc double double)))
(define c_expm1 (make-libm (expm1 double double)))
(define c_log1p (make-libm (log1p double double)))
(define c_hypot (make-libm (hypot double double double)))
(define c_fma (make-libm (fma double double double double)))

(when c_erfc
  (platform-register-implementation! herbie20-platform
                                     (make-operator-impl (erfc.f64 [x : binary64])
                                                         binary64
                                                         #:spec (- 1 (erf x))
                                                         #:fpcore (! :precision binary64 (erfc x))
                                                         #:fl c_erfc
                                                         #:cost 6400)))

(when c_expm1
  (platform-register-implementation! herbie20-platform
                                     (make-operator-impl (expm1.f64 [x : binary64])
                                                         binary64
                                                         #:spec (- (exp x) 1)
                                                         #:fpcore (! :precision binary64 (expm1 x))
                                                         #:fl c_expm1
                                                         #:cost 6400)))

(when c_log1p
  (platform-register-implementation! herbie20-platform
                                     (make-operator-impl (log1p.f64 [x : binary64])
                                                         binary64
                                                         #:spec (log (+ 1 x))
                                                         #:fpcore (! :precision binary64 (log1p x))
                                                         #:fl c_log1p
                                                         #:cost 6400)))

(when c_hypot
  (platform-register-implementation! herbie20-platform
                                     (make-operator-impl (hypot.f64 [x : binary64] [y : binary64])
                                                         binary64
                                                         #:spec (sqrt (+ (* x x) (* y y)))
                                                         #:fpcore (! :precision binary64 (hypot x y))
                                                         #:fl c_hypot
                                                         #:cost 6400)))

(when c_fma
  (platform-register-implementation!
   herbie20-platform
   (make-operator-impl (fma.f64 [x : binary64] [y : binary64] [z : binary64])
                       binary64
                       #:spec (+ (* x y) z)
                       #:fpcore (! :precision binary64 (fma x y z))
                       #:fl c_fma
                       #:cost 256)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; additional converters ;;;;;;;;;;;;;;;;;

#;(platform-register-implementation! herbie20-platform
                                     (make-operator-impl (binary64->binary32 [x : binary64])
                                                         binary32
                                                         #:spec x
                                                         #:fpcore (! :precision binary32 (cast x))
                                                         #:fl flsingle
                                                         #:cost 64))

#;(platform-register-implementation! herbie20-platform
                                     (make-operator-impl (binary32->binary64 [x : binary32])
                                                         binary64
                                                         #:spec x
                                                         #:fpcore (! :precision binary64 (cast x))
                                                         #:fl identity
                                                         #:cost 64))

(register-platform! herbie20-platform)

;; Do not run this file during testing
(module test racket/base
  )
