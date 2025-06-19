#lang racket

;;; The default platform:
;;; C/C++ on Linux with a full libm

(require math/bigfloat
         math/flonum)

(require "runtime/float32.rkt" ; float representation helper functions
         "runtime/libm.rkt") ; libm wrapper

(require "../utils/float.rkt" ; for shift/unshift
         (submod "../syntax/platform.rkt" internals))

(define 64bit-move-cost 0.125384)
(define 32bit-move-cost 0.12963)
(define boolean-move-cost 0.1)

(define c-platform
  (make-empty-platform 'c #:if-cost boolean-move-cost #:default-cost 64bit-move-cost))

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
                       #:cost boolean-move-cost))

(platform-register-representation! c-platform bool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation! c-platform
                                   (make-operator-impl (TRUE)
                                                       bool
                                                       #:spec (TRUE)
                                                       #:fl (const true)
                                                       #:fpcore (! TRUE)
                                                       #:cost boolean-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (FALSE)
                                                       bool
                                                       #:spec (FALSE)
                                                       #:fl (const false)
                                                       #:fpcore (! FALSE)
                                                       #:cost boolean-move-cost))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(platform-register-implementation! c-platform
                                   (make-operator-impl (not [x : bool])
                                                       bool
                                                       #:spec (not x)
                                                       #:fpcore (not x)
                                                       #:fl not
                                                       #:cost boolean-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (and [x : bool] [y : bool])
                                                       bool
                                                       #:spec (and x y)
                                                       #:fpcore (and x y)
                                                       #:fl and-fn
                                                       #:cost boolean-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (or [x : bool] [y : bool])
                                                       bool
                                                       #:spec (or x y)
                                                       #:fpcore (or x y)
                                                       #:fl or-fn
                                                       #:cost boolean-move-cost))

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
                       #:cost 32bit-move-cost))

(platform-register-representation! c-platform binary32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation! c-platform
                                   (make-operator-impl (PI.f32)
                                                       binary32
                                                       #:spec (PI)
                                                       #:fl (const (flsingle pi))
                                                       #:fpcore (! :precision binary32 (PI))
                                                       #:cost 32bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (E.f32)
                                                       binary32
                                                       #:spec (E)
                                                       #:fl (const (flsingle (exp 1.0)))
                                                       #:fpcore (! :precision binary32 (E))
                                                       #:cost 32bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (INFINITY.f32)
                                                       binary32
                                                       #:spec (INFINITY)
                                                       #:fl (const +inf.0)
                                                       #:fpcore (! :precision binary32 (INFINITY))
                                                       #:cost 32bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (NAN.f32)
                                                       binary32
                                                       #:spec (NAN)
                                                       #:fl (const +nan.0)
                                                       #:fpcore (! :precision binary32 (NAN))
                                                       #:cost 32bit-move-cost))

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

(platform-register-implementation! c-platform
                                   (make-operator-impl (neg.f32 [x : binary32])
                                                       binary32
                                                       #:spec (neg x)
                                                       #:fpcore (! :precision binary32 (- x))
                                                       #:fl fl32-
                                                       #:cost 0.115677))

(platform-register-implementation! c-platform
                                   (make-operator-impl (+.f32 [x : binary32] [y : binary32])
                                                       binary32
                                                       #:spec (+ x y)
                                                       #:fpcore (! :precision binary32 (+ x y))
                                                       #:fl fl32+
                                                       #:cost 0.200445))

(platform-register-implementation! c-platform
                                   (make-operator-impl (-.f32 [x : binary32] [y : binary32])
                                                       binary32
                                                       #:spec (- x y)
                                                       #:fpcore (! :precision binary32 (- x y))
                                                       #:fl fl32-
                                                       #:cost 0.191068))

(platform-register-implementation! c-platform
                                   (make-operator-impl (*.f32 [x : binary32] [y : binary32])
                                                       binary32
                                                       #:spec (* x y)
                                                       #:fpcore (! :precision binary32 (* x y))
                                                       #:fl fl32*
                                                       #:cost 0.256602))

(platform-register-implementation! c-platform
                                   (make-operator-impl (/.f32 [x : binary32] [y : binary32])
                                                       binary32
                                                       #:spec (/ x y)
                                                       #:fpcore (! :precision binary32 (/ x y))
                                                       #:fl fl32/
                                                       #:cost 0.346533))

(platform-register-implementation! c-platform
                                   (make-operator-impl (==.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (== x y)
                                                       #:fpcore (== x y)
                                                       #:fl =
                                                       #:cost 32bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (!=.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (!= x y)
                                                       #:fpcore (!= x y)
                                                       #:fl (negate =)
                                                       #:cost 32bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (<.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (< x y)
                                                       #:fpcore (< x y)
                                                       #:fl <
                                                       #:cost 32bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (>.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (> x y)
                                                       #:fpcore (> x y)
                                                       #:fl >
                                                       #:cost 32bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (<=.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (<= x y)
                                                       #:fpcore (<= x y)
                                                       #:fl <=
                                                       #:cost 32bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (>=.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (>= x y)
                                                       #:fpcore (>= x y)
                                                       #:fl >=
                                                       #:cost 32bit-move-cost))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

; Same as
; (define fabsf.libm (make-libm fabsf.libm (fabsf float float)))
; (define fabs.f32
;   (make-operator-impl (fabs.f32 [x : binary32]) binary32 #:spec (fabs x) #:fl fabsf.libm #:cost 1))
; (platform-register-implementation! c-platform fabs.f32)

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
                             ([fabs 0.124646] [sin 4.21853]
                                              [cos 4.273883]
                                              [tan 4.669173]
                                              [sinh 1.746303]
                                              [cosh 1.32949]
                                              [acos 0.521333]
                                              [acosh 0.847705]
                                              [asin 0.480267]
                                              [asinh 1.120725]
                                              [atan 1.101518]
                                              [atanh 0.465283]
                                              [cbrt 1.946652]
                                              [ceil 0.287118]
                                              [erf 1.121118]
                                              [exp 1.386056]
                                              [exp2 1.1716]
                                              [floor 0.295966]
                                              [lgamma 2.22032]
                                              [log 0.778197]
                                              [log10 1.179358]
                                              [log2 0.864416]
                                              [logb 0.36221]
                                              [rint 0.293168]
                                              [round 0.86771]
                                              [sqrt 0.254647]
                                              [tanh 1.056722]
                                              [tgamma 2.628082]
                                              [trunc 0.287654])]
                            [(binary32 binary32 binary32)
                             ([pow 2.028296] [atan2 1.955913]
                                             [copysign 0.20428]
                                             [fdim 0.763562]
                                             [fmax 0.236365]
                                             [fmin 0.241269]
                                             [fmod 1.718247]
                                             [remainder 1.030245])]))

(for ([libm-impl.f32 (in-list libm-impls.f32)])
  (when libm-impl.f32
    (platform-register-implementation! c-platform libm-impl.f32)))

(define c_erfcf (make-libm (erfcf float float)))
(define c_expm1f (make-libm (expm1f float float)))
(define c_log1pf (make-libm (log1pf float float)))
(define c_hypotf (make-libm (hypotf float float float)))
(define c_fmaf (make-libm (fmaf float float float float)))

(when c_erfcf
  (platform-register-implementation! c-platform
                                     (make-operator-impl (erfc.f32 [x : binary32])
                                                         binary32
                                                         #:spec (- 1 (erf x))
                                                         #:fpcore (! :precision binary32 (erfc x))
                                                         #:fl c_erfcf
                                                         #:cost 0.907758)))

(when c_expm1f
  (platform-register-implementation! c-platform
                                     (make-operator-impl (expm1.f32 [x : binary32])
                                                         binary32
                                                         #:spec (- (exp x) 1)
                                                         #:fpcore (! :precision binary32 (expm1 x))
                                                         #:fl c_expm1f
                                                         #:cost 0.906484)))

(when c_log1pf
  (platform-register-implementation! c-platform
                                     (make-operator-impl (log1p.f32 [x : binary32])
                                                         binary32
                                                         #:spec (log (+ 1 x))
                                                         #:fpcore (! :precision binary32 (log1p x))
                                                         #:fl c_log1pf
                                                         #:cost 1.302969)))

(when c_hypotf
  (platform-register-implementation! c-platform
                                     (make-operator-impl (hypot.f32 [x : binary32] [y : binary32])
                                                         binary32
                                                         #:spec (sqrt (+ (* x x) (* y y)))
                                                         #:fpcore (! :precision binary32 (hypot x y))
                                                         #:fl c_hypotf
                                                         #:cost 1.681608)))

(when c_fmaf
  (platform-register-implementation!
   c-platform
   (make-operator-impl (fma.f32 [x : binary32] [y : binary32] [z : binary32])
                       binary32
                       #:spec (+ (* x y) z)
                       #:fpcore (! :precision binary32 (fma x y z))
                       #:fl c_fmaf
                       #:cost 0.38934)))

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
                       #:cost 64bit-move-cost))

(platform-register-representation! c-platform binary64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation! c-platform
                                   (make-operator-impl (PI.f64)
                                                       binary64
                                                       #:spec (PI)
                                                       #:fl (const pi)
                                                       #:fpcore (! :precision binary64 (PI))
                                                       #:cost 64bit-move-cost))
(platform-register-implementation! c-platform
                                   (make-operator-impl (E.f64)
                                                       binary64
                                                       #:spec (E)
                                                       #:fl (const (exp 1.0))
                                                       #:fpcore (! :precision binary64 (E))
                                                       #:cost 64bit-move-cost))
(platform-register-implementation! c-platform
                                   (make-operator-impl (INFINITY.f64)
                                                       binary64
                                                       #:spec (INFINITY)
                                                       #:fl (const +inf.0)
                                                       #:fpcore (! :precision binary64 (INFINITY))
                                                       #:cost 64bit-move-cost))
(platform-register-implementation! c-platform
                                   (make-operator-impl (NAN.f64)
                                                       binary64
                                                       #:spec (NAN)
                                                       #:fl (const +nan.0)
                                                       #:fpcore (! :precision binary64 (NAN))
                                                       #:cost 64bit-move-cost))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation! c-platform
                                   (make-operator-impl (neg.f64 [x : binary64])
                                                       binary64
                                                       #:spec (neg x)
                                                       #:fpcore (! :precision binary64 (- x))
                                                       #:fl -
                                                       #:cost 0.121142))

(platform-register-implementation! c-platform
                                   (make-operator-impl (+.f64 [x : binary64] [y : binary64])
                                                       binary64
                                                       #:spec (+ x y)
                                                       #:fpcore (! :precision binary64 (+ x y))
                                                       #:fl +
                                                       #:cost 0.217419))

(platform-register-implementation! c-platform
                                   (make-operator-impl (-.f64 [x : binary64] [y : binary64])
                                                       binary64
                                                       #:spec (- x y)
                                                       #:fpcore (! :precision binary64 (- x y))
                                                       #:fl -
                                                       #:cost 0.202657))

(platform-register-implementation! c-platform
                                   (make-operator-impl (*.f64 [x : binary64] [y : binary64])
                                                       binary64
                                                       #:spec (* x y)
                                                       #:fpcore (! :precision binary64 (* x y))
                                                       #:fl *
                                                       #:cost 0.245123))

(platform-register-implementation! c-platform
                                   (make-operator-impl (/.f64 [x : binary64] [y : binary64])
                                                       binary64
                                                       #:spec (/ x y)
                                                       #:fpcore (! :precision binary64 (/ x y))
                                                       #:fl /
                                                       #:cost 0.296246))

(platform-register-implementation! c-platform
                                   (make-operator-impl (==.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (== x y)
                                                       #:fpcore (== x y)
                                                       #:fl =
                                                       #:cost 64bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (!=.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (!= x y)
                                                       #:fpcore (!= x y)
                                                       #:fl (negate =)
                                                       #:cost 64bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (<.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (< x y)
                                                       #:fpcore (< x y)
                                                       #:fl <
                                                       #:cost 64bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (>.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (> x y)
                                                       #:fpcore (> x y)
                                                       #:fl >
                                                       #:cost 64bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (<=.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (<= x y)
                                                       #:fpcore (<= x y)
                                                       #:fl <=
                                                       #:cost 64bit-move-cost))

(platform-register-implementation! c-platform
                                   (make-operator-impl (>=.f64 [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (>= x y)
                                                       #:fpcore (>= x y)
                                                       #:fl >=
                                                       #:cost 64bit-move-cost))

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
                             ([fabs 0.149422] [sin 3.979444]
                                              [cos 3.998834]
                                              [tan 4.3931]
                                              [sinh 1.53454]
                                              [cosh 1.332794]
                                              [acos 0.528324]
                                              [acosh 0.929629]
                                              [asin 0.592536]
                                              [asinh 1.120752]
                                              [atan 1.149123]
                                              [atanh 0.477357]
                                              [cbrt 1.89928]
                                              [ceil 0.269904]
                                              [erf 1.064252]
                                              [exp 1.270031]
                                              [exp2 1.070903]
                                              [floor 0.270019]
                                              [lgamma 2.109717]
                                              [log 0.719641]
                                              [log10 1.099494]
                                              [log2 0.805026]
                                              [logb 0.332842]
                                              [rint 0.310159]
                                              [round 0.776288]
                                              [sqrt 0.222512] ; not a typo
                                              [tanh 0.985656]
                                              [tgamma 2.40065]
                                              [trunc 0.27144])]
                            [(binary64 binary64 binary64)
                             ([pow 1.860082] [atan2 1.81383]
                                             [copysign 0.238628]
                                             [fdim 0.725493]
                                             [fmax 0.24807]
                                             [fmin 0.266808]
                                             [fmod 1.532448]
                                             [remainder 0.949438])]))

(for ([libm-impl.f64 (in-list libm-impls.f64)])
  (when libm-impl.f64
    (platform-register-implementation! c-platform libm-impl.f64)))

(define c_erfc (make-libm (erfc double double)))
(define c_expm1 (make-libm (expm1 double double)))
(define c_log1p (make-libm (log1p double double)))
(define c_hypot (make-libm (hypot double double double)))
(define c_fma (make-libm (fma double double double double)))

(when c_erfc
  (platform-register-implementation! c-platform
                                     (make-operator-impl (erfc.f64 [x : binary64])
                                                         binary64
                                                         #:spec (- 1 (erf x))
                                                         #:fpcore (! :precision binary64 (erfc x))
                                                         #:fl c_erfc
                                                         #:cost 0.858862)))

(when c_expm1
  (platform-register-implementation! c-platform
                                     (make-operator-impl (expm1.f64 [x : binary64])
                                                         binary64
                                                         #:spec (- (exp x) 1)
                                                         #:fpcore (! :precision binary64 (expm1 x))
                                                         #:fl c_expm1
                                                         #:cost 0.848349)))

(when c_log1p
  (platform-register-implementation! c-platform
                                     (make-operator-impl (log1p.f64 [x : binary64])
                                                         binary64
                                                         #:spec (log (+ 1 x))
                                                         #:fpcore (! :precision binary64 (log1p x))
                                                         #:fl c_log1p
                                                         #:cost 1.241683)))

(when c_hypot
  (platform-register-implementation! c-platform
                                     (make-operator-impl (hypot.f64 [x : binary64] [y : binary64])
                                                         binary64
                                                         #:spec (sqrt (+ (* x x) (* y y)))
                                                         #:fpcore (! :precision binary64 (hypot x y))
                                                         #:fl c_hypot
                                                         #:cost 1.498331)))

(when c_fma
  (platform-register-implementation!
   c-platform
   (make-operator-impl (fma.f64 [x : binary64] [y : binary64] [z : binary64])
                       binary64
                       #:spec (+ (* x y) z)
                       #:fpcore (! :precision binary64 (fma x y z))
                       #:fl c_fma
                       #:cost 0.371747)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; additional converters ;;;;;;;;;;;;;;;;;

#;(platform-register-implementation! c-platform
                                     (make-operator-impl (binary64->binary32 [x : binary64])
                                                         binary32
                                                         #:spec x
                                                         #:fpcore (! :precision binary32 (cast x))
                                                         #:fl flsingle
                                                         #:cost 32bit-move-cost))

#;(platform-register-implementation! c-platform
                                     (make-operator-impl (binary32->binary64 [x : binary32])
                                                         binary64
                                                         #:spec x
                                                         #:fpcore (! :precision binary64 (cast x))
                                                         #:fl identity
                                                         #:cost 64bit-move-cost))

(register-platform! c-platform)

;; Do not run this file during testing
(module test racket/base
  )
