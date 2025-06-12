#lang racket

;;; The default platform:
;;; C/C++ on Linux with a full libm

(require math/bigfloat
         math/flonum)

(require "runtime/float32.rkt" ; float representation helper functions
         "runtime/libm.rkt") ; libm runtime support functions

(require "../utils/float.rkt" ; for shift/unshift
         "../syntax/platform.rkt"
         (submod "../syntax/platform.rkt" internals)
         (submod "../syntax/syntax.rkt"
                 internals) ; for make-operator-impl, define-constants, define-comparator-impls
         (submod "../syntax/types.rkt" internals)) ; for define-representation

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
                       #:cost 1))

(platform-register-representation! herbie20-platform binary32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (PI.f32)
                                                       binary32
                                                       #:spec (PI)
                                                       #:fl (const (flsingle pi))
                                                       #:fpcore (! :precision binary32 (PI))
                                                       #:cost 1))
(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (E.f32)
                                                       binary32
                                                       #:spec (E)
                                                       #:fl (const (flsingle (exp 1.0)))
                                                       #:fpcore (! :precision binary32 (E))
                                                       #:cost 1))
(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (INFINITY.f32)
                                                       binary32
                                                       #:spec (INFINITY)
                                                       #:fl (const +inf.0)
                                                       #:fpcore (! :precision binary32 (INFINITY))
                                                       #:cost 1))
(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (NAN.f32)
                                                       binary32
                                                       #:spec (NAN)
                                                       #:fl (const +nan.0)
                                                       #:fpcore (! :precision binary32 (NAN))
                                                       #:cost 1))

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
                                                       #:cost 1))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (+.f32 [x : binary32] [y : binary32])
                                                       binary32
                                                       #:spec (+ x y)
                                                       #:fpcore (! :precision binary32 (+ x y))
                                                       #:fl fl32+
                                                       #:cost 1))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (-.f32 [x : binary32] [y : binary32])
                                                       binary32
                                                       #:spec (- x y)
                                                       #:fpcore (! :precision binary32 (- x y))
                                                       #:fl fl32-
                                                       #:cost 1))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (*.f32 [x : binary32] [y : binary32])
                                                       binary32
                                                       #:spec (* x y)
                                                       #:fpcore (! :precision binary32 (* x y))
                                                       #:fl fl32*
                                                       #:cost 1))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (/.f32 [x : binary32] [y : binary32])
                                                       binary32
                                                       #:spec (/ x y)
                                                       #:fpcore (! :precision binary32 (/ x y))
                                                       #:fl fl32/
                                                       #:cost 1))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (==.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (== x y)
                                                       #:fpcore (== x y)
                                                       #:fl =
                                                       #:cost 1))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (!=.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (!= x y)
                                                       #:fpcore (!= x y)
                                                       #:fl (negate =)
                                                       #:cost 1))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (<.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (< x y)
                                                       #:fpcore (< x y)
                                                       #:fl <
                                                       #:cost 1))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (>.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (> x y)
                                                       #:fpcore (> x y)
                                                       #:fl >
                                                       #:cost 1))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (<=.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (<= x y)
                                                       #:fpcore (<= x y)
                                                       #:fl <=
                                                       #:cost 1))

(platform-register-implementation! herbie20-platform
                                   (make-operator-impl (>=.f32 [x : binary32] [y : binary32])
                                                       bool
                                                       #:spec (>= x y)
                                                       #:fpcore (>= x y)
                                                       #:fl >=
                                                       #:cost 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; libm operators ;;;;;;;;;;;;;;;;;;;;;;;;

; (define-libm fabsf.libm (fabsf float float))
; (platform-register-implementation! herbie20-platform
;                                    (make-operator-impl (fabs.f32 [x : binary32])
;                                                        binary32
;                                                        #:spec (fabs x)
;                                                        #:fpcore (! :precision binary32 (fabs x))
;                                                        #:fl fabsf.libm)
;                                                        #:cost ...))
; ...

(define-syntax (define-libm-impl/binary32 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype attrib ...)
     (with-syntax ([impl (string->symbol (format "~a.f32" (syntax->datum #'op)))]
                   [cname (string->symbol (format "~af" (syntax->datum #'op)))])
       #'(define-libm-impl cname (op impl itype ...) otype attrib ...))]))

(define-syntax-rule (define-libm-impls/binary32* (itype ... otype) name ...)
  (begin
    (define-libm-impl/binary32 name (itype ...) otype) ...))

(define-syntax-rule (define-libm-impls/binary32 [(itype ... otype) ([name cost] ...)] ...)
  (begin
    (define-libm-impls/binary32* (itype ... otype) name ...) ...))

(define-libm-impls/binary32
 [(binary32 binary32)
  ([fabs 1] [sin 1]
            [cos 1]
            [tan 1]
            [sinh 1]
            [cosh 1]
            [acos 1]
            [acosh 1]
            [asin 1]
            [asinh 1]
            [atan 1]
            [atanh 1]
            [cbrt 1]
            [ceil 1]
            [erf 1]
            [exp 1]
            [exp2 1]
            [floor 1]
            [lgamma 1]
            [log 1]
            [log10 1]
            [log2 1]
            [logb 1]
            [rint 1]
            [round 1]
            [sqrt 1]
            [tanh 1]
            [tgamma 1]
            [trunc 1])]
 [(binary32 binary32 binary32)
  ([pow 1] [atan2 1] [copysign 1] [fdim 1] [fmax 1] [fmin 1] [fmod 1] [pow 1] [remainder 1])])

#;(define-libm fabsf.libm (fabsf float float))
#;(define fabs.f32
    (make-operator-impl (fabs.f32 [x : binary32]) binary32 #:spec (fabs x) #:fl fabsf.libm #:cost 1))
#;(platform-register-implementation! herbie20-platform fabs.f32)

(define-libm c_erfcf (erfcf float float))
(define-libm c_expm1f (expm1f float float))
(define-libm c_log1pf (log1pf float float))
(define-libm c_hypotf (hypotf float float float))
(define-libm c_fmaf (fmaf float float float float))

(when c_erfcf
  (make-operator-impl (erfc.f32 [x : binary32])
                      binary32
                      #:spec (- 1 (erf x))
                      #:fpcore (! :precision binary32 (erfc x))
                      #:fl c_erfcf))

(when c_expm1f
  (make-operator-impl (expm1.f32 [x : binary32])
                      binary32
                      #:spec (- (exp x) 1)
                      #:fpcore (! :precision binary32 (expm1 x))
                      #:fl c_expm1f))

(when c_log1pf
  (make-operator-impl (log1p.f32 [x : binary32])
                      binary32
                      #:spec (log (+ 1 x))
                      #:fpcore (! :precision binary32 (log1p x))
                      #:fl c_log1pf))

(when c_hypotf
  (make-operator-impl (hypot.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (sqrt (+ (* x x) (* y y)))
                      #:fpcore (! :precision binary32 (hypot x y))
                      #:fl c_hypotf))

(when c_fmaf
  (make-operator-impl (fma.f32 [x : binary32] [y : binary32] [z : binary32])
                      binary32
                      #:spec (+ (* x y) z)
                      #:fpcore (! :precision binary32 (fma x y z))
                      #:fl c_fmaf))

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
                       #:cost 1))

(platform-register-representation! herbie20-platform binary64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants binary64
                  [PI PI.f64 pi]
                  [E E.f64 (exp 1.0)]
                  [INFINITY INFINITY.f64 +inf.0]
                  [NAN NAN.f64 +nan.0])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-operator-impl (neg.f64 [x : binary64])
                    binary64
                    #:spec (neg x)
                    #:fpcore (! :precision binary64 (- x))
                    #:fl -)
(make-operator-impl (+.f64 [x : binary64] [y : binary64])
                    binary64
                    #:spec (+ x y)
                    #:fpcore (! :precision binary64 (+ x y))
                    #:fl +)
(make-operator-impl (-.f64 [x : binary64] [y : binary64])
                    binary64
                    #:spec (- x y)
                    #:fpcore (! :precision binary64 (- x y))
                    #:fl -)
(make-operator-impl (*.f64 [x : binary64] [y : binary64])
                    binary64
                    #:spec (* x y)
                    #:fpcore (! :precision binary64 (* x y))
                    #:fl *)
(make-operator-impl (/.f64 [x : binary64] [y : binary64])
                    binary64
                    #:spec (/ x y)
                    #:fpcore (! :precision binary64 (/ x y))
                    #:fl /)

; Libm operators

(define-syntax (define-libm-impl/binary64 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype attrib ...)
     (with-syntax ([impl (string->symbol (format "~a.f64" (syntax->datum #'op)))])
       #'(define-libm-impl op (op impl itype ...) otype attrib ...))]))

(define-syntax-rule (define-libm-impls/binary64* (itype ... otype) name ...)
  (begin
    (define-libm-impl/binary64 name (itype ...) otype) ...))

(define-syntax-rule (define-libm-impls/binary64 [(itype ... otype) (name ...)] ...)
  (begin
    (define-libm-impls/binary64* (itype ... otype) name ...) ...))

#;(define-libm-impls/binary64 [(binary64 binary64)
                               (fabs exp
                                     sin
                                     cos
                                     tan
                                     sinh
                                     cosh
                                     acos
                                     acosh
                                     asin
                                     asinh
                                     atan
                                     atanh
                                     cbrt
                                     ceil
                                     cosh
                                     erf
                                     exp
                                     exp2
                                     floor
                                     lgamma
                                     log
                                     log10
                                     log2
                                     logb
                                     rint
                                     round
                                     sinh
                                     sqrt
                                     tanh
                                     tgamma
                                     trunc)]
                              [(binary64 binary64 binary64)
                               (pow atan2 copysign fdim fmax fmin fmod pow remainder)])

(define-libm c_erfc (erfc double double))
(define-libm c_expm1 (expm1 double double))
(define-libm c_log1p (log1p double double))
(define-libm c_hypot (hypot double double double))
(define-libm c_fma (fma double double double double))

(when c_erfc
  (make-operator-impl (erfc.f64 [x : binary64])
                      binary64
                      #:spec (- 1 (erf x))
                      #:fpcore (! :precision binary64 (erfc x))
                      #:fl c_erfc))

(when c_expm1
  (make-operator-impl (expm1.f64 [x : binary64])
                      binary64
                      #:spec (- (exp x) 1)
                      #:fpcore (! :precision binary64 (expm1 x))
                      #:fl c_expm1))

(when c_log1p
  (make-operator-impl (log1p.f64 [x : binary64])
                      binary64
                      #:spec (log (+ 1 x))
                      #:fpcore (! :precision binary64 (log1p x))
                      #:fl c_log1p))

(when c_hypot
  (make-operator-impl (hypot.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (sqrt (+ (* x x) (* y y)))
                      #:fpcore (! :precision binary64 (hypot x y))
                      #:fl c_hypot))

(when c_fma
  (make-operator-impl (fma.f64 [x : binary64] [y : binary64] [z : binary64])
                      binary64
                      #:spec (+ (* x y) z)
                      #:fpcore (! :precision binary64 (fma x y z))
                      #:fl c_fma))

#;(define-comparator-impls binary64
                           [== ==.f64 =]
                           [!= !=.f64 (negate =)]
                           [< <.f64 <]
                           [> >.f64 >]
                           [<= <=.f64 <=]
                           [>= >=.f64 >=])

(define-platform boolean-platform
                 #:literal [bool 1]
                 #:default-cost 1
                 #:if-cost 1
                 TRUE
                 FALSE
                 not
                 and
                 or)

;; machine floating-point operations
(define-platform machine-platform
                 #:literal [binary64 64]
                 #:literal [binary32 32]
                 [PI.f64 64]
                 [PI.f32 32]
                 [E.f64 64]
                 [E.f32 32]
                 [INFINITY.f64 64]
                 [INFINITY.f32 32]
                 [NAN.f64 64]
                 [NAN.f32 32]
                 [neg.f64 128]
                 [neg.f32 64]
                 [+.f64 128]
                 [+.f32 64]
                 [-.f64 128]
                 [-.f32 64]
                 [*.f64 256]
                 [*.f32 128]
                 [/.f64 640]
                 [/.f32 320]
                 [==.f64 256]
                 [==.f32 128]
                 [!=.f64 256]
                 [!=.f32 128]
                 [>.f64 256]
                 [>.f32 128]
                 [<.f64 256]
                 [<.f32 128]
                 [>=.f64 256]
                 [>=.f32 128]
                 [<=.f64 256]
                 [<=.f32 128])

;; libm operations
(define-platform libm64-platform
                 #:literal [binary64 64]
                 #:literal [binary32 32]
                 #:default-cost 6400
                 #:optional acos.f64
                 [fabs.f64 128]
                 acosh.f64
                 asin.f64
                 asinh.f64
                 atan.f64
                 atan2.f64
                 atanh.f64
                 cbrt.f64
                 ceil.f64
                 copysign.f64
                 cos.f64
                 cosh.f64
                 erf.f64
                 exp.f64
                 exp2.f64
                 fdim.f64
                 floor.f64
                 fmax.f64
                 fmin.f64
                 fmod.f64
                 lgamma.f64
                 log.f64
                 log10.f64
                 log2.f64
                 logb.f64
                 pow.f64
                 remainder.f64
                 rint.f64
                 round.f64
                 sin.f64
                 sinh.f64
                 [sqrt.f64 640]
                 tan.f64
                 tanh.f64
                 tgamma.f64
                 trunc.f64)

(define-platform libm32-platform
                 #:literal [binary32 32]
                 #:default-cost 3200
                 #:optional acos.f32
                 [fabs.f32 64]
                 acosh.f32
                 asin.f32
                 asinh.f32
                 atan.f32
                 atan2.f32
                 atanh.f32
                 cbrt.f32
                 ceil.f32
                 copysign.f32
                 cos.f32
                 cosh.f32
                 erf.f32
                 exp.f32
                 exp2.f32
                 fdim.f32
                 floor.f32
                 fmax.f32
                 fmin.f32
                 fmod.f32
                 lgamma.f32
                 log.f32
                 log10.f32
                 log2.f32
                 logb.f32
                 pow.f32
                 remainder.f32
                 rint.f32
                 round.f32
                 sin.f32
                 sinh.f32
                 [sqrt.f32 320]
                 tan.f32
                 tanh.f32
                 tgamma.f32
                 trunc.f32)

(define-platform accelerator-platform
                 #:literal [binary64 64]
                 #:literal [binary32 32]
                 #:default-cost 3200
                 #:optional [erfc.f64 6400]
                 [expm1.f64 6400]
                 [log1p.f64 6400]
                 [hypot.f64 6400]
                 [fma.f64 256]
                 erfc.f32
                 expm1.f32
                 log1p.f32
                 hypot.f32
                 [fma.f32 128])

(define herbie-platform
  (platform-union boolean-platform
                  machine-platform
                  libm64-platform
                  libm32-platform
                  accelerator-platform))

; Register all three

(register-platform! 'herbie20 herbie-platform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; additional converters ;;;;;;;;;;;;;;;;;

(make-operator-impl (binary64->binary32 [x : binary64])
                    binary32
                    #:spec x
                    #:fpcore (! :precision binary32 (cast x))
                    #:fl flsingle)

(make-operator-impl (binary32->binary64 [x : binary32])
                    binary64
                    #:spec x
                    #:fpcore (! :precision binary64 (cast x))
                    #:fl identity)

;; Do not run this file during testing
(module test racket/base
  )
