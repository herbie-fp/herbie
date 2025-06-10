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
                 internals) ; for define-operator-impl, define-constants, define-comparator-impls
         (submod "../syntax/types.rkt" internals)) ; for define-representation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (bool bool boolean?)
                       identity
                       identity
                       (λ (x) (= x 0))
                       (λ (x) (if x 1 0))
                       1
                       (const #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't use define-constants because don't want to require :precision bool annotation
(define-operator-impl (TRUE) bool #:spec (TRUE) #:fl (const true) #:fpcore (! TRUE))
(define-operator-impl (FALSE) bool #:spec (FALSE) #:fl (const false) #:fpcore (! FALSE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(define-operator-impl (not [x : bool]) bool #:spec (not x) #:fl not)

(define-operator-impl (and [x : bool] [y : bool]) bool #:spec (and x y) #:fl and-fn)

(define-operator-impl (or [x : bool] [y : bool]) bool #:spec (or x y) #:fl or-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (binary32 real flonum?)
                       bigfloat->float32
                       bf
                       (shift 31 ordinal->float32)
                       (unshift 31 float32->ordinal)
                       32
                       nan?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants binary32
                  [PI PI.f32 (flsingle pi)]
                  [E E.f32 (flsingle (exp 1.0))]
                  [INFINITY INFINITY.f32 +inf.0]
                  [NAN NAN.f32 +nan.0])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-libm-impl/binary32 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype attrib ...)
     (with-syntax ([impl (string->symbol (format "~a.f32" (syntax->datum #'op)))]
                   [cname (string->symbol (format "~af" (syntax->datum #'op)))])
       #'(define-libm-impl cname (op impl itype ...) otype attrib ...))]))

(define-syntax-rule (define-libm-impls/binary32* (itype ... otype) name ...)
  (begin
    (define-libm-impl/binary32 name (itype ...) otype) ...))

(define-syntax-rule (define-libm-impls/binary32 [(itype ... otype) (name ...)] ...)
  (begin
    (define-libm-impls/binary32* (itype ... otype) name ...) ...))

;; Wrapping arithmetic operations with `flsingle` introduces a
;; possible double-rounding problem but, perhaps surprisingly, this
;; double-rounding problem never actually causes error; see:
;;
;;   https://hal.science/hal-01091186/document

(define fl32+ (compose flsingle +))
(define fl32- (compose flsingle -))
(define fl32* (compose flsingle *))
(define fl32/ (compose flsingle /))

(define-operator-impl (neg.f32 [x : binary32])
                      binary32
                      #:spec (neg x)
                      #:fpcore (! :precision binary32 (- x))
                      #:fl fl32-)

(define-operator-impl (+.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (+ x y)
                      #:fpcore (! :precision binary32 (+ x y))
                      #:fl fl32+)

(define-operator-impl (-.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (- x y)
                      #:fpcore (! :precision binary32 (- x y))
                      #:fl fl32-)

(define-operator-impl (*.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (* x y)
                      #:fpcore (! :precision binary32 (* x y))
                      #:fl fl32*)

(define-operator-impl (/.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (/ x y)
                      #:fpcore (! :precision binary32 (/ x y))
                      #:fl fl32/)

(define-comparator-impls binary32
                         [== ==.f32 =]
                         [!= !=.f32 (negate =)]
                         [< <.f32 <]
                         [> >.f32 >]
                         [<= <=.f32 <=]
                         [>= >=.f32 >=])

(define-libm-impls/binary32 [(binary32 binary32)
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
                                   sqrt
                                   tanh
                                   tgamma
                                   trunc)]
                            [(binary32 binary32 binary32)
                             (pow atan2 copysign fdim fmax fmin fmod pow remainder)])

(define-libm c_erfcf (erfcf float float))
(define-libm c_expm1f (expm1f float float))
(define-libm c_log1pf (log1pf float float))
(define-libm c_hypotf (hypotf float float float))
(define-libm c_fmaf (fmaf float float float float))

(when c_erfcf
  (define-operator-impl (erfc.f32 [x : binary32])
                        binary32
                        #:spec (- 1 (erf x))
                        #:fpcore (! :precision binary32 (erfc x))
                        #:fl c_erfcf))

(when c_expm1f
  (define-operator-impl (expm1.f32 [x : binary32])
                        binary32
                        #:spec (- (exp x) 1)
                        #:fpcore (! :precision binary32 (expm1 x))
                        #:fl c_expm1f))

(when c_log1pf
  (define-operator-impl (log1p.f32 [x : binary32])
                        binary32
                        #:spec (log (+ 1 x))
                        #:fpcore (! :precision binary32 (log1p x))
                        #:fl c_log1pf))

(when c_hypotf
  (define-operator-impl (hypot.f32 [x : binary32] [y : binary32])
                        binary32
                        #:spec (sqrt (+ (* x x) (* y y)))
                        #:fpcore (! :precision binary32 (hypot x y))
                        #:fl c_hypotf))

(when c_fmaf
  (define-operator-impl (fma.f32 [x : binary32] [y : binary32] [z : binary32])
                        binary32
                        #:spec (+ (* x y) z)
                        #:fpcore (! :precision binary32 (fma x y z))
                        #:fl c_fmaf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (binary64 real flonum?)
                       bigfloat->flonum
                       bf
                       (shift 63 ordinal->flonum)
                       (unshift 63 flonum->ordinal)
                       64
                       nan?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants binary64
                  [PI PI.f64 pi]
                  [E E.f64 (exp 1.0)]
                  [INFINITY INFINITY.f64 +inf.0]
                  [NAN NAN.f64 +nan.0])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operator-impl (neg.f64 [x : binary64])
                      binary64
                      #:spec (neg x)
                      #:fpcore (! :precision binary64 (- x))
                      #:fl -)
(define-operator-impl (+.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (+ x y)
                      #:fpcore (! :precision binary64 (+ x y))
                      #:fl +)
(define-operator-impl (-.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (- x y)
                      #:fpcore (! :precision binary64 (- x y))
                      #:fl -)
(define-operator-impl (*.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (* x y)
                      #:fpcore (! :precision binary64 (* x y))
                      #:fl *)
(define-operator-impl (/.f64 [x : binary64] [y : binary64])
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

(define-libm-impls/binary64 [(binary64 binary64)
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
  (define-operator-impl (erfc.f64 [x : binary64])
                        binary64
                        #:spec (- 1 (erf x))
                        #:fpcore (! :precision binary64 (erfc x))
                        #:fl c_erfc))

(when c_expm1
  (define-operator-impl (expm1.f64 [x : binary64])
                        binary64
                        #:spec (- (exp x) 1)
                        #:fpcore (! :precision binary64 (expm1 x))
                        #:fl c_expm1))

(when c_log1p
  (define-operator-impl (log1p.f64 [x : binary64])
                        binary64
                        #:spec (log (+ 1 x))
                        #:fpcore (! :precision binary64 (log1p x))
                        #:fl c_log1p))

(when c_hypot
  (define-operator-impl (hypot.f64 [x : binary64] [y : binary64])
                        binary64
                        #:spec (sqrt (+ (* x x) (* y y)))
                        #:fpcore (! :precision binary64 (hypot x y))
                        #:fl c_hypot))

(when c_fma
  (define-operator-impl (fma.f64 [x : binary64] [y : binary64] [z : binary64])
                        binary64
                        #:spec (+ (* x y) z)
                        #:fpcore (! :precision binary64 (fma x y z))
                        #:fl c_fma))

(define-comparator-impls binary64
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

(define-operator-impl (binary64->binary32 [x : binary64])
                      binary32
                      #:spec x
                      #:fpcore (! :precision binary32 (cast x))
                      #:fl flsingle)

(define-operator-impl (binary32->binary64 [x : binary32])
                      binary64
                      #:spec x
                      #:fpcore (! :precision binary64 (cast x))
                      #:fl identity)

;; Do not run this file during testing
(module test racket/base
  )
