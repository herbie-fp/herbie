#lang racket

;; A 32-bit float is emulated in Racket as a 64-bit float,
(require math/bigfloat
         math/flonum)
(require "runtime/float32.rkt"
         "runtime/utils.rkt"
         "runtime/libm.rkt")

;; Do not run this file with `raco test`
(module test racket/base
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-representation (binary32 real flonum?)
                       bigfloat->float32
                       bf
                       (shift 31 ordinal->float32)
                       (unshift 31 float32->ordinal)
                       32
                       (conjoin number? nan?))

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

(define-operator-impl (neg.f32 [x : binary32])
                      binary32
                      #:spec (neg x)
                      #:fpcore (! :precision binary32 (- x))
                      #:fl fl32-)

(define-operator-impl (+.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (+ x y)
                      #:fpcore (! :precision binary32 (+ x y))
                      #:fl fl32+
                      #:commutes)

(define-operator-impl (-.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (- x y)
                      #:fpcore (! :precision binary32 (- x y))
                      #:fl fl32-)

(define-operator-impl (*.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (* x y)
                      #:fpcore (! :precision binary32 (* x y))
                      #:fl fl32*
                      #:commutes)

(define-operator-impl (/.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (/ x y)
                      #:fpcore (! :precision binary32 (/ x y))
                      #:fl fl32/)

(define-libm-impl/binary32 fabs (binary32) binary32)

(define-libm-impl/binary32 exp (binary32) binary32)

(define-libm-impl/binary32 pow (binary32 binary32) binary32)

(define-libm-impl/binary32 sin (binary32) binary32)

(define-libm-impl/binary32 cos (binary32) binary32)

(define-libm-impl/binary32 tan (binary32) binary32)

(define-libm-impl/binary32 sinh (binary32) binary32)

(define-libm-impl/binary32 cosh (binary32) binary32)

(define-comparator-impls binary32
                         [== ==.f32 =]
                         [!= !=.f32 (negate =)]
                         [< <.f32 <]
                         [> >.f32 >]
                         [<= <=.f32 <=]
                         [>= >=.f32 >=])

(define-libm-impls/binary32 [(binary32 binary32)
                             (acos acosh
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
                             (atan2 copysign fdim fmax fmin fmod pow remainder)])

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
