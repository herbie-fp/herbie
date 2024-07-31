#lang racket

;; Builtin double-precision plugin (:precision binary64)

(require math/flonum
         math/bigfloat)

(require "runtime/utils.rkt"
         "runtime/libm.rkt")

;; Do not run this file with `raco test`
(module test racket/base
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (binary64 real flonum?)
                       bigfloat->flonum
                       bf
                       (shift 63 ordinal->flonum)
                       (unshift 63 flonum->ordinal)
                       64
                       (conjoin number? nan?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants binary64
                  [PI PI.f64 pi]
                  [E E.f64 (exp 1.0)]
                  [INFINITY INFINITY.f64 +inf.0]
                  [NAN NAN.f64 +nan.0])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-libm-impl/binary64 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype [key value] ...)
     (with-syntax ([impl (string->symbol (format "~a.f64" (syntax->datum #'op)))])
       #'(define-libm-impl op (op impl itype ...) otype [key value] ...))]))

(define-syntax-rule (define-libm-impls/binary64* (itype ... otype) name ...)
  (begin
    (define-libm-impl/binary64 name (itype ...) otype) ...))

(define-syntax-rule (define-libm-impls/binary64 [(itype ... otype) (name ...)] ...)
  (begin
    (define-libm-impls/binary64* (itype ... otype) name ...) ...))

(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])

(define-libm-impls/binary64 [(binary64 binary64)
                             (acos acosh
                                   asin
                                   asinh
                                   atan
                                   atanh
                                   cbrt
                                   ceil
                                   cos
                                   cosh
                                   erf
                                   exp
                                   exp2
                                   fabs
                                   floor
                                   lgamma
                                   log
                                   log10
                                   log2
                                   logb
                                   rint
                                   round
                                   sin
                                   sinh
                                   sqrt
                                   tan
                                   tanh
                                   tgamma
                                   trunc)]
                            [(binary64 binary64 binary64)
                             (atan2 copysign fdim fmax fmin fmod pow remainder)])

(define-libm c_expm1 (expm1 double double))
(define-libm c_erfc (erfc double double))
(define-libm c_log1p (log1p double double))
(define-libm c_hypot (hypot double double double))
(define-libm c_fma (fma double double double double))

(when c_expm1
  (define-operator-impl (expm1 expm1.f64 binary64)
                        binary64
                        [spec (lambda (x) (- (exp x) 1))]
                        [fpcore (! :precision binary64 (expm1 x))]
                        [fl c_expm1]))

(when c_erfc
  (define-operator-impl (erfc erfc.f64 binary64)
                        binary64
                        [spec (lambda (x) (- 1 (erf x)))]
                        [fpcore (! :precision binary64 (erfc x))]
                        [fl c_erfc]))

(when c_log1p
  (define-operator-impl (log1p log1p.f64 binary64)
                        binary64
                        [spec (lambda (x) (log (+ 1 x)))]
                        [fpcore (! :precision binary64 (log1p x))]
                        [fl c_log1p]))

(when c_hypot
  (define-operator-impl (hypot hypot.f64 binary64 binary64)
                        binary64
                        [spec (lambda (x y) (sqrt (+ (* x x) (* y y))))]
                        [fpcore (! :precision binary64 (hypot x))]
                        [fl c_hypot]))

(when c_fma
  (define-operator-impl (fma fma.f64 binary64 binary64 binary64)
                        binary64
                        [spec (lambda (x y z) (+ (* x y) z))]
                        [fpcore (! :precision binary64 (fma x y z))]
                        [fl c_fma]))

(define-comparator-impls binary64
                         [== ==.f64 =]
                         [!= !=.f64 (negate =)]
                         [< <.f64 <]
                         [> >.f64 >]
                         [<= <=.f64 <=]
                         [>= >=.f64 >=])
