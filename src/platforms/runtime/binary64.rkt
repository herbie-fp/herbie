#lang racket

;; Double-precision platform (math only)

(require math/flonum math/bigfloat ffi/unsafe)
(require "../../plugin.rkt" "bool.rkt"
         (only-in "libm.rkt" [define-binary64-impls/libm define-libm-operators]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(define-representation (binary64 real flonum?)
  bigfloat->flonum
  bf
  (shift 63 ordinal->flonum)
  (unshift 63 flonum->ordinal)
  64
  (conjoin number? nan?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operator-impl (PI PI.f64) binary64
  [fl (const pi)])

(define-operator-impl (E E.f64) binary64
  [fl (const (exp 1.0))])

(define-operator-impl (INFINITY INFINITY.f64) binary64
  [fl (const +inf.0)])

(define-operator-impl (NAN NAN.f64) binary64
  [fl (const +nan.0)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])

(define-libm-operators
  [acos
   acosh
   asin
   asinh
   atan
   atanh
   cbrt
   ceil
   cos
   cosh
   erf
   erfc
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
   trunc]
  [copysign
   fdim
   fmax
   fmin
   fmod
   pow
   remainder])

(define-syntax-rule (define-comparator-impls [name impl-name impl-fn] ...)
  (begin
    (define-operator-impl (name impl-name binary64 binary64) bool
      [fl impl-fn])
    ...))

(define-comparator-impls
  [== ==.f64 =]
  [!= !=.f64 (negate =)]
  [< <.f64 <]
  [> >.f64 >]
  [<= <=.f64 <=]
  [>= >=.f64 >=])
