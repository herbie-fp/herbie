#lang racket

;; Double-precision common math operators

(require math/bigfloat math/flonum)
(require "runtime/utils.rkt"
         (only-in "runtime/libm.rkt"
           [define-binary64-impls/libm define-libm-operators]))

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

(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])

(define-libm-operators
  [acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc
   exp exp2 fabs floor lgamma log log10 log2 logb rint round
   sin sinh sqrt tan tanh tgamma trunc]
  [copysign fdim fmax fmin fmod pow remainder])

(define-libm-operators
  [expm1 log1p]
  [atan2 hypot]
  [fma])

(define-comparator-impls binary64
  [== ==.f64 =]
  [!= !=.f64 (negate =)]
  [< <.f64 <]
  [> >.f64 >]
  [<= <=.f64 <=]
  [>= >=.f64 >=])
