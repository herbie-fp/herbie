#lang racket

;; Single-precision common math operators

(require math/bigfloat)
(require "runtime/float32.rkt"
         "runtime/utils.rkt"
         "bool.rkt"     ;; required for raco test
         "binary64.rkt"  ;; required for raco test
         (only-in "runtime/libm.rkt"
           [define-binary32-impls/libm define-libm-operators]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (binary32 real float32?)
  bigfloat->float32
  bf
  (shift 31 ordinal->float32)
  (unshift 31 float32->ordinal)
  32
  (conjoin number? nan?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants binary32
  [PI PI.f32 (->float32 pi)]
  [E E.f32 (->float32 (exp 1.0))]
  [INFINITY INFINITY.f32 (->float32 +inf.0)]
  [NAN NAN.f32 (->float32 +nan.0)])

(define-operator-impl (neg neg.f32 binary32) binary32 [fl fl32-])
(define-operator-impl (+ +.f32 binary32 binary32) binary32 [fl fl32+])
(define-operator-impl (- -.f32 binary32 binary32) binary32 [fl fl32-])
(define-operator-impl (* *.f32 binary32 binary32) binary32 [fl fl32*])
(define-operator-impl (/ /.f32 binary32 binary32) binary32 [fl fl32/])

(define-libm-operators
  [acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc
   exp exp2 fabs floor lgamma log log10 log2 logb rint round
   sin sinh sqrt tan tanh tgamma trunc]
  [copysign fdim fmax fmin fmod pow remainder])

(define-libm-operators
  [expm1 log1p]
  [atan2 hypot]
  [fma])

(define-comparator-impls binary32
  [== ==.f32 =]
  [!= !=.f32 (negate =)]
  [< <.f32 <]
  [> >.f32 >]
  [<= <=.f32 <=]
  [>= >=.f32 >=])

(define-operator-impl (cast binary64->binary32 binary64) binary32
  [fl (curryr ->float32)])

(define-operator-impl (cast binary32->binary64 binary32) binary64
  [fl identity])
