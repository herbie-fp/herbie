#lang racket

;; Double-precision common math operators

(require "binary64.rkt" "utils.rkt"
         (only-in "libm.rkt"
           [define-binary64-impls/libm define-libm-operators]))

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
