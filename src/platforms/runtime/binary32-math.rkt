#lang racket

;; Single-precision common math operators

(require "binary32.rkt" "float32.rkt" "utils.rkt"
         (only-in "libm.rkt"
           [define-binary32-impls/libm define-libm-operators]))

(define-operator-impl (neg neg.f32 binary32) binary32 [fl fl32-])
(define-operator-impl (+ +.f32 binary32 binary32) binary32 [fl fl32+])
(define-operator-impl (- -.f32 binary32 binary32) binary32 [fl fl32-])
(define-operator-impl (* *.f32 binary32 binary32) binary32 [fl fl32*])
(define-operator-impl (/ /.f32 binary32 binary32) binary32 [fl fl32/])

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
    (define-operator-impl (name impl-name binary32 binary32) bool
      [fl impl-fn])
    ...))

(define-comparator-impls
  [== ==.f32 =]
  [!= !=.f32 (negate =)]
  [< <.f32 <]
  [> >.f32 >]
  [<= <=.f32 <=]
  [>= >=.f32 >=])
