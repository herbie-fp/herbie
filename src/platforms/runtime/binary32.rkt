#lang racket

;; Single-precision platform (math only)

(require math/flonum math/bigfloat ffi/unsafe)
(require "../../plugin.rkt" "bool.rkt" "binary64.rkt" "float32.rkt"
         (only-in "libm.rkt" [define-binary32-impls/libm define-libm-operators]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(define-representation (binary32 real float32?)
  bigfloat->float32
  bf
  (shift 31 ordinal->float32)
  (unshift 31 float32->ordinal)
  32
  (conjoin number? nan?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operator-impl (PI PI.f32) binary32
  [fl (const (->float32 pi))])

(define-operator-impl (E E.f32) binary32
  [fl (const (->float32 (exp 1.0)))])

(define-operator-impl (INFINITY INFINITY.f32) binary32
  [fl (const (->float32 +inf.0))])

(define-operator-impl (NAN NAN.f32) binary32
  [fl (const (->float32 +nan.0))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define-operator-impl (cast binary64->binary32 binary64) binary32
  [fl (curryr ->float32)])

(define-operator-impl (cast binary32->binary64 binary32) binary64
  [fl identity])
