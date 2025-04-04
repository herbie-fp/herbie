#lang racket

;;; The zero-cost platform:
;;; Includes boolean, binary32, binary64, accelerator operations

(require "../plugin.rkt")

; universal boolean opertaions
(define-platform herbie10-platform
                 #:literal [bool 0]
                 #:literal [binary64 0]
                 #:literal [binary32 0]
                 #:default-cost 0
                 #:if-cost 0
                 ; ---- Boolean ----
                 TRUE
                 FALSE
                 not
                 and
                 or
                 ; ---- Machine operations ----
                 PI.f64
                 PI.f32
                 E.f64
                 E.f32
                 INFINITY.f64
                 INFINITY.f32
                 NAN.f64
                 NAN.f32
                 neg.f64
                 neg.f32
                 +.f64
                 +.f32
                 -.f64
                 -.f32
                 *.f64
                 *.f32
                 /.f64
                 /.f32
                 ==.f64
                 ==.f32
                 !=.f64
                 !=.f32
                 >.f64
                 >.f32
                 <.f64
                 <.f32
                 >=.f64
                 >=.f32
                 <=.f64
                 <=.f32
                 ; ---- Binary 64 operations ----
                 acos.f64
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
                 fabs.f64
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
                 sqrt.f64
                 tan.f64
                 tanh.f64
                 tgamma.f64
                 trunc.f64
                 ; ---- Binary 32 operations ----
                 acos.f32
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
                 fabs.f32
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
                 sqrt.f32
                 tan.f32
                 tanh.f32
                 tgamma.f32
                 trunc.f32
                 ; ---- Accelerator operations ----
                 erfc.f64
                 expm1.f64
                 log1p.f64
                 hypot.f64
                 fma.f64
                 erfc.f32
                 expm1.f32
                 log1p.f32
                 hypot.f32
                 fma.f32)

; Register the platform
(register-platform! 'herbie10 herbie10-platform)

;; Do not run this file during testing
(module test racket/base
  )
