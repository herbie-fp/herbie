#lang racket

;;; The default platform:
;;; Optimized for C/C++ on Linux with a full libm

(require "../plugin.rkt")

; boolean platform (universal)
(define-platform boolean-platform ()
  (bool
    #:const [TRUE FALSE]
    #:1ary [not]
    #:2ary [and or]))

; machine floating-point operations
(define-platform-product machine-platform
  #:type real [binary64 binary32]
  #:type bool [bool]
  #:operators [PI E INFINITY NAN neg + - * /]
  #:operators [== != > < >= <=])

; libm platform
(define-platform-product libm-platform
  #:type real [binary64 binary32]
  #:operators [acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc
               exp exp2 fabs floor lgamma log log10 log2 logb rint round
               sin sinh sqrt tan tanh tgamma trunc copysign fdim fmax fmin
               fmod pow remainder]
  #:operators [expm1 log1p atan2 hypot fma])

; Registers all three

(register-platform! 'boolean boolean-platform)

(register-platform! 'hardware
  (platform-union boolean-platform
                  machine-platform))

(register-platform! 'default
  (platform-union boolean-platform
                  machine-platform
                  libm-platform))
