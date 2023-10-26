#lang racket

;;; The default platform:
;;; Optimized for C/C++ on Linux with a full libm

(require "../plugin.rkt")

; universal boolean opertaions
(define boolean-platform
  (platform
    (bool
      #:const [TRUE FALSE]
      #:1ary [not]
      #:2ary [and or])))

; machine floating-point operations
(define machine-platform
  (platform-union
    (platform-product
      #:type real [binary64 binary32]
      #:type bool [bool]
      #:operators [PI E INFINITY NAN neg + - * /]
      #:operators [== != > < >= <=])
    (platform
      #:conversions ([binary64 binary32])
      (binary64)
      (binary32))))

; libm operations
(define libm-platform
  (platform-product
    #:type real [binary64 binary32]
    #:operators [acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc
                 exp exp2 fabs floor lgamma log log10 log2 logb rint round
                 sin sinh sqrt tan tanh tgamma trunc copysign fdim fmax fmin
                 fmod pow remainder]
    #:operators [expm1 log1p atan2 hypot fma]))

; compose platforms

(define hardware-platform
  (platform-union boolean-platform
                  machine-platform))

(define default-platform
  (platform-union boolean-platform
                  machine-platform
                  libm-platform))

; Register all three

(register-platform! 'boolean boolean-platform)
(register-platform! 'hardware hardware-platform)
(register-platform! 'default default-platform)
