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

(register-platform! 'boolean boolean-platform)

; machine floating-point operations
(define-platform machine-platform
  ([binary64 binary32])
  (bool
    #:2ary binary64 [== != > < >= <=]
    #:2ary binary32 [== != > < >= <=])
  (binary64
    #:const [PI E INFINITY NAN]
    #:1ary [neg]
    #:2ary [+ - * /])
  (binary32
    #:const [PI E INFINITY NAN]
    #:1ary [neg]
    #:2ary [+ - * /]))

(register-platform! 'hardware
  (platform-union boolean-platform
                  machine-platform))

; libm platform
(define-platform libm-platform ()
  (binary64
    ; libm (common)
    #:1ary [acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc
            exp exp2 fabs floor lgamma log log10 log2 logb rint round
            sin sinh sqrt tan tanh tgamma trunc]
    #:2ary [copysign fdim fmax fmin fmod pow remainder]
    ; libm (accelerators)
    #:1ary [expm1 log1p]
    #:2ary [atan2 hypot]
    #:3ary [fma])

  (binary32
    ; libm (common)
    #:1ary [acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc
            exp exp2 fabs floor lgamma log log10 log2 logb rint round
            sin sinh sqrt tan tanh tgamma trunc]
    #:2ary [copysign fdim fmax fmin fmod pow remainder]
    ; libm (accelerators)
    #:1ary [expm1 log1p]
    #:2ary [atan2 hypot]
    #:3ary [fma]))

(register-platform! 'default
  (platform-union boolean-platform
                  machine-platform
                  libm-platform))
