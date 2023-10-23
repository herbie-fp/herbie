#lang racket

;;; The default platform:
;;; Optimized for C/C++ on Linux with a full libm

(require "../plugin.rkt")

(define-platform default
  ([binary32 binary64])

  (bool
    #:const [TRUE FALSE]
    #:1ary [not]
    #:2ary [and or]
    #:2ary binary64 [== != > < >= <=]
    #:2ary binary32 [== != > < >= <=])

  (binary64
    #:const [PI E INFINITY NAN]
    ; arithmetic
    #:1ary [neg]
    #:2ary [+ - * /]
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
    #:const [PI E INFINITY NAN]
    ; arithmetic
    #:1ary [neg]
    #:2ary [+ - * /]
    ; libm (common)
    #:1ary [acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc
            exp exp2 fabs floor lgamma log log10 log2 logb rint round
            sin sinh sqrt tan tanh tgamma trunc]
    #:2ary [copysign fdim fmax fmin fmod pow remainder]
    ; libm (accelerators)
    #:1ary [expm1 log1p]
    #:2ary [atan2 hypot]
    #:3ary [fma]))

(register-platform! 'default default)
