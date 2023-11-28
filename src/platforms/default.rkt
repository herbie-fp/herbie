#lang racket

;;; The default platform:
;;; Optimized for C/C++ on Linux with a full libm

(require "../plugin.rkt")

; universal boolean opertaions
(define boolean-platform
  (with-terminal-cost ([bool 1])
    (platform
      #:default-cost 1
      #:if-cost 1
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

; machine floating-point operations (without conversions)
(define machine-platform
  (with-terminal-cost ([binary64 64] [binary32 32])
    (platform-union
      (platform
        [(binary64) (PI E INFINITY NAN) 64]
        [(binary64 binary64) neg 64]
        [(binary64 binary64 binary64) (+ - * /) 64]
        [(binary64 binary64 bool) (== != > < >= <=) (* 3 64)])
      (platform
        [(binary32) (PI E INFINITY NAN) 32]
        [(binary32 binary32) neg 32]
        [(binary32 binary32 binary32) (+ - * /) 32]
        [(binary32 binary32 bool) (== != > < >= <=) (* 3 32)]))))

; libm operations
(define libm-platform
  (with-terminal-cost ([binary64 64] [binary32 32])
    (platform-union
      (platform
        #:default-cost 6400
        [(binary64 binary64)
         (acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc
          exp exp2 expm1 fabs floor lgamma log log10 log2 log1p logb
          rint round sin sinh sqrt tan tanh tgamma trunc)]
        [(binary64 binary64 binary64)
         (atan2 copysign fdim fmax fmin fmod hypot pow remainder)]
        [(binary64 binary64 binary64 binary64)
         (fma)])
      (platform
        #:default-cost 3200
        [(binary32 binary32)
         (acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc
          exp exp2 expm1 fabs floor lgamma log log10 log2 log1p logb
          rint round sin sinh sqrt tan tanh tgamma trunc)]
        [(binary32 binary32 binary32)
         (atan2 copysign fdim fmax fmin fmod hypot pow remainder)]
        [(binary32 binary32 binary32 binary32)
         (fma)]))))

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

;; Do not run this file during testing
(module test racket/base)
