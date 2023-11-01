#lang racket

;;; The default platform:
;;; Optimized for C/C++ on Linux with a full libm

(require "../plugin.rkt")

; universal boolean opertaions
(define boolean-platform
  (platform
    [(bool) (TRUE FALSE)]
    [(bool bool) (not)]
    [(bool bool bool) (and or)]))

; machine floating-point operations (without conversions)
(define machine-platform
  (platform-product
    [real (binary64 binary32)]
    [bool (bool)]
    (operator-set
      [(real) (PI E INFINITY NAN)]
      [(real real) (neg)]
      [(real real real) (+ - * /)]
      [(real real bool) (== != > < >= <=)])))

; libm operations
(define libm-platform
  (platform-product
    [real (binary64 binary32)]
    (operator-set
      [(real real)
       (acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc
        exp exp2 expm1 fabs floor lgamma log log10 log2 log1p logb
        rint round sin sinh sqrt tan tanh tgamma trunc)]
      [(real real real)
       (atan2 copysign fdim fmax fmin fmod hypot pow remainder)]
      [(real real real real)
       (fma)])))

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
