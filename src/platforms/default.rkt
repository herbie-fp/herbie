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
    (let ([rel-costs (cost-map
                        [(PI E INFINITY NAN) 1]
                        [(neg + - * /) 1]
                        [(== != > < >= <=) 3])])
      (platform-product
        [([real binary64] [bool bool]) (cost-map-scale 64 rel-costs)]
        [([real binary32] [bool bool]) (cost-map-scale 32 rel-costs)]
        (operator-set
          [(real) (PI E INFINITY NAN)]
          [(real real) neg]
          [(real real real) (+ - * /)]
          [(real real bool) (== != > < >= <=)])))))

; libm operations
(define libm-platform
  (with-terminal-cost ([binary64 64] [binary32 32])
    (let ([rel-costs (cost-map #:default-cost 100)])
      (platform-product #:optional
        [([real binary64]) (cost-map-scale 64 rel-costs)]
        [([real binary32]) (cost-map-scale 32 rel-costs)] 
        (operator-set
          [(real real)
           (acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc
           exp exp2 expm1 fabs floor lgamma log log10 log2 log1p logb
           rint round sin sinh sqrt tan tanh tgamma trunc)]
          [(real real real)
           (atan2 copysign fdim fmax fmin fmod hypot pow remainder)]
          [(real real real real)
           (fma)])))))

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
