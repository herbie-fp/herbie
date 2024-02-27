#lang racket

;;; The default platform:
;;; Optimized for C/C++ on Linux with a full libm

(require "../plugin.rkt")

(define move-cost 0.027403999999999994)
(define fl-move-cost (* move-cost 0.13701999999999998))

; universal boolean operations
(define boolean-platform
  (with-terminal-cost ([bool 0.027403999999999994])
    (platform
      #:conversions ([binary64 binary32])
      #:default-cost move-cost
      #:if-cost move-cost
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

; non-tunable operations
(define non-tunable
  (with-terminal-cost ([binary64 0.13701999999999998])
    (platform-product
      [([real binary64] [bool bool])
       (cost-map #:default-cost 0.13701999999999998)]
      (operator-set
        [(real) (PI E INFINITY NAN)]
        [(real real bool) (== != > < >= <=)]))))

; cost model that is tunable
(define cost-model
  (cost-map
    [* 0.21627599999999997]
    [+ 0.158984]
    [- 0.151168]
    [/ 0.278124]
    [acos 0.376276]
    [acosh 0.6838960000000001]
    [asin 0.396408]
    [asinh 0.8423720000000001]
    [atan 0.8567639999999997]
    [atan2 1.5888999999999998]
    [atanh 0.35956000000000005]
    [cbrt 1.5998679999999998]
    [ceil 0.471272]
    [copysign 0.20161199999999999]
    [cos 3.4983399999999985]
    [cosh 0.9130119999999999]
    [erf 0.8241080000000001]
    [erfc 0.8430679999999999]
    [exp 1.041172]
    [exp2 0.850124]
    [expm1 0.8420279999999998]
    [fabs 0.107824]
    [fdim 0.5774560000000001]
    [floor 0.5011720000000001]
    [fma 0.345064]
    [fmax 0.2687320000000001]
    [fmin 0.3078199999999999]
    [fmod 95.40786399999999]
    [hypot 1.2333039999999997]
    [lgamma 1.603764]
    [log 0.500272]
    [log10 0.859104]
    [log1p 0.7783519999999998]
    [log2 0.6960999999999998]
    [logb 0.24402000000000004]
    [neg 0.09292]
    [pow 1.5318440000000002]
    [remainder 16.197544]
    [rint 0.10907999999999998]
    [round 0.6640320000000003]
    [sin 3.348824]
    [sinh 1.22988]
    [sqrt 0.21313200000000002]
    [tan 3.6807319999999994]
    [tanh 0.8265439999999998]
    [tgamma 1.935324]
    [trunc 0.46657600000000005]))

; tunable operations
(define tunable
  (with-terminal-cost ([binary64 0.13701999999999998])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real)
         (neg acos acosh asin asinh atan atanh cbrt ceil cos cosh
          erf erfc exp exp2 expm1 fabs floor lgamma log log10 log2
          log1p logb rint round sin sinh sqrt tan tanh tgamma trunc)]
        [(real real real)
         (+ - * / atan2 copysign fdim fmax fmin fmod hypot pow remainder)]
        [(real real real real)
         (fma)]))))

; register-platform

(register-platform! 'default
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

;; Do not run this file during testing
(module test racket/base)
