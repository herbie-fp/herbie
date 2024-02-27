#lang racket

;;; The default platform:
;;; Optimized for Intel MKL library

(require "../plugin.rkt")

(define move-cost 0.092312)
(define fl-move-cost (* move-cost 5))

; universal boolean operations
(define boolean-platform
  (with-terminal-cost ([bool move-cost])
    (platform
      #:default-cost move-cost
      #:if-cost move-cost
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

; non-tunable operations
(define non-tunable
  (with-terminal-cost ([binary64 fl-move-cost])
    (platform-product
      [([real binary64] [bool bool])
       (cost-map #:default-cost fl-move-cost)]
      (operator-set
        [(real) (PI E INFINITY NAN)]
        [(real real bool) (== != > < >= <=)]))))

; cost model that is tunable
(define cost-model
  (cost-map
    [* 3.162164]
    [+ 3.241192]
    [- 3.2175039999999995]
    [/ 3.191132]
    [acos 3.682772]
    [acosh 3.2826839999999997]
    [asin 3.5849399999999996]
    [asinh 3.5263199999999997]
    [atan 3.2978799999999997]
    [atan2 3.8128879999999996]
    [atanh 3.430296]
    [cbrt 3.2194039999999995]
    [ceil 3.089959999999999]
    [copysign 3.262012]
    [cos 3.468388]
    [cosh 5.046712]
    [erf 3.1301839999999994]
    [erfc 4.122711999999999]
    [exp 5.088684]
    [exp2 4.994248]
    [expm1 4.219368]
    [fabs 3.0378]
    [fdim 3.142532]
    [floor 3.1555800000000005]
    [fmax 3.168940000000001]
    [fmin 3.2580959999999988]
    [fmod 12.360895999999999]
    [hypot 5.7492079999999985]
    [lgamma 4.982811999999999]
    [log 3.2879959999999993]
    [log10 3.0914159999999993]
    [log1p 3.257924]
    [log2 3.1631840000000007]
    [logb 3.1657800000000003]
    [neg 3.4522439999999994]
    [pow 5.389356]
    [remainder 13.400463999999998]
    [rint 3.2372760000000005]
    [round 3.213672]
    [sin 3.903532]
    [sinh 5.489183999999999]
    [sqrt 3.228592]
    [tan 3.5400840000000007]
    [tanh 3.411112]
    [tgamma 6.349324]
    [trunc 3.27906]))

; tunable operations
(define tunable
  (with-terminal-cost ([binary64 fl-move-cost])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real)
         (neg acos acosh asin asinh atan atanh cbrt ceil cos cosh
          erf erfc exp exp2 expm1 fabs floor lgamma log log10 log2
          log1p logb rint round sin sinh sqrt tan tanh tgamma trunc)]
        [(real real real)
         (+ - * / atan2 copysign fdim fmax fmin fmod hypot pow remainder)]))))

; register-platform

(register-platform! 'mkl
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

;; Do not run this file during testing
(module test racket/base)
