#lang racket

;;; Python 3.10

(require "../plugin.rkt")

;; --------------------------------------------------------
;; Operators

(define-accelerator (sum3 real real real) real
  (λ (x y z) (+ (+ x y) z)))

(define-accelerator (sum4 real real real real) real
  (λ (x y z w) (+ (+ x y) (+ z w))))

(define-accelerator (sum5 real real real real real) real
  (λ (x y z a b) (+ (+ (+ x y) (+ z a)) b)))

(define-accelerator-impl sum3 sum3.f64 (binary64 binary64 binary64) binary64 +)
(define-accelerator-impl sum4 sum4.f64 (binary64 binary64 binary64 binary64) binary64 +)
(define-accelerator-impl sum5 sum5.f64 (binary64 binary64 binary64 binary64 binary64) binary64 +)

;; --------------------------------------------------------
;; Platform

(define move-cost 0.6966692999999999)

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
  (with-terminal-cost ([binary64 move-cost])
    (platform-product
      [([real binary64] [bool bool])
       (cost-map #:default-cost move-cost)]
      (operator-set
        [(real) (PI E INFINITY NAN)]
        [(real real bool) (== != > < >= <=)]))))

(define cost-model
  (cost-map
[* 0.8054991999999999]
[+ 0.7930968999999999]
[- 0.8559517]
[/ 1.45708]
[acos 1.0407372999999998]
[acosh 1.7229636000000004]
[asin 0.9118355999999999]
[asinh 1.0633067]
[atan 1.0349877]
[atan2 1.4247642999999997]
[atanh 0.8839081]
[ceil 1.3744028]
[copysign 1.1958529]
[cos 1.3994652]
[cosh 0.9880563159953069]
[erf 1.1248549]
[erfc 1.193118]
[exp 1.014421980644306]
[expm1 1.588595571095571]
[fabs 0.8354694]
[floor 1.4696710999999998]
[fmax 1.3901738]
[fmin 2.4015029999999995]
[fmod 1.196488]
[hypot 1.4982294]
[log 1.2457814999999997]
[log10 0.9872526999999998]
[log1p 1.0346914999999999]
[log2 1.0641402000000002]
[neg 0.6458092000000001]
[pow 1.575393637408969]
[remainder 1.1551793]
[sin 1.5502553]
[sinh 1.0403588189284316]
[sqrt 1.0406897999999998]
[sum3 3.0264579]
[sum4 2.45817]
[tan 1.2888167]
[tanh 1.0188225999999998]
[trunc 1.6997523]))

(define tunable
  (with-terminal-cost ([binary64 move-cost])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real)
         (neg acos acosh asin asinh atan atanh ceil cos cosh
          erf erfc exp expm1 fabs floor log log10 log2
          log1p sin sinh sqrt tan tanh trunc)]
        [(real real real)
         (+ - * / atan2 copysign fmax fmin fmod hypot pow remainder)]
        [(real real real real) sum3]
        [(real real real real real) sum4]))))

(register-platform! 'python
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

;; Do not run this file during testing
(module test racket/base)
