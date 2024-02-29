#lang racket

;;; Python 3.10

(require "../plugin.rkt")

;; --------------------------------------------------------
;; Operators

(define-accelerator-operator sum3 (real real real) real
  (λ (x y z) (+ (+ x y) z)))

(define-accelerator-operator sum4 (real real real real) real
  (λ (x y z w) (+ (+ x y) (+ z w))))

(define-accelerator-impl sum3 sum3.f64 (binary64 binary64 binary64) binary64 +)
(define-accelerator-impl sum4 sum4.f64 (binary64 binary64 binary64 binary64) binary64 +)

;; --------------------------------------------------------
;; Platform

(define boolean-platform
  (with-terminal-cost ([bool 1])
    (platform
      #:default-cost 1
      #:if-cost 1
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

; non-tunable operations
(define non-tunable
  (with-terminal-cost ([binary64 1])
    (platform-product
      [([real binary64] [bool bool])
       (cost-map #:default-cost 1)]
      (operator-set
        [(real) (PI E INFINITY NAN)]
        [(real real bool) (== != > < >= <=)]))))

(define cost-model
  (cost-map
[* 1.8024279200000002]
[+ 1.9017800399999998]
[- 1.80564]
[/ 2.00602396]
[acos 2.1566720000000004]
[acosh 2.247624]
[asin 2.0954479999999998]
[asinh 2.2428799999999995]
[atan 2.17490804]
[atan2 2.72122]
[atanh 2.0917279599999996]
[ceil 2.62732396]
[copysign 2.381548]
[cos 2.39462004]
[cosh 4.339468]
[erf 2.0204999599999995]
[erfc 2.1636]
[exp 3.313404]
[expm1 3.224895959999999]
[fabs 1.9273680400000002]
[floor 2.607124]
[fmax 2.88719984]
[fmin 2.88687192]
[fmod 12.598215999999997]
[hypot 2.92877192]
[lgamma 1350.940642]
[log 2.5507919600000006]
[log10 2.33952404]
[log1p 2.1507239999999994]
[log2 2.0593039999999996]
[neg 1.4292119599999997]
[pow 3.9706559999999995]
[remainder 12.82389992]
[sin 2.5678199599999996]
[sinh 4.502907919999999]
[sqrt 2.30032796]
[tan 2.51221608]
[tanh 2.1028519599999997]
[tgamma 2783.5949891200003]
[trunc 2.5861399600000006]
[sum3 1]
[sum4 1]))

(define tunable
  (with-terminal-cost ([binary64 1])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real)
         (neg acos acosh asin asinh atan atanh ceil cos cosh
          erf erfc exp expm1 fabs floor lgamma log log10 log2
          log1p sin sinh sqrt tan tanh tgamma trunc)]
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
