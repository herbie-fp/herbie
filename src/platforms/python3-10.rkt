#lang racket

;;; Python 3.10

(require "../plugin.rkt")

;; --------------------------------------------------------
;; Operators

(define-accelerator-operator sum3 (real real real) real
  (λ (x y z) (+ (+ x y) z)))

(define-accelerator-operator sum4 (real real real real) real
  (λ (x y z w) (+ (+ x y) (+ z w))))

(define-accelerator-operator sum5 (real real real real real) real
  (λ (x y z a b) (+ (+ (+ x y) (+ z a)) b)))

(define-accelerator-impl sum3 sum3.f64 (binary64 binary64 binary64) binary64 +)
(define-accelerator-impl sum4 sum4.f64 (binary64 binary64 binary64 binary64) binary64 +)
(define-accelerator-impl sum5 sum5.f64 (binary64 binary64 binary64 binary64 binary64) binary64 +)

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
   [* 2.013928]
[+ 1.8056880000000004]
[- 2.0186759999999997]
[/ 1.836332]
[acos 2.021984]
[acosh 2.13014]
[asin 1.9804719999999998]
[asinh 2.2921]
[atan 2.14085204]
[atan2 2.720068]
[atanh 1.9331639999999999]
[ceil 2.63332]
[copysign 2.382024]
[cos 2.41401596]
[cosh 4.26983204]
[erf 2.104264]
[erfc 2.059636]
[exp 3.3930520400000006]
[expm1 3.324224039999999]
[fabs 1.94423604]
[floor 2.7368439999999996]
[fmax 2.8649360399999995]
[fmin 2.97901188]
[fmod 2.4950959999999998]
[hypot 2.67242404]
[lgamma 1366.1577325199999]
[log 2.450068]
[log10 2.2717000400000003]
[log1p 2.23565996]
[log2 2.0437640400000006]
[neg 1.5017679999999998]
[pow 5.745251919999999]
[remainder 2.47294]
[sin 2.42194008]
[sinh 4.2463839199999995]
[sqrt 1.96596]
[sum3 11.11731212]
[sum4 13.000808079999997]
[tan 2.6245920000000007]
[tanh 2.101672]
[tgamma 2698.346980519999]
[trunc 2.6800839999999995]))

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
