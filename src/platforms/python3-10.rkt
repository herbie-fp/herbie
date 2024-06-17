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

(define move-cost 10.8809417)

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
[* 15.375737099999998]
[+ 17.2029144]
[- 15.103139899999999]
[/ 15.5982856]
[acos 18.3989225]
[acosh 19.0561946]
[asin 18.137274899999998]
[asinh 19.450490799999997]
[atan 18.9877444]
[atan2 30.3801391]
[atanh 18.3134768]
[ceil 25.946056900000002]
[copysign 21.2346332]
[cos 22.2564289]
[cosh 18.388196572061908]
[erf 17.9652261]
[erfc 17.6717225]
[exp 18.956681091126303]
[expm1 20.639201277955273]
[fabs 17.651108999999998]
[floor 27.4890968]
[fmax 26.6276345]
[fmin 26.6618447]
[fmod 23.2327182]
[hypot 21.6228865]
[log 22.2838693]
[log10 19.9124588]
[log1p 19.886138100000004]
[log2 18.467278499999995]
[neg 13.7091962]
[pow 23.50355657606116]
[remainder 21.7232619]
[sin 22.1226023]
[sinh 19.969460277876735]
[sqrt 17.6939605]
[sum3 35.30416949999999]
[sum4 42.92739947399474]
[tan 23.688525]
[tanh 18.813984400000002]
[trunc 26.193712999999995]
))

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
