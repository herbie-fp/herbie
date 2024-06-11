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

(define move-cost 0.9895935999999999)

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
[* 1.3441783]
[+ 1.5563147000000002]
[- 1.7573510999999997]
[/ 1.7066119]
[acos 1.3864778999999998]
[acosh 1.7386883999999998]
[asin 1.4829444]
[asinh 1.6486083999999999]
[atan 1.7908312]
[atan2 2.6143697999999995]
[atanh 1.9228424999999998]
[ceil 2.4536937]
[copysign 1.8931493999999998]
[cos 2.0010757000000003]
[cosh 1.414769456394212]
[erf 1.8429090000000001]
[erfc 1.6693044999999997]
[exp 1.5050774227760835]
[expm1 1.3489543512043511]
[fabs 1.4383168999999998]
[floor 2.3315106]
[fmax 2.3143651999999997]
[fmin 2.5583871]
[fmod 1.7819245000000001]
[hypot 2.2010568]
[log 1.7538747999999997]
[log10 1.6616018999999997]
[log1p 1.8074973000000003]
[log2 1.4853409999999998]
[neg 1.2910733]
[pow 2.410625827150592]
[remainder 1.8637451999999999]
[sin 2.1324005999999995]
[sinh 1.7532248728979272]
[sqrt 1.6099431]
[sum3 2.871507607607608]
[sum4 3.7123581940134143]
[tan 2.2786129]
[tanh 1.6602894000000004]
[trunc 2.5274514]))

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
