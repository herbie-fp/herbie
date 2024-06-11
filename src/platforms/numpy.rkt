#lang racket

;;; Numpy v1.0

(require "../plugin.rkt")

;; --------------------------------------------------------
;; Operators


  (define-accelerator (deg2rad real) real
  (λ (x) (/ (* x (PI)) 180)))

  (define-accelerator (rad2deg real) real
  (λ (x) (/ (* x 180) (PI))))

  (define-accelerator (logaddexp real real) real
  (λ (x y) (log (+ (exp x) (exp y)))))

  (define-accelerator (logaddexp2 real real) real
  (λ (x y) (log2 (+ (exp2 x) (exp2 y)))))

  (define-accelerator (square real) real
  (λ (x) (* x x)))

(define-accelerator-impl logaddexp logaddexp.f64 (binary64 binary64) binary64)
(define-accelerator-impl logaddexp2 logaddexp2.f64 (binary64 binary64) binary64)
(define-accelerator-impl square square.f64 (binary64) binary64)
(define-accelerator-impl rad2deg rad2deg.f64 (binary64) binary64)
(define-accelerator-impl deg2rad deg2rad.f64 (binary64) binary64)

;; --------------------------------------------------------
;; Platform

(define move-cost 0.00039)

(define boolean-platform
  (with-terminal-cost ([bool move-cost])
    (platform
      #:default-cost move-cost
      #:if-cost (sum move-cost)
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
[* 0.0088726]
[+ 0.005234000000000001]
[- 0.0049093]
[/ 0.0116009]
[acos 0.0258322]
[acosh 0.0411245]
[asin 0.0221878]
[asinh 0.0669369]
[atan 0.06955429999999999]
[atan2 0.12193099999999998]
[atanh 0.025345199999999995]
[cbrt 0.11635209999999999]
[ceil 0.0026107]
[copysign 0.005772999999999999]
[cos 0.24979609999999997]
[cosh 0.10397459999999999]
[deg2rad 0.0168443]
[exp 0.1012828]
[exp2 0.10595539999999999]
[expm1 0.07306579999999999]
[fabs 0.0107785]
[floor 0.0027018999999999997]
[fmax 0.16721650000000002]
[fmin 0.008927399999999999]
[fmod 0.124138]
[hypot 0.12458810000000001]
[log 0.0395305]
[log10 0.07010259999999999]
[log1p 0.072767]
[log2 0.0445627]
[logaddexp 0.153809]
[logaddexp2 0.1550404]
[neg 0.0036109000000000002]
[pow 0.16646729999999998]
[rad2deg 0.019839299999999997]
[recip 0.0061433]
[remainder 0.09573809999999999]
[rint 0.0027891999999999995]
[round 0.0039251]
[sin 0.26159980000000005]
[sinh 0.10444319999999999]
[sqrt 0.008783099999999999]
[square 0.007255599999999999]
[tan 0.30990399999999996]
[tanh 0.06020940000000001]
[trunc 0.0026626]))

(define tunable
  (with-terminal-cost ([binary64 move-cost])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real)    
         (sin cos tan asin acos atan  sinh
          cosh tanh asinh acosh atanh round floor 
          ceil trunc exp expm1 exp2 log log10 log2
          log1p recip sqrt cbrt fabs neg rint
          square deg2rad rad2deg)]
        [(real real real)
         (+ - * / atan2 copysign fmax fmin fmod pow remainder
         logaddexp logaddexp2 hypot)]))))

(register-platform! 'numpy
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

;; Do not run this file during testing
(module test racket/base)
