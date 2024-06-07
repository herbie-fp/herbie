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

(define move-cost 0.07088030000000001)

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
[* 0.1430245]
[+ 0.12121140000000001]
[- 0.12950419999999999]
[/ 0.1241773]
[acos 1.2561282]
[acosh 0.273826]
[asin 1.5134256000000001]
[asinh 1.1625229]
[atan 0.6617165]
[atan2 0.6487112999999999]
[atanh 0.294116]
[cbrt 0.16674049999999999]
[ceil 0.054179200000000004]
[copysign 0.15603440000000002]
[cos 4.4406775]
[cosh 1.2617262]
[deg2rad 0.218731]
[exp 1.5840545000000001]
[exp2 0.8933899000000001]
[expm1 0.9959508999999999]
[fabs 0.20186980000000002]
[floor 0.047367400000000004]
[fmax 0.0691872]
[fmin 0.0643155]
[fmod 0.7343422000000001]
[hypot 1.6392499999999999]
[log 0.15330090000000002]
[log10 0.1386945]
[log1p 0.181128]
[log2 0.18316889999999997]
[logaddexp 0.2726394]
[logaddexp2 0.3117147]
[neg 0.1742401]
[pow 2.5697588999999996]
[rad2deg 0.20293160000000002]
[recip 0.0971971]
[remainder 0.7892183999999999]
[rint 0.0574415]
[round 0.0827448]
[sin 4.5437004]
[sinh 1.4439642000000001]
[sqrt 0.11118970000000002]
[square 0.126326]
[tan 1.7794492999999996]
[tanh 0.27072179999999996]
[trunc 0.05412750000000001]))

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
