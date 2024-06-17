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

(define-accelerator-impl recip rcp.f64 (binary64) binary64)
(define-accelerator-impl logaddexp logaddexp.f64 (binary64 binary64) binary64)
(define-accelerator-impl logaddexp2 logaddexp2.f64 (binary64 binary64) binary64)
(define-accelerator-impl square square.f64 (binary64) binary64)
(define-accelerator-impl rad2deg rad2deg.f64 (binary64) binary64)
(define-accelerator-impl deg2rad deg2rad.f64 (binary64) binary64)

;; --------------------------------------------------------
;; Platform

(define move-cost 0.0004769999999999999)

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
[* 0.0483716]
[+ 0.04311939999999999]
[- 0.042390500000000005]
[/ 0.0814646]
[acos 0.5736976]
[acosh 0.7421665]
[asin 0.6698979999999999]
[asinh 1.0183449]
[atan 1.1253743999999997]
[atan2 5.481924599999999]
[atanh 0.4491854]
[cbrt 1.6887687999999996]
[ceil 0.0615612]
[copysign 0.0818368]
[cos 4.4333674]
[cosh 1.1900187]
[deg2rad 0.1558532]
[exp 1.6213792999999999]
[exp2 1.7100414000000002]
[expm1 0.9179054000000001]
[fabs 0.1529014]
[floor 0.059935]
[fmax 0.5842468]
[fmin 0.5933015]
[fmod 1.7258159999999996]
[hypot 1.1924909]
[log 0.48365749999999996]
[log10 1.0059405]
[log1p 1.1899357999999998]
[log2 0.7371932]
[logaddexp 2.3611219999999995]
[logaddexp2 2.1550466]
[neg 0.027143100000000003]
[pow 2.4806269999999997]
[rad2deg 0.1529932]
[recip 0.08193690000000001]
[remainder 1.2205397000000002]
[rint 0.060565100000000004]
[round 0.0636161]
[sin 4.4936186]
[sinh 1.6129542000000001]
[sqrt 0.13825179999999998]
[square 0.0451454]
[tan 5.905053799999999]
[tanh 1.0148776]
[trunc 0.06039399999999999]))

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
