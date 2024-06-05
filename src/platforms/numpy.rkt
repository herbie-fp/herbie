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

(define move-cost 0.07134069999999999)

(define boolean-platform
  (with-terminal-cost ([bool move-cost])
    (platform
      #:default-cost 1
      #:if-cost 1
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
[* 0.12216369999999999]
[+ 0.0791696]
[- 0.075653]
[/ 0.1271119]
[acos 1.9211072999999999]
[acosh 0.3292633]
[asin 1.4704245999999999]
[asinh 1.3652571]
[atan 0.5899245]
[atan2 1.0054541]
[atanh 0.3177436]
[cbrt 0.1716086]
[ceil 0.05572179999999999]
[copysign 0.11704830000000002]
[cos 4.765178499999999]
[cosh 1.2892335]
[deg2rad 0.18666329999999998]
[exp 1.6616153]
[exp2 0.9110111]
[expm1 1.1227721000000002]
[fabs 0.2134065]
[floor 0.0378623]
[fmax 0.16186899999999999]
[fmin 0.13065290000000002]
[fmod 0.5593287]
[hypot 1.4451802999999999]
[log 0.1319264]
[log10 0.14224089999999998]
[log1p 0.2069829]
[log2 0.2414268]
[logaddexp 0.3470057]
[logaddexp2 0.26595129999999995]
[neg 0.11900129999999998]
[pow 2.1540907999999996]
[rad2deg 0.1790679]
[recip 0.4632897999999999]
[remainder 0.6515112000000001]
[rint 0.13846309999999998]
[round 0.1020501]
[sin 4.2908518]
[sinh 1.4681237]
[sqrt 0.13959949999999996]
[square 0.1132843]
[tan 1.4830210999999998]
[tanh 0.28426389999999996]
[trunc 0.05423589999999999]))

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
