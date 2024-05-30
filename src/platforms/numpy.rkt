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
[* 22.732230200000004]
[+ 24.6968277]
[- 21.4000724]
[/ 23.4853221]
[acos 1354.0952832999997]
[acosh 226.46165389999996]
[asin 1364.2564337]
[asinh 1010.4796609000001]
[atan 496.61785939999993]
[atan2 22.942196699999997]
[atanh 252.30248999999995]
[cbrt 129.52739920000002]
[ceil 30.333692599999996]
[copysign 21.0481445]
[cos 3344.6476219999995]
[cosh 1006.4898482000001]
[deg2rad 166.17671779999998]
[exp 950.6972776]
[exp2 611.8306978999999]
[expm1 878.1427567999999]
[fabs 162.62439719999998]
[floor 30.8996715]
[fmax 22.2657109]
[fmin 22.2242113]
[fmod 21.5150772]
[hypot 22.799912]
[log 162.08192419999997]
[log10 102.99682419999999]
[log1p 144.7509084]
[log2 104.9240186]
[logaddexp 21.2609909]
[logaddexp2 24.6737781]
[neg 34.031109199999996]
[pow 21.6374662]
[rad2deg 177.43291029999997]
[recip 80.68506819999998]
[remainder 22.266038499999997]
[rint 30.107588699999997]
[round 40.6355484]
[sin 3613.7332868999997]
[sinh 1091.2824531]
[sqrt 103.99448559999999]
[square 112.78157099999999]
[tan 1182.8334289999998]
[tanh 175.9177504]
[trunc 29.736554599999998])) 

(define tunable
  (with-terminal-cost ([binary64 1])
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
