#lang racket

(require "../plugin.rkt" vdt-herbie)

(define-accelerator (fast-exp real) real (lambda (x) (exp x)))
(define-accelerator (fast-sin real) real (lambda (x) (sin x)))
(define-accelerator (fast-cos real) real (lambda (x) (cos x)))
(define-accelerator (fast-tan real) real (lambda (x) (tan x)))
(define-accelerator (fast-tanh real) real (lambda (x) (tanh x)))
(define-accelerator (fast-log real) real (lambda (x) (log x)))
(define-accelerator (fast-asin real) real (lambda (x) (asin x)))
(define-accelerator (fast-acos real) real (lambda (x) (acos x)))
(define-accelerator (fast-atan real) real (lambda (x) (atan x)))
(define-accelerator (fast-isqrt real) real (lambda (x) (/ 1 (sqrt x))))

(define-accelerator-impl fast-exp fast-exp.f64 (binary64) binary64 (lambda (x) (vdt-exp x)))
(define-accelerator-impl fast-sin fast-sin.f64 (binary64) binary64 (lambda (x) (vdt-sin x)))
(define-accelerator-impl fast-cos fast-cos.f64 (binary64) binary64 (lambda (x) (vdt-cos x)))
(define-accelerator-impl fast-tan fast-tan.f64 (binary64) binary64 (lambda (x) (vdt-tan x)))
(define-accelerator-impl fast-tanh fast-tanh.f64 (binary64) binary64 (lambda (x) (vdt-tanh x)))
(define-accelerator-impl fast-log fast-log.f64 (binary64) binary64 (lambda (x) (vdt-log x)))
(define-accelerator-impl fast-asin fast-asin.f64 (binary64) binary64 (lambda (x) (vdt-asin x)))
(define-accelerator-impl fast-acos fast-acos.f64 (binary64) binary64 (lambda (x) (vdt-acos x)))
(define-accelerator-impl fast-atan fast-atan.f64 (binary64) binary64 (lambda (x) (vdt-atan x)))
(define-accelerator-impl fast-isqrt fast-isqrt.f64 (binary64) binary64 (lambda (x) (vdt-isqrt x)))

(define-accelerator-impl fast-exp fast-exp.f32 (binary32) binary32 (lambda (x) (vdt-expf x)))
(define-accelerator-impl fast-sin fast-sin.f32 (binary32) binary32 (lambda (x) (vdt-sinf x)))
(define-accelerator-impl fast-cos fast-cos.f32 (binary32) binary32 (lambda (x) (vdt-cosf x)))
(define-accelerator-impl fast-tan fast-tan.f32 (binary32) binary32 (lambda (x) (vdt-tanf x)))
(define-accelerator-impl fast-tanh fast-tanh.f32 (binary32) binary32 (lambda (x) (vdt-tanhf x)))
(define-accelerator-impl fast-log fast-log.f32 (binary32) binary32 (lambda (x) (vdt-logf x)))
(define-accelerator-impl fast-asin fast-asin.f32 (binary32) binary32 (lambda (x) (vdt-asinf x)))
(define-accelerator-impl fast-acos fast-acos.f32 (binary32) binary32 (lambda (x) (vdt-acosf x)))
(define-accelerator-impl fast-atan fast-atan.f64 (binary64) binary64 (lambda (x) (vdt-atanf x)))
(define-accelerator-impl fast-isqrt fast-isqrt.f64 (binary64) binary64 (lambda (x) (vdt-isqrtf x)))

(define move-cost 0.14444819999999936)
(define fl-move-cost move-cost)

(define boolean-platform
  (with-terminal-cost ([bool move-cost])
    (platform
      #:default-cost move-cost
      #:if-cost move-cost
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

(define non-tunable
  (with-terminal-cost ([binary64 fl-move-cost])
    (platform-product
      [([real binary64] [bool bool])
       (cost-map #:default-cost fl-move-cost)]
      (operator-set
        [(real) (PI E INFINITY NAN)]
        [(real real bool) (== != > < >= <=)]))))

(define cost-model
  (cost-map
    [* 0.21550819999999984]
    [+ 0.20742009999999986]
    [- 0.20475410000000024]
    [/ 0.3591601999999998]
    [acos 0.5241982000000004]
    [acosh 0.8385984000000004]
    [asin 0.6151123000000004]
    [asinh 1.0913158999999997]
    [atan 1.1906198000000003]
    [atan2 5.5938568]
    [atanh 0.49703620000000015]
    [cbrt 1.7213246999999996]
    [ceil 0.2750330999999996]
    [copysign 0.19016700000000028]
    [cos 4.524595400000001]
    [cosh 1.2192366]
    [erf 1.0632189]
    [erfc 0.781345300000001]
    [exp 1.555575799999999]
    [exp2 1.1298432999999992]
    [expm1 0.9130766999999995]
    [fabs 0.1370342000000006]
    [fast-acos 2.6102463]
    [fast-asin 2.1099370000000013]
    [fast-atan 2.0487389999999994]
    [fast-cos 1.6880479999999995]
    [fast-exp 1.5170167999999993]
    [fast-isqrt 0.5076020999999997]
    [fast-log 1.4533429000000002]
    [fast-sin 1.7473728999999996]
    [fast-tan 2.0419929999999993]
    [fast-tanh 1.3641086999999992]
    [fdim 0.8039344999999998]
    [floor 0.2765429999999999]
    [fma 0.3970890999999998]
    [fmax 0.22084710000000066]
    [fmin 0.2251020999999998]
    [fmod 1.6331548000000002]
    [hypot 1.2162542000000003]
    [lgamma 2.0076899999999993]
    [log 0.6504712000000005]
    [log10 1.0529677999999993]
    [log1p 1.2597827]
    [log2 0.7819006999999998]
    [logb 0.3418851]
    [neg 0.14364100000000005]
    [pow 2.2531711999999993]
    [remainder 1.317084299999999]
    [rint 0.27493520000000016]
    [round 0.7903963]
    [sin 4.5576493]
    [sinh 1.5770246]
    [sqrt 0.32110629999999996]
    [tan 5.924448999999998]
    [tanh 1.0494703000000005]
    [tgamma 2.6730281999999996]
    [trunc 0.27238209999999974]
))

(define tunable
  (with-terminal-cost ([binary64 fl-move-cost])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real)
         (neg acos acosh asin asinh atan atanh cbrt ceil cos cosh
          erf erfc exp exp2 expm1 fabs floor lgamma log log10 log2
          log1p logb rint round sin sinh sqrt tan tanh tgamma trunc      
          fast-exp fast-sin fast-cos fast-tan fast-tanh fast-log fast-asin
          fast-acos fast-atan fast-isqrt
          )]
        [(real real real)
         (+ - * / atan2 copysign fdim fmax fmin fmod hypot pow remainder)]
        [(real real real real)
         (fma)]))))

(register-platform! 'vdt
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

(module test racket/base)
