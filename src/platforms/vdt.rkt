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

(define move-cost  0.0026300000000000047)
(define fl-move-cost (* move-cost 4))

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
    [* 0.019779999999999985]
    [+ 0.014449999999999996]
    [- 0.014110000000000006]
    [/ 0.027380000000000026]
    [acos 0.32046]
    [acosh 0.08731000000000001]
    [asin 0.05579000000000002]
    [asinh 0.12150999999999998]
    [atan 0.2467]
    [atan2 0.19123999999999997]
    [atanh 0.04122999999999999]
    [cbrt 0.19143999999999997]
    [ceil 0.0262]
    [copysign 0.015479999999999989]
    [cos 0.5782700000000001]
    [cosh 0.13248000000000001]
    [erf 0.11446]
    [erfc 0.08851999999999999]
    [exp 0.14546]
    [exp2 0.15739]
    [expm1 0.09623]
    [fabs 0.009410000000000007]
    [fast-acos 0.17734]
    [fast-asin 0.17116999999999996]
    [fast-atan 0.20786000000000002]
    [fast-cos 0.17641000000000004]
    [fast-exp 0.1162]
    [fast-isqrt 0.04412999999999999]
    [fast-log 0.12131999999999998]
    [fast-sin 0.18503999999999995]
    [fast-tan 0.17883]
    [fast-tanh 0.11866999999999997]
    [fdim 0.07086999999999999]
    [floor 0.025419999999999998]
    [fma 0.08671999999999999]
    [fmax 0.017819999999999996]
    [fmin 0.019980000000000032]
    [fmod 0.17450000000000002]
    [hypot 0.14526999999999998]
    [lgamma 0.37424]
    [log 0.05956]
    [log10 0.12073999999999999]
    [log1p 0.12961]
    [log2 0.10091000000000001]
    [logb 0.02410000000000001]
    [neg 0.010460000000000002]
    [pow 0.17928000000000002]
    [remainder 0.07102]
    [rint 0.02481999999999999]
    [round 0.07981]
    [sin 0.41594999999999993]
    [sinh 0.16324999999999998]
    [sqrt 0.02347999999999999]
    [tan 0.53495]
    [tanh 0.11344000000000001]
    [tgamma 0.26131999999999994]
    [trunc 0.025449999999999966]
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
