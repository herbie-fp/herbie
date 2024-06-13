#lang racket

(require fpbench)
(require "../plugin.rkt")

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
    [acc-log 0.11309]
    [log1pmd 0.10454]
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
    [trunc 0.025449999999999966]))

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
          log1pmd acc-log)]
        [(real real real)
         (+ - * / atan2 copysign fdim fmax fmin fmod hypot pow remainder)]
        [(real real real real)
         (fma)]))))

(register-platform! 'fdlibm
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

(set-unknown->c!
 (lambda (previous)
   (lambda (ctx op args)
     (previous ctx
               (match op
                 ['acc-log "log"]
                 [_ op])
               args))))

(module test racket/base)
