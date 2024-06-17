#lang racket

(require fpbench)
(require "../plugin.rkt")

(define move-cost 0.13377709999999982)
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
[* 0.25015619999999983]
[+ 0.17550119999999972]
[- 0.1863999999999999]
[/ 0.4866432999999999]
[acc-log 1.3495818]
[acos 0.5841043000000002]
[acosh 0.9308504999999998]
[asin 0.8347114000000001]
[asinh 1.4181987999999999]
[atan 1.2569576000000002]
[atan2 6.1162081]
[atanh 0.6125212000000001]
[cbrt 2.0813162]
[ceil 0.3584184000000004]
[copysign 0.23615720000000007]
[cos 4.7159444]
[cosh 1.4785465000000002]
[erf 1.4863018999999995]
[erfc 0.9719807000000005]
[exp 1.8582009]
[exp2 1.2970616999999998]
[expm1 1.2271442000000001]
[fabs 0.13524300000000017]
[fdim 1.0243344]
[floor 0.33133219999999997]
[fma 0.4888761999999998]
[fmax 0.25767210000000007]
[fmin 0.2599392]
[fmod 2.119084]
[hypot 1.6391728]
[lgamma 2.3484122000000007]
[log 1.2594495999999997]
[log10 1.3025845999999999]
[log1p 0.21464099999999958]
[log1pmd 1.3102118]
[log2 1.0316424000000002]
[logb 0.38891509999999985]
[neg 0.14212699999999998]
[pow 2.780106000000001]
[remainder 1.8366317999999997]
[rint 0.3226200999999998]
[round 1.1786984999999999]
[sin 4.830986400000001]
[sinh 1.9668339]
[sqrt 0.39881209999999995]
[tan 6.080997099999999]
[tanh 1.3360257999999994]
[tgamma 2.9566866]
[trunc 0.3288633000000001]
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
