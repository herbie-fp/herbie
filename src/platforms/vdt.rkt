#lang racket

(require "../plugin.rkt")

(define move-cost 0.02333600000000001)
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
    [* 0.20874800000000002]
    [+ 0.164604]
    [- 0.15163999999999997]
    [/ 0.26615199999999994]
    [acos 0.357748]
    [acosh 0.6594719999999998]
    [asin 0.389788]
    [asinh 0.8350280000000001]
    [atan 0.8375199999999997]
    [atan2 1.4928039999999998]
    [atanh 0.3623799999999999]
    [cbrt 1.5651760000000001]
    [ceil 0.4729999999999999]
    [copysign 0.20045199999999996]
    [cos 3.3228800000000005]
    [cosh 0.9538959999999999]
    [erf 0.8064360000000002]
    [erfc 0.816512]
    [exp 1.0806]
    [exp2 0.825484]
    [expm1 0.8374999999999999]
    [fabs 0.10161999999999997]
    [fdim 0.592576]
    [floor 0.46856799999999993]
    [fma 0.3366239999999999]
    [fmax 0.3106]
    [fmin 0.289256]
    [fmod 94.277144]
    [hypot 1.175432]
    [lgamma 1.568012]
    [log 0.5057240000000001]
    [log10 0.868856]
    [log1p 0.768932]
    [log2 0.6812760000000003]
    [logb 0.22065599999999996]
    [neg 0.09659199999999998]
    [pow 1.52482]
    [remainder 16.165012]
    [rint 0.12186400000000001]
    [round 0.6585639999999999]
    [sin 3.3181279999999997]
    [sinh 1.2095399999999996]
    [sqrt 0.19187200000000001]
    [tan 3.7109039999999998]
    [tanh 0.8240159999999999]
    [tgamma 1.8825760000000002]
    [trunc 0.46364400000000006]


    ;; [vdt-exp 1]
    ;; [fast-sin 1]
    ;; [fast-cos 1]
    ;; [fast-tan 1]
    ;; [fast-tanh 1]
    ;; [fast-log 1]
    ;; [fast-asin 1]
    ;; [fast-acos 1]
    ;; [fast-atan 1]
    [fast-exp 1]
    [fast-sin 1]
    [fast-cos 1]
    [fast-tan 1]
    [fast-tanh 1]
    [fast-log 1]
    [fast-asin 1]
    [fast-acos 1]
    [fast-atan 1]
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
          
          ;;vdt-exp
          ;; fast-sin fast-cos fast-tan fast-tanh fast-log fast-asin
          ;; fast-acos fast-atan
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
