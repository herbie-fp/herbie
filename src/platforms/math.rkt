#lang racket

;;; C/C++ on Linux with reduced libm
;;; We use textbook mathematical operators, i.e.,
;;; no special numbers functions

(require "../plugin.rkt")

(define move-cost 0.02333600000000001)
(define fl-move-cost (* move-cost 4))

; universal boolean operations
(define-platform boolean-platform
  #:literals ([bool move-cost])
  #:default-cost move-cost 
  #:if-cost move-cost TRUE FALSE not and or)

; non-tunable operations
(define-platform non-tunable
  #:literals ([binary64 fl-move-cost])
  #:default-cost fl-move-cost
  PI.64 E.64 INFINITY.64 NAN.64 ==.64 !=.64 >.64 <.64 >=.64 <=.64)

; tunable operations
(define-platform tunable
  #:literals([binary64 fl-move-cost])
  #:optional
  [*.64 0.20874800000000002]
            [+.64 0.164604]
            [-.64 0.15163999999999997]
            [/.64 0.26615199999999994]
            [acos.64 0.357748]
            [acosh.64 0.6594719999999998]
            [asin.64 0.389788]
            [asinh.64 0.8350280000000001]
            [atan.64 0.8375199999999997]
            [atan2.64 1.4928039999999998]
            [atanh.64 0.3623799999999999]
            [cbrt.64 1.5651760000000001]
            [ceil.64 0.4729999999999999]
            [copysign.64 0.20045199999999996]
            [cos.64 3.3228800000000005]
            [cosh.64 0.9538959999999999]
            [erf.64 0.8064360000000002]
            [erfc.64 0.816512]
            [exp.64 1.0806]
            [exp2.64 0.825484]
            [fabs.64 0.10161999999999997]
            [fdim.64 0.592576]
            [floor.64 0.46856799999999993]
            [fmax.64 0.3106]
            [fmin.64 0.289256]
            [fmod.64 94.277144]
            [lgamma.64 1.568012]
            [log.64 0.5057240000000001]
            [log10.64 0.868856]
            [log2.64 0.6812760000000003]
            [logb.64 0.22065599999999996]
            [neg.64 0.09659199999999998]
            [pow.64 1.52482]
            [remainder.64 16.165012]
            [rint.64 0.12186400000000001]
            [round.64 0.6585639999999999]
            [sin.64 3.3181279999999997]
            [sinh.64 1.2095399999999996]
            [sqrt.64 0.19187200000000001]
            [tan.64 3.7109039999999998]
            [tanh.64 0.8240159999999999]
            [tgamma.64 1.8825760000000002]
            [trunc.64 0.46364400000000006])

(register-platform! 'math (platform-union boolean-platform non-tunable tunable))

;; Do not run this file during testing
(module test racket/base
  )
