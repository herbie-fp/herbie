#lang racket

;;; The default platform:
;;; C/C++ on Linux with a full libm

(require "../plugin.rkt")

(define fl-move-cost 0.1522809999999996)
(define move-cost fl-move-cost)

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
  #:literals ([binary64 fl-move-cost])
  #:optional
  [*.64 0.284122]
            [+.64 0.21685800000000022]
            [-.64 0.2448160000000003]
            [/.64 0.3915630999999998]
            [acos.64 0.8382765000000001]
            [acosh.64 1.0949965999999995]
            [asin.64 0.8879643999999998]
            [asinh.64 1.3600788]
            [atan.64 1.4839419]
            [atan2.64 5.758423799999999]
            [atanh.64 0.7060143]
            [cbrt.64 2.2140749]
            [ceil.64 0.4065622999999997]
            [copysign.64 0.23500700000000005]
            [cos.64 4.814481299999999]
            [cosh.64 1.3792117999999995]
            [erf.64 1.1157903000000002]
            [erfc.64 0.8191463000000002]
            [exp.64 1.7585839]
            [exp2.64 1.2538337]
            [expm1.64 1.0743543999999998]
            [fabs.64 0.15341399999999988]
            [fdim.64 0.9574675000000002]
            [floor.64 0.40936829999999985]
            [fma.64 0.4402650999999995]
            [fmax.64 0.24361820000000006]
            [fmin.64 0.2473790000000003]
            [fmod.64 1.9713020000000003]
            [hypot.64 1.3146293999999996]
            [lgamma.64 2.270068099999999]
            [log.64 0.6301383000000002]
            [log10.64 1.1023494999999996]
            [log1p.64 1.4341258000000001]
            [log2.64 0.7708615]
            [logb.64 0.484051]
            [neg.64 0.1515359999999996]
            [pow.64 2.5363692]
            [remainder.64 1.5360727999999997]
            [rint.64 0.36840199999999984]
            [round.64 0.9615382000000002]
            [sin.64 4.732287400000001]
            [sinh.64 1.7850157999999996]
            [sqrt.64 0.38716720000000004]
            [tan.64 6.046834999999998]
            [tanh.64 1.2760664999999998]
            [tgamma.64 2.8195002000000007]
            [trunc.64 0.3390401999999998])

(register-platform! 'c (platform-union boolean-platform non-tunable tunable))

;; Do not run this file during testing
(module test racket/base
  )
