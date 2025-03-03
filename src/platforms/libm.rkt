#lang racket

;;; The default platform:
;;; C/C++ on Linux with a full libm

(require "../plugin.rkt")

(define 64bit-move-cost 0.12538399999999972)
(define 32bit-move-cost 0.12961999999999974)
(define boolean-move-cost 0.1) ; should be okay?

; universal boolean operations
(define-platform boolean-platform
                 #:literal [bool boolean-move-cost]
                 #:default-cost boolean-move-cost
                 #:if-cost boolean-move-cost
                 TRUE
                 FALSE
                 not
                 and
                 or)

; non-tunable operations
(define-platform non-tunable-64bit
                 #:literal [binary64 64bit-move-cost]
                 #:default-cost 64bit-move-cost
                 PI.f64
                 E.f64
                 INFINITY.f64
                 NAN.f64
                 ==.f64
                 !=.f64
                 >.f64
                 <.f64
                 >=.f64
                 <=.f64)

(define-platform non-tunable-32bit
                 #:literal [binary32 32bit-move-cost]
                 #:default-cost 32bit-move-cost
                 PI.f32
                 E.f32
                 INFINITY.f32
                 NAN.f32
                 ==.f32
                 !=.f32
                 >.f32
                 <.f32
                 >=.f32
                 <=.f32)

; tunable operations
(define-platform tunable-64bit
                 #:literal [binary64 64bit-move-cost]
                 #:optional [*.f64 0.24512299999999976]
                 [+.f64 0.2174189999999998]
                 [-.f64 0.20265700000000008]
                 [/.f64 0.2962459999999998]
                 [acos.f64 0.5283240000000002]
                 [acosh.f64 0.929629]
                 [asin.f64 0.592536]
                 [asinh.f64 1.120752]
                 [atan.f64 1.1491229999999997]
                 [atan2.f64 1.8138300000000002]
                 [atanh.f64 0.4773569999999998]
                 [cbrt.f64 1.8992809999999996]
                 [ceil.f64 0.2699039999999999]
                 [copysign.f64 0.23862799999999962]
                 [cos.f64 3.9988340000000006]
                 [cosh.f64 1.3327939999999996]
                 [erf.f64 1.0642519999999996]
                 [erfc.f64 0.8588620000000002]
                 [exp.f64 1.270031]
                 [exp2.f64 1.0709029999999997]
                 [expm1.f64 0.8483490000000002]
                 [fabs.f64 0.14942199999999971]
                 [fdim.f64 0.7254930000000001]
                 [floor.f64 0.27001899999999957]
                 [fma.f64 0.37174700000000026]
                 [fmax.f64 0.2480699999999998]
                 [fmin.f64 0.26680799999999994]
                 [fmod.f64 1.5324479999999996]
                 [hypot.f64 1.498331]
                 [lgamma.f64 2.1097169999999998]
                 [log.f64 0.719641]
                 [log10.f64 1.0994940000000002]
                 [log1p.f64 1.2416829999999997]
                 [log2.f64 0.8050260000000001]
                 [logb.f64 0.33284199999999986]
                 [neg.f64 0.12114199999999964]
                 [pow.f64 1.860082]
                 [remainder.f64 0.9494380000000005]
                 [rint.f64 0.3101590000000003]
                 [round.f64 0.7762879999999997]
                 [sin.f64 3.979444]
                 [sinh.f64 1.5345409999999998]
                 [sqrt.f64 0.2225119999999998]
                 [tan.f64 4.3931]
                 [tanh.f64 0.9856559999999996]
                 [tgamma.f64 2.4006499999999997]
                 [trunc.f64 0.2714409999999996])

; tunable operations
(define-platform tunable-32bit
                 #:literal [binary32 32bit-move-cost]
                 #:optional [*.f32 0.256602]
                 [+.f32 0.200445]
                 [-.f32 0.19106800000000014]
                 [/.f32 0.3465330000000001]
                 [acos.f32 0.5213330000000001]
                 [acosh.f32 0.847705]
                 [asin.f32 0.4802670000000002]
                 [asinh.f32 1.1207249999999999]
                 [atan.f32 1.1015179999999995]
                 [atan2.f32 1.9559129999999997]
                 [atanh.f32 0.4652830000000002]
                 [cbrt.f32 1.946652]
                 [ceil.f32 0.287118]
                 [copysign.f32 0.2042799999999998]
                 [cos.f32 4.2738829999999994]
                 [cosh.f32 1.32949]
                 [erf.f32 1.1211180000000001]
                 [erfc.f32 0.907758]
                 [exp.f32 1.3860560000000002]
                 [exp2.f32 1.1716009999999999]
                 [expm1.f32 0.906484]
                 [fabs.f32 0.12464599999999992]
                 [fdim.f32 0.7635619999999999]
                 [floor.f32 0.2959660000000004]
                 [fma.f32 0.38934]
                 [fmax.f32 0.23636500000000005]
                 [fmin.f32 0.24126899999999996]
                 [fmod.f32 1.7182470000000002]
                 [hypot.f32 1.6816069999999997]
                 [lgamma.f32 2.2203209999999998]
                 [log.f32 0.778197]
                 [log10.f32 1.1793579999999997]
                 [log1p.f32 1.302969]
                 [log2.f32 0.8644160000000004]
                 [logb.f32 0.36221]
                 [neg.f32 0.11567699999999992]
                 [pow.f32 2.028296]
                 [remainder.f32 1.030245]
                 [rint.f32 0.29316799999999997]
                 [round.f32 0.8677139999999997]
                 [sin.f32 4.2185290000000003]
                 [sinh.f32 1.7463029999999996]
                 [sqrt.f32 0.25464700000000007]
                 [tan.f32 4.669173000000001]
                 [tanh.f32 1.0567220000000002]
                 [tgamma.f32 2.6280819999999994]
                 [trunc.f32 0.28765399999999997])

(register-platform!
 'c
 (platform-union boolean-platform non-tunable-32bit tunable-32bit non-tunable-64bit tunable-64bit))

;; Do not run this file during testing
(module test racket/base
  )
