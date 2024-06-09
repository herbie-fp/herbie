#lang racket

;;; Julia

(require "../plugin.rkt")

;; --------------------------------------------------------
;; Accelerators

(define-accelerator (sinpi real) real
  (λ (x) (sin (* (* x (PI)) (/ (PI) 180)))))  
(define-accelerator (cospi real) real
  (λ (x) (cos (* (* x (PI)) (/ (PI) 180)))))

(define-accelerator (sec real) real
  (λ (x) (/ 1 (cos x))))
(define-accelerator (csc real) real
  (λ (x) (/ 1 (sin x))))
(define-accelerator (cot real) real
  (λ (x) (/ 1 (tan x))))

(define-accelerator (asec real) real
  (λ (x) (acos (/ 1 x))))
(define-accelerator (acsc real) real
  (λ (x) (asin (/ 1 x))))
(define-accelerator (acot real) real
  (λ (x) (atan (/ 1 x))))

(define-accelerator (sind real) real
  (λ (x) (sin (* x (/ (PI) 180)))))
(define-accelerator (cosd real) real
  (λ (x) (cos (* x (/ (PI) 180)))))
(define-accelerator (tand real) real
  (λ (x) (tan (* x (/ (PI) 180)))))

(define-accelerator (asind real) real
  (λ (x) (* (asin x) (/ 180 (PI)))))
(define-accelerator (acosd real) real
  (λ (x) (* (acos x) (/ 180 (PI)))))
(define-accelerator (atand real) real
  (λ (x) (* (atan x) (/ 180 (PI)))))

(define-accelerator (secd real) real
  (λ (x) (sec (* x (/ (PI) 180)))))
(define-accelerator (cscd real) real
  (λ (x) (csc (* x (/ (PI) 180)))))
(define-accelerator (cotd real) real
  (λ (x) (cot (* x (/ (PI) 180)))))

(define-accelerator (asecd real) real
  (λ (x) (* (asec x) (/ 180 (PI)))))
(define-accelerator (acscd real) real
  (λ (x) (* (acsc x) (/ 180 (PI)))))
(define-accelerator (acotd real) real
  (λ (x) (* (acot x) (/ 180 (PI)))))

(define-accelerator (sech real) real
  (λ (x) (/ 1 (cosh x))))
(define-accelerator (csch real) real
  (λ (x) (/ 1 (sinh x))))
(define-accelerator (coth real) real
  (λ (x) (/ 1 (tanh x))))  

(define-accelerator (asech real) real
  (λ (x) (acosh (/ 1 x))))
(define-accelerator (acsch real) real
  (λ (x) (asinh (/ 1 x))))
(define-accelerator (acoth real) real
  (λ (x) (atanh (/ 1 x))))

(define-accelerator (abs2 real) real
  (λ (x) (* (fabs x) (fabs x))))

(define-accelerator (exp10 real) real
  (λ (x) (pow 10 x)))

;; --------------------------------------------------------
;; Acclerator impls


(define-accelerator-impl sinpi sinpi.f64 (binary64) binary64)
(define-accelerator-impl cospi cospi.f64 (binary64) binary64)

(define-accelerator-impl sec csc.f64 (binary64) binary64)
(define-accelerator-impl csc csc.f64 (binary64) binary64)
(define-accelerator-impl cot cot.f64 (binary64) binary64)

(define-accelerator-impl sech csch.f64 (binary64) binary64)
(define-accelerator-impl csch csch.f64 (binary64) binary64)
(define-accelerator-impl coth coth.f64 (binary64) binary64)

(define-accelerator-impl asec acsc.f64 (binary64) binary64)
(define-accelerator-impl acsc acsc.f64 (binary64) binary64)
(define-accelerator-impl acot acot.f64 (binary64) binary64)

(define-accelerator-impl asech acsch.f64 (binary64) binary64)
(define-accelerator-impl acsch acsch.f64 (binary64) binary64)
(define-accelerator-impl acoth acoth.f64 (binary64) binary64)

(define-accelerator-impl sind sind.f64 (binary64) binary64)
(define-accelerator-impl cosd cosd.f64 (binary64) binary64)
(define-accelerator-impl tand tand.f64 (binary64) binary64)

(define-accelerator-impl acosd acosd.f64 (binary64) binary64)
(define-accelerator-impl asind acosd.f64 (binary64) binary64)
(define-accelerator-impl atand atand.f64 (binary64) binary64)

(define-accelerator-impl secd secd.f64 (binary64) binary64)
(define-accelerator-impl cscd cscd.f64 (binary64) binary64)
(define-accelerator-impl cotd cotd.f64 (binary64) binary64)

(define-accelerator-impl asecd asecd.f64 (binary64) binary64)
(define-accelerator-impl acscd acscd.f64 (binary64) binary64)
(define-accelerator-impl acotd acotd.f64 (binary64) binary64)

(define-accelerator-impl abs2 abs2.f64 (binary64) binary64)
(define-accelerator-impl exp10 exp10.f64 (binary64) binary64)

;; --------------------------------------------------------
;; Platform

(define move-cost 0.4568785)

(define boolean-platform
  (with-terminal-cost ([bool move-cost])
    (platform
      #:default-cost move-cost
      #:if-cost move-cost
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
    #:default-cost 1
    [* 0.7701317]
    [+ 0.8566298]
    [- 0.8459957000000001]
    [/ 0.7320745000000002]
    [abs2 0.42968130000000004]
    [acos 0.477469]
    [acosd 0.4321086]
    [acosh 0.502756]
    [acot 0.5054978]
    [acotd 0.4880763]
    [acoth 0.46290109999999995]
    [acsc 0.4621246]
    [acscd 0.4802612]
    [acsch 0.5234956]
    [asec 0.4549755999999999]
    [asecd 0.6062093999999999]
    [asech 78.9220196]
    [asin 0.44455980000000006]
    [asind 0.44217209999999996]
    [asinh 0.5107948]
    [atan 0.4703429]
    [atand 0.5097086]
    [atanh 0.47477110000000006]
    [cbrt 0.44662120000000005]
    [cos 0.5582415000000001]
    [cosd 0.6317027]
    [cosh 0.5159257]
    [cospi 0.6751872999999999]
    [cot 0.840412]
    [cotd 0.9100374]
    [coth 0.8629778]
    [csc 0.6365002]
    [cscd 0.6306383]
    [csch 0.6098616]
    [deg2rad 0.4535474]
    [exp 0.6127838]
    [exp10 0.5191638000000001]
    [exp2 0.5845414]
    [expm1 0.5346856000000001]
    [fabs 0.4760615]
    [fma 1.0359734]
    [fmax 0.7180883]
    [fmin 0.7023032]
    [hypot 1.1392644]
    [log 0.6012145999999999]
    [log10 0.49181230000000004]
    [log1p 0.4487176999999999]
    [log2 0.5976406000000001]
    [neg 0.4809193999999999]
    [pow 17.962714]
    [rad2deg 0.45006129999999994]
    [sec 0.7041352999999999]
    [secd 0.614807]
    [sech 0.5108573]
    [sin 0.6321362]
    [sind 0.6182961]
    [sinh 0.6341463999999999]
    [sinpi 1.1937567]
    [sqrt 0.4448313]
    [tan 0.7483234000000001]
    [tand 0.8997918]
    [tanh 0.5253689]))

(define tunable
  (with-terminal-cost ([binary64 move-cost])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real)
         (neg sin cos tan sinpi cospi
          sind cosd tand sinh cosh tanh asin acos atan
          asind acosd atand sec csc cot secd cscd cotd asec acsc
          acot asecd acscd acotd sech csch coth asinh acosh atanh
          asech acsch acoth deg2rad rad2deg log log2 log10 log1p
          exp exp2 exp10 expm1 fabs abs2 sqrt cbrt)]
        [(real real real) (+ - * / hypot fmin fmax pow)]
        [(real real real real) (fma)]))))

(register-platform! 'julia
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

;; Do not run this file during testing
(module test racket/base)
