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
    [* 0.9108103999999999]
    [+ 0.7606279]
    [- 0.8476333]
    [/ 0.7545954]
    [abs2 0.6941215000000001]
    [acos 0.4508723999999999]
    [acosd 0.4352463999999999]
    [acosh 0.48062459999999996]
    [acot 0.5061349999999999]
    [acotd 0.5731811]
    [acoth 0.5051881]
    [acsc 0.4443561]
    [acscd 0.5170174000000001]
    [acsch 0.6457511]
    [asec 0.5122041]
    [asecd 0.45039539999999995]
    [asech 85.3786342]
    [asin 0.4431943999999999]
    [asind 0.4730577]
    [asinh 0.5122394]
    [atan 0.5224863]
    [atand 0.4885352]
    [atanh 0.5603445]
    [cbrt 0.4718977]
    [cosd 0.6319440999999999]
    [cosh 0.505257]
    [cospi 0.6680766]
    [cot 0.7091083000000001]
    [cotd 0.9148482]
    [coth 0.6974949]
    [csc 0.5568723999999999]
    [cscd 0.6518005]
    [csch 0.7198664]
    [deg2rad 0.4272582]
    [exp 0.6374896999999999]
    [exp10 0.516925]
    [exp2 0.5348691000000001]
    [expm1 0.6206389999999999]
    [fabs 0.5705577]
    [hypot 1.0935298999999998]
    [log 0.6278926]
    [log10 0.4895154]
    [log1p 0.46211380000000013]
    [log2 0.6277772]
    [rad2deg 0.432775]
    [sec 0.5584011]
    [secd 0.6223377]
    [sech 0.5427124999999999]
    [sind 0.7025140999999999]
    [sinh 0.7491019]
    [sinpi 0.5681352]
    [sqrt 0.42201829999999996]
    [tand 1.0242314999999997]
    [tanh 0.5102909999999999]))

(define tunable
  (with-terminal-cost ([binary64 1])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real)
         (sind cosd tand sinpi cospi sinh cosh tanh asin acos atan
          asind acosd atand sec csc cot secd cscd cotd asec acsc
          acot asecd acscd acotd sech csch coth asinh acosh atanh
          asech acsch acoth deg2rad rad2deg log log2 log10 log1p
          exp exp2 exp10 expm1 fabs abs2 sqrt cbrt)]
        [(real real real) (+ - * / hypot)]))))

(register-platform! 'julia
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

;; Do not run this file during testing
(module test racket/base)
