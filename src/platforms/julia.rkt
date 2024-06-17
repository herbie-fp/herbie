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

(define move-cost 8.853997000000001)

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
[* 9.5106717]
[+ 9.823960700000002]
[- 9.488617800000002]
[/ 9.598798900000002]
[abs2 11.385701399999999]
[acos 11.9424846]
[acosd 11.656213399999999]
[acosh 12.4414174]
[acot 12.3341885]
[acotd 12.4260468]
[acoth 12.195489799999999]
[acsc 11.6382368]
[acscd 11.5901394]
[acsch 13.339254]
[asec 11.696091899999999]
[asecd 11.651059800000002]
[asech 7.247311045349116]
[asin 11.641043100000001]
[asind 11.9058734]
[asinh 12.885667800000002]
[atan 12.478735400000001]
[atand 12.353757300000002]
[atanh 12.150254199999997]
[cbrt 13.175738599999997]
[copysign 9.487941900000001]
[cos 14.549823900000002]
[cosd 13.688523800000002]
[cosh 12.638260999999998]
[cospi 12.895455100000001]
[cot 14.9922017]
[cotd 16.9121991]
[coth 12.934553499999998]
[csc 14.503954699999998]
[cscd 14.0625094]
[csch 13.710520900000002]
[deg2rad 11.497592000000001]
[exp 12.6288872]
[exp10 12.681448200000002]
[exp2 12.93981]
[expm1 12.4753887]
[fabs 11.280543699999999]
[fma 12.888147900000002]
[fmax 10.4350622]
[fmin 10.0536438]
[hypot 10.119838000000003]
[log 12.546868900000002]
[log10 12.7614017]
[log1p 12.284828]
[log2 12.4507217]
[neg 11.550071599999999]
[pow 25.299719300000003]
[rad2deg 11.567879799999996]
[sec 14.457876299999999]
[secd 13.632079200000002]
[sech 12.8347851]
[sin 14.318584699999999]
[sind 14.0453644]
[sinh 13.418012099999999]
[sinpi 13.4222734]
[sqrt 11.2750876]
[tan 14.8165944]
[tand 16.6696167]
[tanh 12.575906100000001]
  ))

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
        [(real real real) (+ - * / hypot fmin fmax pow copysign)]
        [(real real real real) (fma)]))))

(register-platform! 'julia
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

;; Do not run this file during testing
(module test racket/base)
