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

(define move-cost 10.931663100000002)

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
    [* 15.708498400000002]
    [+ 13.7959101]
    [- 14.659079700000001]
    [/ 13.113469899999998]
    [abs2 17.017488]
    [acos 151.21063859999998]
    [acosd 308.1131564]
    [acosh 53.978982200000004]
    [acot 73.7560483]
    [acotd 241.03336940000003]
    [acoth 101.24275730000001]
    [acsc 106.839862]
    [acscd 277.20705610000005]
    [acsch 65.2542035]
    [asec 162.21150190000003]
    [asecd 340.2302114]
    [asech 69.72012576561863]
    [asin 99.9026426]
    [asind 270.7085671]
    [asinh 58.884606299999994]
    [atan 69.70709630000002]
    [atand 235.6862228]
    [atanh 95.6369342]
    [cbrt 497.56571159999993]
    [cos 603.8291484999999]
    [cosd 434.8312848999999]
    [cosh 271.0744469]
    [cospi 279.7672972000001]
    [cot 656.5981756]
    [cotd 804.4701259999999]
    [coth 193.96718070000003]
    [csc 571.4752729999999]
    [cscd 445.50947700000006]
    [csch 365.5275911]
    [deg2rad 83.10589]
    [exp 276.5370742]
    [exp10 298.66386220000004]
    [exp2 297.3495761]
    [expm1 49.4678521]
    [fabs 9.619785400000001]
    [fma 29.515927899999998]
    [fmax 36.0594987]
    [fmin 18.837665899999998]
    [hypot 393.5420137]
    [log 18.9821674]
    [log10 279.0876292]
    [log1p 289.0488997]
    [log2 19.4423722]
    [neg 11.227118]
    [pow 13.081749045587244]
    [rad2deg 34.972950100000006]
    [sec 588.5591283]
    [secd 425.89129409999987]
    [sech 273.64722770000003]
    [sin 592.4867512]
    [sind 456.2358072999999]
    [sinh 393.37429920000005]
    [sinpi 281.9139477]
    [sqrt 16.022606600000003]
    [tan 674.8650769]
    [tand 849.3879987]
    [tanh 184.06018369999998]))

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
