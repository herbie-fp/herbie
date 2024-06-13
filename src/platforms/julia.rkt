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

(define move-cost 0.2208807)

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
[* 0.5447542000000001]
[+ 0.6166463]
[- 0.6105723]
[/ 0.5462448]
[abs2 0.32610140000000004]
[acos 0.36351730000000004]
[acosd 0.35786200000000007]
[acosh 0.3585075000000001]
[acot 0.39934600000000003]
[acotd 0.38522219999999996]
[acoth 0.43155910000000003]
[acsc 0.3669918]
[acscd 0.399751]
[acsch 0.43550130000000004]
[asec 0.337201]
[asecd 0.3469477]
[asech 0.46535892200898327]
[asin 0.38236859999999995]
[asind 0.3357566]
[asinh 0.3925379]
[atan 0.36676780000000003]
[atand 0.41467470000000006]
[atanh 0.34990829999999995]
[cbrt 0.3868153]
[copysign 0.8972391]
[cos 0.7025395999999999]
[cosd 0.4983631]
[cosh 0.42951259999999997]
[cospi 0.45632769999999995]
[cot 0.47890449999999996]
[cotd 0.7434820000000001]
[coth 0.4427633]
[csc 0.5815001000000001]
[cscd 0.5604461]
[csch 0.5040319999999999]
[deg2rad 0.3485081]
[exp 0.4563072999999999]
[exp10 0.41967790000000005]
[exp2 0.4041673]
[expm1 0.44793209999999994]
[fabs 0.3241444]
[fma 1.1955953999999998]
[fmax 0.6028575]
[fmin 0.6746226]
[hypot 0.6016439]
[log 0.37690609999999997]
[log10 0.3757026]
[log1p 0.3775817]
[log2 0.3770783999999999]
[neg 0.35563829999999996]
[pow 1.1137591]
[rad2deg 0.40503820000000007]
[sec 0.4649640999999999]
[secd 0.49238009999999993]
[sech 0.4638438]
[sin 0.4722583]
[sind 0.5011878000000001]
[sinh 0.6072705999999999]
[sinpi 0.4872167000000001]
[sqrt 0.32118800000000003]
[tan 0.49434639999999996]
[tand 0.7728290999999999]
[tanh 0.4061591]))

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
