#lang racket

;;; Python 3.10

(require "../plugin.rkt")

;; --------------------------------------------------------
;; Operators

(define-accelerator (sind real) real
  (λ (x) (sin (* x (/ pi 180)))))
(define-accelerator (cosd real) real
  (λ (x) (cos (* x (/ pi 180)))))
(define-accelerator (tand real) real
  (λ (x) (tan (* x (/ pi 180)))))
;_
(define-accelerator (sinpi real) real
  (λ (x) (sin (* (* x pi) (/ pi 180)))))  
(define-accelerator (cospi real) real
  (λ (x) (cos (* (* x pi) (/ pi 180)))))

(define-accelerator (sinh real) real
  (λ (x) (sinh x)))
(define-accelerator (cosh real) real
  (λ (x) (cosh x)))
(define-accelerator (tanh real) real
  (λ (x) (tanh x)))  

(define-accelerator (asin real) real
  (λ (x) (asin x)))
(define-accelerator (acos real) real
  (λ (x) (acos x)))
(define-accelerator (atan real) real
  (λ (x) (atan x)))

(define-accelerator (asind real) real
  (λ (x) (* (asin x) (/ 180 pi))))
(define-accelerator (acosd real) real
  (λ (x) (* (acos x) (/ 180 pi))))
(define-accelerator (atand real) real
  (λ (x) (* (atan x) (/ 180 pi))))

(define-accelerator (sec real) real
  (λ (x) (sec x)))
(define-accelerator (csc real) real
  (λ (x) (csc x)))
(define-accelerator (cot real) real
  (λ (x) (cot x)))

(define-accelerator (secd real) real
  (λ (x) (sec (* x (/ pi 180)))))
(define-accelerator (cscd real) real
  (λ (x) (csc (* x (/ pi 180)))))
(define-accelerator (cotd real) real
  (λ (x) (cot (* x (/ pi 180)))))

(define-accelerator (asec real) real
  (λ (x) (asec x)))
(define-accelerator (acsc real) real
  (λ (x) (acsc x)))
(define-accelerator (acot real) real
  (λ (x) (acot x)))

(define-accelerator (asecd real) real
  (λ (x) (* (asec x) (/ 180 pi))))
(define-accelerator (acscd real) real
  (λ (x) (* (acsc x) (/ 180 pi))))
(define-accelerator (acotd real) real
  (λ (x) (* (acot x) (/ 180 pi))))

(define-accelerator (sech real) real
  (λ (x) (sech x)))
(define-accelerator (csch real) real
  (λ (x) (csch x)))
(define-accelerator (coth real) real
  (λ (x) (coth x)))    

(define-accelerator (asinh real) real
  (λ (x) (log (+ x (sqrt (+ (* x x) 1))))))
(define-accelerator (acosh real) real
  (λ (x) (log (+ x (sqrt (- (* x x) 1))))))
(define-accelerator (atanh real) real
  (λ (x) (/ (log (/ (+ 1 (sinh (* 2 x))) (- 1 (sinh (* 2 x))))) 2)))    

(define-accelerator (asech real) real
  (λ (x) (/ (log (/ (+ 1 (sqrt (- 1 (* x x)))) x)))))
(define-accelerator (acsch real) real
  (λ (x)  (/ (log (/ (+ 1 (sqrt (+ 1 (* x x)))) x)))))
(define-accelerator (acoth real) real
  (λ (x) (/ (log (/ (+ 1 x) (- 1 x))) 2)))    

(define-accelerator (deg2rad real) real
  (λ (x) (* (/ x 180) pi)))  
(define-accelerator (rad2deg real) real
  (λ (x) (* (/ x pi) 180)))

(define-accelerator (hypot real) real
  (λ (x) (hypot x)))

(define-accelerator (log real) real
  (λ (x) (log x)))
(define-accelerator (log2 real) real
  (λ (x) (log x 2)))
(define-accelerator (log10 real) real
  (λ (x) (log x 10)))
(define-accelerator (log1p real) real
  (λ (x) (log (+ x 1))))

(define-accelerator (exp real) real
  (λ (x) (exp x)))
(define-accelerator (exp2 real) real
  (λ (x) (expt 2 x)))
(define-accelerator (exp10 real) real
  (λ (x) (expt 10 x)))
(define-accelerator (ldexp real real) real
  (λ (x y) (* x (expt 2 y))))
(define-accelerator (expm1 real real) real
  (λ (x y) (- (exp x) 1)))

(define-accelerator (abs real) real
  (λ (x) (abs x)))
(define-accelerator (abs2 real) real
  (λ (x) (* (abs x) (abs x))))

(define-accelerator (sqrt real) real
  (λ (x)  (sqrt x)))
(define-accelerator (cbrt real) real
  (λ (x )  (if (negative? x)
                (- (expt (abs x) (/ 1 3)))
                (expt x (/ 1 3)))))

(define-accelerator (powermod real) real
  (λ (x)  (cond
    ((= m 1) 0)
    ((= b 0) 1)
    (else (remainder (expt a b) m)))))

;; --------------------------------------------------------
;; Platform

(define boolean-platform
  (with-terminal-cost ([bool 1])
    (platform
      #:default-cost 1
      #:if-cost 1
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

; non-tunable operations
(define non-tunable
  (with-terminal-cost ([binary64 1])
    (platform-product
      [([real binary64] [bool bool])
       (cost-map #:default-cost 1)]
      (operator-set
        [(real) (PI E INFINITY NAN)]
        [(real real bool) (== != > < >= <=)]))))

(define cost-model 
  (cost-map #:default-cost 1))
(define tunable
  (with-terminal-cost ([binary64 1])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real)
         (sind cosd tand sinpi cospi sinh cosh tanh asin acos atan asind acosd atand sec csc cot secd cscd cotd asec acsc acot asecd acscd acotd sech csch coth asinh acosh atanh asech acsch acoth deg2rad rad2deg hypot log log2 log10 log1p exp exp2 exp10 ldexp expm1 abs abs2 sqrt cbrt powermod)]
        [(real real real)
         (+ - * / ldexp expm1)]))))

(register-platform! 'julia
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))
(displayln "hiiiiiiiiiiiiiiiii")
;; Do not run this file during testing
(module test racket/base)
