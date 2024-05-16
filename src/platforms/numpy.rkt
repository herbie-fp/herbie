#lang racket

;;; Numpy v1.0

(require "../plugin.rkt")

;; --------------------------------------------------------
;; Operators


  (define-accelerator (deg2rad real) real
  (λ (x) (/ (* x (PI)) 180)))

  (define-accelerator (rad2deg real) real
  (λ (x) (/ (* x 180) (PI))))

  (define-accelerator (logaddexp real real) real
  (λ (x y) (log (+ (exp x) (exp y)))))

  (define-accelerator (logaddexp2 real real) real
  (λ (x y) (log2 (+ (exp x) (exp y)))))

   (define-accelerator (square real) real
  (λ (x) (* x x)))

(define-accelerator-impl logaddexp logaddexp.f64 (binary64 binary64) binary64)
(define-accelerator-impl logaddexp2 logaddexp2.f64 (binary64 binary64) binary64)
(define-accelerator-impl square square.f64 (binary64) binary64)
(define-accelerator-impl rad2deg rad2deg.f64 (binary64) binary64)
(define-accelerator-impl deg2rad deg2rad.f64 (binary64) binary64)

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

(define tunable
  (with-terminal-cost ([binary64 1])
    (platform-product
      #:optional
      [([real binary64])(cost-map #:default-cost 1)]
      (operator-set
        [(real real)    
         (sin cos tan asin acos atan hypot sinh
          cosh tanh asinh acosh atanh round floor 
          ceil trunc exp expm1 exp2 log log10 log2
          log1p recip sqrt cbrt fabs neg ;add rint back in
          square deg2rad rad2deg)]
          
        [(real real real)
         (+ - * / atan2 copysign fmax fmin fmod pow remainder
         logaddexp logaddexp2)]))))

(register-platform! 'numpy
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))

;; Do not run this file during testing
(module test racket/base)
