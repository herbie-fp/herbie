#lang racket

(require "../plugin.rkt"
        "./default.rkt"
         math/flonum)


;;; (define-operator-impl (dotprod.f64 [a : binary64] [b : binary64] [c : binary64] [d : binary64])
;;;                       binary64
;;;                       #:spec  (+ (* a b) (* c d))
;;;                       #:fpcore (! :precision binary64 (dotprod a b c d))
;;;                       #:fl fl2dotprod)

;;; (define-operator-impl (add3.f64 [a : binary64] [b : binary64] [c : binary64])
;;;                       binary64
;;;                       #:spec (+ (+ a b) c)
;;;                       #:fpcore (! :precision binary64 (add3 a b c))
;;;                       #:fl fl2add3)

(define-operator-impl (ratio-of-squares.f64 [z0 : binary64] [z1 : binary64])
                      binary64
                      #:spec (/ (* z0 z0) (* z1 z1))
                      #:fpcore (! :precision binary64 (ratio-of-squares z0 z1)))

(define-operator-impl (ratio-square-sum.f64 [z0 : binary64] [z1 : binary64])
                      binary64
                      #:spec (/ (* z0 z0) (+ z0 z1))
                      #:fpcore (! :precision binary64 (ratio-square-sum z0 z1))
                      )            

(define-operator-impl (sqrt-product.f64 [z0 : binary64] [z1 : binary64])
                      binary64
                      #:spec (sqrt (* (+ z0 z1) (- z0 z1)))
                      #:fpcore (! :precision binary64 (sqrt-product z0 z1))
                      )            

(define-operator-impl (log1m.f64 [z0 : binary64])
                      binary64
                      #:spec (log (- 1 z0))
                      #:fpcore (! :precision binary64 (log1m z0))
                      )     

(define-operator-impl (expnp1.f64 [z0 : binary64])
                      binary64
                      #:spec (+ (exp (neg z0)) 1)
                      #:fpcore (! :precision binary64 (expnp1 z0))
                      )     

(define-operator-impl (pow2/3s.f64 [z0 : binary64])
                      binary64
                      #:spec (sqrt (/ 1 (pow z0 3)))
                      #:fpcore (! :precision binary64 (pow2/3s z0))
                      ) 


(define-platform new-accelerator-platform
                 #:literal [binary64 64]
                 #:default-cost 3200
                 ratio-of-squares.f64
                 ratio-square-sum.f64
                 sqrt-product.f64
                 log1m.f64
                 expnp1.f64
                 pow2/3s.f64) 


(define no-accel-platform
(platform-union boolean-platform
                  machine-platform
                  libm64-platform))

(define new-accel-platform
(platform-union boolean-platform
                  machine-platform
                  libm64-platform
                  new-accelerator-platform))

(register-platform! 'noaccel no-accel-platform)
(register-platform! 'newaccel new-accel-platform)

