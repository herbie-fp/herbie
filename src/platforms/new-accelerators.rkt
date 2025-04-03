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

(define-operator-impl (pown3/2s.f64 [z0 : binary64])
                      binary64
                      #:spec (sqrt (/ 1 (pow z0 3)))
                      #:fpcore (! :precision binary64 (pown3/2s z0))
                      ) 


(define-operator-impl (ratio-of-squares.f32 [z0 : binary32] [z1 : binary32])
                      binary32
                      #:spec (/ (* z0 z0) (* z1 z1))
                      #:fpcore (! :precision binary32 (ratio-of-squares z0 z1)))

(define-operator-impl (ratio-square-sum.f32 [z0 : binary32] [z1 : binary32])
                      binary32
                      #:spec (/ (* z0 z0) (+ z0 z1))
                      #:fpcore (! :precision binary32 (ratio-square-sum z0 z1))
                      )            

(define-operator-impl (sqrt-product.f32 [z0 : binary32] [z1 : binary32])
                      binary32
                      #:spec (sqrt (* (+ z0 z1) (- z0 z1)))
                      #:fpcore (! :precision binary32 (sqrt-product z0 z1))
                      )            

(define-operator-impl (log1m.f32 [z0 : binary32])
                      binary32
                      #:spec (log (- 1 z0))
                      #:fpcore (! :precision binary32 (log1m z0))
                      )     

(define-operator-impl (expnp1.f32 [z0 : binary32])
                      binary32
                      #:spec (+ (exp (neg z0)) 1)
                      #:fpcore (! :precision binary32 (expnp1 z0))
                      )     

(define-operator-impl (pown3/2s.f32 [z0 : binary32])
                      binary32
                      #:spec (sqrt (/ 1 (pow z0 3)))
                      #:fpcore (! :precision binary32 (pown3/2s z0))
                      ) 

(define-operator-impl (cosD.f32 [z0 : binary32])
                      binary32
                      #:spec (cos (* (PI) (/ z0 180)))
                      #:fpcore (! :precision binary32 (cosD z0))
                      ) 

(define-operator-impl (cosD.f64 [z0 : binary64])
                      binary64
                      #:spec (cos (* (PI) (/ z0 180)))
                      #:fpcore (! :precision binary64 (cosD z0))
                      ) 

(define-operator-impl (sinD.f32 [z0 : binary32])
                      binary32
                      #:spec (sin (* (PI) (/ z0 180)))
                      #:fpcore (! :precision binary32 (sinD z0))
                      ) 

(define-operator-impl (sinD.f64 [z0 : binary64])
                      binary64
                      #:spec (sin (* (PI) (/ z0 180)))
                      #:fpcore (! :precision binary64 (sinD z0))
                      ) 


(define-operator-impl (cos30.f32 [z0 : binary32])
                      binary32
                      #:spec (cos (* z0 30))
                      #:fpcore (! :precision binary32 (cos30 z0))
                      ) 

(define-operator-impl (cos30.f64 [z0 : binary64])
                      binary64
                      #:spec (cos (* z0 30))
                      #:fpcore (! :precision binary64 (cos30 z0))
                      ) 

(define-operator-impl (sin30.f32 [z0 : binary32])
                      binary32
                      #:spec (sin (* z0 30))
                      #:fpcore (! :precision binary32 (sin30 z0))
                      ) 

(define-operator-impl (sin30.f64 [z0 : binary64])
                      binary64
                      #:spec (sin (* z0 30))
                      #:fpcore (! :precision binary64 (sin30 z0))
                      ) 

(define-platform new-accelerator-platform
                    #:literal [binary64 64]
                    #:literal [binary32 32]
                    #:default-cost 100
                    ;;; [ratio-of-squares.f64 352]
                    ;;; [ratio-square-sum.f64 320]
                    ;;; [sqrt-product.f64 352]
                    ;;; [log1m.f64 1664]
                    ;;; [expnp1.f64 1696]
                    ;;; [pown3/2s.f64 1968]
                    ;;; [ratio-of-squares.f32 352]
                    ;;; [ratio-square-sum.f32 320]
                    ;;; [sqrt-product.f32 352]
                    ;;; [log1m.f32 1664]
                    ;;; [expnp1.f32 1696]
                    ;;; [pown3/2s.f32 1968]
                    [cosD.f32 888]
                    [cosD.f64 1776]
                    [sinD.f32 888]
                    [sinD.f64 1776]
                    [sin30.f32 848]
                    [sin30.f64 1696]
                    [cos30.f32 848]
                    [cos30.f64 1696]
                    ) 


(define no-accel-platform
(platform-union boolean-platform
                  machine-platform
                  libm64-platform
                  libm32-platform))

(define new-accel-platform
(platform-union boolean-platform
                  machine-platform
                  libm64-platform
                  libm32-platform
                  new-accelerator-platform))

(define default-new-accel-platform
  (platform-union boolean-platform
                  machine-platform
                  libm64-platform
                  libm32-platform
                  accelerator-platform
                  new-accelerator-platform))


(register-platform! 'noaccel no-accel-platform)
(register-platform! 'new-accel new-accel-platform)
(register-platform! 'default-new-accel default-new-accel-platform)


