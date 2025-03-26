#lang racket

(require "../plugin.rkt"
         "default.rkt")


(define-operator-impl (vcubic.f32 [a : binary32] [b : binary32] [c : binary32] [d : binary32] [x : binary32])
                      binary32
                      #:spec  (+ (+ (+ (* (* (* x x) x) a) (* (* x x) b)) (* x c)) d)
                      #:fpcore (! :precision binary32 (vcubic a b c d x)))  
        
(define-operator-impl (vcubic.f64 [a : binary64] [b : binary64] [c : binary64] [d : binary64] [x : binary64])
                      binary64
                      #:spec  (+ (+ (+ (* (* (* x x) x) a) (* (* x x) b)) (* x c)) d)
                      #:fpcore (! :precision binary64 (vcubic a b c d x)))  

(define-platform cubic-accelerator-platform
                 #:literal [binary64 64]
                 #:literal [binary32 32]
                 [vcubic.f64 512]
                 [vcubic.f32 256])

(define cubic-platform
  (platform-union boolean-platform
                  machine-platform
                  libm64-platform
                  libm32-platform
                  accelerator-platform
                  cubic-accelerator-platform))

(register-platform! 'cubic cubic-platform)