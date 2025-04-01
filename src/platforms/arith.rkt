#lang racket

(require "../plugin.rkt")

(define-operator-impl (vcubic.f32 [a : binary32] [b : binary32] [c : binary32] [d : binary32] [x : binary32])
                      binary32
                      #:spec  (+ (+ (+ (* (* (* x x) x) a) (* (* x x) b)) (* x c)) d)
                      #:fpcore (! :precision binary32 (vcubic a b c d x)))  
        
(define-operator-impl (vcubic.f64 [a : binary64] [b : binary64] [c : binary64] [d : binary64] [x : binary64])
                      binary64
                      #:spec  (+ (+ (+ (* (* (* x x) x) a) (* (* x x) b)) (* x c)) d)
                      #:fpcore (! :precision binary64 (vcubic a b c d x)))  

(define-platform vcubic-accelerator-platform
                 #:literal [binary64 64]
                 #:literal [binary32 32]
                 [vcubic.f64 512]
                 [vcubic.f32 256])

; universal boolean opertaions
(define-platform boolean-platform
                 #:literal [bool 1]
                 #:default-cost 1
                 #:if-cost 1
                 TRUE
                 FALSE
                 not
                 and
                 or)

(define-platform fma-accelerator-platform
                #:literal [binary64 64]
                #:literal [binary32 32]
                [fma.f64 256]
                [fma.f32 128])

(define-platform arith-platform
                 #:literal [binary64 64]
                 #:literal [binary32 32]
                [PI.f64 64]
                [PI.f32 32]
                [E.f64 64]
                [E.f32 32]
                [INFINITY.f64 64]
                [INFINITY.f32 32]
                [NAN.f64 64]
                [NAN.f32 32]
                [neg.f64 128]
                [neg.f32 64]
                [+.f64 128]
                [+.f32 64]
                [-.f64 128]
                [-.f32 64]
                [*.f64 256]
                [*.f32 128]
                [/.f64 640]
                [/.f32 320]
                [==.f64 256]
                [==.f32 128]
                [!=.f64 256]
                [!=.f32 128]
                [>.f64 256]
                [>.f32 128]
                [<.f64 256]
                [<.f32 128]
                [>=.f64 256]
                [>=.f32 128]
                [<=.f64 256]
                [<=.f32 128]
                [fabs.f64 128]
                [fabs.f32 64]
                [fmax.f64 6400]
                [fmax.f32 3200]
                [fmin.f64 6400]
                [fmin.f32 3200]
                [sqrt.f64 640]
                [sqrt.f32 320])

(define arith (platform-union boolean-platform arith-platform))
(define arith-fma (platform-union arith fma-accelerator-platform))
(define arith-fma-vcubic (platform-union arith-fma vcubic-accelerator-platform))


(register-platform! 'arith arith)
(register-platform! 'arith-fma arith-fma)
(register-platform! 'arith-fma-vcubic arith-fma-vcubic)