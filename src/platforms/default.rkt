#lang racket

;;; The default platform:
;;; C/C++ on Linux with a full libm

(require "../plugin.rkt")

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

;; machine floating-point operations
(define-platform machine-platform
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
                 [<=.f32 128])

;; libm operations
(define-platform libm64-platform
                 #:literal [binary64 64]
                 #:literal [binary32 32]
                 #:default-cost 6400
                 #:optional acos.f64
                 acosh.f64
                 asin.f64
                 asinh.f64
                 atan.f64
                 atan2.f64
                 atanh.f64
                 cbrt.f64
                 ceil.f64
                 copysign.f64
                 cos.f64
                 cosh.f64
                 erf.f64
                 exp.f64
                 exp2.f64
                 [fabs.f64 128]
                 fdim.f64
                 floor.f64
                 fmax.f64
                 fmin.f64
                 fmod.f64
                 lgamma.f64
                 log.f64
                 log10.f64
                 log2.f64
                 logb.f64
                 pow.f64
                 remainder.f64
                 rint.f64
                 round.f64
                 sin.f64
                 sinh.f64
                 [sqrt.f64 640]
                 tan.f64
                 tanh.f64
                 tgamma.f64
                 trunc.f64)

(define-platform libm32-platform
                 #:literal [binary32 32]
                 #:default-cost 3200
                 #:optional acos.f32
                 acosh.f32
                 asin.f32
                 asinh.f32
                 atan.f32
                 atan2.f32
                 atanh.f32
                 cbrt.f32
                 ceil.f32
                 copysign.f32
                 cos.f32
                 cosh.f32
                 erf.f32
                 exp.f32
                 exp2.f32
                 [fabs.f32 64]
                 fdim.f32
                 floor.f32
                 fmax.f32
                 fmin.f32
                 fmod.f32
                 lgamma.f32
                 log.f32
                 log10.f32
                 log2.f32
                 logb.f32
                 pow.f32
                 remainder.f32
                 rint.f32
                 round.f32
                 sin.f32
                 sinh.f32
                 [sqrt.f32 320]
                 tan.f32
                 tanh.f32
                 tgamma.f32
                 trunc.f32)

(define-platform accelerator-platform
                 #:literal [binary64 64]
                 #:literal [binary32 32]
                 #:default-cost 3200
                 #:optional [erfc.f64 6400]
                 [expm1.f64 6400]
                 [log1p.f64 6400]
                 [hypot.f64 6400]
                 [fma.f64 256]
                 erfc.f32
                 expm1.f32
                 log1p.f32
                 hypot.f32
                 [fma.f32 128])

(define-operator-impl (cosd.f32 [x : binary32])
                      binary32
                      #:spec (cos (* (* (PI) x) 1/180))
                      #:fpcore (! :precision binary32 (cosd x)))

(define-operator-impl (cosd.f64 [x : binary64])
                      binary64
                      #:spec (cos (* (* (PI) x) 1/180))
                      #:fpcore (! :precision binary64 (cosd x)))

(define-operator-impl (cosratio.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (cos (/ x y))
                      #:fpcore (! :precision binary32 (cosratio x y)))

(define-operator-impl (cosratio.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (cos (/ x y))
                      #:fpcore (! :precision binary64 (cosratio x y)))

(define-operator-impl (sinratio.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (sin (/ x y))
                      #:fpcore (! :precision binary32 (sinratio x y)))

(define-operator-impl (sinratio.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (sin (/ x y))
                      #:fpcore (! :precision binary64 (sinratio x y)))

(define-operator-impl (cos2pi.f32 [x : binary32])
                      binary32
                      #:spec (cos (* (* (PI) x) 2))
                      #:fpcore (! :precision binary32 (cos2pi x)))

(define-operator-impl (cos2pi.f64 [x : binary64])
                      binary64
                      #:spec (cos (* (* (PI) x) 2))
                      #:fpcore (! :precision binary64 (cos2pi x)))

(define-operator-impl (cosnd.f32 [x : binary32])
                      binary32
                      #:spec (cos (* (* (PI) x) -1/180))
                      #:fpcore (! :precision binary32 (cosnd x)))

(define-operator-impl (cosnd.f64 [x : binary64])
                      binary64
                      #:spec (cos (* (* (PI) x) -1/180))
                      #:fpcore (! :precision binary64 (cosnd x)))

(define-operator-impl (sind.f32 [x : binary32])
                      binary32
                      #:spec (sin (* (* (PI) x) 1/180))
                      #:fpcore (! :precision binary32 (sind x)))

(define-operator-impl (sind.f64 [x : binary64])
                      binary64
                      #:spec (sin (* (* (PI) x) 1/180))
                      #:fpcore (! :precision binary64 (sind x)))

;;; (define-operator-impl (sinnd.f32 [x : binary32])
;;;                       binary32
;;;                       #:spec (sin (* (* (PI) x) -1/180))
;;;                       #:fpcore (! :precision binary32 (sinnd x)))

;;; (define-operator-impl (sinnd.f64 [x : binary64])
;;;                       binary64
;;;                       #:spec (sin (* (* (PI) x) -1/180))
;;;                       #:fpcore (! :precision binary64 (sinnd x)))

(define-operator-impl (sin2pi.f32 [x : binary32])
                      binary32
                      #:spec (sin (* (* (PI) x) 2))
                      #:fpcore (! :precision binary32 (sin2pi x)))

(define-operator-impl (sin2pi.f64 [x : binary64])
                      binary64
                      #:spec (sin (* (* (PI) x) 2))
                      #:fpcore (! :precision binary64 (sin2pi x)))
                      
;;; (define-operator-impl (cos30.f32 [x : binary32])
;;;                       binary32
;;;                       #:spec (cos (* x 30))
;;;                       #:fpcore (! :precision binary32 (cos30 x)))

;;; (define-operator-impl (cos30.f64 [x : binary64])
;;;                       binary64
;;;                       #:spec (cos (* x 30))
;;;                       #:fpcore (! :precision binary64 (cos30 x)))

;;; (define-operator-impl (sin30.f32 [x : binary32])
;;;                       binary32
;;;                       #:spec (sin (* x 30))
;;;                       #:fpcore (! :precision binary32 (sin30 x)))

;;; (define-operator-impl (sin30.f64 [x : binary64])
;;;                       binary64
;;;                       #:spec (sin (* x 30))
;;;                       #:fpcore (! :precision binary64 (sin30 x)))
                      

(define-platform new-accelerators-full
                 #:literal [binary64 64]
                 #:literal [binary32 32]
                 #:default-cost 3200
                 cosd.f32
                 cosd.f64
                 sind.f32
                 sind.f64
                 cosnd.f32
                 cosnd.f64
                 sinnd.f32
                 sinnd.f64
                 cos2pi.f32
                 cos2pi.f64
                 sin2pi.f32
                 sin2pi.f64
                 cos30.f32
                 cos30.f64
                 sin30.f32
                 sin30.f64
                 sinratio.f32
                 sinratio.f64
                 cosratio.f32
                 cosratio.f64)



(define-operator-impl (log1m.f32 [x : binary32])
                      binary32
                      #:spec (log (- 1 x))
                      #:fpcore (! :precision binary32 (log1m x)))

(define-operator-impl (log1m.f64 [x : binary64])
                      binary64
                      #:spec (log (- 1 x))
                      #:fpcore (! :precision binary64 (log1m x)))

(define-operator-impl (sinpi.f32 [x : binary32])
                      binary32
                      #:spec (sin (* x (PI)))
                      #:fpcore (! :precision binary32 (sinpi x)))

(define-operator-impl (sinpi.f64 [x : binary64])
                      binary64
                      #:spec (sin (* x (PI)))
                      #:fpcore (! :precision binary64 (sinpi x)))

(define-operator-impl (cospi.f32 [x : binary32])
                      binary32
                      #:spec (cos (* x (PI)))
                      #:fpcore (! :precision binary32 (cospi x)))

(define-operator-impl (cospi.f64 [x : binary64])
                      binary64
                      #:spec (cos (* x (PI)))
                      #:fpcore (! :precision binary64 (cospi x)))


(define-platform new-accelerators-pbrt
                 #:literal [binary64 64]
                 #:literal [binary32 32]
                 #:default-cost 3200
                 log1m.f32
                 log1m.f64
                 sinpi.f32
                 sinpi.f64
                 cospi.f32
                 cospi.f64
                 )
; compose platforms

(define hardware-platform (platform-union boolean-platform machine-platform))

(define default-platform
  (platform-union boolean-platform
                  machine-platform
                  libm64-platform
                  libm32-platform
                  ))

(define improve-platform
(platform-union default-platform
new-accelerators-full
))

; Register all three

(register-platform! 'boolean boolean-platform)
(register-platform! 'hardware hardware-platform)
(register-platform! 'default default-platform)
(register-platform! 'improve improve-platform)


;; Do not run this file during testing
(module test racket/base
  )
