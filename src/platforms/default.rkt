#lang racket

;;; The default platform:
;;; C/C++ on Linux with a full libm

(require "../plugin.rkt")

; universal boolean opertaions
(define-platform boolean-platform
                 #:literals ([bool 1])
                 #:default-cost 1
                 #:if-cost 1
                 TRUE
                 FALSE
                 not
                 and
                 or)

;; machine floating-point operations
(define-platform machine-platform
                 #:literals ([binary64 64] [binary32 32])
                 [PI.64 64]
                 [PI.32 32]
                 [E.64 64]
                 [E.32 32]
                 [INFINITY.64 64]
                 [INFINITY.32 32]
                 [NAN.64 64]
                 [NAN.32 32]
                 [neg.64 128]
                 [neg.32 64]
                 [+.64 128]
                 [+.32 64]
                 [-.64 128]
                 [-.32 64]
                 [*.64 256]
                 [*.32 128]
                 [/.64 640]
                 [/.32 320]
                 [==.64 256]
                 [==.32 128]
                 [!=.64 256]
                 [!=.32 128]
                 [>.64 256]
                 [>.32 128]
                 [<.64 256]
                 [<.32 128]
                 [>=.64 256]
                 [>=.32 128]
                 [<=.64 256]
                 [<=.32 128])

;; libm operations
(define-platform libm-platform
                 #:literals ([binary64 64] [binary32 32])
                 #:default-cost 3200
                 #:optional [acos.64 6400]
                 [acosh.64 6400]
                 [asin.64 6400]
                 [asinh.64 6400]
                 [atan.64 6400]
                 [atan2.64 6400]
                 [atanh.64 6400]
                 [cbrt.64 6400]
                 [ceil.64 6400]
                 [copysign.64 6400]
                 [cos.64 6400]
                 [cosh.64 6400]
                 [erf.64 6400]
                 [erfc.64 6400]
                 [exp.64 6400]
                 [exp2.64 6400]
                 [expm1.64 6400]
                 [fabs.64 128]
                 [fdim.64 6400]
                 [floor.64 6400]
                 [fma.64 6400]
                 [fmax.64 6400]
                 [fmin.64 6400]
                 [fmod.64 6400]
                 [hypot.64 6400]
                 [lgamma.64 6400]
                 [log.64 6400]
                 [log10.64 6400]
                 [log1p.64 6400]
                 [log2.64 6400]
                 [logb.64 6400]
                 [neg.64 6400]
                 [pow.64 6400]
                 [remainder.64 6400]
                 [rint.64 6400]
                 [round.64 6400]
                 [sin.64 6400]
                 [sinh.64 6400]
                 [sqrt.64 640]
                 [tan.64 6400]
                 [tanh.64 6400]
                 [tgamma.64 6400]
                 [trunc.64 6400]
                 acos.32
                 acosh.32
                 asin.32
                 asinh.32
                 atan.32
                 atan2.32
                 atanh.32
                 cbrt.32
                 ceil.32
                 copysign.32
                 cos.32
                 cosh.32
                 erf.32
                 erfc.32
                 exp.32
                 exp2.32
                 expm1.32
                 [fabs.32 64]
                 fdim.32
                 floor.32
                 fma.32
                 fmax.32
                 fmin.32
                 fmod.32
                 hypot.32
                 lgamma.32
                 log.32
                 log10.32
                 log1p.32
                 log2.32
                 logb.32
                 neg.32
                 pow.32
                 remainder.32
                 rint.32
                 round.32
                 sin.32
                 sinh.32
                 [sqrt.32 320]
                 tan.32
                 tanh.32
                 tgamma.32
                 trunc.32)

;; accelerator operations (minus fma)
(define accelerator-platform
  (with-terminal-cost ([binary64 64] [binary32 32])
                      (let ([relative-costs (cost-map #:default-cost 100 [(fma) 4])])
                        (platform-product #:optional [([real binary64])
                                                      (cost-map-scale 64 relative-costs)]
                                          [([real binary32]) (cost-map-scale 32 relative-costs)]
                                          (operator-set [(real real) (erfc expm1 log1p)]
                                                        [(real real real) (hypot)]
                                                        [(real real real real) (fma)])))))

; compose platforms

(define hardware-platform (platform-union boolean-platform machine-platform))

(define default-platform
  (platform-union boolean-platform machine-platform libm-platform accelerator-platform))

; Register all three

(register-platform! 'boolean boolean-platform)
(register-platform! 'hardware hardware-platform)
(register-platform! 'default default-platform)

;; Do not run this file during testing
(module test racket/base
  )
