#lang racket

;;; A platform for libsoftposit (used by CI)
;;; Optimized for C/C++ on Linux with a full libm

(require herbie/plugin)

(define boolean-platform
  (with-terminal-cost ([bool 1])
    (platform
      #:default-cost 1
      #:if-cost 1
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

(define softposit-platform
  (with-terminal-cost ([posit8 1] [posit16 1] [posit32 1]
                       [quire8 1] [quire16 1] [quire32 1])
    (platform-union
      ; posit operators
      (platform-product
        [([real posit8] [bool bool]) (cost-map #:default-cost 1)]
        [([real posit16] [bool bool]) (cost-map #:default-cost 1)]
        [([real posit32] [bool bool]) (cost-map #:default-cost 1)]
        (operator-set
          [(real real) (neg sqrt)]
          [(real real real) (+ - * /)]
          [(real real bool) (== != > < >= <=)]))
      ; quire operations
      (platform
        #:default-cost 1
        ; these conversions fail due to bugs in resugaring/desugaring
        #:conversions ([posit8 quire8]
                       [posit16 quire16]
                       [posit32 quire32])
        [(quire8 posit8 posit8 quire8) (fdp fdm)]
        [(quire16 posit16 posit16 quire16) (fdp fdm)]
        [(quire32 posit32 posit32 quire32) (fdp fdm)]))))

(register-platform! 'softposit
  (platform-union boolean-platform
                  softposit-platform))

;; Do not run this file during testing
(module test racket/base)
