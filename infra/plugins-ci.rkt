#lang racket

;;; The default platform with libsoftposit
;;; Optimized for C/C++ on Linux with a full libm

(require herbie/plugin)

(define default-platform (get-platform 'default))

(define softposit-platform
  (platform-union
    ; posit operators
    (platform-product
      [real (posit8 posit16 posit32)]
      [bool (bool)]
      (operator-set
        [(real real) (neg sqrt)]
        [(real real real) (+ - * /)]
        [(real real bool) (== != > < >= <=)]))
    ; quire operations
    (platform
      #:conversions ([posit8 quire8]
                     [posit16 quire16]
                     [posit32 quire32])
                    ;  [posit8 binary64]
                    ;  [posit16 binary64]
                    ;  [posit32 binary64])
      [(quire8 posit8 posit8 quire8) (fdp fdm)]
      [(quire16 posit16 posit16 quire16) (fdp fdm)]
      [(quire32 posit32 posit32 quire32) (fdp fdm)])))

(register-platform! 'softposit
  (platform-union default-platform
                  softposit-platform))

;; Do not run this file during testing
(module test racket/base)
