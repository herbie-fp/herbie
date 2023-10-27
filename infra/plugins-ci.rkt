#lang racket

;;; The default platform with libsoftposit
;;; Optimized for C/C++ on Linux with a full libm

(require herbie/plugin)

(define default-platform (get-platform 'default))

(define softposit-platform
  (platform-union
    ; posit operators
    (platform-product
      #:type real [posit8 posit16 posit32]
      #:type bool [bool]
      #:operators [neg + - * / sqrt]
      #:operators [== != > < >= <=])
    ; quire operators
    (platform
      #:conversions ([posit8 quire8]
                     [posit16 quire16]
                     [posit32 quire32])
     (posit8)
     (posit16)
     (posit32)
     (quire8
       [fdp quire8 posit8 posit8 quire8]
       [fdm quire8 posit8 posit8 quire8])
     (quire16
       [fdp quire16 posit16 posit16 quire16]
       [fdm quire16 posit16 posit16 quire16])
     (quire32
       [fdp quire32 posit32 posit32 quire32]
       [fdm quire32 posit32 posit32 quire32]))))

(register-platform! 'softposit (platform-union default-platform softposit-platform))

;; Do not run this file during testing
(module test racket/base)
