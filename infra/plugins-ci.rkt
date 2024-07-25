#lang racket

;;; A platform for libsoftposit (used by CI)
;;; Optimized for C/C++ on Linux with a full libm

(require herbie/plugin)

(define-platform boolean-platform
                 #:literal [bool 1]
                 #:default-cost 1
                 #:if-cost 1
                 TRUE
                 FALSE
                 not
                 and
                 or)

(define-platform softposit-platform
  #:literal [posit8 1]
  #:literal [posit16 1]
  #:literal [posit32 1]
  #:literal [quire8 1]
  #:literal [quire16 1]
  #:literal [quire32 1]
  #:default-cost 1
  neg.p32
  neg.p16
  neg.p8
  sqrt.p32
  sqrt.p16
  sqrt.p8
  +.p32
  +.p16
  +.p8
  -.p32
  -.p16
  -.p8
  *.p32
  *.p16
  *.p8
  /.p32
  /.p16
  /.p8
  ==.p32
  ==.p16
  ==.p8
  !=.p32
  !=.p16
  !=.p8
  >.p32
  >.p16
  >.p8
  <.p32
  <.p16
  <.p8
  >=.p32
  >=.p16
  >=.p8
  <=.p32
  <=.p16
  <=.p8
  quire8-mul-add
  quire16-mul-add
  quire32-mul-add
  quire8-mul-sub
  quire16-mul-sub
  quire32-mul-sub)

(register-platform! 'softposit (platform-union boolean-platform softposit-platform))

;; Do not run this file during testing
(module test racket/base
  )
