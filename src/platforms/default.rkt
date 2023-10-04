#lang racket

;; Default platform:
;; - double-precision, single-precision, bool types
;; - double-precision libc operators
;; - single-precision libc operators
;; - boolean operators

(require "components/bool.rkt"
         "components/binary64.rkt"
         "components/binary32.rkt"
         "components/binary64-accel.rkt"
         "components/binary32-accel.rkt")
