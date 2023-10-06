#lang racket

;; Math platform:
;; - double-precision, single-precision, bool types
;; - double-precision (no numerics) libc operators
;; - single-precision (no numerics) libc operators
;; - boolean operators

(require "runtime/bool.rkt"
         "runtime/binary64.rkt"
         "runtime/binary64-math.rkt"
         "runtime/binary32.rkt"
         "runtime/binary32-math.rkt"
         ; for conversions
         "runtime/float32.rkt"
         "runtime/utils.rkt")

(define-operator-impl (cast binary64->binary32 binary64) binary32
  [fl (curryr ->float32)])

(define-operator-impl (cast binary32->binary64 binary32) binary64
  [fl identity])
