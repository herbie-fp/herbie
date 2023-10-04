#lang racket

;; Default platform:
;; - double-precision, single-precision, bool types
;; - double-precision libc operators
;; - single-precision libc operators
;; - boolean operators

(require "runtime/bool.rkt"
         "runtime/binary64.rkt"
         "runtime/binary32.rkt"
         "runtime/binary64-accel.rkt"
         "runtime/binary32-accel.rkt")
