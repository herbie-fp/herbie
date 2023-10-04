#lang racket

;; Math platform:
;; - double-precision, single-precision, bool types
;; - double-precision (no numerics) libc operators
;; - single-precision (no numerics) libc operators
;; - boolean operators

(require "runtime/bool.rkt"
         "runtime/binary64.rkt"
         "runtime/binary32.rkt")
