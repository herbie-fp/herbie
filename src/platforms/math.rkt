#lang racket

;; Math platform:
;; - double-precision, single-precision, bool types
;; - double-precision (no numerics) libc operators
;; - single-precision (no numerics) libc operators
;; - boolean operators

(require "components/bool.rkt"
         "components/binary64.rkt"
         "components/binary32.rkt")
