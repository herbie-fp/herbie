#lang racket

;; Single-precision accelerators

(require ffi/unsafe)
(require "binary32.rkt" "utils.rkt"
         (only-in "libm.rkt" [define-binary32-impls/libm define-libm-operators]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libm-operators
  [expm1 log1p]
  [atan2 hypot]
  [fma])
