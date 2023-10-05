#lang racket

;; Double-precision platform (accelerators only)

(require ffi/unsafe)
(require "../../plugin.rkt" "binary64.rkt"
         (only-in "libm.rkt" [define-binary64-impls/libm define-libm-operators]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libm-operators
  [expm1 log1p]
  [atan2 hypot]
  [fma])
