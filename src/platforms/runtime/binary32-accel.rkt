#lang racket

;; Single-precision platform (math only)

(require ffi/unsafe)
(require "../../plugin.rkt" "binary32.rkt"
         (only-in "libm.rkt" [define-binary32-impls/libm define-libm-operators]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libm-operators
  [expm1 log1p]
  [atan2 hypot]
  [fma])
