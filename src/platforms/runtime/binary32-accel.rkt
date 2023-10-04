#lang racket

;; Single-precision platform (math only)

(require ffi/unsafe)
(require "../../plugin.rkt" "binary32.rkt"
         (rename-in "libm.rkt"
            [define-binary32-impl/libm define-libm-operator]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-1ary-libm-operator op)
  (define-libm-operator (op real)))

(define-syntax-rule (define-2ary-libm-operator op)
  (define-libm-operator (op real real)))

(define-syntax-rule (define-1ary-libm-operators op ...)
  (begin (define-1ary-libm-operator op) ...))

(define-syntax-rule (define-2ary-libm-operators op ...)
  (begin (define-2ary-libm-operator op) ...))

(define-1ary-libm-operators expm1 log1p)
(define-2ary-libm-operators atan2 hypot)
(define-libm-operator (fma real real real))
