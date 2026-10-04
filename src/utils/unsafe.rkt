#lang typed/racket/base

(require typed/racket/unsafe
         math/private/flonum/flonum-bits)

;; Typed Racket normally installs contracts when a typed binding crosses into
;; untyped code. These bindings are used with their declared types, so export
;; them without a contract.
(unsafe-provide flonums-between)
