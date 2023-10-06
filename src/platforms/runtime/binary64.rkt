#lang racket

;; Double-precision representation and consants

(require math/flonum math/bigfloat)
(require "bool.rkt" "utils.rkt")

(define-representation (binary64 real flonum?)
  bigfloat->flonum
  bf
  (shift 63 ordinal->flonum)
  (unshift 63 flonum->ordinal)
  64
  (conjoin number? nan?))

(define-syntax-rule (define-constants [name impl-name value] ...)
  (begin
    (define-operator-impl (name impl-name) binary64
      [fl (const value)])
    ...))

(define-constants
  [PI PI.f64 pi]
  [E E.f64 (exp 1.0)]
  [INFINITY INFINITY.f64 +inf.0]
  [NAN NAN.f64 +nan.0])
