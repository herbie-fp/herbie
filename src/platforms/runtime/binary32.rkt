#lang racket

;; Single-precision representation and constants

(require math/flonum math/bigfloat)
(require "bool.rkt" "float32.rkt" "utils.rkt")

(define-representation (binary32 real float32?)
  bigfloat->float32
  bf
  (shift 31 ordinal->float32)
  (unshift 31 float32->ordinal)
  32
  (conjoin number? nan?))

(define-syntax-rule (define-constants [name impl-name value] ...)
  (begin
    (define-operator-impl (name impl-name) binary32
      [fl (const value)])
    ...))

(define-constants
  [PI PI.f32 (->float32 pi)]
  [E E.f32 (->float32 (exp 1.0))]
  [INFINITY INFINITY.f32 (->float32 +inf.0)]
  [NAN NAN.f32 (->float32 +nan.0)])
