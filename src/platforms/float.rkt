#lang racket

;; All floating-point types platform:
;; - all IEEE 754 float formats, booleans
;; - IEEE 754 operators
;; - boolean operators

;; This platform will be slow for floating-point formats
;; without a hardware implementation. The underlying library
;; is just MPFR (math/bigfloat).

;; Load in hardware-based floating point
(require "default.rkt")

;; Dynamically-generated software floating-point
(require "runtime/generic-flonum.rkt" "runtime/utils.rkt")

(define (generate-float-format name)
  (match name
    ; we have hardware acceleration for some formats
    [(list 'float 11 64)
     (define repr (get-representation 'binary64))
     (register-representation-alias! name repr)
     repr]
    [(list 'float 8 32)
     (define repr (get-representation 'binary32))
     (register-representation-alias! name repr)
     repr]
    ; otherwise, we use software
    [(list 'float es nbits)
     (register-floating-point-impl! es nbits)
     (get-representation name)]
    ; aliases
    ['binary256
     (register-floating-point-impl! 19 256)
     (define repr (get-representation '(float 19 256)))
     (register-representation-alias! name repr)
     repr]
    ['binary128
     (register-floating-point-impl! 15 128)
     (define repr (get-representation '(float 15 128)))
     (register-representation-alias! name repr)
     repr]
    ['binary80
     (register-floating-point-impl! 15 80)
     (define repr (get-representation '(float 15 80)))
     (register-representation-alias! name repr)
     repr]
    ['pxr24
     (register-floating-point-impl! 8 24)
     (define repr (get-representation '(float 8 24)))
     (register-representation-alias! name repr)
     repr]
    ['fp24
     (register-floating-point-impl! 7 24)
     (define repr (get-representation '(float 7 24)))
     (register-representation-alias! name repr)
     repr]
    ['tf32
     (register-floating-point-impl! 8 19)
     (define repr (get-representation '(float 8 19)))
     (register-representation-alias! name repr)
     repr]
    ['bfloat16
     (register-floating-point-impl! 8 16)
     (define repr (get-representation '(float 8 16)))
     (register-representation-alias! name repr)
     repr]
    ['binary16
     (register-floating-point-impl! 5 16)
     (define repr (get-representation '(float 5 16)))
     (register-representation-alias! name repr)
     repr]
    ; otherwise we don't know
    [_ #f]))

(register-generator! generate-float-format)
