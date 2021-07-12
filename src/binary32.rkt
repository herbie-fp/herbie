#lang racket

;; binary32 builtin plugin

(require math/bigfloat math/flonum)
(require (submod "syntax/syntax.rkt" internals)
         (submod "interface.rkt" internals)
         "common.rkt" "float32.rkt")

(eprintf "Loading binary32 support...\n")

;; Only load everything before if single flonum supported?
;; BC or CS (>= 8.0)
(when (single-flonum-supported?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(define-representation (binary32 real float32?)
  bigfloat->float32
  bf
  (shift 31 ordinal->float32)
  (unshift 31 float32->ordinal)
  32
  (disjoin nan? infinite?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant-impl (PI PI.f32) binary32
  [fl (const (->float32 pi))])

(define-constant-impl (E E.f32) binary32
  [fl (const (->float32 (exp 1.0)))])

(define-constant-impl (INFINITY INFINITY.f32) binary32
  [fl (const (->float32 +inf.0))])

(define-constant-impl (NAN NAN.f32) binary32
  [fl (const (->float32 +nan.0))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operator-impl (neg neg.f32 binary32) binary32 [fl fl32-])
(define-operator-impl (+ +.f32 binary32 binary32) binary32 [fl fl32+])
(define-operator-impl (- -.f32 binary32 binary32) binary32 [fl fl32-])
(define-operator-impl (* *.f32 binary32 binary32) binary32 [fl fl32*])
(define-operator-impl (/ /.f32 binary32 binary32) binary32 [fl fl32/])


(define-operator-impl (== ==.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator =)])

(define-operator-impl (!= !=.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (negate (comparator =))])

(define-operator-impl (< <.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator <)])

(define-operator-impl (> >.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator >)])

(define-operator-impl (<= <=.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator <=)])

(define-operator-impl (>= >=.f32 binary32 binary32) bool
  [itype 'binary32] [otype 'bool] ; Override number of arguments
  [fl (comparator >=)])


(define-operator-impl (cast binary64->binary32 binary64) binary32
  [fl (curryr ->float32)])

(define-operator-impl (cast binary32->binary64 binary32) binary64
  [fl identity])

)
