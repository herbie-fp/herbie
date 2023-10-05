#lang racket

;; "Hardware" platforms
;;  - double-precision, single-precision, bool types
;;  - floating-point arithmetic operators, sqrt, log2
;;  - boolean operators

(require math/flonum math/bigfloat ffi/unsafe)
(require "../plugin.rkt" "runtime/bool.rkt" "runtime/float32.rkt"
        (only-in "runtime/libm.rkt"
                 define-binary64-impls/libm
                 define-binary32-impls/libm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(define-representation (binary64 real flonum?)
  bigfloat->flonum
  bf
  (shift 63 ordinal->flonum)
  (unshift 63 flonum->ordinal)
  64
  (conjoin number? nan?))

(define-representation (binary32 real float32?)
  bigfloat->float32
  bf
  (shift 31 ordinal->float32)
  (unshift 31 float32->ordinal)
  32
  (conjoin number? nan?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operator-impl (PI PI.f64) binary64
  [fl (const pi)])

(define-operator-impl (E E.f64) binary64
  [fl (const (exp 1.0))])

(define-operator-impl (INFINITY INFINITY.f64) binary64
  [fl (const +inf.0)])

(define-operator-impl (NAN NAN.f64) binary64
  [fl (const +nan.0)])

(define-operator-impl (PI PI.f32) binary32
  [fl (const (->float32 pi))])

(define-operator-impl (E E.f32) binary32
  [fl (const (->float32 (exp 1.0)))])

(define-operator-impl (INFINITY INFINITY.f32) binary32
  [fl (const (->float32 +inf.0))])

(define-operator-impl (NAN NAN.f32) binary32
  [fl (const (->float32 +nan.0))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])

(define-binary64-impls/libm
  [sqrt log2 fabs floor ceil]
  [fmin fmax])

(define-syntax-rule (define-comparator-binary64-impls [name impl-name impl-fn] ...)
  (begin
    (define-operator-impl (name impl-name binary64 binary64) bool
      [fl impl-fn])
    ...))

(define-comparator-binary64-impls
  [== ==.f64 =]
  [!= !=.f64 (negate =)]
  [< <.f64 <]
  [> >.f64 >]
  [<= <=.f64 <=]
  [>= >=.f64 >=])

(define-operator-impl (neg neg.f32 binary32) binary32 [fl fl32-])
(define-operator-impl (+ +.f32 binary32 binary32) binary32 [fl fl32+])
(define-operator-impl (- -.f32 binary32 binary32) binary32 [fl fl32-])
(define-operator-impl (* *.f32 binary32 binary32) binary32 [fl fl32*])
(define-operator-impl (/ /.f32 binary32 binary32) binary32 [fl fl32/])

(define-binary32-impls/libm
  [sqrt log2 fabs floor ceil]
  [fmin fmax])

(define-syntax-rule (define-comparator-binary32-impls [name impl-name impl-fn] ...)
  (begin
    (define-operator-impl (name impl-name binary32 binary32) bool
      [fl impl-fn])
    ...))

(define-comparator-binary32-impls
  [== ==.f32 =]
  [!= !=.f32 (negate =)]
  [< <.f32 <]
  [> >.f32 >]
  [<= <=.f32 <=]
  [>= >=.f32 >=])

(define-operator-impl (cast binary64->binary32 binary64) binary32
  [fl (curryr ->float32)])

(define-operator-impl (cast binary32->binary64 binary32) binary64
  [fl identity])
