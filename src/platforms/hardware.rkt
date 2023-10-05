#lang racket

;; "Hardware" platforms
;;  - double-precision, single-precision, bool types
;;  - floating-point arithmetic operators, sqrt, log2
;;  - boolean operators

(require math/flonum math/bigfloat ffi/unsafe)
(require "../plugin.rkt" "runtime/bool.rkt" "runtime/float32.rkt"
        (only-in "runtime/libm.rkt"
          define-binary64-impl/libm
          define-binary32-impl/libm))

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

;;;;;;;;;;;;;;;;;;;;;;;;;; libm support ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-binary64-impls/libm stx)
  (syntax-case stx ()
    [(_ argc ops ...)
     (with-syntax ([(args ...) (build-list (syntax->datum #'argc) (λ (_) #'real))])
       #'(begin (define-binary64-impl/libm (ops args ...)) ...))]))

(define-syntax (define-binary32-impls/libm stx)
  (syntax-case stx ()
    [(_ argc ops ...)
     (with-syntax ([(args ...) (build-list (syntax->datum #'argc) (λ (_) #'real))])
       #'(begin (define-binary32-impl/libm (ops args ...)) ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])

(define-binary64-impls/libm 1 sqrt log2 fabs floor ceil)
(define-binary64-impls/libm 2 fmin fmax)

(define-operator-impl (== ==.f64 binary64 binary64) bool
  [fl =])

(define-operator-impl (!= !=.f64 binary64 binary64) bool
  [fl (negate =)])

(define-operator-impl (< <.f64 binary64 binary64) bool
  [fl <])

(define-operator-impl (> >.f64 binary64 binary64) bool
  [fl >])

(define-operator-impl (<= <=.f64 binary64 binary64) bool
  [fl <=])

(define-operator-impl (>= >=.f64 binary64 binary64) bool
  [fl >=])

(define-operator-impl (neg neg.f32 binary32) binary32 [fl fl32-])
(define-operator-impl (+ +.f32 binary32 binary32) binary32 [fl fl32+])
(define-operator-impl (- -.f32 binary32 binary32) binary32 [fl fl32-])
(define-operator-impl (* *.f32 binary32 binary32) binary32 [fl fl32*])
(define-operator-impl (/ /.f32 binary32 binary32) binary32 [fl fl32/])

(define-binary32-impls/libm 1 sqrt log2 fabs floor ceil)
(define-binary32-impls/libm 2 fmin fmax)

(define-operator-impl (== ==.f32 binary32 binary32) bool
  [fl =])

(define-operator-impl (!= !=.f32 binary32 binary32) bool
  [fl (negate =)])

(define-operator-impl (< <.f32 binary32 binary32) bool
  [fl <])

(define-operator-impl (> >.f32 binary32 binary32) bool
  [fl >])

(define-operator-impl (<= <=.f32 binary32 binary32) bool
  [fl <=])

(define-operator-impl (>= >=.f32 binary32 binary32) bool
  [fl >=])

(define-operator-impl (cast binary64->binary32 binary64) binary32
  [fl (curryr ->float32)])

(define-operator-impl (cast binary32->binary64 binary32) binary64
  [fl identity])
