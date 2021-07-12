#lang racket

;; binary64 builtin plugin

(require math/bigfloat math/flonum)
(require (submod "syntax/syntax.rkt" internals)
         (submod "interface.rkt" internals)
         "common.rkt")

(eprintf "Loading binary64 support...\n")

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
  (disjoin nan? infinite?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant-impl (PI PI.f64) binary64
  [fl (const pi)])

(define-constant-impl (E E.f64) binary64
  [fl (const (exp 1.0))])

(define-constant-impl (INFINITY INFINITY.f64) binary64
  [fl (const +inf.0)])

(define-constant-impl (NAN NAN.f64) binary64
  [fl (const +nan.0)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])


(define-operator-impl (== ==.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator =)])

(define-operator-impl (!= !=.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (negate (comparator =))])

(define-operator-impl (< <.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator <)])

(define-operator-impl (> >.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator >)])

(define-operator-impl (<= <=.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator <=)])

(define-operator-impl (>= >=.f64 binary64 binary64) bool
  [itype 'binary64] [otype 'bool] ; Override number of arguments
  [fl (comparator >=)])
