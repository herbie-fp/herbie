#lang racket

;; Builtin double-precision plugin (:precision binary64)

(require math/flonum
         math/bigfloat)

(require "runtime/utils.rkt"
         "runtime/libm.rkt")

;; Do not run this file with `raco test`
(module test racket/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (binary64 real flonum?)
  bigfloat->flonum
  bf
  (shift 63 ordinal->flonum)
  (unshift 63 flonum->ordinal)
  64
  (conjoin number? nan?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants binary64
  [PI PI.f64 pi]
  [E E.f64 (exp 1.0)]
  [INFINITY INFINITY.f64 +inf.0]
  [NAN NAN.f64 +nan.0])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-libm-impl/binary64 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype [key value] ...)
     (with-syntax ([impl (string->symbol (format "~a.f64" (syntax->datum #'op)))])
       #'(define-libm-impl op (op impl itype ...) otype [key value] ...))]))

(define-syntax-rule (define-libm-impls/binary64* (itype ... otype) name ...)
  (begin (define-libm-impl/binary64 name (itype ...) otype) ...))

(define-syntax-rule (define-libm-impls/binary64 [(itype ... otype) (name ...)] ...)
  (begin (define-libm-impls/binary64* (itype ... otype) name ...) ...))

(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])

(define-libm-impls/binary64
  [(binary64 binary64)
   (acos acosh asin asinh atan atanh cbrt ceil cos cosh erf exp exp2
    fabs floor lgamma log log10 log2 logb rint round sin sinh sqrt
    tan tanh tgamma trunc)]
  [(binary64 binary64 binary64)
   (atan2 copysign fdim fmax fmin fmod pow remainder)])

(define-comparator-impls binary64
  [== ==.f64 =]
  [!= !=.f64 (negate =)]
  [< <.f64 <]
  [> >.f64 >]
  [<= <=.f64 <=]
  [>= >=.f64 >=])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; accelerators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libm expm1 (expm1 double double))
(when expm1
  (define-operator-impl (expm1 expm1.f64 binary64) binary64
    [fl expm1]))

(define-operator-impl (fma binary64 binary64 binary64) binary64)

; (define-libm expm1.f64 (expm1 double double))
; (when expm1.f64
;   (define-accelerator-impl expm1 expm1.f64 (binary64) binary64 expm1.f64))

; (define-libm log1p.f64 (log1p double double))
; (when log1p.f64
;   (define-accelerator-impl log1p log1p.f64 (binary64) binary64 log1p.f64))

; (define-libm hypot.f64 (hypot double double double))
; (when hypot.f64
;   (define-accelerator-impl hypot hypot.f64 (binary64 binary64) binary64 hypot.f64))

; (define-libm fma.f64 (fma double double double double))
; (when fma.f64
;   (define-accelerator-impl fma fma.f64 (binary64 binary64 binary64) binary64 fma.f64))

; (define-libm erfc.f64 (erfc double double))
; (when erfc.f64
;   (define-accelerator-impl erfc erfc.f64 (binary64) binary64 erfc.f64))
