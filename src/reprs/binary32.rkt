#lang racket

;; Builtin single-precision plugin (:precision binary32)

(require math/bigfloat)
(require "runtime/float32.rkt" "runtime/utils.rkt" "runtime/libm.rkt")

;; Do not run this file with `raco test`
(module test racket/base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (binary32 real float32?)
  bigfloat->float32
  bf
  (shift 31 ordinal->float32)
  (unshift 31 float32->ordinal)
  32
  (conjoin number? nan?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants binary32
  [PI PI.f32 (->float32 pi)]
  [E E.f32 (->float32 (exp 1.0))]
  [INFINITY INFINITY.f32 (->float32 +inf.0)]
  [NAN NAN.f32 (->float32 +nan.0)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-libm-impl/binary32 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype [key value] ...)
     (with-syntax ([impl (string->symbol (format "~a.f32" (syntax->datum #'op)))]
                   [cname (string->symbol (format "~af" (syntax->datum #'op)))])
       #'(define-libm-impl cname (op impl itype ...) otype [key value] ...))]))

(define-syntax-rule (define-libm-impls/binary32* (itype ... otype) name ...)
  (begin (define-libm-impl/binary32 name (itype ...) otype) ...))

(define-syntax-rule (define-libm-impls/binary32 [(itype ... otype) (name ...)] ...)
  (begin (define-libm-impls/binary32* (itype ... otype) name ...) ...))

(define-operator-impl (neg neg.f32 binary32) binary32 [fl fl32-])
(define-operator-impl (+ +.f32 binary32 binary32) binary32 [fl fl32+])
(define-operator-impl (- -.f32 binary32 binary32) binary32 [fl fl32-])
(define-operator-impl (* *.f32 binary32 binary32) binary32 [fl fl32*])
(define-operator-impl (/ /.f32 binary32 binary32) binary32 [fl fl32/])

(define-libm-impls/binary32
  [(binary32 binary32)
   (acos acosh asin asinh atan atanh cbrt ceil cos cosh erf exp exp2
    fabs floor lgamma log log10 log2 logb rint round sin sinh sqrt
    tan tanh tgamma trunc)]
  [(binary32 binary32 binary32)
   (atan2 copysign fdim fmax fmin fmod pow remainder)])

(define-comparator-impls binary32
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

; (define-libm expm1 (expm1f float float))
; (when expm1
;   (define-operator-impl (expm1 expm1.f32 binary32) binary32
;     [fl expm1]))

; (define-libm log1p.f32 (log1pf float float))
; (when log1p.f32
;   (define-accelerator-impl log1p log1p.f32 (binary32) binary32 log1p.f32))

; (define-libm hypot.f32 (hypotf float float float))
; (when hypot.f32
;   (define-accelerator-impl hypot hypot.f32 (binary32 binary32) binary32 hypot.f32))

; (define-libm fma.f32 (fmaf float float float float))
; (when fma.f32
;   (define-accelerator-impl fma fma.f32 (binary32 binary32 binary32) binary32 fma.f32))

; (define-libm erfc.f32 (erfcf float float))
; (when erfc.f32
;   (define-accelerator-impl erfc erfc.f32 (binary32) binary32 erfc.f32))
