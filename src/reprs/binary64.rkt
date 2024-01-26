#lang racket

;; Builtin double-precision plugin (:precision binary64)

(require math/flonum math/bigfloat)
(require ffi/unsafe)
(require "runtime/utils.rkt" "runtime/libm.rkt")

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
   (acos acosh asin asinh atan atanh cbrt ceil cos cosh erf erfc exp exp2 fabs floor lgamma log log10 log1p log2 logb rint round sin sinh sqrt tan tanh tgamma trunc)]
  [(binary64 binary64 binary64)
   (atan2 copysign fdim fmax fmin fmod hypot pow remainder)]
  [(binary64 binary64 binary64 binary64)
   (fma)])

(define-comparator-impls binary64
  [== ==.f64 =]
  [!= !=.f64 (negate =)]
  [< <.f64 <]
  [> >.f64 >]
  [<= <=.f64 <=]
  [>= >=.f64 >=])

(register-accelerator-implementation!
 'expm1 'expm1.f64
 (list (get-representation 'binary64)) (get-representation 'binary64)
 (get-ffi-obj 'expm1 #f (_fun _double -> _double) (const #f)))
