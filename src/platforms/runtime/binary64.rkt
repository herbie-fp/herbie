#lang racket

;; Double-precision platform (math only)

(require math/flonum math/bigfloat ffi/unsafe)
(require "../../plugin.rkt" "bool.rkt"
         (only-in "libm.rkt"
           [define-binary64-impl/libm define-libm-operator]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operator-impl (PI PI.f64) binary64
  [fl (const pi)])

(define-operator-impl (E E.f64) binary64
  [fl (const (exp 1.0))])

(define-operator-impl (INFINITY INFINITY.f64) binary64
  [fl (const +inf.0)])

(define-operator-impl (NAN NAN.f64) binary64
  [fl (const +nan.0)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-1ary-libm-operator op)
  (define-libm-operator (op real)))

(define-syntax-rule (define-2ary-libm-operator op)
  (define-libm-operator (op real real)))

(define-syntax-rule (define-1ary-libm-operators op ...)
  (begin (define-1ary-libm-operator op) ...))

(define-syntax-rule (define-2ary-libm-operators op ...)
  (begin (define-2ary-libm-operator op) ...))


(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])

(define-1ary-libm-operators
 acos
 acosh
 asin
 asinh
 atan
 atanh
 cbrt
 ceil
 cos
 cosh
 erf
 erfc
 exp
 exp2
;  expm1
 fabs
 floor
 lgamma
 log
 log10
;  log1p
 log2
 logb
 rint
 round
 sin
 sinh
 sqrt
 tan
 tanh
 tgamma
 trunc)

(define-2ary-libm-operators
;  atan2
 copysign
 fdim
 fmax
 fmin
 fmod
;  hypot
 pow
 remainder)

; (define-libm-operator (fma real real real))

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
