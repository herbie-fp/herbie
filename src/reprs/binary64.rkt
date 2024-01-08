#lang racket

;; Builtin double-precision plugin (:precision binary64)

(require math/flonum math/bigfloat)
(require "runtime/utils.rkt" "bool.rkt")

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

(require ffi/unsafe)
(define-syntax (define-libm-operator stx)
  (syntax-case stx (real)
    [(_ (op real ...) [key value] ...)
     (let* ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))]
            [sym2-append (λ (x y) (string->symbol (string-append (symbol->string x) (symbol->string y))))]
            [name (sym2-append (syntax-e (car (syntax-e (cadr (syntax-e stx))))) '.f64)])
       #`(begin
          (define fl-proc
            (get-ffi-obj 'op #f (_fun #,@(build-list num-args (λ (_) #'_double)) -> _double)
                          (λ () #f)))
           (when fl-proc
            (define-operator-impl (op #,name #,@(build-list num-args (λ (_) #'binary64))) binary64
              [fl fl-proc] [key value] ...))))]))

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
 expm1
 fabs
 floor
 lgamma
 log
 log10
 log1p
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
 atan2
 copysign
 fdim
 fmax
 fmin
 fmod
 hypot
 pow
 remainder)

(define-libm-operator (fma real real real))

(define-comparator-impls binary64
  [== ==.f64 =]
  [!= !=.f64 (negate =)]
  [< <.f64 <]
  [> >.f64 >]
  [<= <=.f64 <=]
  [>= >=.f64 >=])
