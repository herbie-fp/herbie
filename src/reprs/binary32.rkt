#lang racket

;; Builtin single-precision plugin (:precision binary32)

(require math/flonum math/bigfloat)
(require "runtime/float32.rkt" "runtime/utils.rkt" "binary64.rkt")

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

(require ffi/unsafe)

(define-syntax (define-libm-operator stx)
  (syntax-case stx (real)
    [(_ (op real ...) [key value] ...)
     (let* ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))]
            [sym2-append (λ (x y) (string->symbol (string-append (symbol->string x) (symbol->string y))))]
            [name (sym2-append (syntax-e (car (syntax-e (cadr (syntax-e stx))))) '.f32)]
            [cname (sym2-append (syntax-e (car (syntax-e (cadr (syntax-e stx))))) 'f)])
       #`(begin
           (define fl-proc
            (get-ffi-obj '#,cname #f (_fun #,@(build-list num-args (λ (_) #'_float)) -> _float)
                         (λ () #f)))
           (when fl-proc
            (define-operator-impl (op #,name #,@(build-list num-args (λ (_) #'binary32))) binary32
              [fl fl-proc] [key value] ...))))]))

(define-syntax-rule (define-1ary-libm-operator op)
  (define-libm-operator (op real)))

(define-syntax-rule (define-2ary-libm-operator op)
  (define-libm-operator (op real real)))

(define-syntax-rule (define-1ary-libm-operators op ...)
  (begin (define-1ary-libm-operator op) ...))

(define-syntax-rule (define-2ary-libm-operators op ...)
  (begin (define-2ary-libm-operator op) ...))

(define-operator-impl (neg neg.f32 binary32) binary32 [fl fl32-])
(define-operator-impl (+ +.f32 binary32 binary32) binary32 [fl fl32+])
(define-operator-impl (- -.f32 binary32 binary32) binary32 [fl fl32-])
(define-operator-impl (* *.f32 binary32 binary32) binary32 [fl fl32*])
(define-operator-impl (/ /.f32 binary32 binary32) binary32 [fl fl32/])

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
