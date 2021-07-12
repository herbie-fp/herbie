#lang racket

;; binary32 builtin plugin

(require math/bigfloat math/flonum)
(require (submod "syntax/syntax.rkt" internals)
         (submod "interface.rkt" internals)
         "syntax/syntax.rkt" "common.rkt"
         "errors.rkt" "float32.rkt")

(eprintf "Loading binary32 support...\n")

; for define-libm-operator (must be top-level)
(require ffi/unsafe)

;; Only load everything below if single flonum supported?
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

(define-syntax (define-libm-operator stx)
  (syntax-case stx (real libm)
    [(_ (op real ...) [libm id] [key value] ...)
     (let* ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))]
            [sym2-append (λ (x y) (string->symbol (string-append (symbol->string x) (symbol->string y))))]
            [name (sym2-append (syntax-e (car (syntax-e (cadr (syntax-e stx))))) '.f32)])
       #`(begin
           (define (fallback . args)
             (warn 'fallback #:url "faq.html#native-ops"
                   "native `~a` not supported on your system, using fallback; ~a"
                   'op
                   "use --disable precision:fallback to disable fallbacks")
             (apply (operator-info 'op 'nonffi) args))
           (define fl-proc (get-ffi-obj 'id #f (_fun #,@(build-list num-args (λ (_) #'_float)) -> _float)
                                        (lambda () (*unknown-ops* (cons 'name (*unknown-ops*))) fallback)))
           (define-operator-impl (op #,name #,@(build-list num-args (λ (_) #'binary32))) binary32
             [fl (λ args (->float32 (apply fl-proc args)))])))]))

(define-syntax-rule (define-1ary-libm-operator op name)
  (define-libm-operator (op real) [libm name]))

(define-syntax-rule (define-2ary-libm-operator op name)
  (define-libm-operator (op real real) [libm name]))

(define-syntax-rule (define-1ary-libm-operators [op name] ...)
  (begin (define-1ary-libm-operator op name) ...))

(define-syntax-rule (define-2ary-libm-operators [op name] ...)
  (begin (define-2ary-libm-operator op name) ...))


(define-operator-impl (neg neg.f32 binary32) binary32 [fl fl32-])
(define-operator-impl (+ +.f32 binary32 binary32) binary32 [fl fl32+])
(define-operator-impl (- -.f32 binary32 binary32) binary32 [fl fl32-])
(define-operator-impl (* *.f32 binary32 binary32) binary32 [fl fl32*])
(define-operator-impl (/ /.f32 binary32 binary32) binary32 [fl fl32/])

(define-1ary-libm-operators
 [acos acosf]
 [acosh acoshf]
 [asin asinf]
 [asinh asinhf]
 [atan atanf]
 [atanh atanhf]
 [cbrt cbrtf]
 [ceil ceilf]
 [cos cosf]
 [cosh coshf]
 [erf erff]
 [erfc erfcf]
 [exp expf]
 [exp2 exp2f]
 [expm1 expm1f]
 [fabs fabsf]
 [floor floorf]
 [j0 j0f]
 [j1 j1f]
 [lgamma lgammaf]
 [log logf]
 [log10 log10f]
 [log1p log1pf]
 [log2 log2f]
 [logb logbf]
 [rint rintf]
 [round roundf]
 [sin sinf]
 [sinh sinhf]
 [sqrt sqrtf]
 [tan tanf]
 [tanh tanhf]
 [tgamma tgammaf]
 [trunc truncf]
 [y0 y0f]
 [y1 y1f])

(define-2ary-libm-operators
 [atan2 atan2f]
 [copysign copysignf]
 [fdim fdimf]
 [fmax fmaxf]
 [fmin fminf]
 [fmod fmodf]
 [hypot hypotf]
 [pow powf]
 [remainder remainderf])

(define-libm-operator (fma real real real)
 [libm fmaf])

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
