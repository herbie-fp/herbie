#lang racket

;; binary64 builtin plugin

(require math/bigfloat math/flonum)
(require (submod "syntax/syntax.rkt" internals)
         (submod "interface.rkt" internals)
         "syntax/syntax.rkt" "common.rkt"
         "errors.rkt")

; (eprintf "Loading binary64 support...\n")

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

(require ffi/unsafe)
(define-syntax (define-libm-operator stx)
  (syntax-case stx (real libm)
    [(_ (op real ...) [libm id] [key value] ...)
     (let* ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))]
            [sym2-append (λ (x y) (string->symbol (string-append (symbol->string x) (symbol->string y))))]
            [name (sym2-append (syntax-e (car (syntax-e (cadr (syntax-e stx))))) '.f64)])
       #`(begin
           (define (fallback . args)
             (warn 'fallback #:url "faq.html#native-ops"
                   "native `~a` not supported on your system, using fallback; ~a"
                   'op
                   "use --disable precision:fallback to disable fallbacks")
             (apply (operator-info 'op 'nonffi) args))
           (define fl-proc (get-ffi-obj 'id #f (_fun #,@(build-list num-args (λ (_) #'_double)) -> _double)
                                         (lambda () (*unknown-ops* (cons 'name (*unknown-ops*))) fallback)))
           (define-operator-impl (op #,name #,@(build-list num-args (λ (_) #'binary64))) binary64
             [fl fl-proc])))]))

(define-syntax-rule (define-1ary-libm-operator op name)
  (define-libm-operator (op real) [libm name]))

(define-syntax-rule (define-2ary-libm-operator op name)
  (define-libm-operator (op real real) [libm name]))

(define-syntax-rule (define-1ary-libm-operators [op name] ...)
  (begin (define-1ary-libm-operator op name) ...))

(define-syntax-rule (define-2ary-libm-operators [op name] ...)
  (begin (define-2ary-libm-operator op name) ...))


(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])

(define-1ary-libm-operators
 [acos acos]
 [acosh acosh]
 [asin asin]
 [asinh asinh]
 [atan atan]
 [atanh atanh]
 [cbrt cbrt]
 [ceil ceil]
 [cos cos]
 [cosh cosh]
 [erf erf]
 [erfc erfc]
 [exp exp]
 [exp2 exp2]
 [expm1 expm1]
 [fabs fabs]
 [floor floor]
 [j0 j0]
 [j1 j1]
 [lgamma lgamma]
 [log log]
 [log10 log10]
 [log1p log1p]
 [log2 log2]
 [logb logb]
 [rint rint]
 [round round]
 [sin sin]
 [sinh sinh]
 [sqrt sqrt]
 [tan tan]
 [tanh tanh]
 [tgamma tgamma]
 [trunc trunc]
 [y0 y0]
 [y1 y1])

(define-2ary-libm-operators
 [atan2 atan2]
 [copysign copysign]
 [fdim fdim]
 [fmax fmax]
 [fmin fmin]
 [fmod fmod]
 [hypot hypot]
 [pow pow]
 [remainder remainder])

(define-libm-operator (fma real real real)
 [libm fma])

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
