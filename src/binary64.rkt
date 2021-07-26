#lang racket

;; binary64 builtin plugin

(require math/base math/bigfloat math/flonum math/special-functions)
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
    [(_ (op real ...) [libm id] [nonffi fn] [key value] ...)
     (let* ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))]
            [sym2-append (λ (x y) (string->symbol (string-append (symbol->string x) (symbol->string y))))]
            [name (sym2-append (syntax-e (car (syntax-e (cadr (syntax-e stx))))) '.f64)])
       #`(begin
           (define fl-proc (get-ffi-obj 'id #f (_fun #,@(build-list num-args (λ (_) #'_double)) -> _double)
                                         (lambda ()
                                          (*unknown-ops* (cons '#,name (*unknown-ops*)))
                                          (warn 'fallback #:url "faq.html#native-ops"
                                                "native `~a` not supported on your system, using fallback; ~a"
                                                'op
                                                "use --disable precision:fallback to disable fallbacks")
                                          fn)))
           (define-operator-impl (op #,name #,@(build-list num-args (λ (_) #'binary64))) binary64
             [fl fl-proc])))]))

(define-syntax-rule (define-1ary-libm-operator op name fallback)
  (define-libm-operator (op real) [libm name] [nonffi fallback]))

(define-syntax-rule (define-2ary-libm-operator op name fallback)
  (define-libm-operator (op real real) [libm name] [nonffi fallback]))

(define-syntax-rule (define-1ary-libm-operators [op name fallback] ...)
  (begin (define-1ary-libm-operator op name fallback) ...))

(define-syntax-rule (define-2ary-libm-operators [op name fallback] ...)
  (begin (define-2ary-libm-operator op name fallback) ...))

(define (no-complex fun)
  (λ xs
     (define res (apply fun xs))
     (if (real? res) res +nan.0)))

(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define (bffmod x mod)
  (bf- x (bf* (bftruncate (bf/ x mod)) mod)))

(define (bffma x y z)
  (bf+ (bf* x y) z))


(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])

(define-1ary-libm-operators
 [acos acos (no-complex acos)]
 [acosh acosh (no-complex acosh)]
 [asin asin (no-complex asin)]
 [asinh asinh (no-complex asinh)]
 [atan atan (no-complex atan)]
 [atanh atanh (no-complex atanh)]
 [cbrt cbrt (no-complex (λ (x) (expt x 1/3)))]
 [ceil ceil ceiling]
 [cos cos cos]
 [cosh cosh cosh]
 [erf erf (no-complex erf)]
 [erfc erfc erfc]
 [exp exp exp]
 [exp2 exp2 (no-complex (λ (x) (expt 2 x)))]
 [expm1 expm1 (from-bigfloat bfexpm1)]
 [fabs fabs abs]
 [floor floor floor]
 [j0 j0 (from-bigfloat bfbesj0)]
 [j1 j1 (from-bigfloat bfbesj1)]
 [lgamma lgamma log-gamma]
 [log log (no-complex log)]
 [log10 log10 (no-complex (λ (x) (log x 10)))]
 [log1p log1p (from-bigfloat bflog1p)]
 [log2 log2 (from-bigfloat bflog2)]
 [logb logb (λ (x) (floor (bigfloat->flonum (bflog2 (bf (abs x))))))]
 [rint rint round]
 [round round round]
 [sin sin sin]
 [sinh sinh sinh]
 [sqrt sqrt (no-complex sqrt)]
 [tan tan tan]
 [tanh tanh tanh]
 [tgamma tgamma gamma]
 [trunc trunc truncate]
 [y0 y0 (from-bigfloat bfbesy0)]
 [y1 y1 (from-bigfloat bfbesy1)])

(define-2ary-libm-operators
 [atan2 atan2 (no-complex atan)]
 [copysign copysign (λ (x y) (if (>= y 0) (abs x) (- (abs x))))]
 [fdim fdim (λ (x y) (max (- x y) 0))]
 [fmax fmax (λ (x y) (cond [(nan? x) y] [(nan? y) x] [else (max x y)]))]
 [fmin fmin (λ (x y) (cond [(nan? x) y] [(nan? y) x] [else (min x y)]))]
 [fmod fmod (from-bigfloat bffmod)]
 [hypot hypot (from-bigfloat bfhypot)]
 [pow pow (no-complex expt)]
 [remainder remainder remainder])

(define-libm-operator (fma real real real)
 [libm fma] [nonffi (from-bigfloat bffma)])

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
