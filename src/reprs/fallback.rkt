#lang racket

;; Builtin fallback plugin (:precision racket)

(require math/base math/bigfloat math/flonum math/special-functions)
(require "../plugin.rkt" "bool.rkt")

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (racket real flonum?)
  bigfloat->flonum
  bf
  (shift 63 ordinal->flonum)
  (unshift 63 flonum->ordinal)
  64
  (disjoin nan? infinite?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-operator-impl (PI PI.rkt) racket
  [fl (const pi)])

(define-operator-impl (E E.rkt) racket
  [fl (const (exp 1.0))])

(define-operator-impl (INFINITY INFINITY.rkt) racket
  [fl (const +inf.0)])

(define-operator-impl (NAN NAN.rkt) racket
  [fl (const +nan.0)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-fallback-operator stx)
  (syntax-case stx (real fl)
    [(_ (op real ...) [fl fn] [key value] ...)
     (let* ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))]
            [sym2-append (λ (x y) (string->symbol (string-append (symbol->string x) (symbol->string y))))]
            [name (sym2-append (syntax-e (car (syntax-e (cadr (syntax-e stx))))) '.rkt)])
       #`(define-operator-impl (op #,name #,@(build-list num-args (λ (_) #'racket))) racket
          [fl fn] [key value] ...))]))

(define-syntax-rule (define-1ary-fallback-operator op fn)
  (define-fallback-operator (op real) [fl fn]))

(define-syntax-rule (define-2ary-fallback-operator op fn)
  (define-fallback-operator (op real real) [fl fn]))

(define-syntax-rule (define-1ary-fallback-operators [op fn] ...)
  (begin (define-1ary-fallback-operator op fn) ...))

(define-syntax-rule (define-2ary-fallback-operators [op fn] ...)
  (begin (define-2ary-fallback-operator op fn) ...))

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


(define-1ary-fallback-operators
 [neg -]
 [acos (no-complex acos)]
 [acosh (no-complex acosh)]
 [asin (no-complex asin)]
 [asinh (no-complex asinh)]
 [atan (no-complex atan)]
 [atanh (no-complex atanh)]
 [cbrt (no-complex (λ (x) (expt x 1/3)))]
 [ceil ceiling]
 [cos cos]
 [cosh cosh]
 [erf (no-complex erf)]
 [erfc erfc]
 [exp exp]
 [exp2 (no-complex (λ (x) (expt 2 x)))]
 [expm1 (from-bigfloat bfexpm1)]
 [fabs abs]
 [floor floor]
 [lgamma log-gamma]
 [log (no-complex log)]
 [log10 (no-complex (λ (x) (log x 10)))]
 [log1p (from-bigfloat bflog1p)]
 [log2 (from-bigfloat bflog2)]
 [logb (λ (x) (floor (bigfloat->flonum (bflog2 (bf (abs x))))))]
 [rint round]
 [round round]
 [sin sin]
 [sinh sinh]
 [sqrt (no-complex sqrt)]
 [tan tan]
 [tanh tanh]
 [tgamma gamma]
 [trunc truncate])

(define-2ary-fallback-operators
 [+ +]
 [- -]
 [* *]
 [/ /]
 [atan2 (no-complex atan)]
 [copysign (λ (x y) (if (>= y 0) (abs x) (- (abs x))))]
 [fdim (λ (x y) (max (- x y) 0))]
 [fmax (λ (x y) (cond [(nan? x) y] [(nan? y) x] [else (max x y)]))]
 [fmin (λ (x y) (cond [(nan? x) y] [(nan? y) x] [else (min x y)]))]
 [fmod (from-bigfloat bffmod)]
 [hypot (from-bigfloat bfhypot)]
 [pow (no-complex expt)]
 [remainder remainder])

(define-fallback-operator (fma real real real)
 [fl (from-bigfloat bffma)])

(define-operator-impl (== ==.rkt racket racket) bool
  [fl =])

(define-operator-impl (!= !=.rkt racket racket) bool
  [fl (negate =)])

(define-operator-impl (< <.rkt racket racket) bool
  [fl <])

(define-operator-impl (> >.rkt racket racket) bool
  [fl >])

(define-operator-impl (<= <=.rkt racket racket) bool
  [fl <=])

(define-operator-impl (>= >=.rkt racket racket) bool
  [fl >=])

