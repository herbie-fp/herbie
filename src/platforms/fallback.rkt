#lang racket

;; Builtin fallback plugin (:precision racket)

(require math/base
         math/bigfloat
         math/flonum
         math/special-functions)
(require "runtime/utils.rkt")

;; Do not run this file with `raco test`
(module test racket/base
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants racket
                  [PI PI.rkt pi]
                  [E E.rkt (exp 1.0)]
                  [INFINITY INFINITY.rkt +inf.0]
                  [NAN NAN.rkt +nan.0])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-1ary-fallback-operator op fn)
  (with-syntax ([impl (string->symbol (format "~a.rkt" (syntax->datum #'op)))] [fl fn])
    #'(define-operator-impl (impl [x : binary64])
                             binary64
                             #:spec (op x)
                             #:fpcore (! :precision binary64 :math-library racket (op x))
                             #:fl fn)))

(define-syntax-rule (define-2ary-fallback-operator op fn)
  (with-syntax ([impl (string->symbol (format "~a.rkt" (syntax->datum #'op)))])
    #'(define-operator-impl (impl [x : binary64] [y : binary64])
                             binary64
                             #:spec (op x y)
                             #:fpcore (! :precision binary64 :math-library racket (op x y))
                             #:fl fn)))

(define-syntax-rule (define-1ary-fallback-operators [op fn] ...)
  (begin
    (define-1ary-fallback-operator op fn) ...))

(define-syntax-rule (define-2ary-fallback-operators [op fn] ...)
  (begin
    (define-2ary-fallback-operator op fn) ...))

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

(define-1ary-fallback-operators [neg -]
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

(define-2ary-fallback-operators [+ +]
                                [- -]
                                [* *]
                                [/ /]
                                [atan2 (no-complex atan)]
                                [copysign (λ (x y) (if (>= y 0) (abs x) (- (abs x))))]
                                [fdim (λ (x y) (max (- x y) 0))]
                                [fmax
                                 (λ (x y)
                                   (cond
                                     [(nan? x) y]
                                     [(nan? y) x]
                                     [else (max x y)]))]
                                [fmin
                                 (λ (x y)
                                   (cond
                                     [(nan? x) y]
                                     [(nan? y) x]
                                     [else (min x y)]))]
                                [fmod (from-bigfloat bffmod)]
                                [hypot (from-bigfloat bfhypot)]
                                [pow (no-complex expt)]
                                [remainder remainder])

(define-operator-impl (fma.rkt [x : binary64] [y : binary64] [z : binary64])
                       binary64
                       #:spec (+ (* x y) z)
                       #:fpcore (! :precision binary64 :math-library racket (fma x y z))
                       #:fl (from-bigfloat bffma)
                       #:op fma)

(define-comparator-impls racket
                         [== ==.rkt =]
                         [!= !=.rkt (negate =)]
                         [< <.rkt <]
                         [> >.rkt >]
                         [<= <=.rkt <=]
                         [>= >=.rkt >=])
