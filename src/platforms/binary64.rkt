#lang racket

;; Builtin double-precision plugin (:precision binary64)

(require math/flonum
         math/bigfloat)

(require "runtime/utils.rkt"
         "runtime/libm.rkt")

;; Do not run this file with `raco test`
(module test racket/base
  )

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
  (begin
    (define-libm-impl/binary64 name (itype ...) otype) ...))

(define-syntax-rule (define-libm-impls/binary64 [(itype ... otype) (name ...)] ...)
  (begin
    (define-libm-impls/binary64* (itype ... otype) name ...) ...))

(define-operator-impl (neg.f64 [x : binary64])
                      binary64
                      #:spec (neg x)
                      #:fpcore (! :precision binary64 (- x))
                      #:fl fl64-
                      #identities
                      (#:exact (neg.f64 x)
                               [distribute-lft-neg-in (neg.f64 (* a b)) (* (neg.f64 a) b)]
                               [distribute-rgt-neg-in (neg.f64 (* a b)) (* a (neg.f64 b))]
                               [distribute-neg-in (neg.f64 (+ a b)) (+ (neg a) (neg b))]
                               [distribute-neg-frac (neg.f64 (/ x y)) (/ (neg.f64 x) y)]
                               [distribute-neg-frac2 (neg.f64 (/ x y)) (/ x (neg.f64 y))]
                               [remove-double-neg (neg.f32 (neg.f32 a)) a]
                               [neg-sub0 (neg.f64 b) (- 0 b)]
                               [neg-mul-1 (neg.f64 a) (* -1 a)]))
(define-operator-impl (+.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (+ x y)
                      #:fpcore (! :precision binary64 (+ x y))
                      #:fl fl64+
                      #:identities
                      (#:commutes [distribute-neg-out (+.f64 (neg a) (neg b)) (neg (+.f64 a b))]
                                  [+-lft-identity (+.f64 0 a) a]
                                  [+-rgt-identity (+.f64 a 0) a]
                                  [unsub-neg (+.f64 a (neg b)) (- a b)]))
(define-operator-impl (-.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (- x y)
                      #:fpcore (! :precision binary64 (- x y))
                      #:fl fl64-
                      #:identities ([cancel-sign-sub (-.f64 a (* (neg b) c)) (+ a (* b c))]
                                    [cancel-sign-sub-inv (-.f64 a (* b c)) (+ a (* (neg b) c))]
                                    [+-inverses (-.f64 a a) 0]
                                    [--rgt-identity (-.f64 a 0) a]
                                    [sub0-neg (-.f64 0 a) (neg a)]
                                    [sub-neg (-.f64 a b) (+ a (neg b))]))
(define-operator-impl (*.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (* x y)
                      #:fpcore (! :precision binary64 (* x y))
                      #:fl fl64*
                      #identities
                      (#:commutes [distribute-lft-neg-out (*.f64 (neg x) y) (neg (*.f64 x y))]
                                  [distribute-rgt-neg-out (*.f64 x (neg y)) (neg (*.f64 x y))]
                                  [mul0-lft (*.f64 0 a) 0]
                                  [mul0-rgt (*.f64 a 0) 0]
                                  [*-lft-identity (*.f64 1 a) a]
                                  [*-rgt-identity (*.f64 a 1) a]
                                  [mul-1-neg (*.f64 -1 a) (neg a)]
                                  [*-un-lft-identity a (*.f64 1 a)]))
(define-operator-impl (/.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (/ x y)
                      #:fpcore (! :precision binary64 (/ x y))
                      #:fl fl64/
                      #identities
                      ([distribute-frac-neg (/.f64 (neg x) y) (neg (/.f64 x y))]
                       [distribute-frac-neg2 (/.f64 x (neg y)) (neg (/.f64 x y))]
                       [div0 (/.f64 0 a) 0]
                       [*-inverses (/.f64 a a) 1]
                       [/-rgt-identity (/.f64 a 1) a]))

(define-libm-impls/binary64 [(binary64 binary64)
                             (acos acosh
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
                                   trunc)]
                            [(binary64 binary64 binary64)
                             (atan2 copysign fdim fmax fmin fmod hypot pow remainder)]
                            [(binary64 binary64 binary64 binary64) (fma)])

(define-comparator-impls binary64
                         [== ==.f64 =]
                         [!= !=.f64 (negate =)]
                         [< <.f64 <]
                         [> >.f64 >]
                         [<= <=.f64 <=]
                         [>= >=.f64 >=])
