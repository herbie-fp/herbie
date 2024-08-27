#lang racket

;; Builtin single-precision plugin (:precision binary32)

(require math/bigfloat)
(require "runtime/float32.rkt"
         "runtime/utils.rkt"
         "runtime/libm.rkt")

;; Do not run this file with `raco test`
(module test racket/base
  )

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

(define-syntax (define-libm-impl/binary32 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype [key value] ...)
     (with-syntax ([impl (string->symbol (format "~a.f32" (syntax->datum #'op)))]
                   [cname (string->symbol (format "~af" (syntax->datum #'op)))])
       #'(define-libm-impl cname (op impl itype ...) otype [key value] ...))]))

(define-syntax-rule (define-libm-impls/binary32* (itype ... otype) name ...)
  (begin
    (define-libm-impl/binary32 name (itype ...) otype) ...))

(define-syntax-rule (define-libm-impls/binary32 [(itype ... otype) (name ...)] ...)
  (begin
    (define-libm-impls/binary32* (itype ... otype) name ...) ...))

(define-operator-impl (neg.f32 [x : binary32])
                      binary32
                      #:spec (neg x)
                      #:fpcore (! :precision binary32 (- x))
                      #:fl fl32-
                      #identities
                      (#:exact (neg.f32 x)
                               [distribute-lft-neg-in (neg.f32 (* a b)) (* (neg.f32 a) b)]
                               [distribute-rgt-neg-in (neg.f32 (* a b)) (* a (neg.f32 b))]
                               [distribute-neg-in (neg.f32 (+ a b)) (+ (neg a) (neg b))]
                               [distribute-neg-frac (neg.f32 (/ x y)) (/ (neg.f32 x) y)]
                               [distribute-neg-frac2 (neg.f32 (/ x y)) (/ x (neg.f32 y))]
                               [remove-double-neg (neg.f32 (neg.f32 a)) a]
                               [neg-sub0 (neg.f64 b) (- 0 b)]
                               [neg-mul-1 (neg.f64 a) (* -1 a)]))
(define-operator-impl (+.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (+ x y)
                      #:fpcore (! :precision binary32 (+ x y))
                      #:fl fl32+
                      #:identities
                      (#:commutes [distribute-neg-out (+.f32 (neg a) (neg b)) (neg (+.f32 a b))]
                                  [+-lft-identity (+.f32 0 a) a]
                                  [+-rgt-identity (+.f32 a 0) a]
                                  [unsub-neg (+.f32 a (neg b)) (- a b)]))
(define-operator-impl (-.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (- x y)
                      #:fpcore (! :precision binary32 (- x y))
                      #:fl fl32-
                      #:identities ([cancel-sign-sub (-.f32 a (* (neg b) c)) (+ a (* b c))]
                                    [cancel-sign-sub-inv (-.f32 a (* b c)) (+ a (* (neg b) c))]
                                    [+-inverses (-.f32 a a) 0]
                                    [--rgt-identity (-.f64 a 0) a]
                                    [sub0-neg (-.f64 0 a) (neg a)]
                                    [sub-neg (-.f32 a b) (+ a (neg b))]))
(define-operator-impl (*.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (* x y)
                      #:fpcore (! :precision binary32 (* x y))
                      #:fl fl32*
                      #identities
                      (#:commutes [distribute-lft-neg-out (*.f32 (neg x) y) (neg (*.f32 x y))]
                                  [distribute-rgt-neg-out (*.f32 x (neg y)) (neg (*.f32 x y))]
                                  [mul0-lft (*.f32 0 a) 0]
                                  [mul0-rgt (*.f32 a 0) 0]
                                  [*-lft-identity (*.f32 1 a) a]
                                  [*-rgt-identity (*.f32 a 1) a]
                                  [mul-1-neg (*.f32 -1 a) (neg a)]
                                  [*-un-lft-identity a (*.f32 1 a)]))
(define-operator-impl (/.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (/ x y)
                      #:fpcore (! :precision binary32 (/ x y))
                      #:fl fl32/
                      #identities
                      ([distribute-frac-neg (/.f32 (neg x) y) (neg (/.f32 x y))]
                       [distribute-frac-neg2 (/.f32 x (neg y)) (neg (/.f32 x y))]
                       [div0 (/.f32 0 a) 0]
                       [*-inverses (/.f32 a a) 1]
                       [/-rgt-identity (/.f32 a 1) a]))

(define-libm-impls/binary32 [(binary32 binary32)
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
                            [(binary32 binary32 binary32)
                             (atan2 copysign fdim fmax fmin fmod hypot pow remainder)]
                            [(binary32 binary32 binary32 binary32) (fma)])

(define-comparator-impls binary32
                         [== ==.f32 =]
                         [!= !=.f32 (negate =)]
                         [< <.f32 <]
                         [> >.f32 >]
                         [<= <=.f32 <=]
                         [>= >=.f32 >=])

(define-operator-impl (binary64->binary32 [x : binary64])
                      binary32
                      #:spec x
                      #:fpcore (! :precision binary32 (cast x))
                      #:fl (curryr ->float32)
                      #:op cast)

(define-operator-impl (binary32->binary64 [x : binary32])
                      binary64
                      #:spec x
                      #:fpcore (! :precision binary64 (cast x))
                      #:fl identity
                      #:op cast)
