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
    [(_ op (itype ...) otype attrib ...)
     (with-syntax ([impl (string->symbol (format "~a.f32" (syntax->datum #'op)))]
                   [cname (string->symbol (format "~af" (syntax->datum #'op)))])
       #'(define-libm-impl cname (op impl itype ...) otype attrib ...))]))

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
                      #:identities (#:exact (neg.f32 x)
                                    [distribute-lft-neg-in (neg.f32 (* a b)) (* (neg.f32 a) b)]
                                    [distribute-rgt-neg-in (neg.f32 (* a b)) (* a (neg.f32 b))]
                                    [distribute-neg-in (neg.f32 (+ a b)) (+ (neg a) (neg b))]
                                    [distribute-neg-frac (neg.f32 (/ a b)) (/ (neg.f32 a) b)]
                                    [distribute-neg-frac2 (neg.f32 (/ a b)) (/ a (neg.f32 b))]
                                    [remove-double-neg (neg.f32 (neg.f32 a)) a]
                                    [neg-sub0 (neg.f32 b) (- 0 b)]
                                    [neg-mul-1 (neg.f32 a) (* -1 a)]))
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
                                    [--rgt-identity (-.f32 a 0) a]
                                    [sub0-neg (-.f32 0 a) (neg a)]
                                    [sub-neg (-.f32 a b) (+ a (neg b))]))
(define-operator-impl (*.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (* x y)
                      #:fpcore (! :precision binary32 (* x y))
                      #:fl fl32*
                      #:identities
                      (#:commutes [distribute-lft-neg-out (*.f32 (neg x) y) (neg (*.f32 x y))]
                                  [distribute-rgt-neg-out (*.f32 x (neg y)) (neg (*.f32 x y))]
                                  [mul0-lft (*.f32 0 a) 0]
                                  [mul0-rgt (*.f32 a 0) 0]
                                  [*-lft-identity (*.f32 1 a) a]
                                  [*-rgt-identity (*.f32 a 1) a]
                                  [mul-1-neg (*.f32 -1 a) (neg a)]
                                  [*-un-lft-identity a (*.f32 1 a)]
                                  [sqr-neg (*.f32 (neg x) (neg x)) (*.f32 x x)]
                                  [sqr-abs (*.f32 (fabs x) (fabs x)) (*.f32 x x)]))
(define-operator-impl (/.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (/ x y)
                      #:fpcore (! :precision binary32 (/ x y))
                      #:fl fl32/
                      #:identities ([distribute-frac-neg (/.f32 (neg x) y) (neg (/.f32 x y))]
                                    [distribute-frac-neg2 (/.f32 x (neg y)) (neg (/.f32 x y))]
                                    [div0 (/.f32 0 a) 0]
                                    [*-inverses (/.f32 a a) 1]
                                    [/-rgt-identity (/.f32 a 1) a]
                                    [inv-pow (/.f32 1 a) (pow a -1)]))

(define-libm-impl/binary32 fabs
                           (binary32)
                           binary32
                           #:identities ([fabs-fabs (fabs.f32 (fabs.f32 a)) (fabs.f32 a)]
                                         [fabs-sub (fabs.f32 (- a b)) (fabs.f32 (- b a))]
                                         [fabs-neg (fabs.f32 (neg a)) (fabs.f32 a)]
                                         [fabs-sqr (fabs.f32 (* a a)) (* a a)]
                                         [fabs-mul (fabs.f32 (* a b)) (* (fabs.f32 a) (fabs.f32 b))]
                                         [fabs-div (fabs.f32 (/ a b)) (/ (fabs.f32 a) (fabs.f32 b))]
                                         [neg-fabs (fabs.f32 x) (fabs.f32 (neg x))]))

(define-libm-impl/binary32 exp
                           (binary32)
                           binary32
                           #:identities ([exp-0 (exp.f32 0) 1] [exp-1-e (exp.f32 1) (E)]
                                                               [1-exp 1 (exp.f32 0)]
                                                               [e-exp-1 (E) (exp.f32 1)]))

(define-libm-impl/binary32 pow
                           (binary64 binary32)
                           binary32
                           #:identities ([unpow1 (pow.f32 a 1) a] [unpow0 (pow.f32 a 0) 1]
                                                                  [pow-base-1 (pow.f32 1 a) 1]
                                                                  [pow1 a (pow.f32 a 1)]
                                                                  [pow-base-0 (pow.f32 0 a) 0]))

(define-libm-impl/binary32 sin
                           (binary32)
                           binary32
                           #:identities
                           ([sin-0 (sin.f32 0) 0] [sin-neg (sin.f32 (neg x)) (neg (sin.f32 x))]))

(define-libm-impl/binary32 cos
                           (binary32)
                           binary32
                           #:identities
                           ([cos-0 (cos.f32 0) 1] [cos-neg (cos.f32 (neg x)) (cos.f32 x)]))

(define-libm-impl/binary32 tan
                           (binary32)
                           binary32
                           #:identities
                           ([tan-0 (tan.f32 0) 0] [tan-neg (tan.f32 (neg x)) (neg (tan.f32 x))]))

(define-libm-impl/binary32 sinh
                           (binary32)
                           binary32
                           #:identities
                           ([sinh-neg (sinh.f32 (neg x)) (neg (sinh.f32 x))] [sinh-0 (sinh.f32 0) 0]))

(define-libm-impl/binary32 cosh
                           (binary32)
                           binary32
                           #:identities
                           ([cosh-neg (cosh.f32 (neg x)) (cosh.f32 x)] [cosh-0 (cosh.f32 0) 1]))

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
                             (atan2 copysign fdim fmax fmin fmod hypot remainder)]
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
