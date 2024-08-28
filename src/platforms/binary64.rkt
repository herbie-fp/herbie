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
    [(_ op (itype ...) otype attrib ...)
     (with-syntax ([impl (string->symbol (format "~a.f64" (syntax->datum #'op)))])
       #'(define-libm-impl op (op impl itype ...) otype attrib ...))]))

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
                      #:identities (#:exact (neg.f64 a)
                                    [distribute-lft-neg-in (neg.f64 (* a b)) (* (neg.f64 a) b)]
                                    [distribute-rgt-neg-in (neg.f64 (* a b)) (* a (neg.f64 b))]
                                    [distribute-neg-in (neg.f64 (+ a b)) (+ (neg a) (neg b))]
                                    [distribute-neg-frac (neg.f64 (/ a b)) (/ (neg.f64 a) b)]
                                    [distribute-neg-frac2 (neg.f64 (/ a b)) (/ a (neg.f64 b))]
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
                      (#:commutes [distribute-lft-neg-out (*.f64 (neg a) b) (neg (*.f64 a b))]
                                  [distribute-rgt-neg-out (*.f64 a (neg b)) (neg (*.f64 a b))]
                                  [mul0-lft (*.f64 0 a) 0]
                                  [mul0-rgt (*.f64 a 0) 0]
                                  [*-lft-identity (*.f64 1 a) a]
                                  [*-rgt-identity (*.f64 a 1) a]
                                  [mul-1-neg (*.f64 -1 a) (neg a)]
                                  [*-un-lft-identity a (*.f64 1 a)]
                                  [sqr-neg (*.f64 (neg a) (neg a)) (*.f64 a a)]
                                  [sqr-abs (*.f64 (fabs a) (fabs a)) (*.f64 a a)]
                                  [mul-fabs (*.f64 (fabs a) (fabs b)) (fabs (*.f64 a b))]
                                  [sqr-sin-b (*.f64 (sin x) (sin x)) (- 1 (*.f64 (cos x) (cos x)))]
                                  [sqr-cos-b (*.f64 (cos x) (cos x)) (- 1 (*.f64 (sin x) (sin x)))]))
(define-operator-impl (/.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (/ x y)
                      #:fpcore (! :precision binary64 (/ x y))
                      #:fl fl64/
                      #identities
                      ([distribute-frac-neg (/.f64 (neg a) b) (neg (/.f64 a b))]
                       [distribute-frac-neg2 (/.f64 a (neg b)) (neg (/.f64 a b))]
                       [div0 (/.f64 0 a) 0]
                       [*-inverses (/.f64 a a) 1]
                       [/-rgt-identity (/.f64 a 1) a]
                       [div-fabs (/.f64 (fabs a) (fabs b)) (fabs (/.f64 a b))]
                       [inv-pow (/.f64 1 a) (pow a -1)]))

(define-libm-impl/binary64 fabs
                           (binary64)
                           binary64
                           #:identities ([(fabs.f64 (fabs.f64 a)) (fabs.f64 a)]
                                         [fabs-sub (fabs.f64 (- a b)) (fabs.f64 (- b a))]
                                         [fabs-neg (fabs.f64 (neg a)) (fabs.f64 a)]
                                         [fabs-sqr (fabs.f64 (* a a)) (* a a)]
                                         [fabs-mul (fabs.f64 (* a b)) (* (fabs.f64 a) (fabs.f64 b))]
                                         [fabs-div (fabs.f64 (/ a b)) (/ (fabs.f64 a) (fabs.f64 b))]
                                         [neg-fabs (fabs.f64 x) (fabs.f64 (neg x))]))

(define-libm-impl/binary64 pow
                           (binary64 binary64)
                           binary64
                           #:identities ([unpow1 (pow.f64 a 1) a] [unpow0 (pow.f64 a 0) 1]
                                                                  [pow-base-1 (pow.f64 1 a) 1]
                                                                  [pow1 a (pow.f64 a 1)]
                                                                  [pow-base-0 (pow.f64 0 a) 0]))

(define-libm-impl/binary64 sin
                           (binary64)
                           binary64
                           #:identities
                           ([sin-0 (sin.f64 0) 0] [sin-neg (sin.f64 (neg x)) (neg (sin.f64 x))]))

(define-libm-impl/binary64 cos
                           (binary64)
                           binary64
                           #:identities
                           ([cos-0 (cos.f64 0) 1] [cos-neg (cos.f64 (neg x)) (cos.f64 x)]))

(define-libm-impl/binary64 tan
                           (binary64)
                           binary64
                           #:identities
                           ([tan-0 (tan.f64 0) 0] [tan-neg (tan.f64 (neg x)) (neg (tan.f64 x))]))

(define-libm-impl/binary64 sinh
                           (binary64)
                           binary64
                           #:identities
                           ([sinh-neg (sinh.f64 (neg x)) (neg (sinh.f64 x))] [sinh-0 (sinh.f64 0) 0]))

(define-libm-impl/binary64 cosh
                           (binary64)
                           binary64
                           #:identities
                           ([cosh-neg (cosh.f64 (neg x)) (cosh.f64 x)] [cosh-0 (cosh.f64 0) 1]))

(define-libm-impls/binary64 [(binary64 binary64)
                             (acos acosh
                                   asin
                                   asinh
                                   atan
                                   atanh
                                   cbrt
                                   ceil
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
                                   sinh
                                   sqrt
                                   tanh
                                   tgamma
                                   trunc)]
                            [(binary64 binary64 binary64)
                             (atan2 copysign fdim fmax fmin fmod hypot remainder)]
                            [(binary64 binary64 binary64 binary64) (fma)])

(define-comparator-impls binary64
                         [== ==.f64 =]
                         [!= !=.f64 (negate =)]
                         [< <.f64 < #:identities ([lt-same (<.f64 x x) (FALSE)])]
                         [> >.f64 > #:identities ([gt-same (>.f64 x x) (FALSE)])]
                         [<= <=.f64 <= #:identities ([lte-same (<=.f64 x x) (TRUE)])]
                         [>= >=.f64 >= #:identities ([gte-same (>=.f64 x x) (TRUE)])])
