#lang racket

;;; Racket platform:
;;; Default Racket math functions

(require math/bigfloat
         math/flonum
         math/base
         math/special-functions
         "../syntax/types.rkt" ; for shift/unshift
         "../syntax/platform.rkt")
(provide platform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMPTY PLATFORM ;;;;;;;;;;;;;;;;;;;;;;;;

(define platform (make-empty-platform 'racket))

(platform-register-if-cost! platform #:if-cost 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define bool <bool>)

(platform-register-representation! platform #:repr bool #:cost 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([TRUE.rkt  () bool (TRUE)  (const true)  (! TRUE)  1]
  [FALSE.rkt () bool (FALSE) (const false) (! FALSE) 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(platform-register-implementations!
 platform
 ([not.rkt ([x : bool])            bool (not x)   not    (not x)   1]
  [and.rkt ([x : bool] [y : bool]) bool (and x y) and-fn (and x y) 1]
  [or.rkt  ([x : bool] [y : bool]) bool (or x y)  or-fn  (or x y)  1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define binary64 <binary64>)

(platform-register-representation! platform #:repr binary64 #:cost 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([PI.rkt       () binary64 (PI)       (const pi)        (! :precision binary64 PI)       1]
  [E.rkt        () binary64 (E)        (const (exp 1.0)) (! :precision binary64 E)        1]
  [INFINITY.rkt () binary64 (INFINITY) (const +inf.0)    (! :precision binary64 INFINITY) 1]
  [NAN.rkt      () binary64 (NAN)      (const +nan.0)    (! :precision binary64 NAN)      1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; comparators ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementations!
 platform
 ([==.rkt ([x : binary64] [y : binary64]) bool (== x y) =          (== x y) 1]
  [!=.rkt ([x : binary64] [y : binary64]) bool (!= x y) (negate =) (!= x y) 1]
  [<.rkt  ([x : binary64] [y : binary64]) bool (< x y)  <          (< x y)  1]
  [>.rkt  ([x : binary64] [y : binary64]) bool (> x y)  >          (> x y)  1]
  [<=.rkt ([x : binary64] [y : binary64]) bool (<= x y) <=         (<= x y) 1]
  [>=.rkt ([x : binary64] [y : binary64]) bool (>= x y) >=         (>= x y) 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (register-racket-operator stx)
  (syntax-case stx (real fl)
    [(_ (name tsig ...) fields ...)
     (let ([name (syntax-e #'name)])
       (with-syntax ([name (string->symbol (format "~a.rkt" name))])
         #'(platform-register-implementation!
            platform
            (make-operator-impl (name tsig ...) binary64 fields ...))))]))

(define-syntax-rule (register-1ary-racket-operator op fn cost)
  (register-racket-operator (op [x : binary64])
                              #:spec (op x)
                              #:fpcore (! :precision binary64 (op x))
                              #:fl fn
                              #:cost cost))

(define-syntax-rule (register-2ary-racket-operator op fn cost)
  (register-racket-operator (op [x : binary64] [y : binary64])
                              #:spec (op x y)
                              #:fpcore (! :precision binary64 (op x y))
                              #:fl fn
                              #:cost cost))

(define-syntax-rule (register-1ary-racket-operators [op fn cost] ...)
  (begin
    (register-1ary-racket-operator op fn cost) ...))

(define-syntax-rule (register-2ary-racket-operators [op fn cost] ...)
  (begin
    (register-2ary-racket-operator op fn cost) ...))

(define ((no-complex fun) . xs)
  (define res (apply fun xs))
  (if (real? res) res +nan.0))

(define (bffmod x mod)
  (bigfloat->flonum (bf- (bf x) (bf* (bftruncate (bf/ (bf x) (bf mod))) (bf mod)))))

(define (bffma x y z)
  (bigfloat->flonum (bf+ (bf* (bf x) (bf y)) (bf z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; unary operators ;;;;;;;;;;;;;;;;;;;;;;;

;; neg operation has a specific format with regard to fpcore, (- x) instead of (neg x)
(platform-register-implementation! platform
                                     (make-operator-impl (neg.rkt [x : binary64])
                                                         binary64
                                                         #:spec (neg x)
                                                         #:fpcore (! :precision binary64 (- x))
                                                         #:fl -
                                                         #:cost 1))

; ([op fn cost] ...) 
(register-1ary-racket-operators
 [acos   (no-complex acos)                                        1]
 [acosh  (no-complex acosh)                                       1]
 [asin   (no-complex asin)                                        1]
 [asinh  (no-complex asinh)                                       1]
 [atan   (no-complex atan)                                        1]
 [atanh  (no-complex atanh)                                       1]
 [cbrt   (no-complex (λ (x) (expt x 1/3)))                        1]
 [ceil   ceiling                                                  1]
 [cos    cos                                                      1]
 [cosh   cosh                                                     1]
 [erf    (no-complex erf)                                         1]
 [exp    exp                                                      1]
 [exp2   (no-complex (λ (x) (expt 2 x)))                          1]
 [fabs   abs                                                      1]
 [floor  floor                                                    1]
 [lgamma log-gamma                                                1]
 [log    (no-complex log)                                         1]
 [log10  (no-complex (λ (x) (log x 10)))                          1]
 [log2   (from-bigfloat 'bflog2)                                  1]
 [logb   (λ (x) (floor (bigfloat->flonum (bflog2 (bf (abs x)))))) 1]
 [rint   round                                                    1]
 [round  round                                                    1]
 [sin    sin                                                      1]
 [sinh   sinh                                                     1]
 [sqrt   (no-complex sqrt)                                        1]
 [tan    tan                                                      1]
 [tanh   tanh                                                     1]
 [tgamma gamma                                                    1]
 [trunc  truncate                                                 1])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; binary operators ;;;;;;;;;;;;;;;;;;;;;;

; ([op fn cost] ...) 
(register-2ary-racket-operators
 [+         +                         1]
 [-         -                         1]
 [*         *                         1]
 [/         /                         1]
 [atan2     (no-complex atan)         1]
 [copysign  (λ (x y)
              (if (>= y 0)
                  (abs x)
                  (- (abs x))))       1]
 [fdim      (λ (x y) (max (- x y) 0)) 1]
 [fmax      (λ (x y)
              (cond
                [(nan? x) y]
                [(nan? y) x]
                [else (max x y)]))    1]
 [fmin      (λ (x y)
              (cond
                [(nan? x) y]
                [(nan? y) x]
                [else (min x y)]))    1]
 [fmod      bffmod                    1]
 [pow       (no-complex expt)         1]
 [remainder remainder                 1])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; accelerators ;;;;;;;;;;;;;;;;;;;;;;;;;;

; ([name     ([var : repr] ...)                             otype    spec                       fl                      fpcore                           cost])
(platform-register-implementations!
 platform
 ([erfc.rkt  ([x : binary64])                               binary64 (- 1 (erf x))              erfc                     (! :precision binary64 (erfc x))    1]
  [expm1.rkt ([x : binary64])                               binary64 (- (exp x) 1)              (from-bigfloat 'bfexpm1) (! :precision binary64 (expm1 x))   1]
  [log1p.rkt ([x : binary64])                               binary64 (log (+ 1 x))              (from-bigfloat 'bflog1p) (! :precision binary64 (log1p x))   1]
  [hypot.rkt ([x : binary64] [y : binary64])                binary64 (sqrt (+ (* x x) (* y y))) (from-bigfloat 'bfhypot) (! :precision binary64 (hypot x y)) 1]
  [fma.rkt   ([x : binary64] [y : binary64] [z : binary64]) binary64 (+ (* x y) z)              bffma                    (! :precision binary64 (fma x y z)) 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REGISTER PLATFORM ;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (display-platform platform))

;; Do not run this file during testing
(module test racket/base
  )
