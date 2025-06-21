#lang racket

;;; The default platform:
;;; C/C++ on Linux with a full libm

(require math/bigfloat
         math/flonum
         math/base
         math/special-functions)

(require "../utils/float.rkt" ; for shift/unshift
         (submod "../syntax/platform.rkt" internals))

(define racket-platform (make-empty-platform 'racket #:if-cost 1 #:default-cost 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define bool
  (make-representation #:name 'bool
                       #:type 'bool
                       #:repr? boolean?
                       #:bf->repr identity
                       #:repr->bf identity
                       #:ordinal->repr (λ (x) (= x 0))
                       #:repr->ordinal (λ (x) (if x 1 0))
                       #:total-bits 1
                       #:special-value? (const #f)))

(platform-register-representation! racket-platform bool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation!
 racket-platform
 (make-operator-impl (TRUE.rkt) bool #:spec (TRUE) #:fl (const true) #:fpcore (! TRUE)))

(platform-register-implementation!
 racket-platform
 (make-operator-impl (FALSE.rkt) bool #:spec (FALSE) #:fl (const false) #:fpcore (! FALSE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(platform-register-implementation!
 racket-platform
 (make-operator-impl (not.rkt [x : bool]) bool #:spec (not x) #:fpcore (not x) #:fl not))

(platform-register-implementation! racket-platform
                                   (make-operator-impl (and.rkt [x : bool] [y : bool])
                                                       bool
                                                       #:spec (and x y)
                                                       #:fpcore (and x y)
                                                       #:fl and-fn))

(platform-register-implementation! racket-platform
                                   (make-operator-impl (or.rkt [x : bool] [y : bool])
                                                       bool
                                                       #:spec (or x y)
                                                       #:fpcore (or x y)
                                                       #:fl or-fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BINARY 64 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;

(define binary64
  (make-representation #:name 'binary64
                       #:type 'real
                       #:repr? flonum?
                       #:bf->repr bigfloat->flonum
                       #:repr->bf bf
                       #:ordinal->repr (shift 63 ordinal->flonum)
                       #:repr->ordinal (unshift 63 flonum->ordinal)
                       #:total-bits 64
                       #:special-value? nan?))

(platform-register-representation! racket-platform binary64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation! racket-platform
                                   (make-operator-impl (PI.rkt)
                                                       binary64
                                                       #:spec (PI)
                                                       #:fl (const pi)
                                                       #:fpcore (! :precision binary64 (PI))))

(platform-register-implementation! racket-platform
                                   (make-operator-impl (E.rkt)
                                                       binary64
                                                       #:spec (E)
                                                       #:fl (const (exp 1.0))
                                                       #:fpcore (! :precision binary64 (E))))

(platform-register-implementation! racket-platform
                                   (make-operator-impl (INFINITY.rkt)
                                                       binary64
                                                       #:spec (INFINITY)
                                                       #:fl (const +inf.0)
                                                       #:fpcore (! :precision binary64 (INFINITY))))

(platform-register-implementation! racket-platform
                                   (make-operator-impl (NAN.rkt)
                                                       binary64
                                                       #:spec (NAN)
                                                       #:fl (const +nan.0)
                                                       #:fpcore (! :precision binary64 (NAN))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; comparators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(platform-register-implementation! racket-platform
                                   (make-operator-impl (==.rkt [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (== x y)
                                                       #:fpcore (== x y)
                                                       #:fl =))

(platform-register-implementation! racket-platform
                                   (make-operator-impl (!=.rkt [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (!= x y)
                                                       #:fpcore (!= x y)
                                                       #:fl (negate =)))

(platform-register-implementation! racket-platform
                                   (make-operator-impl (<.rkt [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (< x y)
                                                       #:fpcore (< x y)
                                                       #:fl <))

(platform-register-implementation! racket-platform
                                   (make-operator-impl (>.rkt [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (> x y)
                                                       #:fpcore (> x y)
                                                       #:fl >))

(platform-register-implementation! racket-platform
                                   (make-operator-impl (<=. [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (<= x y)
                                                       #:fpcore (<= x y)
                                                       #:fl <=))

(platform-register-implementation! racket-platform
                                   (make-operator-impl (>=.rkt [x : binary64] [y : binary64])
                                                       bool
                                                       #:spec (>= x y)
                                                       #:fpcore (>= x y)
                                                       #:fl >=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (register-fallback-operator stx)
  (syntax-case stx (real fl)
    [(_ (name tsig ...) fields ...)
     (let ([name (syntax-e #'name)])
       (with-syntax ([name (string->symbol (format "~a.rkt" name))])
         #'(platform-register-implementation!
            racket-platform
            (make-operator-impl (name tsig ...) binary64 fields ...))))]))

(define-syntax-rule (register-1ary-fallback-operator op fn cost)
  (register-fallback-operator (op [x : binary64])
                              #:spec (op x)
                              #:fpcore (! :precision binary64 (op x))
                              #:fl fn
                              #:cost cost))

(define-syntax-rule (register-2ary-fallback-operator op fn cost)
  (register-fallback-operator (op [x : binary64] [y : binary64])
                              #:spec (op x y)
                              #:fpcore (! :precision binary64 (op x y))
                              #:fl fn
                              #:cost cost))

(define-syntax-rule (register-1ary-fallback-operators [op fn cost] ...)
  (begin
    (register-1ary-fallback-operator op fn cost) ...))

(define-syntax-rule (register-2ary-fallback-operators [op fn cost] ...)
  (begin
    (register-2ary-fallback-operator op fn cost) ...))

(define ((no-complex fun) . xs)
  (define res (apply fun xs))
  (if (real? res) res +nan.0))

(define ((from-bigfloat bff) . args)
  (bigfloat->flonum (apply bff (map bf args))))

(define (bffmod x mod)
  (bf- x (bf* (bftruncate (bf/ x mod)) mod)))

(define (bffma x y z)
  (bf+ (bf* x y) z))

(register-1ary-fallback-operators [neg - #f]
                                  [acos (no-complex acos) #f]
                                  [acosh (no-complex acosh) #f]
                                  [asin (no-complex asin) #f]
                                  [asinh (no-complex asinh) #f]
                                  [atan (no-complex atan) #f]
                                  [atanh (no-complex atanh) #f]
                                  [cbrt (no-complex (λ (x) (expt x 1/3))) #f]
                                  [ceil ceiling #f]
                                  [cos cos #f]
                                  [cosh cosh #f]
                                  [erf (no-complex erf) #f]
                                  [exp exp #f]
                                  [exp2 (no-complex (λ (x) (expt 2 x))) #f]
                                  [fabs abs #f]
                                  [floor floor #f]
                                  [lgamma log-gamma #f]
                                  [log (no-complex log) #f]
                                  [log10 (no-complex (λ (x) (log x 10))) #f]
                                  [log2 (from-bigfloat bflog2) #f]
                                  [logb (λ (x) (floor (bigfloat->flonum (bflog2 (bf (abs x)))))) #f]
                                  [rint round #f]
                                  [round round #f]
                                  [sin sin #f]
                                  [sinh sinh #f]
                                  [sqrt (no-complex sqrt) #f]
                                  [tan tan #f]
                                  [tanh tanh #f]
                                  [tgamma gamma #f]
                                  [trunc truncate #f])

(register-2ary-fallback-operators [+ + #f]
                                  [- - #f]
                                  [* * #f]
                                  [/ / #f]
                                  [atan2 (no-complex atan) #f]
                                  [copysign
                                   (λ (x y)
                                     (if (>= y 0)
                                         (abs x)
                                         (- (abs x))))
                                   #f]
                                  [fdim (λ (x y) (max (- x y) 0)) #f]
                                  [fmax
                                   (λ (x y)
                                     (cond
                                       [(nan? x) y]
                                       [(nan? y) x]
                                       [else (max x y)]))
                                   #f]
                                  [fmin
                                   (λ (x y)
                                     (cond
                                       [(nan? x) y]
                                       [(nan? y) x]
                                       [else (min x y)]))
                                   #f]
                                  [fmod (from-bigfloat bffmod) #f]
                                  [pow (no-complex expt) #f]
                                  [remainder remainder #f])

(platform-register-implementation!
 racket-platform
 (make-operator-impl (erfc.rkt [x : binary64])
                     binary64
                     #:spec (- 1 (erf x))
                     #:fpcore (! :precision binary64 :math-library racket (erfc x))
                     #:fl erfc))

(platform-register-implementation!
 racket-platform
 (make-operator-impl (expm1.rkt [x : binary64])
                     binary64
                     #:spec (- (exp x) 1)
                     #:fpcore (! :precision binary64 :math-library racket (expm1 x))
                     #:fl (from-bigfloat bfexpm1)))

(platform-register-implementation!
 racket-platform
 (make-operator-impl (log1p.rkt [x : binary64])
                     binary64
                     #:spec (log (+ 1 x))
                     #:fpcore (! :precision binary64 :math-library racket (log1p x))
                     #:fl (from-bigfloat bflog1p)))

(platform-register-implementation!
 racket-platform
 (make-operator-impl (hypot.rkt [x : binary64] [y : binary64])
                     binary64
                     #:spec (sqrt (+ (* x x) (* y y)))
                     #:fpcore (! :precision binary64 :math-library racket (hypot x y))
                     #:fl (from-bigfloat bfhypot)))

(platform-register-implementation!
 racket-platform
 (make-operator-impl (fma.rkt [x : binary64] [y : binary64] [z : binary64])
                     binary64
                     #:spec (+ (* x y) z)
                     #:fpcore (! :precision binary64 :math-library racket (fma x y z))
                     #:fl (from-bigfloat bffma)))

(register-platform! racket-platform)

;; Do not run this file during testing
(module test racket/base
  )
