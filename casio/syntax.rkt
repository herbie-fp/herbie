#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/common)

(provide operations predicates constants constant? variable?
         mode:bf mode:fl ->bf ->flonum
         common-eval-ns casio-eval
         program-body program-variables)

; Programs are just lambda expressions
(define program-body caddr)
(define program-variables cadr)

; Functions and constants used in our language
(define (cotan x)
  (/ 1 (tan x)))

(define (bfmod x mod)
  (bf- x (bf* mod (bffloor (bf/ x mod)))))

(define (flmod x mod)
  (fl- x (fl* mod (flfloor (fl/ x mod)))))

(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

; Table defining costs and translations to bigfloat and regular float
; See "costs.c" for details of how these costs were determined
(define-table operations
  [+        bf+       fl+       1]
  [-        bf-       -         1]
  [*        bf*       fl*       1]
  [/        bf/       /         1]
  [sqrt     bfsqrt    flsqrt    1]
  [sqr      bfsqr     sqr       1]
  [exp      bfexp     flexp     270]
  [expt     bfexpt    flexpt    640]
  [log      bflog     fllog     300]
  [sin      bfsin     flsin     145]
  [cos      bfcos     flcos     185]
  [tan      bftan     fltan     160]
  [cotan    bfcot     cotan     160]
  [asin     bfasin    flasin    140]
  [acos     bfacos    flacos    155]
  [atan     bfatan    flatan    130]
  [sinh     bfsinh    flsinh    300]
  [cosh     bfcosh    flcosh    300]
  [tanh     bftanh    fltanh    300]
  [atan2    bfatan2   atan      230]
  [abs      bfabs     flabs     1]
  [mod      bfmod     flmod     1]
  ; TODO : These are different and should be treated differently
  [if       if-fn     if-fn     1]
  [>        bf>       fl>       1]
  [<        bf<       fl<       1]
  [<=       bf<=      fl<=      1]
  [>=       bf>=      fl>=      1]
  [and      and-fn    and-fn    1]
  [or       or-fn     or-fn     1])

(define constants '(pi e))

(define predicates '(or and < > <= >= =))

(define mode:bf 0)
(define mode:fl 1)

(define (variable? var)
  (and (symbol? var) (not (member var constants))))

(define (constant? var)
  (or (member var constants) (real? var)))

(define (->flonum x)
  (cond
   [(real? x) (real->double-flonum x)]
   [(bigfloat? x) (real->double-flonum (bigfloat->flonum x))]
   [(complex? x)
    (if (= (imag-part x) 0)
        (->flonum (real-part x))
        +nan.0)]
   [(eq? x 'pi) pi]
   [(eq? x 'e) (exp 1)]
   [else x]))

(define (->bf x)
  (cond
   [(real? x) (bf x)]
   [(bigfloat? x) x]
   [(complex? x)
    (if (= (imag-part x) 0) (->bf (real-part x)) +nan.bf)]
   [(eq? x 'pi) pi.bf]
   [(eq? x 'e) (bfexp 1.bf)]
   [else x]))

(define-namespace-anchor common-eval-ns-anchor)
(define common-eval-ns (namespace-anchor->namespace common-eval-ns-anchor))

(define (casio-eval expr) (eval expr common-eval-ns))
