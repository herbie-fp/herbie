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
(define nan ((flag 'evaluate 'double-precision) +nan.0 +nan.f))

(define (cotan x)
  (/ 1 (tan x)))

(define (bfmod x mod)
  (bf- x (bf* mod (bffloor (bf/ x mod)))))

(define (flmod x mod)
  (fl- x (fl* mod (flfloor (fl/ x mod)))))

(define (cmod x mod)
  (- x (* mod (floor (/ x mod)))))

(define (make-safe f)
  (λ args
     (let ([ans (apply f args)])
       (if (and (complex? ans) (not (= 0 (imag-part ans))))
	   nan
	   (real-part ans)))))

(define csinh
  ((flag 'evaluate 'double-precision)
   flsinh
   (λ (x) (real->single-flonum (flsinh (real->double-flonum x))))))

(define ctanh
  ((flag 'evaluate 'double-precision)
   fltanh
   (λ (x) (real->single-flonum (fltanh (real-double-flonum x))))))

(define csqrt (make-safe sqrt))
(define clog (make-safe log))
(define casin (make-safe asin))
(define cacos (make-safe acos))
(define cexpt (make-safe expt))

(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

; Table defining costs and translations to bigfloat and regular float
; See "costs.c" for details of how these costs were determined
(define-table operations
  [+        bf+       +         1]
  [-        bf-       -         1]
  [*        bf*       *         1]
  [/        bf/       /         1]
  [sqrt     bfsqrt    csqrt     1]
  [sqr      bfsqr     sqr       1]
  [exp      bfexp     exp     270]
  [expt     bfexpt    cexpt   640]
  [log      bflog     clog    300]
  [sin      bfsin     sin     145]
  [cos      bfcos     cos     185]
  [tan      bftan     tan     160]
  [cotan    bfcot     cotan   160]
  [asin     bfasin    casin   140]
  [acos     bfacos    cacos   155]
  [atan     bfatan    atan    130]
  [sinh     bfsinh    csinh   300]
  [cosh     bfcosh    cosh    300]
  [tanh     bftanh    ctanh   300]
  [atan2    bfatan2   atan    230]
  [abs      bfabs     abs       1]
  [mod      bfmod     cmod      1]
  ; TODO : These are different and should be treated differently
  [if       if-fn     if-fn     1]
  [>        bf>       >         1]
  [<        bf<       <         1]
  [<=       bf<=      <=        1]
  [>=       bf>=      >=        1]
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
  (let ([convert ((flag 'evaluate 'double-precision)
		  real->double-flonum
		  real->single-flonum)])
    (cond
     [(real? x) (convert x)]
     [(bigfloat? x) (convert (bigfloat->flonum x))]
     [(complex? x)
      (if (= (imag-part x) 0)
	  (->flonum (real-part x))
	  +nan.0)]
     [(eq? x 'pi) (convert pi)]
     [(eq? x 'e) (convert (exp 1))]
     [else x])))

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
