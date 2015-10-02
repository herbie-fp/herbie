#lang racket

(require math/flonum)
(require math/bigfloat)
(require "common.rkt")

(provide *operations* predicates constants constant? variable?
         mode:bf mode:fl mode:args mode:cost ->bf ->flonum
         common-eval-ns common-eval
         program-body program-variables
         real-op->bigfloat-op
         real-op->float-op)

; Programs are just lambda expressions
(define program-body caddr)
(define program-variables cadr)

; Functions and constants used in our language
(define nan ((flag 'precision 'double) +nan.0 +nan.f))

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

(define (csinh x)
  (((flag 'precision 'double)
    flsinh
    (compose real->single-flonum flsinh real->double-flonum)) x))

(define (ctanh x)
  (((flag 'precision 'double)
    fltanh
    (compose real->single-flonum fltanh real->double-flonum)) x))

(define csqrt (make-safe sqrt))
(define clog (make-safe log))
(define casin (make-safe asin))
(define cacos (make-safe acos))
(define cexpt (make-safe expt))
(define atan2 atan)

(define log1p fllog1p)
(define expm1 flexpm1)
(define hypot flhypot)

(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

; Table defining costs and translations to bigfloat and regular float
; See "costs.c" for details of how these costs were determined
(define-table operations
  [+        '(2)      bf+       +         1]
  [-        '(1 2)    bf-       -         1]
  [*        '(2)      bf*       *         1]
  [/        '(2)      bf/       /         1]
  [sqrt     '(1)      bfsqrt    csqrt     1]
  [sqr      '(1)      bfsqr     sqr       1]
  [exp      '(1)      bfexp     exp     270]
  [expt     '(2)      bfexpt    cexpt   640]
  [log      '(1)      bflog     clog    300]
  [sin      '(1)      bfsin     sin     145]
  [cos      '(1)      bfcos     cos     185]
  [tan      '(1)      bftan     tan     160]
  [cotan    '(1)      bfcot     cotan   160]
  [asin     '(1)      bfasin    casin   140]
  [acos     '(1)      bfacos    cacos   155]
  [atan     '(1)      bfatan    atan    130]
  [sinh     '(1)      bfsinh    csinh   300]
  [cosh     '(1)      bfcosh    cosh    300]
  [tanh     '(1)      bftanh    ctanh   300]
  [atan2    '(2)      bfatan2   atan    230]
  [abs      '(1)      bfabs     abs       1]
  [mod      '(2)      bfmod     cmod      1]
  [expm1    '(1)      bfexpm1   flexpm1 666] ; TODO : cost made up
  [log1p    '(1)      bflog1p   fllog1p 666] ; TODO : cost made up
  [hypot    '(2)      bfhypot   flhypot 666] ; TODO : cost made up
  ; TODO : These are different and should be treated differently
  [if       '(3)      if-fn     if-fn     1]
  [=        '(2)      bf=       =         1]
  [>        '(2)      bf>       >         1]
  [<        '(2)      bf<       <         1]
  [<=       '(2)      bf<=      <=        1]
  [>=       '(2)      bf>=      >=        1]
  [and      '(2)      and-fn    and-fn    1]
  [or       '(2)      or-fn     or-fn     1])

(define *operations* (make-parameter operations))

(define constants '(pi e))

(define predicates '(or and < > <= >= =))

(define mode:args 0)
(define mode:bf 1)
(define mode:fl 2)
(define mode:cost 3)

(define (variable? var)
  (and (symbol? var) (not (member var constants))))

(define (constant? var)
  (or (member var constants) (number? var)))

(define (->flonum x)
  (let ([convert ((flag 'precision 'double)
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
(define (common-eval expr) (eval expr common-eval-ns))
(define (real-op->bigfloat-op op) (list-ref (hash-ref (*operations*) op) mode:bf))
(define (real-op->float-op op) (list-ref (hash-ref (*operations*) op) mode:fl))
