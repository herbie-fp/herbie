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

(define (make-safe f)
  (λ args
     (let ([ans (apply f args)])
       (if (and (complex? ans) (not (= 0 (imag-part ans))))
	   nan
	   (real-part ans)))))

(define csqrt (make-safe sqrt))
(define clog  (make-safe log))
(define casin (make-safe asin))
(define cacos (make-safe acos))
(define cexpt (make-safe expt))

; use C ffi for numerical ops missing from math/flonum
; TODO: import all portable numeric ops from libm
(require ffi/unsafe ffi/unsafe/define)
(define-ffi-definer define-libm #f)

(define-libm fma  (_fun _double _double _double -> _double))
(define-libm fmaf (_fun _float  _float  _float  -> _float ))
(define (_flfma x y z)
  ((flag 'precision 'double)
    (fma  (real->double-flonum x) (real->double-flonum y) (real->double-flonum z))
    (fmaf (real->single-flonum x) (real->single-flonum y) (real->single-flonum z))))

(define-libm hypot  (_fun _double _double -> _double))
(define-libm hypotf (_fun _float  _float  -> _float ))
(define (_flhypot x y)
  ((flag 'precision 'double)
    (hypot  (real->double-flonum x) (real->double-flonum y))
    (hypotf (real->single-flonum x) (real->single-flonum y))))

(define-libm atan2  (_fun _double _double -> _double))
(define-libm atan2f (_fun _float  _float  -> _float ))
(define (_flatan2 x y)
  ((flag 'precision 'double)
    (atan2  (real->double-flonum x) (real->double-flonum y))
    (atan2f (real->single-flonum x) (real->single-flonum y))))

(define-libm fmod  (_fun _double _double -> _double))
(define-libm fmodf (_fun _float  _float  -> _float ))
(define (_flfmod x y)
  ((flag 'precision 'double)
    (fmod  (real->double-flonum x) (real->double-flonum y))
    (fmodf (real->single-flonum x) (real->single-flonum y))))

(define-libm log1p  (_fun _double -> _double))
(define-libm log1pf (_fun _float  -> _float ))
(define (_fllog1p x)
  ((flag 'precision 'double)
    (log1p  (real->double-flonum x))
    (log1pf (real->single-flonum x))))

(define-libm expm1  (_fun _double -> _double))
(define-libm expm1f (_fun _float  -> _float ))
(define (_flexpm1 x)
  ((flag 'precision 'double)
    (expm1  (real->double-flonum x))
    (expm1f (real->single-flonum x))))

(define-libm sinh  (_fun _double -> _double))
(define-libm sinhf (_fun _float  -> _float ))
(define (_flsinh x)
  ((flag 'precision 'double)
    (sinh  (real->double-flonum x))
    (sinhf (real->single-flonum x))))

(define-libm tanh  (_fun _double -> _double))
(define-libm tanhf (_fun _float  -> _float ))
(define (_fltanh x)
  ((flag 'precision 'double)
    (tanh  (real->double-flonum x))
    (tanhf (real->single-flonum x))))

(define-libm cbrt  (_fun _double -> _double))
(define-libm cbrtf (_fun _float  -> _float ))
(define (_flcbrt x)
  ((flag 'precision 'double)
    (cbrt  (real->double-flonum x))
    (cbrtf (real->single-flonum x))))

(define (bffma x y z)
  (bf+ (bf* x y) z))

(define (bffmod x mod)
  (bf- x (bf* mod (bffloor (bf/ x mod)))))

(define (bfcube x)
  (bf* x (bf* x x)))

(define (_flcube x)
  (* x (* x x)))


(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

; Table defining costs and translations to bigfloat and regular float
; See "costs.c" for details of how these costs were determined
(define-table operations
  ; arithmetic
  [+        '(2)      bf+       +          40]
  [-        '(1 2)    bf-       -          40]
  [*        '(2)      bf*       *          40]
  [/        '(2)      bf/       /          40]

  ; exponents
  [sqrt     '(1)      bfsqrt    csqrt      40]
  [sqr      '(1)      bfsqr     sqr        40] ; = multiply cost
  [cbrt     '(1)      bfcbrt    _flcbrt    80]
  [cube     '(1)      bfcube    _flcube    80] ; = 2 * multiply cost
  [exp      '(1)      bfexp     exp        70]
  [expm1    '(1)      bfexpm1   _flexpm1   70]
  [expt     '(2)      bfexpt    cexpt     210]
  [log      '(1)      bflog     clog       70]
  [log1p    '(1)      bflog1p   _fllog1p   90]

  ; trig
  [sin      '(1)      bfsin     sin        60]
  [cos      '(1)      bfcos     cos        60]
  [tan      '(1)      bftan     tan        95]
  [cotan    '(1)      bfcot     cotan     135] ; = tan + div cost
  [asin     '(1)      bfasin    casin     105]
  [acos     '(1)      bfacos    cacos      90]
  [atan     '(1)      bfatan    atan      105]
  [sinh     '(1)      bfsinh    _flsinh    55]
  [cosh     '(1)      bfcosh    cosh       55]
  [tanh     '(1)      bftanh    _fltanh    55]
  [atan2    '(2)      bfatan2   _flatan2  140]

  ; misc
  [abs      '(1)      bfabs     abs        40]
  [fma      '(3)      bffma     _flfma     55]
  [hypot    '(2)      bfhypot   _flhypot   55]
  [mod      '(2)      bffmod    _flfmod    70]

  ; comparison and conditional
  ; TODO : These are different and should be treated differently
  [if       '(3)      if-fn     if-fn      65]
  [=        '(2)      bf=       =          65]
  [>        '(2)      bf>       >          65]
  [<        '(2)      bf<       <          65]
  [>=       '(2)      bf>=      >=         65]
  [<=       '(2)      bf<=      <=         65]
  [not      '(1)      not       not        65]
  [and      '(2)      and-fn    and-fn     55]
  [or       '(2)      or-fn     or-fn      55])

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
