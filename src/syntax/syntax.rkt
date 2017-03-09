#lang racket

(require math/flonum)
(require math/bigfloat)
(require "../common.rkt")

(provide *operations* predicates constants constant? variable?
         mode:bf mode:fl mode:args mode:cost ->bf ->flonum
         program-body program-variables
         real-op->bigfloat-op
         real-op->float-op)

; Programs are just lambda expressions
(define program-body caddr)
(define program-variables cadr)

; Functions and constants used in our language
(define nan ((flag 'precision 'double) +nan.0 +nan.f))
; TODO add infinity

; Use C ffi to get numerical ops from libm
(require ffi/unsafe ffi/unsafe/define)
(define-ffi-definer define-libm #f
  #:default-make-fail make-not-available)

(define-syntax-rule (libm_op1 id_fl id_d id_f)
  (begin
    (define-libm id_d (_fun _double -> _double))
    (define-libm id_f (_fun _float  -> _float ))
    (define (id_fl x)
      ((flag 'precision 'double)
        (id_d (real->double-flonum x))
        (id_f (real->single-flonum x))))))

(define-syntax-rule (libm_op2 id_fl id_d id_f)
  (begin
    (define-libm id_d (_fun _double _double -> _double))
    (define-libm id_f (_fun _float  _float  -> _float ))
    (define (id_fl x y)
      ((flag 'precision 'double)
        (id_d (real->double-flonum x) (real->double-flonum y))
        (id_f (real->single-flonum x) (real->single-flonum y))))))

(define-syntax-rule (libm_op3 id_fl id_d id_f)
  (begin
    (define-libm id_d (_fun _double _double _double -> _double))
    (define-libm id_f (_fun _float  _float  _float  -> _float ))
    (define (id_fl x y z)
      ((flag 'precision 'double)
        (id_d (real->double-flonum x) (real->double-flonum y) (real->double-flonum z))
        (id_f (real->single-flonum x) (real->single-flonum y) (real->single-flonum z))))))

; Supported ops from libm (https://goo.gl/auVJi5)
(libm_op1  _flacos       acos       acosf)
(libm_op1  _flacosh      acosh      acoshf)
(libm_op1  _flasin       asin       asinf)
(libm_op1  _flasinh      asinh      asinhf)
(libm_op1  _flatan       atan       atanf)
(libm_op2  _flatan2      atan2      atan2f)
(libm_op1  _flatanh      atanh      atanhf)
(libm_op1  _flcbrt       cbrt       cbrtf)
(libm_op1  _flceil       ceil       ceilf)
(libm_op2  _flcopysign   copysign   copysignf)
(libm_op1  _flcos        cos        cosf)
(libm_op1  _flcosh       cosh       coshf)
(libm_op1  _flerf        erf        erff)
(libm_op1  _flerfc       erfc       erfcf)
(libm_op1  _flexp        exp        expf)
(libm_op1  _flexp2       exp2       exp2f)
(libm_op1  _flexpm1      expm1      expm1f)
(libm_op1  _flfabs       fabs       fabsf)
(libm_op2  _flfdim       fdim       fdimf)
(libm_op1  _flfloor      floor      floorf)
(libm_op3  _flfma        fma        fmaf)
(libm_op2  _flfmax       fmax       fmaxf)
(libm_op2  _flfmin       fmin       fminf)
(libm_op2  _flfmod       fmod       fmodf)
(libm_op2  _flhypot      hypot      hypotf)
(libm_op1  _flj0         j0         j0f)
(libm_op1  _flj1         j1         j1f)
(libm_op1  _fllgamma     lgamma     lgammaf)
(libm_op1  _fllog        log        logf)
(libm_op1  _fllog10      log10      log10f)
(libm_op1  _fllog1p      log1p      log1pf)
(libm_op1  _fllog2       log2       log2f)
(libm_op1  _fllogb       logb       logbf)
(libm_op2  _flpow        pow        powf)
(libm_op2  _flremainder  remainder  remainderf)
(libm_op1  _flrint       rint       rintf)
(libm_op1  _flround      round      roundf)
(libm_op1  _flsin        sin        sinf)
(libm_op1  _flsinh       sinh       sinhf)
(libm_op1  _flsqrt       sqrt       sqrtf)
(libm_op1  _fltan        tan        tanf)
(libm_op1  _fltanh       tanh       tanhf)
(libm_op1  _fltgamma     tgamma     tgammaf)
(libm_op1  _fltrunc      trunc      truncf)
(libm_op1  _fly0         y0         y0f)
(libm_op1  _fly1         y1         y1f)

(define (_flcube x)
  (* x (* x x)))

(define (_flsqr x)
  (* x x))

(define (bfcopysign x y)
  (bf* (bfabs x)
       (bf (expt -1 (bigfloat-signbit y)))))

(define (bffdim x y)
  (if (bf> x y)
    (bf- x y)
    0.bf))

(define (bfcube x)
  (bf* x (bf* x x)))

(define (bffma x y z)
  (bf+ (bf* x y) z))

(define (bflogb x)
  (bigfloat-exponent x))

(define (bffmod x mod)
  (bf- x (bf* mod (bffloor (bf/ x mod)))))

(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

(define (!=-fn . args)
  (not (check-duplicates args =)))

(define (bf!=-fn . args)
  (not (check-duplicates args bf=)))

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

; Table defining costs and translations to bigfloat and regular float
; See "costs.c" for details of how these costs were determined
(define-table operations
  ; arithmetic
  [+  '(2)    bf+  +  40]
  [-  '(1 2)  bf-  -  40]
  [*  '(2)    bf*  *  40]
  [/  '(2)    bf/  /  40]

  [sqr    '(1)  bfsqr   _flsqr     40]
  [cube   '(1)  bfcube  _flcube    80]

  [acos      '(1)  bfacos       _flacos        90]
  [acosh     '(1)  bfacosh      _flacosh       55]
  [asin      '(1)  bfasin       _flasin       105]
  [asinh     '(1)  bfasinh      _flasinh       55]
  [atan      '(1)  bfatan       _flatan       105]
  [atan2     '(2)  bfatan2      _flatan2      140]
  [atanh     '(1)  bfatanh      _flatanh       55]
  [cbrt      '(1)  bfcbrt       _flcbrt        80]
  [ceil      '(1)  bfceiling    _flceil        80]
  [copysign  '(2)  bfcopysign   _flcopysign    80]
  [cos       '(1)  bfcos        _flcos         60]
  [cosh      '(1)  bfcosh       _flcosh        55]
  [erf       '(1)  bferf        _flerf         70]
  [erfc      '(1)  bferfc       _flerfc        70]
  [exp       '(1)  bfexp        _flexp         70]
  [exp2      '(1)  bfexp2       _flexp2        70]
  [expm1     '(1)  bfexpm1      _flexpm1       70]
  [fabs      '(1)  bfabs        _flfabs        40]
  [fdim      '(2)  bffdim       _flfdim        55]
  [floor     '(1)  bffloor      _flfloor       55]
  [fma       '(3)  bffma        _flfma         55]
  [fmax      '(2)  bfmax        _flfmax        55]
  [fmin      '(2)  bfmin        _flfmin        55]
  [fmod      '(2)  bffmod       _flfmod        70]
  [hypot     '(2)  bfhypot      _flhypot       55]
  [j0        '(1)  bfbesj0      _flj0          55]
  [j1        '(1)  bfbesj1      _flj1          55]
  [lgamma    '(1)  bflog-gamma  _fllgamma      55]
  [log       '(1)  bflog        _fllog         70]
  [log10     '(1)  bflog10      _fllog10       70]
  [log1p     '(1)  bflog1p      _fllog1p       90]
  [log2      '(1)  bflog2       _fllog2        70]
  [logb      '(1)  bflogb       _fllogb        70]
  [pow       '(2)  bfexpt       _flpow        210]
  [remainder '(2)  bfremainder  _flremainder   70]
  [rint      '(1)  bfrint       _flrint        70]
  [round     '(1)  bfround      _flround       70]
  [sin       '(1)  bfsin        _flsin         60]
  [sinh      '(1)  bfsinh       _flsinh        55]
  [sqrt      '(1)  bfsqrt       _flsqrt        40]
  [tan       '(1)  bftan        _fltan         95]
  [tanh      '(1)  bftanh       _fltanh        55]
  [tgamma    '(1)  bfgamma      _fltgamma      55]
  [trunc     '(1)  bftruncate   _fltrunc       55]
  [y0        '(1)  bfbesy0      _fly0          55]
  [y1        '(1)  bfbesy1      _fly1          55]

  ; TODO : These are different and should be treated differently
  [if       '(3)      if-fn                  if-fn                   65]
  [==       '(*)      (comparator bf=)       (comparator =)          65]
  [!=       '(*)      bf!=-fn                !=-fn                   65]
  [>        '(*)      (comparator bf>)       (comparator >)          65]
  [<        '(*)      (comparator bf<)       (comparator <)          65]
  [>=       '(*)      (comparator bf>=)      (comparator >=)         65]
  [<=       '(*)      (comparator bf<=)      (comparator <=)         65]
  [not      '(1)      not                    not                     65]
  [and      '(*)      and-fn                 and-fn                  55]
  [or       '(*)      or-fn                  or-fn                   55])

(define *operations* (make-parameter operations))

(define constants '(PI E TRUE FALSE))

(define predicates '(or and < > <= >= == !=))

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
     [(eq? x 'PI) (convert pi)]
     [(eq? x 'E) (convert (exp 1.0))]
     [(eq? x 'TRUE) #t]
     [(eq? x 'FALSE) #f]
     [else x])))

(define (->bf x)
  (cond
   [(real? x) (bf x)]
   [(bigfloat? x) x]
   [(complex? x)
    (if (= (imag-part x) 0) (->bf (real-part x)) +nan.bf)]
   [(eq? x 'PI) pi.bf]
   [(eq? x 'E) (bfexp 1.bf)]
   [(eq? x 'TRUE) #t]
   [(eq? x 'FALSE) #f]
   [else x]))

(define (real-op->bigfloat-op op) (list-ref (hash-ref (*operations*) op) mode:bf))
(define (real-op->float-op op) (list-ref (hash-ref (*operations*) op) mode:fl))
