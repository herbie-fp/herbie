#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/common)

(provide operations constants mode:bf mode:fl)

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
  [abs      bfabs     flabs     1]
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
  [if       if-fn     if-fn     1]
  [>        bf>       fl>       1]
  [<        bf<       fl<       1]
  [<=       bf<=      fl<=      1]
  [>=       bf>=      fl>=      1]
  [and      and-fn    and-fn    1]
  [or       or-fn     or-fn     1]
  [atan2    bfatan2   atan      230]
  [mod      bfmod     flmod     1])

(define-table constants
  [pi       (λ () pi.bf)           (λ () pi)]
  [e        (λ () (bfexp 1.bf))    (λ () (exp 1))]
  [#f       bf                     real->double-flonum])

(define mode:bf 0)
(define mode:fl 1)
