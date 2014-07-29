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
  [+        bf+       fl+       1      "~a + ~a"]
  [-        bf-       -         1      '(#f "-~a" "~a - ~a")]
  [*        bf*       fl*       1      "~a * ~a"]
  [/        bf/       /         1      '(#f "1.0/~a" "~a / ~a")]
  [abs      bfabs     flabs     1      "abs(~a)"]
  [sqrt     bfsqrt    flsqrt    1      "sqrt(~a)"]
  [sqr      bfsqr     sqr       1      (λ (x) (format "~a * ~a" x x))]
  [exp      bfexp     flexp     270    "exp(~a)"]
  [expt     bfexpt    flexpt    640    "pow(~a, ~a)"]
  [log      bflog     fllog     300    "log(~a)"]
  [sin      bfsin     flsin     145    "sin(~a)"]
  [cos      bfcos     flcos     185    "cos(~a)"]
  [tan      bftan     fltan     160    "tan(~a)"]
  [cotan    bfcot     cotan     160    "1.0 / tan(~a)"]
  [asin     bfasin    flasin    140    "asin(~a)"]
  [acos     bfacos    flacos    155    "acos(~a)"]
  [atan     bfatan    flatan    130    "atan(~a)"]
  [sinh     bfsinh    flsinh    300    "sinh(~a)"]
  [cosh     bfcosh    flcosh    300    "cosh(~a)"]
  [tanh     bftanh    fltanh    300    "tanh(~a)"]
  [if       if-fn     if-fn     1      "~a > 0 ? ~a : ~a"]
  [>        bf>       fl>       1      "~a > ~a ? 1 : -1"]
  [<        bf<       fl<       1      "~a < ~a ? 1 : -1"]
  [<=       bf<=      fl<=      1      "~a <= ~a ? 1 : -1"]
  [>=       bf>=      fl>=      1      "~a >= ~a ? 1 : -1"]
  [and      and-fn    and-fn    1      "((~a > 0) && (~a > 0)) ? 1 : -1"]
  [or       or-fn     or-fn     1      "((~a > 0) || (~a > 0)) ? 1 : -1"]
  [atan2    bfatan2   atan      230    "atan2(~a, ~a)"]
  [mod      bfmod     flmod     1      "fmod2(~a, ~a)"])

(define constants '(pi e))

(define mode:bf 0)
(define mode:fl 1)
