#lang racket/base

(require racket/contract racket/match racket/function math/private/bigfloat/mpfr)

(define value? (or/c bigfloat? boolean?))
(struct ival (lo hi err? err) #:transparent)

(provide (contract-out
          [struct ival ([lo value?] [hi value?] [err? boolean?] [err boolean?])]
          [mk-ival (-> value? ival?)]
          [ival-pi (-> ival?)]
          [ival-e  (-> ival?)]
          [ival-bool (-> boolean? ival?)]
          [ival-add (-> ival? ival? ival?)]
          [ival-sub (-> ival? ival? ival?)]
          [ival-neg (-> ival? ival?)]
          [ival-mult (-> ival? ival? ival?)]
          [ival-div (-> ival? ival? ival?)]
          [ival-fma (-> ival? ival? ival? ival?)] ; TODO: untested
          [ival-fabs (-> ival? ival?)]
          [ival-sqrt (-> ival? ival?)]
          [ival-cbrt (-> ival? ival?)]
          [ival-hypot (-> ival? ival? ival?)]
          [ival-exp (-> ival? ival?)]
          [ival-exp2 (-> ival? ival?)]
          [ival-expm1 (-> ival? ival?)]
          [ival-log (-> ival? ival?)]
          [ival-log2 (-> ival? ival?)]
          [ival-log10 (-> ival? ival?)]
          [ival-log1p (-> ival? ival?)]
          [ival-logb (-> ival? ival?)]
          [ival-pow (-> ival? ival? ival?)]
          [ival-sin (-> ival? ival?)]
          [ival-cos (-> ival? ival?)]
          [ival-tan (-> ival? ival?)]
          [ival-asin (-> ival? ival?)]
          [ival-acos (-> ival? ival?)]
          [ival-atan (-> ival? ival?)]
          [ival-atan2 (-> ival? ival? ival?)]
          [ival-sinh (-> ival? ival?)]
          [ival-cosh (-> ival? ival?)]
          [ival-tanh (-> ival? ival?)]
          [ival-asinh (-> ival? ival?)]
          [ival-acosh (-> ival? ival?)]
          [ival-atanh (-> ival? ival?)]
          [ival-erf (-> ival? ival?)]
          [ival-erfc (-> ival? ival?)]
          [ival-fmod (-> ival? ival? ival?)]
          [ival-remainder (-> ival? ival? ival?)]
          [ival-rint (-> ival? ival?)]
          [ival-round (-> ival? ival?)]
          [ival-ceil (-> ival? ival?)]
          [ival-floor (-> ival? ival?)]
          [ival-trunc (-> ival? ival?)]
          [ival-and (->* () #:rest (listof ival?) ival?)] ; TODO: untested
          [ival-or  (->* () #:rest (listof ival?) ival?)] ; TODO: untested
          [ival-not (-> ival? ival?)] ; TODO: untested
          [ival-<  (->* () #:rest (listof ival?) ival?)]
          [ival-<= (->* () #:rest (listof ival?) ival?)]
          [ival->  (->* () #:rest (listof ival?) ival?)]
          [ival->= (->* () #:rest (listof ival?) ival?)]
          [ival-== (->* () #:rest (listof ival?) ival?)]
          [ival-!= (->* () #:rest (listof ival?) ival?)]
          [ival-if (-> ival? ival? ival? ival?)] ; TODO: untested
          [ival-fmin (-> ival? ival? ival?)]
          [ival-fmax (-> ival? ival? ival?)]
          [ival-copysign (-> ival? ival? ival?)]
          [ival-fdim (-> ival? ival? ival?)]))

(define (mk-ival x)
  (match x
    [(? bigfloat?)
     (ival x x #f #f)]
    [(? boolean?)
     (ival x x #f #f)]
    [_
     (error "Invalid exact value for interval arithmetic" x)]))

(define -inf.bf (bf -inf.0))
(define -1.bf (bf -1))
(define 0.bf (bf 0))
(define half.bf (bf 0.5))
(define 1.bf (bf 1))
(define 2.bf (bf 2))
(define +inf.bf (bf +inf.0))
(define +nan.bf (bf +nan.0))

(define (ival-pi)
  (ival (rnd 'down pi.bf) (rnd 'up pi.bf) #f #f))

(define (ival-e)
  (ival (rnd 'down bfexp 1.bf) (rnd 'up bfexp 1.bf) #f #f))

(define (ival-bool b)
  (ival b b #f #f))

(define ival-true (ival-bool #t))

(define-syntax-rule (rnd mode op args ...)
  (parameterize ([bf-rounding-mode mode])
    (op args ...)))

(define (ival-neg x)
  ;; No rounding, negation is exact
  (ival (bfneg (ival-hi x)) (bfneg (ival-lo x)) (ival-err? x) (ival-err x)))

(define (ival-add x y)
  (ival (rnd 'down bfadd (ival-lo x) (ival-lo y))
        (rnd 'up bfadd (ival-hi x) (ival-hi y))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (ival-sub x y)
  (ival (rnd 'down bfsub (ival-lo x) (ival-hi y))
        (rnd 'up bfsub (ival-hi x) (ival-lo y))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (bfmin* a . as)
  (if (null? as) a (apply bfmin* (bfmin2 a (car as)) (cdr as))))

(define (bfmax* a . as)
  (if (null? as) a (apply bfmax* (bfmax2 a (car as)) (cdr as))))

(define (ival-mult x y)
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)
  (define err? (or xerr? yerr?))
  (define err (or xerr yerr))
  (define (mkmult a b c d)
    (ival (rnd 'down bfmul a b) (rnd 'up bfmul c d) err? err))
  (match* ((classify-ival x) (classify-ival y))
    [( 1  1) (mkmult xlo ylo xhi yhi)]
    [( 1  0) (mkmult xhi ylo xhi yhi)]
    [( 1 -1) (mkmult xhi ylo xlo yhi)]
    [( 0  1) (mkmult xlo yhi xhi yhi)]
    [( 0 -1) (mkmult xhi ylo xlo ylo)]
    [(-1  1) (mkmult xlo yhi xhi ylo)]
    [(-1  0) (mkmult xlo yhi xlo ylo)]
    [(-1 -1) (mkmult xhi yhi xlo ylo)]
    [( 0  0) ;; Special case
     (ival (rnd 'down bfmin2 (bfmul xlo yhi) (bfmul xhi ylo))
           (rnd 'up bfmax2 (bfmul xhi yhi) (bfmul xlo ylo)) err? err)]))

(define (ival-div x y)
  (define err? (or (ival-err? x) (ival-err? y) (and (bflte? (ival-lo y) 0.bf) (bfgte? (ival-hi y) 0.bf))))
  (define err (or (ival-err x) (ival-err y) (and (bf=? (ival-lo y) 0.bf) (bf=? (ival-hi y) 0.bf))))
    ;; We round only down, and approximate rounding up with bfnext below
  (match* ((classify-ival x) (classify-ival y))
    [(_ 0)
     (ival -inf.bf +inf.bf err? err)]
    [(1 1)
     (ival (rnd 'down bfdiv (ival-lo x) (ival-hi y))
           (rnd 'up bfdiv (ival-hi x) (ival-lo y)) err? err)]
    [(1 -1)
     (ival (rnd 'down bfdiv (ival-hi x) (ival-hi y))
           (rnd 'up bfdiv (ival-lo x) (ival-lo y)) err? err)]
    [(-1 1)
     (ival (rnd 'down bfdiv (ival-lo x) (ival-lo y))
           (rnd 'up bfdiv (ival-hi x) (ival-hi y)) err? err)]
    [(-1 -1)
     (ival (rnd 'down bfdiv (ival-hi x) (ival-lo y))
           (rnd 'up bfdiv (ival-lo x) (ival-hi y)) err? err)]
    [(0 1)
     (ival (rnd 'down bfdiv (ival-lo x) (ival-lo y))
           (rnd 'up bfdiv (ival-hi x) (ival-lo y)) err? err)]
    [(0 -1)
     (ival (rnd 'down bfdiv (ival-hi x) (ival-hi y))
           (rnd 'up bfdiv (ival-lo x) (ival-hi y)) err? err)]))

(define-syntax-rule (define-monotonic name bffn)
  (define (name x)
    (ival (rnd 'down bffn (ival-lo x)) (rnd 'up bffn (ival-hi x)) (ival-err? x) (ival-err x))))

(define-monotonic ival-exp bfexp)
(define-monotonic ival-exp2 bfexp2)
(define-monotonic ival-expm1 bfexpm1)

(define-syntax-rule (define-monotonic-positive name bffn)
  (define (name x)
    (define err (or (ival-err x) (bflte? (ival-hi x) 0.bf)))
    (define err? (or err (ival-err? x) (bflte? (ival-lo x) 0.bf)))
    (ival (rnd 'down bffn (ival-lo x)) (rnd 'up bffn (ival-hi x)) err? err)))

(define-monotonic-positive ival-log bflog)
(define-monotonic-positive ival-log2 bflog2)
(define-monotonic-positive ival-log10 bflog10)

(define (ival-log1p x)
  (define err (or (ival-err x) (bflte? (ival-hi x) -1.bf)))
  (define err? (or err (ival-err? x) (bflte? (ival-lo x) -1.bf)))
  (ival (rnd 'down bflog1p (ival-lo x)) (rnd 'up bflog1p (ival-hi x))
        err? err))

(define (ival-logb x)
  (match-define (ival ylo yhi yerr? yerr) (ival-log2 (ival-fabs x)))
  (ival (rnd 'down bffloor ylo) (rnd 'up bffloor yhi) yerr? yerr))

(define (ival-sqrt x)
  (define err (or (ival-err x) (bflt? (ival-hi x) 0.bf)))
  (define err? (or err (ival-err? x) (bflt? (ival-lo x) 0.bf)))
  (ival (rnd 'down bfsqrt (ival-lo x)) (rnd 'up bfsqrt (ival-hi x))
        err? err))

(define (ival-cbrt x)
  (ival (rnd 'down bfcbrt (ival-lo x)) (rnd 'up bfcbrt (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-hypot x y)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))
  (define x* (ival-fabs x))
  (define y* (ival-fabs y))
  (ival (rnd 'down bfhypot (ival-lo x*) (ival-lo y*))
        (rnd 'up bfhypot (ival-hi x*) (ival-hi y*))
        err? err))

(define (ival-pow-pos x y)
  ;; Assumes x is positive; code copied from ival-mult
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)
  (define err? (or xerr? yerr? (and (bflte? xlo 0.bf) (bflte? ylo 0.bf))))
  (define err (or xerr yerr (and (bflte? xhi 0.bf) (bflte? yhi 0.bf))))
  (define (mkpow a b c d)
    (ival (rnd 'down bfexpt a b) (rnd 'up bfexpt c d) err? err))
  (match* ((classify-ival x 1.bf) (classify-ival y))
    [( 1  1) (mkpow xlo ylo xhi yhi)]
    [( 1  0) (mkpow xhi ylo xhi yhi)]
    [( 1 -1) (mkpow xhi ylo xlo yhi)]
    [( 0  1) (mkpow xlo yhi xhi yhi)]
    [( 0 -1) (mkpow xhi ylo xlo ylo)]
    [(-1  1) (mkpow xlo yhi xhi ylo)]
    [(-1  0) (mkpow xlo yhi xlo ylo)]
    [(-1 -1) (mkpow xhi yhi xlo ylo)]
    [( 0  0) ;; Special case
     (ival (rnd 'down bfmin2 (bfexpt xlo yhi) (bfexpt xhi ylo))
           (rnd 'up bfmax2 (bfexpt xhi yhi) (bfexpt xlo ylo)) err? err)]))

(define (ival-pow-neg x y)
  ;; Assumes x is positive
  (define err? (or (ival-err? x) (ival-err? y) (bflt? (ival-lo y) (ival-hi y))))
  (define err (or (ival-err x) (ival-err y)))
  (define xpos (ival-fabs x))
  ;; Assumes x is negative
  (define a (bfceiling (ival-lo y)))
  (define b (bffloor (ival-hi y)))
  (cond
   [(bflt? b a)
    (ival +nan.bf +nan.bf #t #t)]
   [(bf=? a b)
    (if (bfodd? a)
        (ival-neg (ival-pow-pos xpos (ival a a err? err)))
        (ival-pow-pos xpos (ival a a err? err)))]
   [else
    (define odds (ival (if (bfodd? a) a (bfadd a 1.bf)) (if (bfodd? b) b (bfsub b 1.bf)) err? err))
    (define evens (ival (if (bfodd? a) (bfadd a 1.bf) a) (if (bfodd? b) (bfsub b 1.bf) b) err? err))
    (ival-union (ival-pow-pos xpos evens)
                (ival-neg (ival-pow-pos xpos odds)))]))

(define (ival-pow x y)
  (match (classify-ival x)
    [-1 (ival-pow-neg x y)]
    [1 (ival-pow-pos x y)]
    [0 (ival-union (ival-pow-neg (ival (ival-lo x) 0.bf (ival-err? x) (ival-err x)) y)
                   (ival-pow-pos (ival 0.bf (ival-hi x) (ival-err? x) (ival-err x)) y))]))

(define (ival-fma a b c)
  (ival-add (ival-mult a b) c))

(define (ival-and . as)
  (ival (andmap ival-lo as) (andmap ival-hi as)
        (ormap ival-err? as) (ormap ival-err as)))

(define (ival-or . as)
  (ival (ormap ival-lo as) (ormap ival-hi as)
        (ormap ival-err? as) (ormap ival-err as)))

(define (ival-not x)
  (ival (not (ival-hi x)) (not (ival-lo x)) (ival-err? x) (ival-err x)))

(define (ival-cos x)
  (define lopi (rnd 'down pi.bf))
  (define hipi (rnd 'up pi.bf))
  (define a (rnd 'down bffloor (bfdiv (ival-lo x) (if (bflt? (ival-lo x) 0.bf) lopi hipi))))
  (define b (rnd 'up   bffloor (bfdiv (ival-hi x) (if (bflt? (ival-hi x) 0.bf) hipi lopi))))
  (cond
   [(and (bf=? a b) (bfeven? a))
    (ival (rnd 'down bfcos (ival-hi x)) (rnd 'up bfcos (ival-lo x)) (ival-err? x) (ival-err x))]
   [(and (bf=? a b) (bfodd? a))
    (ival (rnd 'down bfcos (ival-lo x)) (rnd 'up bfcos (ival-hi x)) (ival-err? x) (ival-err x))]
   [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
    (ival -1.bf (rnd 'up bfmax2 (bfcos (ival-lo x)) (bfcos (ival-hi x))) (ival-err? x) (ival-err x))]
   [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
    (ival (rnd 'down bfmin2 (bfcos (ival-lo x)) (bfcos (ival-hi x))) 1.bf (ival-err? x) (ival-err x))]
   [else
    (ival -1.bf 1.bf (ival-err? x) (ival-err x))]))

(define (ival-sin x)
  (define lopi (rnd 'down pi.bf))
  (define hipi (rnd 'up pi.bf))
  (define a (rnd 'down bffloor (bfsub (bfdiv (ival-lo x) (if (bflt? (ival-lo x) 0.bf) lopi hipi)) half.bf))) ; half.bf is exact
  (define b (rnd 'up bffloor (bfsub (bfdiv (ival-hi x) (if (bflt? (ival-hi x) 0.bf) hipi lopi)) half.bf)))
  (cond
   [(and (bf=? a b) (bfeven? a))
    (ival (rnd 'down bfsin (ival-hi x)) (rnd 'up bfsin (ival-lo x)) (ival-err? x) (ival-err x))]
   [(and (bf=? a b) (bfodd? a))
    (ival (rnd 'down bfsin (ival-lo x)) (rnd 'up bfsin (ival-hi x)) (ival-err? x) (ival-err x))]
   [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
    (ival -1.bf (rnd 'up bfmax2 (bfsin (ival-lo x)) (bfsin (ival-hi x))) (ival-err? x) (ival-err x))]
   [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
    (ival (rnd 'down bfmin2 (bfsin (ival-lo x)) (bfsin (ival-hi x))) 1.bf (ival-err? x) (ival-err x))]
   [else
    (ival -1.bf 1.bf (ival-err? x) (ival-err x))]))

(define (ival-tan x)
  (define lopi (rnd 'down pi.bf))
  (define hipi (rnd 'up pi.bf))
  (define a (rnd 'down bffloor (bfsub (bfdiv (ival-lo x) (if (bflt? (ival-lo x) 0.bf) lopi hipi)) half.bf))) ; half.bf is exact
  (define b (rnd 'up bffloor (bfsub (bfdiv (ival-hi x) (if (bflt? (ival-hi x) 0.bf) hipi lopi)) half.bf)))
  (if (bf=? a b) ; Same period
      (ival (rnd 'down bftan (ival-lo x)) (rnd 'up bftan (ival-hi x)) #f #f)
      (ival -inf.bf +inf.bf #t #f)))

(define (ival-atan x)
  (ival (rnd 'down bfatan (ival-lo x)) (rnd 'up bfatan (ival-hi x)) (ival-err? x) (ival-err x)))

(define (classify-ival x [val 0.bf])
  (cond [(bfgte? (ival-lo x) val) 1] [(bflte? (ival-hi x) val) -1] [else 0]))

(define (ival-atan2 y x)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))

  (define tl (list (ival-hi y) (ival-lo x)))
  (define tr (list (ival-hi y) (ival-hi x)))
  (define bl (list (ival-lo y) (ival-lo x)))
  (define br (list (ival-lo y) (ival-hi x)))

  (define-values (a-lo a-hi)
    (match* ((classify-ival x) (classify-ival y))
      [(-1 -1) (values tl br)]
      [( 0 -1) (values tl tr)]
      [( 1 -1) (values bl tr)]
      [( 1  0) (values bl tl)]
      [( 1  1) (values br tl)]
      [( 0  1) (values br bl)]
      [(-1  1) (values tr bl)]
      [( _  _) (values #f #f)]))

  (if a-lo
      (ival (rnd 'down apply bfatan2 a-lo) (rnd 'up apply bfatan2 a-hi) err? err)
      (ival (bfneg (rnd 'up pi.bf)) (rnd 'up pi.bf)
            (or err? (bfgte? (ival-hi x) 0.bf))
            (or err (and (bf=? (ival-lo x) 0.bf) (bf=? (ival-hi x) 0.bf) (bf=? (ival-lo y) 0.bf) (bf=? (ival-hi y) 0.bf))))))

(define (ival-asin x)
  (ival (rnd 'down bfasin (ival-lo x)) (rnd 'up bfasin (ival-hi x))
        (or (ival-err? x) (bflt? (ival-lo x) -1.bf) (bfgt? (ival-hi x) 1.bf))
        (or (ival-err x) (bflt? (ival-hi x) -1.bf) (bfgt? (ival-lo x) 1.bf))))

(define (ival-acos x)
  (ival (rnd 'down bfacos (ival-hi x)) (rnd 'up bfacos (ival-lo x))
        (or (ival-err? x) (bflt? (ival-lo x) -1.bf) (bfgt? (ival-hi x) 1.bf))
        (or (ival-err x) (bflt? (ival-hi x) -1.bf) (bfgt? (ival-lo x) 1.bf))))

(define (ival-fabs x)
  (cond
   [(bfgt? (ival-lo x) 0.bf) x]
   [(bflt? (ival-hi x) 0.bf)
    (ival (bfneg (ival-hi x)) (bfneg (ival-lo x)) (ival-err? x) (ival-err x))]
   [else ; interval stradles 0
    (ival 0.bf (bfmax2 (bfneg (ival-lo x)) (ival-hi x)) (ival-err? x) (ival-err x))]))

(define (ival-sinh x)
  (ival (rnd 'down bfsinh (ival-lo x)) (rnd 'up bfsinh (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-cosh x)
  (define y (ival-fabs x))
  (ival (rnd 'down bfcosh (ival-lo y)) (rnd 'up bfcosh (ival-hi y)) (ival-err? y) (ival-err y)))

(define (ival-tanh x)
  (ival (rnd 'down bftanh (ival-lo x)) (rnd 'up bftanh (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-asinh x)
  (ival (rnd 'down bfasinh (ival-lo x)) (rnd 'up bfasinh (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-acosh x)
  (ival (rnd 'down bfacosh (bfmax2 (ival-lo x) 1.bf)) (rnd 'up bfacosh (ival-hi x))
        (or (bflte? (ival-lo x) 1.bf) (ival-err? x)) (or (bflt? (ival-hi x) 1.bf) (ival-err x))))

(define (ival-atanh x)
  (ival (rnd 'down bfatanh (ival-lo x))
        (rnd 'up bfatanh (ival-hi x))
        (or (bflte? (ival-lo x) -1.bf) (bfgte? (ival-hi x) 1.bf) (ival-err? x))
        (or (bflte? (ival-hi x) -1.bf) (bfgte? (ival-lo x) 1.bf) (ival-err x))))

(define (ival-fmod x y)
  (define y* (ival-fabs y))
  (define quot (ival-div x y*))
  (define a (rnd 'down bftruncate (ival-lo quot)))
  (define b (rnd 'up bftruncate (ival-hi quot)))
  (define err? (or (ival-err? x) (ival-err? y) (bf=? (ival-lo y*) 0.bf)))
  (define err (or (ival-err x) (ival-err y) (bf=? (ival-hi y*) 0.bf)))
  (define tquot (ival a b err? err))

  (cond
   [(bf=? a b) (ival-sub x (ival-mult tquot y*))]
   [(bflte? b 0.bf) (ival (bfneg (ival-hi y*)) 0.bf err? err)]
   [(bfgte? a 0.bf) (ival 0.bf (ival-hi y*) err? err)]
   [else (ival (bfneg (ival-hi y*)) (ival-hi y*) err? err)]))

(define (ival-remainder x y)
  (define y* (ival-fabs y))
  (define quot (ival-div x y*))
  (define a (rnd 'down bfround (ival-lo quot)))
  (define b (rnd 'up bfround (ival-hi quot)))
  (define err? (or (ival-err? x) (ival-err? y) (bf=? (ival-lo y*) 0.bf)))
  (define err (or (ival-err x) (ival-err y) (bf=? (ival-hi y*) 0.bf)))

  (if (bf=? a b)
      (ival-sub x (ival-mult (ival a b err? err) y*))
      (ival (bfneg (bfdiv (ival-hi y*) 2.bf)) (bfdiv (ival-hi y*) 2.bf) err? err)))

(define-monotonic ival-rint bfrint)
(define-monotonic ival-round bfround)
(define-monotonic ival-ceil bfceiling)
(define-monotonic ival-floor bffloor)
(define-monotonic ival-trunc bftruncate)

(define (ival-erf x)
  (ival (rnd 'down bferf (ival-lo x)) (rnd 'up bferf (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-erfc x)
  (ival (rnd 'down bferfc (ival-hi x)) (rnd 'up bferfc (ival-lo x)) (ival-err? x) (ival-err x)))

(define (ival-cmp x y)
  (define can-< (bflt? (ival-lo x) (ival-hi y)))
  (define must-< (bflt? (ival-hi x) (ival-lo y)))
  (define can-> (bfgt? (ival-hi x) (ival-lo y)))
  (define must-> (bfgt? (ival-lo x) (ival-hi y)))
  (values can-< must-< can-> must->))

(define (ival-<2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival m< c< (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-<=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (not c>) (not m>) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival->2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival m> c> (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival->=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (not c<) (not m<) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-==2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (and (not c<) (not c>)) (or (not m<) (not m>)) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-comparator f name)
  (procedure-rename
   (λ as
     (if (null? as)
         ival-true
         (let loop ([head (car as)] [tail (cdr as)] [acc ival-true])
           (match tail
             ['() acc]
             [(cons next rest)
              (loop next rest (ival-and (f head next) acc))]))))
   name))

(define ival-<  (ival-comparator ival-<2  'ival-<))
(define ival-<= (ival-comparator ival-<=2 'ival-<=))
(define ival->  (ival-comparator ival->2  'ival->))
(define ival->= (ival-comparator ival->=2 'ival->=))
(define ival-== (ival-comparator ival-==2 'ival-==))

(define (ival-!=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (or m< m>) (or c< c>) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-!= . as)
  (if (null? as)
      ival-true
      (let loop ([head (car as)] [tail (cdr as)])
        (if (null? tail)
            ival-true
            (ival-and
             (foldl ival-and ival-true (map (curry ival-!=2 head) tail))
             (loop (car tail) (cdr tail)))))))

(define (ival-union x y)
  (match (ival-lo x)
   [(? bigfloat?)
    (ival (bfmin2 (ival-lo x) (ival-lo y)) (bfmax2 (ival-hi x) (ival-hi y))
          (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y)))]
   [(? boolean?)
    (ival (and (ival-lo x) (ival-lo y)) (or (ival-hi x) (ival-hi y))
          (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y)))]))

(define (propagate-err c x)
  (ival (ival-lo x) (ival-hi x)
        (or (ival-err? c) (ival-err? x))
        (or (ival-err c) (ival-err x))))

(define (ival-if c x y)
  (cond
   [(ival-lo c) (propagate-err c x)]
   [(not (ival-hi c)) (propagate-err c y)]
   [else (propagate-err c (ival-union x y))]))

(define (ival-fmin x y)
  (ival (bfmin2 (ival-lo x) (ival-lo y)) (bfmin2 (ival-hi x) (ival-hi y))
        (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-fmax x y)
  (ival (bfmax2 (ival-lo x) (ival-lo y)) (bfmax2 (ival-hi x) (ival-hi y))
        (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-copysign x y)
  (match-define (ival xlo xhi xerr? xerr) (ival-fabs x))
  (define can-neg (= (bigfloat-signbit (ival-lo y)) 1))
  (define can-pos (= (bigfloat-signbit (ival-hi y)) 0))
  (define err? (or (ival-err? y) xerr?))
  (define err (or (ival-err y) xerr))
  (match* (can-neg can-pos)
    [(#t #t) (ival (bfneg xhi) xhi err? err)]
    [(#t #f) (ival (bfneg xhi) (bfneg xlo) err? err)]
    [(#f #t) (ival xlo xhi err? err)]))

(define (ival-fdim x y)
  (ival-fabs (ival-sub x y)))

(module+ test
  (require rackunit racket/math racket/dict racket/format math/flonum)
  (require (only-in "common.rkt" sample-double))

  (define num-tests 1000)

  (define (sample-interval)
    (if (= (random 0 2) 0)
        (let ([v1 (sample-double)] [v2 (sample-double)])
          (ival (bf (min v1 v2)) (bf (max v1 v2)) (or (nan? v1) (nan? v2)) (and (nan? v1) (nan? v2))))
        (let* ([v1 (bf (sample-double))] [exp (random 0 31)] [mantissa (random 0 (expt 2 exp))] [sign (- (* 2 (random 0 2)) 1)])
          (define v2 (bfstep v1 (* sign (+ exp mantissa))))
          (if (= sign -1)
              (ival v2 v1 (or (bfnan? v1) (bfnan? v2)) (and (bfnan? v1) (bfnan? v2)))
              (ival v1 v2 (or (bfnan? v1) (bfnan? v2)) (and (bfnan? v1) (bfnan? v2)))))))

  (define (sample-from ival)
    (if (bigfloat? (ival-lo ival))
        (let ([p (random)])
          (bfadd (bfmul (bf p) (ival-lo ival)) (bfmul (bfsub 1.bf (bf p)) (ival-hi ival))))
        (let ([p (random 0 2)])
          (if (= p 0) (ival-lo ival) (ival-hi ival)))))

  (define (ival-valid? ival)
    (if (boolean? (ival-lo ival))
        (or (not (ival-lo ival)) (ival-hi ival))
        (or
         (bfnan? (ival-lo ival))
         (bfnan? (ival-hi ival))
         (bflte? (ival-lo ival) (ival-hi ival)))))

  (define (ival-contains? ival pt)
    (or (ival-err? ival)
        (if (bigfloat? pt)
            (if (bfnan? pt)
                (or (ival-err? ival)
                    (and (bfnan? (ival-lo ival)) (bfnan? (ival-hi ival))))
                (and (bflte? (ival-lo ival) pt) (bflte? pt (ival-hi ival))))
            (or (equal? pt (ival-lo ival)) (equal? pt (ival-hi ival))))))

  (define (bf-equals? bf1 bf2)
    (if (boolean? bf1)
        (equal? bf1 bf2)
        (or (bf=? bf1 bf2) (and (bfnan? bf1) (bfnan? bf2)))))

  (define (ival-equals? ival1 ival2)
    (or (ival-err? ival1)
        (and (bf-equals? (ival-lo ival1) (ival-lo ival2))
             (bf-equals? (ival-hi ival1) (ival-hi ival2)))))

  (check ival-contains? (ival-bool #f) #f)
  (check ival-contains? (ival-bool #t) #t)
  (check ival-contains? (ival-pi) (pi.bf))
  (check ival-contains? (ival-e) (bfexp 1.bf))
  (test-case "mk-ival"
    (for ([i (in-range num-tests)])
      (define pt (sample-double))
      (with-check-info (['point pt])
        (check-pred ival-valid? (mk-ival (bf pt)))
        (check ival-contains? (mk-ival (bf pt)) (bf pt)))))
  
  (define (bflogb x)
    (bffloor (bflog2 (bfabs x))))

  (define arg1
    (list (cons ival-neg   bfneg)
          (cons ival-fabs  bfabs)
          (cons ival-sqrt  bfsqrt)
          (cons ival-cbrt  bfcbrt)
          (cons ival-exp   bfexp)
          (cons ival-exp2  bfexp2)
          (cons ival-expm1 bfexpm1)
          (cons ival-log   bflog)
          (cons ival-log2  bflog2)
          (cons ival-log10 bflog10)
          (cons ival-log1p bflog1p)
          (cons ival-logb  bflogb)
          (cons ival-sin   bfsin)
          (cons ival-cos   bfcos)
          (cons ival-tan   bftan)
          (cons ival-asin  bfasin)
          (cons ival-acos  bfacos)
          (cons ival-atan  bfatan)
          (cons ival-sinh  bfsinh)
          (cons ival-cosh  bfcosh)
          (cons ival-tanh  bftanh)
          (cons ival-asinh bfasinh)
          (cons ival-acosh bfacosh)
          (cons ival-atanh bfatanh)
          (cons ival-erf   bferf)
          (cons ival-erfc  bferfc)
          (cons ival-rint  bfrint)
          (cons ival-round bfround)
          (cons ival-ceil  bfceiling)
          (cons ival-floor bffloor)
          (cons ival-trunc bftruncate)))

  (for ([(ival-fn fn) (in-dict arg1)])
    (test-case (~a (object-name ival-fn))
       (for ([n (in-range num-tests)])
         (define i (sample-interval))
         (define x (sample-from i))
         (define ilo (struct-copy ival i [lo x]))
         (define ihi (struct-copy ival i [hi x]))
         (with-check-info (['fn ival-fn] ['interval i] ['point x] ['number n])
           (define out (ival-fn i))
           (check-pred ival-valid? out)
           (check ival-contains? out (fn x))
           (check ival-equals? out (ival-union (ival-fn ilo) (ival-fn ihi)))))))

  (define (bfcopysign x y)
    (bfmul (bfabs x) (if (= (bigfloat-signbit y) 1) -1.bf 1.bf)))

  (define (bffdim x y)
    (if (bfgt? x y) (bfsub x y) (bfsub y x)))

  (define arg2
    (list (cons ival-add bfadd)
          (cons ival-sub bfsub)
          (cons ival-mult bfmul)
          (cons ival-div bfdiv)
          (cons ival-pow   bfexpt)
          (cons ival-hypot bfhypot)
          (cons ival-atan2 bfatan2)
          (cons ival-< bflt?)
          (cons ival-<= bflte?)
          (cons ival-> bfgt?)
          (cons ival->= bfgte?)
          (cons ival-== bf=?)
          (cons ival-!= (compose not bf=?))
          (cons ival-fmin bfmin2)
          (cons ival-fmax bfmax2)
          (cons ival-copysign bfcopysign)
          (cons ival-fdim bffdim)))

  (for ([(ival-fn fn) (in-dict arg2)])
    (test-case (~a (object-name ival-fn))
       (for ([n (in-range num-tests)])
         (define i1 (sample-interval))
         (define i2 (sample-interval))
         (define x1 (sample-from i1))
         (define x2 (sample-from i2))

         (define i1lo (struct-copy ival i1 [lo x1]))
         (define i1hi (struct-copy ival i1 [hi x1]))
         (define i2lo (struct-copy ival i2 [lo x2]))
         (define i2hi (struct-copy ival i2 [hi x2]))

         (with-check-info (['fn ival-fn] ['interval1 i1] ['interval2 i2] ['point1 x1] ['point2 x2] ['number n])
           (define iy (ival-fn i1 i2))
           (check-pred ival-valid? iy)
           (check ival-contains? iy (fn x1 x2))
           (check ival-equals? iy (ival-union (ival-fn i1lo i2) (ival-fn i1hi i2)))
           (check ival-equals? iy (ival-union (ival-fn i1 i2lo) (ival-fn i1 i2hi)))))))

  (define (bffmod x y)
    (parameterize ([bf-precision 8000]) (bfsub x (bfmul (bftruncate (bfdiv x y)) y))))

  (define (bfremainder x mod)
    (parameterize ([bf-precision 8000]) (bfsub x (bfmul (bfround (bfdiv x mod)) mod))))

  (define weird (list (cons ival-fmod bffmod) (cons ival-remainder bfremainder)))

  (for ([(ival-fn fn) (in-dict weird)])
    (test-case (~a (object-name ival-fn))
      (for ([n (in-range num-tests)])
        (define i1 (sample-interval))
        (define i2 (sample-interval))
        (define x1 (sample-from i1))
        (define x2 (sample-from i2))
        (define i1lo (struct-copy ival i1 [lo x1]))
        (define i1hi (struct-copy ival i1 [hi x1]))
        (define i2lo (struct-copy ival i2 [lo x2]))
        (define i2hi (struct-copy ival i2 [hi x2]))

        (define y (fn x1 x2))

        ;; Known bug in bffmod where rounding error causes invalid output
        (unless (or (bflte? (bfmul y x1) 0.bf) (bfgt? (bfabs y) (bfabs x2)))
          (with-check-info (['fn ival-fn] ['interval1 i1] ['interval2 i2]
                            ['point1 x1] ['point2 x2] ['number n])
            (define iy (ival-fn i1 i2))
            (check-pred ival-valid? iy)
            (check ival-contains? iy y)
            (check ival-equals? iy (ival-union (ival-fn i1lo i2) (ival-fn i1hi i2)))
            (check ival-equals? iy (ival-union (ival-fn i1 i2lo) (ival-fn i1 i2hi))))))))
  )
