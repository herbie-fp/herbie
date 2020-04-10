#lang racket/base

(require racket/contract racket/match racket/function math/private/bigfloat/mpfr)

(struct endpoint (val immovable?) #:transparent)
(struct ival (lo hi err? err) #:transparent)
(define value? (or/c bigfloat? boolean?))

(define (ival-hi-val ival)
  (endpoint-val (ival-hi ival)))
(define (ival-lo-val ival)
  (endpoint-val (ival-lo ival)))

(provide (contract-out
          [struct endpoint ([val value?] [immovable? boolean?])]
          [struct ival ([lo endpoint?] [hi endpoint?] [err? boolean?] [err boolean?])]
          [mk-ival (-> value? ival?)]
          [ival-hi-val (-> ival? value?)]
          [ival-lo-val (-> ival? value?)]
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
          [ival-and (->* () #:rest (listof ival?) ival?)]
          [ival-or  (->* () #:rest (listof ival?) ival?)]
          [ival-not (-> ival? ival?)]
          [ival-<  (->* () #:rest (listof ival?) ival?)]
          [ival-<= (->* () #:rest (listof ival?) ival?)]
          [ival->  (->* () #:rest (listof ival?) ival?)]
          [ival->= (->* () #:rest (listof ival?) ival?)]
          [ival-== (->* () #:rest (listof ival?) ival?)]
          [ival-!= (->* () #:rest (listof ival?) ival?)]
          [ival-if (-> ival? ival? ival? ival?)]
          [ival-fmin (-> ival? ival? ival?)]
          [ival-fmax (-> ival? ival? ival?)]
          [ival-copysign (-> ival? ival? ival?)]
          [ival-fdim (-> ival? ival? ival?)]))

(define (mk-ival x)
  (match x
    [(? bigfloat?)
     (define err? (bfnan? x))
     (ival (endpoint x #t) (endpoint x #t) err? err?)]
    [(? boolean?)
     (ival (endpoint x #t) (endpoint x #t) #f #f)]
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

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(define (ival-pi)
  (ival (endpoint (rnd 'down pi.bf) #f) (endpoint (rnd 'up pi.bf) #f) #f #f))

(define (ival-e)
  (ival (endpoint (rnd 'down bfexp 1.bf) #f) (endpoint (rnd 'up bfexp 1.bf) #f) #f #f))

(define (ival-bool b)
  (ival (endpoint b #t) (endpoint b #t) #f #f))

(define ival-true (ival-bool #t))

(define-syntax-rule (rnd mode op args ...)
  (parameterize ([bf-rounding-mode mode])
    (op args ...)))

(define (split-ival i val)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) i)
  (values (struct-copy ival i [hi (endpoint val xhi!)])
          (struct-copy ival i [lo (endpoint val xlo!)])))

(define (classify-ival x [val 0.bf])
  (cond [(bfgte? (ival-lo-val x) val) 1] [(bflte? (ival-hi-val x) val) -1] [else 0]))

(define (endpoint-min2 e1 e2)
  (if (bflt? (endpoint-val e1) (endpoint-val e2))
      e1
      e2))

(define (endpoint-max2 e1 e2)
  (if (bfgt? (endpoint-val e1) (endpoint-val e2))
      e1
      e2))

(define (ival-union x y)
  (match (ival-lo-val x)
   [(? bigfloat?)
    (ival (endpoint-min2 (ival-lo x) (ival-lo y))
          (endpoint-max2 (ival-hi x) (ival-hi y))
          (or (ival-err? x) (ival-err? y)) (and (ival-err x) (ival-err y)))]
   [(? boolean?)
    (ival (e-compute and-fn (ival-lo x) (ival-lo y))
          (e-compute or-fn (ival-hi x) (ival-hi y))
          (or (ival-err? x) (ival-err? y)) (and (ival-err x) (ival-err y)))]))

(define (propagate-err c x)
  (ival (ival-lo x) (ival-hi x)
        (or (ival-err? c) (ival-err? x))
        (or (ival-err c) (ival-err x))))

;; This function computes and propagates the immovable? flag for endpoints
(define (e-compute op . args)
  (define args-bf (map endpoint-val args))
  (define-values (result exact?) (bf-return-exact? op args-bf))
  (endpoint result (and (andmap endpoint-immovable? args) exact?)))

;; Some hairy code follows to access the MPFR "inexact" exception.
;; It assumes no one else cares about the flag, so it clobbers it.
(module hairy racket/base
  (require ffi/unsafe math/private/bigfloat/mpfr)
  (provide mpfr_clear_inexflag mpfr_get_inexflag)
  (define mpfr_clear_inexflag (get-mpfr-fun 'mpfr_clear_inexflag (_fun -> _void)))
  (define mpfr_get_inexflag (get-mpfr-fun 'mpfr_inexflag_p (_fun -> _int))))
(require (submod "." hairy))

(define (bf-return-exact? op args)
  (mpfr_clear_inexflag)
  (define out (apply op args))
  (define exact? (= (mpfr_get_inexflag) 0))
  (values out exact?))
;; End hairy code

(define (ival-neg x)
  ;; No rounding, negation is exact
  (ival
   (e-compute bfneg (ival-hi x))
   (e-compute bfneg (ival-lo x))
   (ival-err? x) (ival-err x)))

;; Endpoint computation for both `add`, `sub`, and `hypot` (which has an add inside)
(define (eplinear bffn a-endpoint b-endpoint)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (define-values (val exact?) (bf-return-exact? bffn (list a b)))
  (endpoint val (or (and a! b! exact?) (and a! (bfinfinite? a)) (and b! (bfinfinite? b)))))

(define (ival-add x y)
  (ival
   (rnd 'down eplinear bfadd (ival-lo x) (ival-lo y))
   (rnd 'up   eplinear bfadd (ival-hi x) (ival-hi y))
   (or (ival-err? x) (ival-err? y))
   (or (ival-err x) (ival-err y))))

(define (ival-sub x y)
  (ival (rnd 'down eplinear bfsub (ival-lo x) (ival-hi y))
        (rnd 'up   eplinear bfsub (ival-hi x) (ival-lo y))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (epmul a-endpoint b-endpoint a-class b-class)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (define-values (val exact?) (bf-return-exact? bfmul (list a b)))
  (endpoint val
   (or (and a! b! exact?)
       (and a! (bfzero? a))
       (and a! (bfinfinite? a) (not (= b-class 0)))
       (and b! (bfzero? b))
       (and b! (bfinfinite? b) (not (= a-class 0))))))

(define (ival-mult x y)
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)

  (define (mkmult a b c d)
    (ival (rnd 'down epmul a b x-sign y-sign)
          (rnd 'up   epmul c d x-sign y-sign)
          (or xerr? yerr?)  (or xerr yerr)))

  (define x-sign (classify-ival x))
  (define y-sign (classify-ival y))

  (match* (x-sign y-sign)
    [( 1  1) (mkmult xlo ylo xhi yhi)]
    [( 1 -1) (mkmult xhi ylo xlo yhi)]
    [( 1  0) (mkmult xhi ylo xhi yhi)]
    [(-1  0) (mkmult xlo yhi xlo ylo)]
    [(-1  1) (mkmult xlo yhi xhi ylo)]
    [(-1 -1) (mkmult xhi yhi xlo ylo)]
    [( 0  1) (mkmult xlo yhi xhi yhi)]
    [( 0 -1) (mkmult xhi ylo xlo ylo)]
    [( 0  0)
     ;; Here, the two branches of the union are meaningless on their own;
     ;; however, both branches compute possible lo/hi's to min/max together
     (ival-union (mkmult xhi ylo xlo ylo)
                 (mkmult xlo yhi xhi yhi))]))

(define (epdiv a-endpoint b-endpoint a-class)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (define-values (val exact?) (bf-return-exact? bfdiv (list a b)))
  (endpoint val
   (or (and a! b! exact?)
       (and a! (bfzero? a))
       (and a! (bfinfinite? a))
       (and b! (bfinfinite? b))
       (and b! (bfzero? b) (not (= a-class 0))))))

(define (ival-div x y)
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)
  (define err? (or xerr? yerr? (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err (or xerr yerr (and (bfzero? (ival-lo-val y)) (bfzero? (ival-hi-val y)))))
  (define x-class (classify-ival x))
  (define y-class (classify-ival y))

  (define (mkdiv a b c d)
    (ival (rnd 'down epdiv a b x-class) (rnd 'up epdiv c d x-class) err? err))
  
  (match* (x-class y-class)
    [(_ 0) ; In this case, y stradles 0
     (define immovable? (and (endpoint-immovable? xlo) (endpoint-immovable? ylo)))
     (ival (endpoint -inf.bf immovable?) (endpoint +inf.bf immovable?) err? err)]
    [( 1  1) (mkdiv xlo yhi xhi ylo)]
    [( 1 -1) (mkdiv xhi yhi xlo ylo)]
    [(-1  1) (mkdiv xlo ylo xhi yhi)]
    [(-1 -1) (mkdiv xhi ylo xlo yhi)]
    [( 0  1) (mkdiv xlo ylo xhi ylo)]
    [( 0 -1) (mkdiv xhi yhi xlo yhi)]))

;; Helpers for defining interval functions

(define-syntax-rule (define* name expr)
  (define name (procedure-rename expr 'name)))

(define ((monotonic bffn) x)
  (match-define (ival lo hi err? err) x)
  (ival  (rnd 'down e-compute bffn lo) (rnd 'up   e-compute bffn hi) err? err))

(define ((comonotonic bffn) x)
  (match-define (ival lo hi err? err) x)
  (ival  (rnd 'down e-compute bffn hi) (rnd 'up   e-compute bffn lo) err? err))

(define ((clamp lo hi) x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (ival (endpoint (bfmax2 xlo lo) xlo!)
        (endpoint (bfmin2 xhi hi) xhi!)
        (or xerr? (bflt? xlo lo) (bfgt? xhi hi))
        (or xerr (bflt? xhi lo) (bfgt? xlo hi))))

(define ((overflows-at fn lo hi) x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (match-define (ival (endpoint ylo ylo!) (endpoint yhi yhi!) yerr? yerr) (fn x))
  (ival (endpoint ylo (or ylo! (bflte? xhi lo) (and (bflte? xlo lo) xlo!)))
        (endpoint yhi (or yhi! (bfgte? xlo hi) (and (bfgte? xhi hi) xhi!)))
        xerr? xerr))

(define* ival-rint (monotonic bfrint))
(define* ival-round (monotonic bfround))
(define* ival-ceil (monotonic bfceiling))
(define* ival-floor (monotonic bffloor))
(define* ival-trunc (monotonic bftruncate))

(define (ival-fabs x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (cond
   [(bfgt? xlo 0.bf) x]
   [(bflt? xhi 0.bf) (ival-neg x)]
   [else ; interval stradles 0
    (ival (endpoint 0.bf (and xlo! xhi!))
          (endpoint-max2 (endpoint (bfneg xlo) xlo!) (ival-hi x))
          (ival-err? x) (ival-err x))]))

;; Since MPFR has a cap on exponents, no value can be more than twice MAX_VAL
(define exp-overflow-threshold  (bfadd (bflog (bfprev +inf.bf)) 1.bf))
(define exp2-overflow-threshold (bfadd (bflog2 (bfprev +inf.bf)) 1.bf))

(define* ival-exp
  (overflows-at (monotonic bfexp) (bfneg exp-overflow-threshold) exp-overflow-threshold))
(define* ival-expm1
  (overflows-at (monotonic bfexpm1) (bfneg exp-overflow-threshold) exp-overflow-threshold))
(define* ival-exp2
  (overflows-at (monotonic bfexp2) (bfneg exp2-overflow-threshold) exp2-overflow-threshold))

(define* ival-log (compose (monotonic bflog) (clamp 0.bf +inf.bf)))
(define* ival-log2 (compose (monotonic bflog2) (clamp 0.bf +inf.bf)))
(define* ival-log10 (compose (monotonic bflog10) (clamp 0.bf +inf.bf)))
(define* ival-log1p (compose (monotonic bflog1p) (clamp -1.bf +inf.bf)))
(define* ival-logb (compose ival-floor ival-log2 ival-fabs))

(define* ival-sqrt (compose (monotonic bfsqrt) (clamp 0.bf +inf.bf)))
(define* ival-cbrt (monotonic bfcbrt))

(define (ival-hypot x y)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))
  (define x* (ival-fabs x))
  (define y* (ival-fabs y))
  (ival (rnd 'down eplinear bfhypot (ival-lo x*) (ival-lo y*))
        (rnd 'up   eplinear bfhypot (ival-hi x*) (ival-hi y*)) err? err))

(define (eppow a-endpoint b-endpoint a-class b-class)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (define-values (val exact?) (bf-return-exact? bfexpt (list a b)))
  (endpoint val
   (or (and a! b! exact?)
       (and a! (bf=? a 1.bf))
       (and a! (bfzero? a) (not (= b-class 0)))
       (and a! (bfinfinite? a) (not (= b-class 0)))
       (and b! (bfzero? b))
       (and b! (bfinfinite? b) (not (= a-class 0))))))

(define (ival-pow-pos x y)
  ;; Assumes x is positive; code copied from ival-mult
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)
  (define x-class (classify-ival x 1.bf))
  (define y-class (classify-ival y))

  (define (mk-pow a b c d)
    (ival (rnd 'down eppow a b x-class y-class)
          (rnd 'up   eppow c d x-class y-class)
          (or xerr? yerr? (and (bfzero? (endpoint-val xlo)) (bflte? (endpoint-val ylo) 0.bf)))
          (or xerr yerr (and (bfzero? (endpoint-val xhi)) (bflte? (endpoint-val yhi) 0.bf)))))

  (match* (x-class y-class)
    [( 1  1) (mk-pow xlo ylo xhi yhi)]
    [( 1  0) (mk-pow xhi ylo xhi yhi)]
    [( 1 -1) (mk-pow xhi ylo xlo yhi)]
    [( 0  1) (mk-pow xlo yhi xhi yhi)]
    [( 0 -1) (mk-pow xhi ylo xlo ylo)]
    [(-1  1) (mk-pow xlo yhi xhi ylo)]
    [(-1  0) (mk-pow xlo yhi xlo ylo)]
    [(-1 -1) (mk-pow xhi yhi xlo ylo)]
    [( 0  0) ;; Special case
     (ival-union (mk-pow xlo yhi xhi yhi) (mk-pow xhi ylo xlo ylo))]))


(define (ival-pow-neg x y)
  ;; Assumes x is negative
  (define err? (or (ival-err? x) (ival-err? y) (bflt? (ival-lo-val y) (ival-hi-val y))))
  (define err (or (ival-err x) (ival-err y)))
  (define xpos (ival-fabs x))
  (define a (bfceiling (ival-lo-val y)))
  (define b (bffloor (ival-hi-val y)))
  (cond
   [(bflt? b a)
    (ival (endpoint +nan.bf #t) (endpoint +nan.bf #t) #t #t)]
   [(bf=? a b)
    (define aep (endpoint a (and (endpoint-immovable? (ival-lo y)) (endpoint-immovable? (ival-hi y)))))
    (if (bfodd? a)
        (ival-neg (ival-pow-pos xpos (ival aep aep err? err)))
        (ival-pow-pos xpos (ival aep aep err? err)))]
   [else
    ;; TODO: the movability here is pretty subtle
    (define odds (ival (endpoint (if (bfodd? a) a (bfadd a 1.bf)) #f)
                       (endpoint (if (bfodd? b) b (bfsub b 1.bf)) #f) err? err))
    (define evens (ival (endpoint (if (bfodd? a) (bfadd a 1.bf) a) #f)
                        (endpoint (if (bfodd? b) (bfsub b 1.bf) b) #f) err? err))
    (ival-union (ival-pow-pos xpos evens)
                (ival-neg (ival-pow-pos xpos odds)))]))

(define (ival-pow x y)
  (match (classify-ival x)
    [-1 (ival-pow-neg x y)]
    [1 (ival-pow-pos x y)]
    [0
     (define-values (neg pos) (split-ival x 0.bf))
     (ival-union (ival-pow-neg neg y) (ival-pow-pos pos y))]))

(define (ival-fma a b c)
  (ival-add (ival-mult a b) c))

(define (ival-and . as)
  (ival (endpoint (andmap ival-lo-val as) (andmap (compose endpoint-immovable? ival-lo) as))
        (endpoint (andmap ival-hi-val as) (andmap (compose endpoint-immovable? ival-hi) as))
        (ormap ival-err? as) (ormap ival-err as)))

(define (ival-or . as)
  (ival (endpoint (ormap ival-lo-val as) (andmap (compose endpoint-immovable? ival-lo) as))
        (endpoint (ormap ival-hi-val as) (andmap (compose endpoint-immovable? ival-hi) as))
        (ormap ival-err? as) (ormap ival-err as)))

(define (ival-not x)
  (ival (e-compute not (ival-hi x))
        (e-compute not (ival-lo x))
        (ival-err? x)
        (ival-err x)))

(define (ival-cos x)
  (define lopi (rnd 'down pi.bf))
  (define hipi (rnd 'up pi.bf))
  (define a (rnd 'down bffloor (bfdiv (ival-lo-val x) (if (bflt? (ival-lo-val x) 0.bf) lopi hipi))))
  (define b (rnd 'up   bffloor (bfdiv (ival-hi-val x) (if (bflt? (ival-hi-val x) 0.bf) hipi lopi))))
  (cond
   [(and (bf=? a b) (bfeven? a))
    (ival (rnd 'down e-compute bfcos (ival-hi x))
          (rnd 'up e-compute bfcos (ival-lo x))
          (ival-err? x) (ival-err x))]
   [(and (bf=? a b) (bfodd? a))
    (ival (rnd 'down e-compute bfcos (ival-lo x))
          (rnd 'up e-compute bfcos (ival-hi x)) (ival-err? x) (ival-err x))]
   [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
    (ival (endpoint -1.bf #f)
          (rnd 'up e-compute bfmax2 (e-compute bfcos (ival-lo x)) (e-compute bfcos (ival-hi x)))
          (ival-err? x) (ival-err x))]
   [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
    (ival (rnd 'down e-compute bfmin2 (e-compute bfcos (ival-lo x)) (e-compute bfcos (ival-hi x)))
          (endpoint 1.bf #f) (ival-err? x) (ival-err x))]
   [else
    (ival (endpoint -1.bf #f) (endpoint 1.bf #f) (ival-err? x) (ival-err x))]))

(define (ival-sin x)
  (define lopi (rnd 'down pi.bf))
  (define hipi (rnd 'up pi.bf))
  (define a (rnd 'down bffloor (bfsub (bfdiv (ival-lo-val x) (if (bflt? (ival-lo-val x) 0.bf) lopi hipi)) half.bf))) ; half.bf is exact
  (define b (rnd 'up bffloor (bfsub (bfdiv (ival-hi-val x) (if (bflt? (ival-hi-val x) 0.bf) hipi lopi)) half.bf)))
  (cond
    [(and (bf=? a b) (bfeven? a))
     (ival (rnd 'down e-compute bfsin (ival-hi x))
           (rnd 'up e-compute bfsin (ival-lo x)) (ival-err? x) (ival-err x))]
    [(and (bf=? a b) (bfodd? a))
     (ival (rnd 'down e-compute bfsin (ival-lo x)) (rnd 'up e-compute bfsin (ival-hi x)) (ival-err? x) (ival-err x))]
    [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
     (ival (endpoint -1.bf #f) (rnd 'up e-compute bfmax2 (e-compute bfsin (ival-lo x)) (e-compute bfsin (ival-hi x))) (ival-err? x) (ival-err x))]
    [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
     (ival (rnd 'down e-compute bfmin2 (e-compute bfsin (ival-lo x)) (e-compute bfsin (ival-hi x))) (endpoint 1.bf #f) (ival-err? x) (ival-err x))]
    [else
     (ival (endpoint -1.bf #f) (endpoint 1.bf #f) (ival-err? x) (ival-err x))]))

(define (ival-tan x)
  (define lopi (rnd 'down pi.bf))
  (define hipi (rnd 'up pi.bf))
  (define a (rnd 'down bffloor (bfsub (bfdiv (ival-lo-val x) (if (bflt? (ival-lo-val x) 0.bf) lopi hipi)) half.bf))) ; half.bf is exact
  (define b (rnd 'up bffloor (bfsub (bfdiv (ival-hi-val x) (if (bflt? (ival-hi-val x) 0.bf) hipi lopi)) half.bf)))
  (if (bf=? a b) ; Same period
      (ival (rnd 'down e-compute bftan (ival-lo x)) (rnd 'up e-compute bftan (ival-hi x)) #f #f)
      (ival (endpoint -inf.bf #f) (endpoint +inf.bf #f) #t #f)))

(define* ival-asin (compose (monotonic bfasin) (clamp -1.bf 1.bf)))
(define* ival-acos (compose (comonotonic bfacos) (clamp -1.bf 1.bf)))
(define* ival-atan (monotonic bfatan))

(define (ival-atan2 y x)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))

  (define tl (cons (ival-hi y) (ival-lo x)))
  (define tr (cons (ival-hi y) (ival-hi x)))
  (define bl (cons (ival-lo y) (ival-lo x)))
  (define br (cons (ival-lo y) (ival-hi x)))

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
      (ival (rnd 'down e-compute bfatan2 (car a-lo) (cdr a-lo))
            (rnd 'up   e-compute bfatan2 (car a-hi) (cdr a-hi)) err? err)
      (ival (endpoint (bfneg (rnd 'up pi.bf)) #f) (endpoint (rnd 'up pi.bf) #f)
            (or err? (bfgte? (ival-hi-val x) 0.bf))
            (or err (and (bf=? (ival-lo-val x) 0.bf) (bf=? (ival-hi-val x) 0.bf)
                         (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))))

(define* ival-cosh (compose (monotonic bfcosh) ival-fabs))
(define* ival-sinh (monotonic bfsinh))
(define* ival-tanh (monotonic bftanh))
(define* ival-asinh (monotonic bfasinh))
(define* ival-acosh (compose (monotonic bfacosh) (clamp 1.bf +inf.bf)))
(define* ival-atanh (compose (monotonic bfatanh) (clamp -1.bf 1.bf)))

(define (ival-fmod-pos x y err? err)
  ;; Assumes both `x` and `y` are entirely positive
  (define a (rnd 'down bftruncate (bfdiv (ival-lo-val x) (ival-hi-val y))))
  (define b (rnd 'up bftruncate (bfdiv (ival-hi-val x) (ival-hi-val y))))
  (cond
   [(bf=? a b) ; No intersection along `y.hi` edge
    (define c (rnd 'down bftruncate (bfdiv (ival-hi-val x) (ival-hi-val y))))
    (define d (rnd 'up bftruncate (bfdiv (ival-hi-val x) (ival-lo-val y))))
    (cond
     [(bf=? c d) ; No intersection along `x.hi` either; use top-left/bottom-right point
      (ival (endpoint (rnd 'down bfsub (ival-lo-val x) (rnd 'up bfmul c (ival-hi-val y))) #f)
            (endpoint (rnd 'up bfsub (ival-hi-val x) (rnd 'down bfmul c (ival-lo-val y))) #f)
            err? err)]
     [else
      (ival (endpoint 0.bf #f) (endpoint (rnd 'up bfdiv (ival-hi-val x) (bfadd c 1.bf)) #f) err? err)])]
   [else
    (ival (endpoint 0.bf #f) (ival-hi y) err? err)]))

(define (ival-fmod x y)
  (define err? (or (ival-err? x) (ival-err? y)
                   (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err (or (ival-err x) (ival-err y)
                  (and (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))
  (define y* (ival-fabs y))
  (cond
   [(bflte? (ival-hi-val x) 0.bf)
    (ival-neg (ival-fmod-pos (ival-neg x) y* err? err))]
   [(bfgte? (ival-lo-val x) 0.bf)
    (ival-fmod-pos x y* err? err)]
   [else
    (define-values (neg pos) (split-ival x 0.bf))
    (ival-union (ival-fmod-pos pos y* err? err)
                (ival-neg (ival-fmod-pos (ival-neg neg) y* err? err)))]))

(define (ival-remainder-pos x y err? err)
  ;; Assumes both `x` and `y` are entirely positive
  (define a (rnd 'down bfround (bfdiv (ival-lo-val x) (ival-hi-val y))))
  (define b (rnd 'up bfround (bfdiv (ival-hi-val x) (ival-hi-val y))))
  (cond
   [(bf=? a b) ; No intersection along `y.hi` edge
    (define c (rnd 'down bfround (bfdiv (ival-hi-val x) (ival-hi-val y))))
    (define d (rnd 'up bfround (bfdiv (ival-hi-val x) (ival-lo-val y))))
    (cond
     [(bf=? c d) ; No intersection along `x.hi` either; use top-left/bottom-right point
      (ival (endpoint (rnd 'down bfsub (ival-lo-val x) (rnd 'up bfmul c (ival-hi-val y))) #f)
            (endpoint (rnd 'up bfsub (ival-hi-val x) (rnd 'down bfmul c (ival-lo-val y))) #f)
            err? err)]
     [else
      (define y* (bfdiv (rnd 'down bfdiv (ival-hi-val x) (bfadd c half.bf)) 2.bf))
      (ival (endpoint (bfneg y*) #f) (endpoint y* #f) err? err)])]
   [else
    (define y* (bfdiv (ival-hi-val y) 2.bf))
    (ival (endpoint (bfneg y*) #f) (endpoint y* #f) err? err)]))

(define (ival-remainder x y)
  (define err? (or (ival-err? x) (ival-err? y)
                   (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err (or (ival-err x) (ival-err y)
                  (and (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))
  (define y* (ival-fabs y))
  (cond
   [(bflte? (ival-hi-val x) 0.bf)
    (ival-neg (ival-remainder-pos (ival-neg x) y* err? err))]
   [(bfgte? (ival-lo-val x) 0.bf)
    (ival-remainder-pos x y* err? err)]
   [else
    (define-values (neg pos) (split-ival x 0.bf))
    (ival-union (ival-remainder-pos pos y* err? err)
                (ival-neg (ival-remainder-pos (ival-neg neg) y* err? err)))]))

(define* ival-erf (monotonic bferf))
(define* ival-erfc (comonotonic bferfc))

(define (ival-cmp x y)
  (define can-< (e-compute bflt? (ival-lo x) (ival-hi y)))
  (define must-< (e-compute bflt? (ival-hi x) (ival-lo y)))
  (define can-> (e-compute bfgt? (ival-hi x) (ival-lo y)))
  (define must-> (e-compute bfgt? (ival-lo x) (ival-hi y)))
  (values can-< must-< can-> must->))

(define (ival-<2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival m< c< (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-<=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (e-compute not c>) (e-compute not m>) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival->2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival m> c> (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival->=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (e-compute not c<) (e-compute not m<) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-==2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (e-compute and-fn (e-compute not c<) (e-compute not c>))
        (e-compute and-fn (e-compute not m<) (e-compute not m>))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define ((ival-comparator f name) . as)
  (if (null? as)
      ival-true
      (let loop ([head (car as)] [tail (cdr as)] [acc ival-true])
        (match tail
          ['() acc]
          [(cons next rest)
           (loop next rest (ival-and (f head next) acc))]))))

(define* ival-<  (ival-comparator ival-<2  'ival-<))
(define* ival-<= (ival-comparator ival-<=2 'ival-<=))
(define* ival->  (ival-comparator ival->2  'ival->))
(define* ival->= (ival-comparator ival->=2 'ival->=))
(define* ival-== (ival-comparator ival-==2 'ival-==))

(define (ival-!=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (e-compute or-fn m< m>) (e-compute or-fn c< c>)
        (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-!= . as)
  (if (null? as)
      ival-true
      (let loop ([head (car as)] [tail (cdr as)])
        (if (null? tail)
            ival-true
            (ival-and
             (foldl ival-and ival-true (map (curry ival-!=2 head) tail))
             (loop (car tail) (cdr tail)))))))

(define (ival-if c x y)
  (cond
   [(ival-lo-val c) (propagate-err c x)]
   [(not (ival-hi-val c)) (propagate-err c y)]
   [else (propagate-err c (ival-union x y))]))

(define (ival-fmin x y)
  (ival (endpoint-min2 (ival-lo x) (ival-lo y)) (endpoint-min2 (ival-hi x) (ival-hi y))
        (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-fmax x y)
  (ival (endpoint-max2 (ival-lo x) (ival-lo y)) (endpoint-max2 (ival-hi x) (ival-hi y))
        (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-copysign x y)
  (match-define (ival xlo xhi xerr? xerr) (ival-fabs x))
  (define can-neg (= (bigfloat-signbit (ival-lo-val y)) 1))
  (define can-pos (= (bigfloat-signbit (ival-hi-val y)) 0))
  (define err? (or (ival-err? y) xerr?))
  (define err (or (ival-err y) xerr))
  (match* (can-neg can-pos)
    [(#t #t) (ival (e-compute bfneg xhi) xhi err? err)]
    [(#t #f) (ival (e-compute bfneg xhi) (e-compute bfneg xlo) err? err)]
    [(#f #t) (ival xlo xhi err? err)]))

(define* ival-fdim (compose ival-fabs ival-sub))

(module+ test
  (require rackunit racket/math racket/dict racket/format math/flonum racket/list)
  (require (only-in "common.rkt" sample-double))

  (define num-tests 2500)

  (define (sample-interval type)
    (match type
      ['real
       (if (= (random 0 2) 0)
           (let ([v1 (sample-double)] [v2 (sample-double)])
             (if (or (nan? v1) (nan? v2))
                 (sample-interval type)
                 (ival (endpoint (bf (min v1 v2)) #t) (endpoint (bf (max v1 v2)) #t) #f #f)))
           (let* ([v1 (bf (sample-double))] [exp (random 0 31)] [mantissa (random 0 (expt 2 exp))] [sign (- (* 2 (random 0 2)) 1)])
             (define v2 (bfstep v1 (* sign (+ exp mantissa))))
             (if (or (bfnan? v1) (bfnan? v2))
                 (sample-interval type)
                 (if (= sign -1)
                     (ival (endpoint v2 #t) (endpoint v1 #t) #f #f)
                     (ival (endpoint v1 #t) (endpoint v2 #t) #f #f)))))]
      ['bool
       (match (random 0 3)
         [0 (ival-bool #f)]
         [1 (ival-bool #t)]
         [2 (ival (endpoint #f #t) (endpoint #t #t) #f #f)])]))

  (define (sample-interval-sized [size-limit 1])
    (let* ([v1 (bf (sample-double))] [exp (random 0 31)] [mantissa (random 0 (expt 2 exp))] [sign (- (* 2 (random 0 2)) 1)])
          (define v2 (bfstep v1 (exact-floor (* (* sign (+ exp mantissa)) size-limit))))
          (if (= sign -1)
              (ival (endpoint v2 #t) (endpoint v1 #t) (or (bfnan? v1) (bfnan? v2)) (and (bfnan? v1) (bfnan? v2)))
              (ival (endpoint v1 #t) (endpoint v2 #t) (or (bfnan? v1) (bfnan? v2)) (and (bfnan? v1) (bfnan? v2))))))

  (define (sample-from ival)
    (if (bigfloat? (ival-lo ival))
        (let ([p (random)])
          (bfadd (bfmul (bf p) (ival-lo-val ival)) (bfmul (bfsub 1.bf (bf p)) (ival-hi-val ival))))
        (let ([p (random 0 2)])
          (if (= p 0) (ival-lo-val ival) (ival-hi-val ival)))))

  (define-simple-check (check-ival-valid? ival)
    (if (ival-err ival)
        (ival-err? ival)
        (if (boolean? (ival-lo-val ival))
            (or (not (ival-lo-val ival)) (ival-hi-val ival))
            (bflte? (ival-lo-val ival) (ival-hi-val ival)))))

  (define-simple-check (check-ival-contains? ival pt)
    (if (bigfloat? pt)
        (if (bfnan? pt)
            (ival-err? ival)
            (and (bflte? (ival-lo-val ival) pt) (bflte? pt (ival-hi-val ival))))
        (or (equal? pt (ival-lo-val ival)) (equal? pt (ival-hi-val ival)))))

  (define (bf-equals? bf1 bf2)
    (if (boolean? bf1)
        (equal? bf1 bf2)
        (or (bf=? bf1 bf2) (and (bfnan? bf1) (bfnan? bf2)))))

  (define-binary-check (check-ival-equals? ival1 ival2)
    (or (ival-err? ival1)
        (and (bf-equals? (ival-lo-val ival1) (ival-lo-val ival2))
             (bf-equals? (ival-hi-val ival1) (ival-hi-val ival2)))))

  (check-ival-contains? (ival-bool #f) #f)
  (check-ival-contains? (ival-bool #t) #t)
  (check-ival-contains? (ival-pi) (pi.bf))
  (check-ival-contains? (ival-e) (bfexp 1.bf))
  (test-case "mk-ival"
    (for ([i (in-range num-tests)])
      (define pt (sample-double))
      (with-check-info (['point pt])
        (check-ival-valid? (mk-ival (bf pt)))
        (check-ival-contains? (mk-ival (bf pt)) (bf pt)))))
  
  (define (bflogb x)
    (bffloor (bflog2 (bfabs x))))

  (define (bfcopysign x y)
    (bfmul (bfabs x) (if (= (bigfloat-signbit y) 1) -1.bf 1.bf)))

  (define (bffdim x y)
    (if (bfgt? x y) (bfsub x y) (bfsub y x)))

  (define (if-fn c x y)
    (if c x y))

  ;;; These functions don't always work; they return 'bad when results are unreliable
  (define (bffmod x mod)
    (parameterize ([bf-precision 8000]) 
      (define r (bfsub x (bfmul (bftruncate (bfdiv x mod)) mod)))
      (if (or (bflte? (bfmul r x) 0.bf) (bfgt? (bfabs r) (bfabs mod)))
          'bad
          r)))

  (define (bfremainder x mod)
    (parameterize ([bf-precision 8000])
      (define r (bfsub x (bfmul (bfround (bfdiv x mod)) mod)))
      (if (or (bflte? (bfmul r x) 0.bf) (bfgt? (bfabs r) (bfabs mod)))
          'bad
          r)))

  (define function-table
    (list (list ival-neg   bfneg      '(real) 'real)
          (list ival-fabs  bfabs      '(real) 'real)
          (list ival-sqrt  bfsqrt     '(real) 'real)
          (list ival-cbrt  bfcbrt     '(real) 'real)
          (list ival-exp   bfexp      '(real) 'real)
          (list ival-exp2  bfexp2     '(real) 'real)
          (list ival-expm1 bfexpm1    '(real) 'real)
          (list ival-log   bflog      '(real) 'real)
          (list ival-log2  bflog2     '(real) 'real)
          (list ival-log10 bflog10    '(real) 'real)
          (list ival-log1p bflog1p    '(real) 'real)
          (list ival-logb  bflogb     '(real) 'real)
          (list ival-sin   bfsin      '(real) 'real)
          (list ival-cos   bfcos      '(real) 'real)
          (list ival-tan   bftan      '(real) 'real)
          (list ival-asin  bfasin     '(real) 'real)
          (list ival-acos  bfacos     '(real) 'real)
          (list ival-atan  bfatan     '(real) 'real)
          (list ival-sinh  bfsinh     '(real) 'real)
          (list ival-cosh  bfcosh     '(real) 'real)
          (list ival-tanh  bftanh     '(real) 'real)
          (list ival-asinh bfasinh    '(real) 'real)
          (list ival-acosh bfacosh    '(real) 'real)
          (list ival-atanh bfatanh    '(real) 'real)
          (list ival-erf   bferf      '(real) 'real)
          (list ival-erfc  bferfc     '(real) 'real)
          (list ival-rint  bfrint     '(real) 'real)
          (list ival-round bfround    '(real) 'real)
          (list ival-ceil  bfceiling  '(real) 'real)
          (list ival-floor bffloor    '(real) 'real)
          (list ival-trunc bftruncate '(real) 'real)
          (list ival-add   bfadd      '(real real) 'real)
          (list ival-sub   bfsub      '(real real) 'real)
          (list ival-mult  bfmul      '(real real) 'real)
          (list ival-div   bfdiv      '(real real) 'real)
          (list ival-pow   bfexpt     '(real real) 'real)
          (list ival-hypot bfhypot    '(real real) 'real)
          (list ival-atan2 bfatan2    '(real real) 'real)
          (list ival-fmod  bffmod     '(real real) 'real)
          (list ival-remainder bfremainder '(real real) 'real)
          (list ival-<     bflt?      '(real real) 'bool)
          (list ival-<=    bflte?     '(real real) 'bool)
          (list ival->     bfgt?      '(real real) 'bool)
          (list ival->=    bfgte?     '(real real) 'bool)
          (list ival-==    bf=?       '(real real) 'bool)
          (list ival-!= (compose not bf=?) '(real real) 'bool)
          (list ival-fmin  bfmin2     '(real real) 'real)
          (list ival-fmax  bfmax2     '(real real) 'real)
          (list ival-copysign bfcopysign '(real real) 'real)
          (list ival-fdim  bffdim     '(real real) 'real)
          (list ival-and   and-fn     '(bool bool bool) 'bool)
          (list ival-or    or-fn      '(bool bool bool) 'bool)
          (list ival-not   not        '(bool) 'bool)
          (list ival-if    if-fn      '(bool real real) 'real)
          ))

  (for ([entry (in-list function-table)])
    (match-define (list ival-fn fn args _) entry)
    (test-case (~a (object-name ival-fn))
       (for ([n (in-range num-tests)])
         (define is (for/list ([arg args]) (sample-interval arg)))
         (define xs (for/list ([i is]) (sample-from i)))
         (define iy (apply ival-fn is))
         (define y (apply fn xs))

         (unless (equal? y 'bad) ; for bffmod and bfremainder, which sometimes fail
           (with-check-info (['fn ival-fn] ['intervals is] ['points xs] ['number n])
             (check-ival-valid? iy)
             (check-ival-contains? iy y)
             (for ([k (in-naturals)] [i is] [x xs])
               (define-values (ilo ihi) (split-ival i x))
               (with-check-info (['split-argument k])
                 (check-ival-equals? iy
                   (ival-union (apply ival-fn (list-set is k ilo))
                               (apply ival-fn (list-set is k ihi)))))))))))

  ;; ##################################################### tests for endpoint-immovable
  
  (define-binary-check (check-movability coarse fine)
    (and
     (or (not (endpoint-immovable? (ival-lo coarse)))
         (bf-equals? (ival-lo-val coarse) (ival-lo-val fine)))
     (or (not (endpoint-immovable? (ival-hi coarse)))
         (bf-equals? (ival-hi-val coarse) (ival-hi-val fine)))))

  (define (test-function-overflows ival-fn fn num-of-arguments)
    (parameterize ([bf-precision 80])
      (let find-overflow-loop ([interval-size 1])
        (define intervals
          (for/list ([n (in-range num-of-arguments)])
            (sample-interval-sized interval-size)))
        (define points
          (for/list ([i intervals])
            (sample-from i)))
        (with-check-info (['fn ival-fn] ['intervals intervals] ['points points])
          (let ([result (apply ival-fn intervals)])
            (if (or (endpoint-immovable? (ival-lo result)) (endpoint-immovable? (ival-hi result)))
                (parameterize ([bf-precision 16000])
                  (let ([higher-precision-result (apply ival-fn intervals)])
                    (check-movability result higher-precision-result)))
                (when (> interval-size 0.005)
                  (find-overflow-loop (/ interval-size 4.0)))))))))

  
  (for ([entry (in-list function-table)] #:when (andmap (curry equal? 'real) (caddr entry)))
    (test-case (~a (object-name (car entry)))
      (for ([n (in-range num-tests)])
        (test-function-overflows (car entry) (cadr entry) (length (caddr entry)))))))

