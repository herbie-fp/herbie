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
          [strong-immovable-endpoint? (-> endpoint? boolean?)]
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

(define max-bf-rounded-down
  (parameterize ([bf-precision 80])
    (rnd 'down bfexp (bf 9999999999999999))))
(define min-bf-rounded-up
  (parameterize ([bf-precision 80])
    (bfneg max-bf-rounded-down)))

(define (overflow-up? low-val)
  (bfgte? low-val max-bf-rounded-down))

(define (overflow-down? high-val)
  (bflte? high-val min-bf-rounded-up))

(define (strong-immovable-endpoint? endpoint)
  (and
   (endpoint-immovable? endpoint)
   (or (equal? (endpoint-val endpoint) +nan.bf)
       (equal? (endpoint-val endpoint) +inf.bf)
       (equal? (endpoint-val endpoint) -inf.bf))))

(define (immovable-0? endpoint)
  (and (endpoint-immovable? endpoint) (equal? 0.bf endpoint)))

(define (strong-immovable-endpoint-0? endpoint)
  (and
   (endpoint-immovable? endpoint)
   (or (equal? (endpoint-val endpoint) +nan.bf)
       (equal? (endpoint-val endpoint) +inf.bf)
       (equal? (endpoint-val endpoint) -inf.bf)
       (equal? (endpoint-val endpoint) 0.bf))))

(define (and-2 a b)
  (and a b))
(define (or-2 a b)
  (or a b))

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

(define (split-ival i x)
  (values (struct-copy ival i [hi (endpoint x #t)]) (struct-copy ival i [lo (endpoint x #t)])))

;; This function computes and propagates the immovable? flag for endpoints
(define (e-compute #:check [check (const #f)] op . args)
  (define args-bf (map endpoint-val args))
  (define-values (result exact?) (bf-return-exact? op args-bf))

  ;; Inputs don't move and the rounding doesn't affect things
  (define immovable? (and (andmap endpoint-immovable? args) exact?))
  (endpoint result (or immovable? (check args))))

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

(define any-strong? (curry ormap strong-immovable-endpoint?))
(define any-strong-0? (curry ormap strong-immovable-endpoint-0?))
(define first-strong-0? (compose strong-immovable-endpoint-0? car))
(define second-strong-0? (compose strong-immovable-endpoint-0? cadr))
(define any-weak-0? (curry ormap immovable-0?))

(define (ival-neg x)
  ;; No rounding, negation is exact
  (ival
   (e-compute bfneg (ival-hi x))
   (e-compute bfneg (ival-lo x))
   (ival-err? x) (ival-err x)))

(define (ival-add x y)
  (ival
   (rnd 'down e-compute #:check any-strong? bfadd (ival-lo x) (ival-lo y))
   (rnd 'up   e-compute #:check any-strong? bfadd (ival-hi x) (ival-hi y))
   (or (ival-err? x) (ival-err? y))
   (or (ival-err x) (ival-err y))))

(define (ival-sub x y)
  (ival (rnd 'down e-compute #:check any-strong? bfsub (ival-lo x) (ival-hi y))
        (rnd 'up   e-compute #:check any-strong? bfsub (ival-hi x) (ival-lo y))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (endpoint-min2 e1 e2)
  (if (bfgt? (endpoint-val e1) (endpoint-val e2))
      e2
      e1))

(define (endpoint-max2 e1 e2)
  (if (bfgt? (endpoint-val e1) (endpoint-val e2))
      e1
      e2))

(define (ival-mult x y)
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)
  (define err? (or xerr? yerr?))
  (define err (or xerr yerr))
  (define (mkmult a b c d check)
    (ival (rnd 'down e-compute #:check check bfmul a b)
          (rnd 'up   e-compute #:check check bfmul c d) err? err))
  (match* ((classify-ival x) (classify-ival y))
    [( 1  1) (mkmult xlo ylo xhi yhi any-strong-0?)]
    [( 1 -1) (mkmult xhi ylo xlo yhi any-strong-0?)]
    [( 1  0) (mkmult xhi ylo xhi yhi second-strong-0?)]
    [(-1  0) (mkmult xlo yhi xlo ylo second-strong-0?)]
    [(-1  1) (mkmult xlo yhi xhi ylo any-strong-0?)]
    [(-1 -1) (mkmult xhi yhi xlo ylo any-strong-0?)]
    [( 0  1) (mkmult xlo yhi xhi yhi first-strong-0?)]
    [( 0 -1) (mkmult xhi ylo xlo ylo first-strong-0?)]
    [( 0  0)
     (define lo
      (rnd 'up endpoint-min2
           (e-compute #:check any-weak-0? bfmul (ival-hi x) (ival-lo y))
           (e-compute #:check any-weak-0? bfmul (ival-lo x) (ival-hi y))))
     (define hi
      (rnd 'down endpoint-max2
           (e-compute #:check any-weak-0? bfmul (ival-lo x) (ival-lo y))
           (e-compute #:check any-weak-0? bfmul (ival-hi x) (ival-hi y))))
    (ival lo hi err? err)]))

(define (ival-div x y)
  (define err? (or (ival-err? x) (ival-err? y) (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err (or (ival-err x) (ival-err y) (and (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))

  (define (mkdiv a b c d check)
    (ival (rnd 'down e-compute #:check check bfdiv a b) (rnd 'up e-compute #:check check bfdiv c d) err? err))
  
    ;; We round only down, and approximate rounding up with bfnext below
  (match* ((classify-ival x) (classify-ival y))
    [(_ 0)
     (define immovable?
       (or (and (endpoint-immovable? (ival-lo y)) (endpoint-immovable? (ival-hi y)))
           (or (immovable-0? (ival-lo y))
               (immovable-0? (ival-hi y)))))
     (ival (endpoint -inf.bf immovable?)
           (endpoint +inf.bf immovable?)
           err? err)]
    [( 1  1) (mkdiv (ival-lo x) (ival-hi y) (ival-hi x) (ival-lo y) any-strong-0?)]
    [( 1 -1) (mkdiv (ival-hi x) (ival-hi y) (ival-lo x) (ival-lo y) any-strong-0?)]
    [(-1  1) (mkdiv (ival-lo x) (ival-lo y) (ival-hi x) (ival-hi y) any-strong-0?)]
    [(-1 -1) (mkdiv (ival-hi x) (ival-lo y) (ival-lo x) (ival-hi y) any-strong-0?)]
    [( 0  1) (mkdiv (ival-lo x) (ival-lo y) (ival-hi x) (ival-lo y) any-weak-0?)]
    [( 0 -1) (mkdiv (ival-hi x) (ival-hi y) (ival-lo x) (ival-hi y) any-weak-0?)]))

;; Also detects for overflow to decide that endpoints are immovable
(define-syntax-rule (define-monotonic name bffn overflow-threshold)
  (define (name x)
    (let ([low (rnd 'down e-compute bffn (ival-lo x))]
          [high (rnd 'up e-compute bffn (ival-hi x))])
      (ival (endpoint (endpoint-val low)
                      (or (endpoint-immovable? low)
                          (bflte? (ival-hi-val x) (bfneg overflow-threshold))))
            (endpoint (endpoint-val high)
                      (or (endpoint-immovable? high)
                          (bfgte? (ival-lo-val x) overflow-threshold)))
            (ival-err? x) (ival-err x)))))

(define exp-overflow-threshold (rnd 'up bflog max-bf-rounded-down))
(define-monotonic ival-exp bfexp exp-overflow-threshold)

(define exp2-overflow-threshold (rnd 'up bflog2 max-bf-rounded-down))
(define-monotonic ival-exp2 bfexp2 exp2-overflow-threshold)

(define-monotonic ival-expm1 bfexpm1 exp-overflow-threshold)

(define-syntax-rule (define-monotonic-positive name bffn)
  (define (name x)
    (define too-low? (bflte? (ival-lo-val x) 0.bf))
    (define err (or (ival-err x) (bflte? (ival-hi-val x) 0.bf)))
    (define err? (or err (ival-err? x) too-low?))
    (ival (rnd 'down e-compute bffn (if too-low? (endpoint 0.bf #f) (ival-lo x)))
          (rnd 'up   e-compute bffn (ival-hi x)) err? err)))

(define-monotonic-positive ival-log bflog)
(define-monotonic-positive ival-log2 bflog2)
(define-monotonic-positive ival-log10 bflog10)

(define (ival-log1p x)
  (define too-low? (bflte? (ival-lo-val x) -1.bf))
  (define err (or (ival-err x) (bflte? (ival-hi-val x) -1.bf)))
  (define err? (or err (ival-err? x) too-low?))
  (ival (if too-low? (endpoint -inf.bf #f) (rnd 'down e-compute bflog1p (ival-lo x)))
        (rnd 'up   e-compute bflog1p (ival-hi x))
        err? err))

(define (ival-logb x)
  (match-define (ival ylo yhi yerr? yerr) (ival-log2 (ival-fabs x)))
  (ival (rnd 'down e-compute bffloor ylo) (rnd 'up e-compute bffloor yhi) yerr? yerr))

(define-monotonic-positive ival-sqrt bfsqrt)

(define-monotonic ival-cbrt bfcbrt +inf.bf)

(define (ival-hypot x y)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))
  (define x* (ival-fabs x))
  (define y* (ival-fabs y))
  (ival (rnd 'down e-compute #:check any-strong? bfhypot (ival-lo x*) (ival-lo y*))
        (rnd 'up   e-compute #:check any-strong? bfhypot (ival-hi x*) (ival-hi y*))
        err? err))

(define (ival-pow-pos x y)
  ;; Assumes x is positive; code copied from ival-mult
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)
  (define err? (or xerr? yerr? (and (bflte? (endpoint-val xlo) 0.bf) (bflte? (endpoint-val ylo) 0.bf))))
  (define err (or xerr yerr (and (bflte? (endpoint-val xhi) 0.bf) (bflte? (endpoint-val yhi) 0.bf))))
  (define x-fixed (classify-ival x 1.bf))
  (define y-fixed (classify-ival y))

  (define (check a&b)
    (match-define (list a b) a&b)
    (or (and (endpoint-immovable? a)
             (or (equal? (endpoint-val a) 1.bf)
                 (and (bfzero? (endpoint-val a)) (not (= y-fixed 0)))
                 (and (bfinfinite? (endpoint-val a)) (not (= y-fixed 0)))))
        (and (endpoint-immovable? b)
             (or (equal? (endpoint-val b) 0.bf)
                 (and (bfinfinite? (endpoint-val b)) (not (= x-fixed) 0))))))

  (define (mk-pow a b c d)
    (ival (rnd 'down e-compute #:check check bfexpt a b)
          (rnd 'up e-compute #:check check bfexpt c d) err? err))

  (match* (x-fixed y-fixed)
    [( 1  1) (mk-pow xlo ylo xhi yhi)]
    [( 1  0) (mk-pow xhi ylo xhi yhi)]
    [( 1 -1) (mk-pow xhi ylo xlo yhi)]
    [( 0  1) (mk-pow xlo yhi xhi yhi)]
    [( 0 -1) (mk-pow xhi ylo xlo ylo)]
    [(-1  1) (mk-pow xlo yhi xhi ylo)]
    [(-1  0) (mk-pow xlo yhi xlo ylo)]
    [(-1 -1) (mk-pow xhi yhi xlo ylo)]
    [( 0  0) ;; Special case
     (define lo
       (rnd 'down endpoint-min2
            (e-compute #:check check bfexpt xlo yhi)
            (e-compute #:check check bfexpt xhi ylo)))
     (define hi
       (rnd 'up endpoint-max2
            (e-compute #:check check bfexpt xhi yhi)
            (e-compute #:check check bfexpt xlo ylo)))
     (ival lo hi err? err)]))


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
    [0 (ival-union (ival-pow-neg (ival (ival-lo x) (endpoint 0.bf #t) (ival-err? x) (ival-err x)) y)
                   (ival-pow-pos (ival (endpoint 0.bf #t) (ival-hi x) (ival-err? x) (ival-err x)) y))]))

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

(define-monotonic ival-atan bfatan +inf.bf)

(define (classify-ival x [val 0.bf])
  (cond [(bfgte? (ival-lo-val x) val) 1] [(bflte? (ival-hi-val x) val) -1] [else 0]))
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
            (or err (and (bf=? (ival-lo-val x) 0.bf) (bf=? (ival-hi-val x) 0.bf) (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))))

(define (ival-asin x)
  (define too-low? (bflt? (ival-lo-val x) -1.bf))
  (define too-high? (bfgt? (ival-hi-val x) 1.bf))
  (ival (rnd 'down e-compute bfasin (if too-low? (endpoint -1.bf #f) (ival-lo x)))
        (rnd 'up e-compute bfasin (if too-high? (endpoint 1.bf #f) (ival-hi x)))
        (or (ival-err? x) too-low? too-high?)
        (or (ival-err x) (bflt? (ival-hi-val x) -1.bf) (bfgt? (ival-lo-val x) 1.bf))))

(define (ival-acos x)
  (define too-low? (bflt? (ival-lo-val x) -1.bf))
  (define too-high? (bfgt? (ival-hi-val x) 1.bf))
  (ival (rnd 'down e-compute bfacos (if too-high? (endpoint 1.bf #f) (ival-hi x)))
        (rnd 'up e-compute bfacos (if too-low? (endpoint -1.bf #f) (ival-lo x)))
        (or (ival-err? x) too-low? too-high?)
        (or (ival-err x) (bflt? (ival-hi-val x) -1.bf) (bfgt? (ival-lo-val x) 1.bf))))

(define (ival-fabs x)
  (cond
   [(bfgt? (ival-lo-val x) 0.bf) x]
   [(bflt? (ival-hi-val x) 0.bf)
    (ival (e-compute bfneg (ival-hi x)) (e-compute bfneg (ival-lo x)) (ival-err? x) (ival-err x))]
   [else ; interval stradles 0
    (ival (endpoint 0.bf #f) (endpoint-max2 (e-compute bfneg (ival-lo x)) (ival-hi x))
          (ival-err? x) (ival-err x))]))

(define (ival-cosh x)
  (define y (ival-fabs x))
  (ival (rnd 'down e-compute bfcosh (ival-lo y))
        (rnd 'up e-compute bfcosh (ival-hi y)) (ival-err? y) (ival-err y)))


(define-monotonic ival-sinh bfsinh +inf.bf)
(define-monotonic ival-tanh bftanh +inf.bf)
(define-monotonic ival-asinh bfasinh +inf.bf)

(define (ival-acosh x)
  (define too-low? (bflt? (ival-lo-val x) 1.bf))
  (ival (if too-low? (endpoint 0.bf #f) (rnd 'down e-compute bfacosh (e-compute bfmax2 (ival-lo x) (endpoint 1.bf #f))))
        (rnd 'up e-compute bfacosh (ival-hi x))
        (or too-low? (ival-err? x))
        (or (bflt? (ival-hi-val x) 1.bf) (ival-err x))
))

(define (ival-atanh x)
  (define too-low? (bflt? (ival-lo-val x) -1.bf))
  (define too-high? (bfgt? (ival-hi-val x) 1.bf))
  (ival (if too-low? (endpoint -inf.bf #f) (rnd 'down e-compute bfatanh (ival-lo x)))
        (if too-high? (endpoint +inf.bf #f) (rnd 'up e-compute bfatanh (ival-hi x)))
        (or too-low? too-high? (ival-err? x))
        (or (bflte? (ival-hi-val x) -1.bf) (bfgte? (ival-lo-val x) 1.bf) (ival-err x))))

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

(define-monotonic ival-rint bfrint +inf.bf)
(define-monotonic ival-round bfround +inf.bf)
(define-monotonic ival-ceil bfceiling +inf.bf)
(define-monotonic ival-floor bffloor +inf.bf)
(define-monotonic ival-trunc bftruncate +inf.bf)

(define (ival-erf x)
  (ival (rnd 'down e-compute bferf (ival-lo x))
        (rnd 'up e-compute bferf (ival-hi x))
        (ival-err? x) (ival-err x)))

(define (ival-erfc x)
  (ival (rnd 'down e-compute bferfc (ival-hi x))
        (rnd 'up   e-compute bferfc (ival-lo x))
        (ival-err? x) (ival-err x)))

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
  (ival (e-compute and-2 (e-compute not c<) (e-compute not c>))
        (e-compute and-2 (e-compute not m<) (e-compute not m>))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

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
  (ival (e-compute or-2 m< m>) (e-compute or-2 c< c>) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

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
  (match (ival-lo-val x)
   [(? bigfloat?)
    (ival (endpoint-min2 (ival-lo x) (ival-lo y))
          (endpoint-max2 (ival-hi x) (ival-hi y))
          (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y)))]
   [(? boolean?)
    (ival (e-compute and-2 (ival-lo x) (ival-lo y))
          (e-compute or-2 (ival-hi x) (ival-hi y))
          (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y)))]))

(define (propagate-err c x)
  (ival (ival-lo x) (ival-hi x)
        (or (ival-err? c) (ival-err? x))
        (or (ival-err c) (ival-err x))))

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

(define (ival-fdim x y)
  (ival-fabs (ival-sub x y)))

(module+ test
  (require rackunit racket/math racket/dict racket/format math/flonum)
  (require (only-in "common.rkt" sample-double))

  (define num-tests 2500)

  (define (sample-interval)
    (if (= (random 0 2) 0)
        (let ([v1 (sample-double)] [v2 (sample-double)])
          (if (or (nan? v1) (nan? v2))
              (sample-interval)
              (ival (endpoint (bf (min v1 v2)) #t) (endpoint (bf (max v1 v2)) #t) #f #f)))
        (let* ([v1 (bf (sample-double))] [exp (random 0 31)] [mantissa (random 0 (expt 2 exp))] [sign (- (* 2 (random 0 2)) 1)])
          (define v2 (bfstep v1 (* sign (+ exp mantissa))))
          (if (or (bfnan? v1) (bfnan? v2))
              (sample-interval)
              (if (= sign -1)
                  (ival (endpoint v2 #t) (endpoint v1 #t) #f #f)
                  (ival (endpoint v1 #t) (endpoint v2 #t) #f #f))))))

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
         (define-values (ilo ihi) (split-ival i x))
         (define iy (ival-fn i))
         (with-check-info (['fn ival-fn] ['interval i] ['point x] ['number n])
           (check-ival-valid? iy)
           (check-ival-contains? iy (fn x))
           (check-ival-equals? iy (ival-union (ival-fn ilo) (ival-fn ihi)))))))

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

         (define-values (i1lo i1hi) (split-ival i1 x1))
         (define-values (i2lo i2hi) (split-ival i2 x2))

         (define iy (ival-fn i1 i2))

         (with-check-info (['fn ival-fn] ['interval1 i1] ['interval2 i2] ['point1 x1] ['point2 x2] ['number n])
           (check-ival-valid? iy)
           (check-ival-contains? iy (fn x1 x2))
           (with-check-info (['split 'left])
             (check-ival-equals? iy (ival-union (ival-fn i1lo i2) (ival-fn i1hi i2))))
           (with-check-info (['split 'right])
             (check-ival-equals? iy (ival-union (ival-fn i1 i2lo) (ival-fn i1 i2hi))))))))

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

        (define-values (i1lo i1hi) (split-ival i1 x1))
        (define-values (i2lo i2hi) (split-ival i2 x2))

        (define y (fn x1 x2))
        (define iy (ival-fn i1 i2))

        ;; Known bug in bffmod where rounding error causes invalid output
        (unless (or (bflte? (bfmul y x1) 0.bf) (bfgt? (bfabs y) (bfabs x2)))
          (with-check-info (['fn ival-fn] ['interval1 i1] ['interval2 i2]
                            ['point1 x1] ['point2 x2] ['number n])
            (check-ival-valid? iy)
            (check-ival-contains? iy y)
            (with-check-info (['split 'left])
              (check-ival-equals? iy (ival-union (ival-fn i1lo i2) (ival-fn i1hi i2))))
            (with-check-info (['split 'right])
              (check-ival-equals? iy (ival-union (ival-fn i1 i2lo) (ival-fn i1 i2hi)))))))))

  ;; ##################################################### tests for endpoint-immovable
  (define num-immovable-tests (/ num-tests 2))

  (define (check-endpoints-consistant result result2)
    (when (endpoint-immovable? (ival-lo result))
      (check (disjoin equal? bf=?) (ival-lo-val result) (ival-lo-val result2)))
    (when (endpoint-immovable? (ival-hi result))
      (check (disjoin equal? bf=?) (ival-hi-val result) (ival-hi-val result2))))

  (define (test-function-overflows ival-fn fn num-of-arguments iterations)
    (parameterize ([bf-precision 80])
      (test-case (symbol->string (object-name ival-fn))
        (for ([n (in-range iterations)])
          (let find-overflow-loop ([interval-size 1])
            (define intervals
              (for/list ([n (in-range num-of-arguments)])
                (sample-interval-sized interval-size)))
            (define points
              (for/list ([i intervals])
                (sample-from i)))
            (with-check-info (['fn ival-fn] ['intervals intervals] ['points points] ['number n])
              (let ([result (apply ival-fn intervals)])
                (if (or (endpoint-immovable? (ival-lo result)) (endpoint-immovable? (ival-hi result)))
                    (parameterize ([bf-precision 16000])
                      (let ([higher-precision-result (apply ival-fn intervals)])
                        (check-endpoints-consistant result higher-precision-result)))
                    (if (> interval-size 0.005)
                        (find-overflow-loop (/ interval-size 4.0))
                        void)))))))))

  
  (for ([(ival-fn fn) (in-dict arg1)])
    (test-function-overflows ival-fn fn 1 num-immovable-tests))
  (for ([(ival-fn fn) (in-dict arg2)])
    (test-function-overflows ival-fn fn 2 num-immovable-tests))

  (define arg1-list (dict->list arg1))
  (define arg2-list (dict->list arg2))

  ;; test endpoint-immovable also works with compoisition of functions
  (for ([n (in-range 20)])
    (define func1 (list-ref arg1-list (random 0 (length arg1-list))))
    (define func2 (list-ref arg1-list (random 0 (length arg1-list))))
    (test-function-overflows (procedure-rename (compose (car func1) (car func2))
                                               (string->symbol
                                                (string-append (symbol->string (object-name (car func1)))
                                                               "-composed-with-"
                                                               (symbol->string (object-name (car func2))))))
                             (procedure-rename (compose (cdr func1) (cdr func2))
                                               (string->symbol
                                                (string-append (symbol->string (object-name (cdr func1)))
                                                               "-composed-with-"
                                                               (symbol->string (object-name (cdr func2))))))
                             1
                             (/ num-immovable-tests 40)))

  (define (compose-2-with-1 arity-2-func arity-1-func-1 arity-1-func-2)
    (lambda (a b)
      (arity-2-func (arity-1-func-1 a) (arity-1-func-2 b))))

  (for ([n (in-range 20)])
    (define func1 (list-ref arg2-list (random 0 (length arg2-list))))
    (define func2 (list-ref arg1-list (random 0 (length arg1-list))))
    (define func3 (list-ref arg1-list (random 0 (length arg1-list))))
    (test-function-overflows (procedure-rename (compose-2-with-1 (car func1) (car func2) (car func3))
                                               (string->symbol
                                                (string-append (symbol->string (object-name (car func1)))
                                                               "-composed-with-"
                                                               (symbol->string (object-name (car func2))))))
                             (procedure-rename (compose-2-with-1 (cdr func1) (cdr func2) (cdr func3))
                                               (string->symbol
                                                (string-append (symbol->string (object-name (cdr func1)))
                                                               "-composed-with-"
                                                               (symbol->string (object-name (cdr func2))))))
                             2
                             (/ num-immovable-tests 40)))
  )

