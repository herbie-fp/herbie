#lang racket/base
(require racket/match)
(require "../mpfr.rkt"
         "core.rkt")
(provide ival-add!
         ival-add
         ival-sub!
         ival-sub
         ival-mult!
         ival-mult
         ival-div!
         ival-div
         ival-fma
         ival-fdim
         ival-hypot)

;; Endpoint computation for both `add`, `sub`, and `hypot` (which has an add inside)
(define (eplinear bffn a-endpoint b-endpoint)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (define-values (val exact?) (bf-return-exact? bffn (list a b)))
  (endpoint val (or (and a! b! exact?) (and a! (bfinfinite? a)) (and b! (bfinfinite? b)))))

(define (eplinear! out mpfr-fn! a-endpoint b-endpoint rnd)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (mpfr-set-prec! out (bf-precision))
  (define exact? (= 0 (mpfr-fn! out a b rnd)))
  (endpoint out (or (and a! b! exact?) (and a! (bfinfinite? a)) (and b! (bfinfinite? b)))))

(define (ival-add! out x y)
  (ival (eplinear! (ival-lo-val out) mpfr-add! (ival-lo x) (ival-lo y) 'down)
        (eplinear! (ival-hi-val out) mpfr-add! (ival-hi x) (ival-hi y) 'up)
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (ival-add x y)
  (ival-add! (new-ival) x y))

(define (ival-sub! out x y)
  (ival (eplinear! (ival-lo-val out) mpfr-sub! (ival-lo x) (ival-hi y) 'down)
        (eplinear! (ival-hi-val out) mpfr-sub! (ival-hi x) (ival-lo y) 'up)
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (ival-sub x y)
  (ival-sub! (new-ival) x y))

(define (epmul! out rnd a-endpoint b-endpoint a-class b-class)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (define a0 (bfzero? a))
  (define b0 (bfzero? b))
  (mpfr-set-prec! out (bf-precision))
  (define exact?
    (cond
      [(or a0 b0)
       (mpfr-set! out 0.bf 'nearest)
       #t]
      [else (= 0 (mpfr-mul! out a b rnd))]))
  (endpoint out
            (or (and a! b! exact?)
                (and a! a0)
                (and a! (bfinfinite? a) (not (= b-class 0)))
                (and b! b0)
                (and b! (bfinfinite? b) (not (= a-class 0))))))

(define (ival-mult x y)
  (ival-mult! (new-ival) x y))

(define extra-mult-ival (new-ival))

(define (ival-mult! out x y)
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)

  (define (mkmult out a b c d)
    (match-define (ival (endpoint rlo _) (endpoint rhi _) _ _) out)
    (ival (epmul! rlo 'down a b x-sign y-sign)
          (epmul! rhi 'up c d x-sign y-sign)
          (or xerr? yerr?)
          (or xerr yerr)))

  (define x-sign (classify-ival x))
  (define y-sign (classify-ival y))

  (match* (x-sign y-sign)
    [(1 1) (mkmult out xlo ylo xhi yhi)]
    [(1 -1) (mkmult out xhi ylo xlo yhi)]
    [(1 0) (mkmult out xhi ylo xhi yhi)]
    [(-1 0) (mkmult out xlo yhi xlo ylo)]
    [(-1 1) (mkmult out xlo yhi xhi ylo)]
    [(-1 -1) (mkmult out xhi yhi xlo ylo)]
    [(0 1) (mkmult out xlo yhi xhi yhi)]
    [(0 -1) (mkmult out xhi ylo xlo ylo)]
    ;; Here, the two branches of the union are meaningless on their own;
    ;; however, both branches compute possible lo/hi's to min/max together
    [(0 0)
     (match-define (ival (endpoint lo lo!) (endpoint hi hi!) err? err)
       (ival-union (mkmult extra-mult-ival xhi ylo xlo ylo) (mkmult out xlo yhi xhi yhi)))
     (mpfr-set! (ival-lo-val out) lo 'down) ; should be exact
     (mpfr-set! (ival-hi-val out) hi 'up) ; should be exact
     (ival (endpoint (ival-lo-val out) lo!) (endpoint (ival-hi-val out) hi!) err? err)]))

(define (epdiv! out rnd a-endpoint b-endpoint a-class)
  (match-define (endpoint a a!) a-endpoint)
  (match-define (endpoint b b!) b-endpoint)
  (mpfr-set-prec! out (bf-precision))
  (define exact? (= 0 (mpfr-div! out a b rnd)))
  (endpoint out
            (or (and a! b! exact?)
                (and a! (bfzero? a))
                (and a! (bfinfinite? a))
                (and b! (bfinfinite? b))
                (and b! (bfzero? b) (not (= a-class 0))))))

(define (ival-div! out x y)
  (match-define (ival (endpoint rlo _) (endpoint rhi _) _ _) out)
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)
  (define err? (or xerr? yerr? (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err (or xerr yerr (and (bfzero? (ival-lo-val y)) (bfzero? (ival-hi-val y)))))
  (define x-class (classify-ival-strict x))
  (define y-class (classify-ival-strict y))

  (define (mkdiv a b c d)
    (ival (epdiv! rlo 'down a b x-class) (epdiv! rhi 'up c d x-class) err? err))

  (match* (x-class y-class)
    [(_ 0) ; In this case, y stradles 0
     (define immovable?
       ;; If either endpoint is 0 and fixed, or if both endpoints are fixed,
       ;; then class 0 will stay at any higher precision
       (or (and (endpoint-immovable? ylo) (bfzero? (endpoint-val ylo)))
           (and (endpoint-immovable? yhi) (bfzero? (endpoint-val yhi)))
           (and (endpoint-immovable? ylo) (endpoint-immovable? yhi))))
     (mpfr-set! rlo -inf.bf 'down)
     (mpfr-set! rhi +inf.bf 'up)
     (ival (endpoint rlo immovable?) (endpoint rhi immovable?) err? err)]
    [(1 1) (mkdiv xlo yhi xhi ylo)]
    [(1 -1) (mkdiv xhi yhi xlo ylo)]
    [(-1 1) (mkdiv xlo ylo xhi yhi)]
    [(-1 -1) (mkdiv xhi ylo xlo yhi)]
    [(0 1) (mkdiv xlo ylo xhi ylo)]
    [(0 -1) (mkdiv xhi yhi xlo yhi)]))

(define (ival-div x y)
  (ival-div! (new-ival) x y))

(define (ival-fma a b c)
  (ival-add (ival-mult a b) c))

(define (ival-fdim x y)
  (ival-fmax (ival-sub x y) (mk-ival 0.bf)))

(define (ival-hypot x y)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))
  (define x* (ival-exact-fabs x))
  (define y* (ival-exact-fabs y))
  (ival (eplinear! (mpfr-new! (bf-precision)) mpfr-hypot! (ival-lo x*) (ival-lo y*) 'down)
        (eplinear! (mpfr-new! (bf-precision)) mpfr-hypot! (ival-hi x*) (ival-hi y*) 'up)
        err?
        err))
