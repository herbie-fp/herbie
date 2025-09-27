#lang racket/base

(require racket/contract
         racket/match
         racket/function
         racket/list)
(require (for-syntax racket/base))
(require "../mpfr.rkt")

(provide ival-lo-val
         ival-hi-val
         classify-ival
         classify-ival-strict
         mk-big-ival
         new-ival
         ival-exact-fabs
         ival-maybe
         epfn
         split-ival
         ival-max-prec
         ival-exact-neg
         ival-exact-fabs
         bf-return-exact?
         ival-lo-fixed?
         ival-hi-fixed?
         overflows-loose-at
         exp2-overflow-threshold)

(provide (struct-out ival)
         (struct-out endpoint)
         ival-expander
         ival-union
         ival-split
         (rename-out [monotonic monotonic->ival] [comonotonic comonotonic->ival])
         ival-illegal
         ival-pi
         ival-e
         ival-bool
         ival-neg
         ival-fabs
         ival-sqrt
         ival-cbrt
         ival-exp
         ival-exp2
         ival-expm1
         ival-log
         ival-log2
         ival-log10
         ival-log1p
         ival-logb
         ival-asin
         ival-acos
         ival-atan
         ival-atan2
         ival-sinh
         ival-cosh
         ival-tanh
         ival-asinh
         ival-acosh
         ival-atanh
         ival-erf
         ival-erfc
         ival-rint
         ival-round
         ival-ceil
         ival-floor
         ival-trunc
         ival-fmin
         ival-fmax
         ival-copysign
         ival-sort
         ival-<
         ival-<=
         ival->
         ival->=
         ival-==
         ival-!=
         ival-if
         ival-and
         ival-or
         ival-not
         ival-error?
         ival-assert
         ival-then
         close-enough->ival
         ;; Deprecated
         ival-lo-fixed?
         ival-hi-fixed?
         ival-err?
         ival-err
         mk-ival)

(define-match-expander ival-expander
  (λ (stx)
    (syntax-case stx ()
      [(_me lo hi) #'(ival (endpoint lo _) (endpoint hi _) _ _)]
      [(_me name lo hi) #'(and name (ival (endpoint lo _) (endpoint hi _) _ _))]))
  (λ (stx)
    (syntax-case stx ()
      [(_ lo) #'(mk-big-ival lo lo)]
      [(_ lo hi) #'(mk-big-ival lo hi)])))

(struct endpoint (val immovable?) #:transparent)
(struct ival (lo hi err? err)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc ival port mode)
     (if (ival-err ival)
         (fprintf port "ival-illegal")
         (fprintf port "(ival ~s ~s)" (ival-lo-val ival) (ival-hi-val ival))))])

(define (ival-hi-val ival)
  (endpoint-val (ival-hi ival)))
(define (ival-lo-val ival)
  (endpoint-val (ival-lo ival)))
(define (ival-lo-fixed? ival)
  (endpoint-immovable? (ival-lo ival)))
(define (ival-hi-fixed? ival)
  (endpoint-immovable? (ival-hi ival)))

(define (new-ival)
  ; Warning, leaks memory unless `mpfr-clear!` called eventually
  (define bf1 (mpfr-new! (bf-precision)))
  (define bf2 (mpfr-new! (bf-precision)))
  (ival (endpoint bf1 #f) (endpoint bf2 #f) #f #f))

(define (mk-big-ival x y)
  (cond
    [(and (bigfloat? x) (bigfloat? y))
     (define fix? (bf=? x y))
     (define err? (or (bfnan? x) (bfnan? y) (and (bfinfinite? x) fix?)))
     (ival (endpoint x fix?) (endpoint y fix?) err? err?)]
    [(and (boolean? x) (boolean? y))
     (define fix? (equal? x y))
     (ival (endpoint x fix?) (endpoint y fix?) #f #f)]
    [else (error 'ival "Invalid interval endpoints" x y)]))

(define (mk-ival x)
  (mk-big-ival x x))

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
(define ival-false (ival-bool #f))
(define ival-uncertain (ival (endpoint #f #f) (endpoint #t #f) #f #f))
(define ival-illegal (ival (endpoint +nan.bf #t) (endpoint +nan.bf #t) #t #t))

(define (split-ival i val)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) i)
  (values (struct-copy ival i [hi (endpoint val xhi!)])
          (struct-copy ival i [lo (endpoint val xlo!)])))

(define (ival-split i val)
  (cond
    [(boolean? val)
     (if val
         (values i #f)
         (values #f i))]
    [(bflte? (ival-hi-val i) val) (values i #f)]
    [(bfgte? (ival-lo-val i) val) (values #f i)]
    [else (split-ival i val)]))

(define (classify-ival x)
  (cond
    [(not (bfnegative? (ival-lo-val x))) 1]
    [(not (bfpositive? (ival-hi-val x))) -1]
    [else 0]))

(define (classify-ival-strict x)
  (cond
    [(bfpositive? (ival-lo-val x)) 1]
    [(bfnegative? (ival-hi-val x)) -1]
    [else 0]))

(define (endpoint-min2 e1 e2)
  (match-define (endpoint x x!) e1)
  (match-define (endpoint y y!) e2)
  (define out (bfmin2 x y))
  (endpoint out (or (and (bf=? out x) x!) (and (bf=? out y) y!))))

(define (endpoint-max2 e1 e2)
  (match-define (endpoint x x!) e1)
  (match-define (endpoint y y!) e2)
  (define out (bfmax2 x y))
  (endpoint out (or (and (bf=? out x) x!) (and (bf=? out y) y!))))

(define (ival-union x y)
  (cond
    [(ival-err x) (struct-copy ival y [err? #t])]
    [(ival-err y) (struct-copy ival x [err? #t])]
    [(bigfloat? (ival-lo-val x))
     (ival (rnd 'down endpoint-min2 (ival-lo x) (ival-lo y))
           (rnd 'up endpoint-max2 (ival-hi x) (ival-hi y))
           (or (ival-err? x) (ival-err? y))
           (and (ival-err x) (ival-err y)))]
    [(boolean? (ival-lo-val x))
     (ival (epfn and-fn (ival-lo x) (ival-lo y))
           (epfn or-fn (ival-hi x) (ival-hi y))
           (or (ival-err? x) (ival-err? y))
           (and (ival-err x) (ival-err y)))]))

;; This function computes and propagates the immovable? flag for endpoints
(define (epfn op . args)
  (define args-bf (map endpoint-val args))
  (define-values (result exact?) (bf-return-exact? op args-bf))
  (endpoint result (and (andmap endpoint-immovable? args) exact?)))

(define (epunary! out mpfr-fn! ep rnd)
  (match-define (endpoint a a!) ep)
  (mpfr-set-prec! out (bf-precision))
  (define exact? (= 0 (mpfr-fn! out a rnd)))
  (endpoint out (and a! exact?)))

;; Helpers for defining interval functions

(define-syntax-rule (define* name expr)
  (define name (procedure-rename expr 'name)))

(define ((monotonic bffn) x)
  (match-define (ival lo hi err? err) x)
  (ival (rnd 'down epfn bffn lo) (rnd 'up epfn bffn hi) err? err))

(define ((comonotonic bffn) x)
  (match-define (ival lo hi err? err) x)
  (ival (rnd 'down epfn bffn hi) (rnd 'up epfn bffn lo) err? err))

(define ((close-enough->ival bffn) x)
  (match-define (ival (endpoint lo lo!) (endpoint hi hi!) err? err) x)
  (define close-enough? (bffn lo hi))
  (ival (endpoint close-enough? #f) (endpoint (or (not lo!) (not hi!) close-enough?) #f) err? err))

(define ((clamp lo hi) x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (define err? (or xerr? (bflt? xlo lo) (bfgt? xhi hi)))
  (define err (or (or xerr (bflt? xhi lo) (bfgt? xlo hi))))

  (if (and (bfzero? lo) (bfzero? xhi))
      (ival (endpoint (bf 0) xlo!) (endpoint (bf 0) xhi!) err? err)
      (ival (endpoint (if (bflt? xlo lo) lo xlo) xlo!)
            (endpoint (if (bfgt? xhi hi) hi xhi) xhi!)
            err?
            err)))

(define ((clamp-strict lo hi) x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (ival (endpoint (if (bflt? xlo lo) lo xlo) xlo!)
        (endpoint (if (bfgt? xhi hi) hi xhi) xhi!)
        (or xerr? (bflte? xlo lo) (bfgte? xhi hi))
        (or xerr (bflte? xhi lo) (bfgte? xlo hi))))

(define ((overflows-at fn lo hi) x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (match-define (ival (endpoint ylo ylo!) (endpoint yhi yhi!) yerr? yerr) (fn x))
  (ival (endpoint ylo (or ylo! (bflte? xhi lo) (and (bflte? xlo lo) xlo!)))
        (endpoint yhi (or yhi! (bfgte? xlo hi) (and (bfgte? xhi hi) xhi!)))
        xerr?
        xerr))

;; Biggest non-inf value
;;
;; + 1 . 1111111111111111111111 e MAXEXP
;;       Adding more 1s makes a bigger number
;;
;; + 1 . 0000000000000000000000 e MINEXP
;;       Adding more 0s does not change value

(define ((overflows-loose-at lo hi) x y)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (match-define (ival (endpoint ylo ylo!) (endpoint yhi yhi!) yerr? yerr) y)
  (ival (endpoint ylo (or ylo! (bflte? xhi lo) (and (bflte? xlo lo) xlo!)))
        (endpoint yhi (or yhi! (bflte? xhi lo) (bfgte? xlo hi) (and (bfgte? xhi hi) xhi!)))
        yerr?
        yerr))

(define* ival-neg (comonotonic bfneg))

;; This function fixes a bug in MPFR where mixed-precision
;; rint/round/ceil/floor/trunc operations are rounded in the input
;; precision, not the output precision, so (rnd 'down bfround xxx) can
;; return +inf.bf
(define ((fix-rounding f) x)
  (if (>= (bigfloat-exponent x) 0)
      (bfrint x)
      (f x)))

(define* ival-rint (monotonic bfrint))
(define* ival-round (monotonic (fix-rounding bfround)))
(define* ival-ceil (monotonic (fix-rounding bfceiling)))
(define* ival-floor (monotonic (fix-rounding bffloor)))
(define* ival-trunc (monotonic (fix-rounding bftruncate)))

(define (ival-fabs x)
  (match (classify-ival x)
    [-1 ((comonotonic bfabs) x)]
    [1 ((monotonic bfabs) x)]
    [0
     (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
     (ival (endpoint (bf 0) (and xlo! xhi!))
           (rnd 'up endpoint-max2 (epfn bfabs (ival-lo x)) (ival-hi x))
           (ival-err? x)
           (ival-err x))]))

;; These functions execute ival-fabs and ival-neg with input's precision
(define (ival-max-prec x)
  (max (bigfloat-precision (ival-lo-val x)) (bigfloat-precision (ival-hi-val x))))

(define (ival-exact-fabs x)
  (parameterize ([bf-precision (ival-max-prec x)])
    (ival-fabs x)))

(define (ival-exact-neg x)
  (parameterize ([bf-precision (ival-max-prec x)])
    (ival-neg x)))

;; Since MPFR has a cap on exponents, no value can be more than twice MAX_VAL
(define exp-overflow-threshold (bfadd (bflog (bfprev +inf.bf)) 1.bf))
(define exp2-overflow-threshold (bfadd (bflog2 (bfprev +inf.bf)) 1.bf))
(define sinh-overflow-threshold (bfadd (bfasinh (bfprev +inf.bf)) 1.bf))
(define acosh-overflow-threshold (bfadd (bfacosh (bfprev +inf.bf)) 1.bf))

(define (ival-exp x)
  (define y ((monotonic bfexp) x))
  ((overflows-loose-at (bfneg exp-overflow-threshold) exp-overflow-threshold) x y))

(define (ival-expm1-mpfr x)
  (match-define (ival lo hi err? err) x)
  (define prec (bf-precision))
  (define lo-val (mpfr-new! prec))
  (define hi-val (mpfr-new! prec))
  (ival (epunary! lo-val mpfr-expm1! lo 'down) (epunary! hi-val mpfr-expm1! hi 'up) err? err))

(define* ival-expm1
         (overflows-at ival-expm1-mpfr (bfneg exp-overflow-threshold) exp-overflow-threshold))
(define (ival-exp2 x)
  (define y ((monotonic bfexp2) x))
  ((overflows-loose-at (bfneg exp2-overflow-threshold) exp2-overflow-threshold) x y))

(define* ival-log (compose (monotonic bflog) (clamp-strict 0.bf +inf.bf)))
(define* ival-log2 (compose (monotonic bflog2) (clamp-strict 0.bf +inf.bf)))
(define* ival-log10 (compose (monotonic bflog10) (clamp-strict 0.bf +inf.bf)))
(define* ival-log1p (compose (monotonic bflog1p) (clamp-strict -1.bf +inf.bf)))
[define* ival-logb (compose ival-floor ival-log2 ival-exact-fabs)]

(define* ival-sqrt (compose (monotonic bfsqrt) (clamp 0.bf +inf.bf)))
(define* ival-cbrt (monotonic bfcbrt))

(define (ival-and . as)
  (ival (endpoint (andmap ival-lo-val as) (andmap (compose endpoint-immovable? ival-lo) as))
        (endpoint (andmap ival-hi-val as) (andmap (compose endpoint-immovable? ival-hi) as))
        (ormap ival-err? as)
        (ormap ival-err as)))

(define (ival-or . as)
  (ival (endpoint (ormap ival-lo-val as) (andmap (compose endpoint-immovable? ival-lo) as))
        (endpoint (ormap ival-hi-val as) (andmap (compose endpoint-immovable? ival-hi) as))
        (ormap ival-err? as)
        (ormap ival-err as)))

(define (ival-not x)
  (ival (epfn not (ival-hi x)) (epfn not (ival-lo x)) (ival-err? x) (ival-err x)))

(define* ival-asin (compose (monotonic bfasin) (clamp -1.bf 1.bf)))
(define* ival-acos (compose (comonotonic bfacos) (clamp -1.bf 1.bf)))
(define* ival-atan (monotonic bfatan))

(define (ival-atan2 y x)
  (match-define (ival xlo xhi xerr? xerr) x)
  (match-define (ival ylo yhi yerr? yerr) y)

  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))

  (define (mkatan a b c d)
    (ival (rnd 'down epfn bfatan2 a b) (rnd 'up epfn bfatan2 c d) err? err))

  (match* ((classify-ival-strict x) (classify-ival-strict y))
    [(-1 -1) (mkatan yhi xlo ylo xhi)]
    [(0 -1) (mkatan yhi xlo yhi xhi)]
    [(1 -1) (mkatan ylo xlo yhi xhi)]
    [(1 0) (mkatan ylo xlo yhi xlo)]
    [(1 1) (mkatan ylo xhi yhi xlo)]
    [(0 1) (mkatan ylo xhi ylo xlo)]
    [(-1 1) (mkatan yhi xhi ylo xlo)]
    [(_ 0)
     (ival (endpoint (bfneg (rnd 'up pi.bf)) #f)
           (endpoint (rnd 'up pi.bf) #f)
           (or err? (bfgte? (ival-hi-val x) 0.bf))
           (or err
               (and (bfzero? (ival-lo-val x))
                    (bfzero? (ival-hi-val x))
                    (bfzero? (ival-lo-val y))
                    (bfzero? (ival-hi-val y)))))]))

(define*
 ival-cosh
 (compose (overflows-at (monotonic bfcosh) (bfneg acosh-overflow-threshold) acosh-overflow-threshold)
          ival-exact-fabs))
(define* ival-sinh
         (overflows-at (monotonic bfsinh) (bfneg sinh-overflow-threshold) sinh-overflow-threshold))
(define* ival-tanh (monotonic bftanh))
(define* ival-asinh (monotonic bfasinh))
(define* ival-acosh (compose (monotonic bfacosh) (clamp 1.bf +inf.bf)))
(define* ival-atanh (compose (monotonic bfatanh) (clamp-strict -1.bf 1.bf)))

(define* ival-erf (monotonic bferf))
(define* ival-erfc (comonotonic bferfc))

(define (ival-cmp x y)
  (define can-< (epfn bflt? (ival-lo x) (ival-hi y)))
  (define must-< (epfn bflt? (ival-hi x) (ival-lo y)))
  (define can-> (epfn bfgt? (ival-hi x) (ival-lo y)))
  (define must-> (epfn bfgt? (ival-lo x) (ival-hi y)))
  (values can-< must-< can-> must->))

(define (ival-<2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival m< c< (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-<=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (epfn not c>) (epfn not m>) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival->2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival m> c> (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival->=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (epfn not c<) (epfn not m<) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

(define (ival-==2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (epfn and-fn (epfn not c<) (epfn not c>))
        (epfn and-fn (epfn not m<) (epfn not m>))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define ((ival-comparator f name) . as)
  (let loop ([as as])
    (match as
      ['() ival-true]
      [(list x) ival-true]
      [(list x y) (f x y)]
      [(cons x (and (cons y _) tail)) (ival-and (f x y) (loop tail))])))

(define* ival-< (ival-comparator ival-<2 'ival-<))
(define* ival-<= (ival-comparator ival-<=2 'ival-<=))
(define* ival-> (ival-comparator ival->2 'ival->))
(define* ival->= (ival-comparator ival->=2 'ival->=))
(define* ival-== (ival-comparator ival-==2 'ival-==))

(define (ival-!=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (epfn or-fn m< m>)
        (epfn or-fn c< c>)
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (ival-!= . as)
  (if (null? as)
      ival-true
      (let loop ([head (car as)]
                 [tail (cdr as)])
        (if (null? tail)
            ival-true
            (ival-and (foldl ival-and ival-true (map (curry ival-!=2 head) tail))
                      (loop (car tail) (cdr tail)))))))

(define (ival-error? x)
  (ival (endpoint (ival-err x) #f) (endpoint (ival-err? x) #f) #f #f))

(define (ival-assert c [msg #t])
  (ival (endpoint #t #t)
        (endpoint #t #t)
        (or (ival-err? c) (if (ival-lo-val c) #f msg))
        (or (ival-err c) (if (ival-hi-val c) #f msg))))

(define ival-maybe (ival (endpoint #f #t) (endpoint #t #t) #f #f))

(define (ival-then a . as)
  (match-define (ival alo ahi aerr? aerr) a)
  (for/fold ([lo alo]
             [hi ahi]
             [err? aerr?]
             [err aerr]
             #:result (ival lo hi err? err))
            ([a (in-list as)])
    (match-define (ival alo ahi aerr? aerr) a)
    (values alo ahi (or err? aerr?) (or err aerr))))

(define* ival-identity (monotonic bfcopy))

(define (ival-if c x y)
  (cond
    [(ival-lo-val c) (ival-then c (ival-identity x))]
    [(not (ival-hi-val c)) (ival-then c (ival-identity y))]
    [else
     (define out (ival-then c (ival-union x y)))
     ; If condition is movable output should be too
     (if (not (and (ival-lo-fixed? c) (ival-hi-fixed? c)))
         (ival-mobilize out)
         out)]))

(define (ival-mobilize x)
  (match-define (ival (endpoint lo lo!) (endpoint hi hi!) err? err) x)
  (ival (endpoint lo #f) (endpoint hi #f) err? err))

(define (ival-fmin x y)
  (ival (rnd 'down endpoint-min2 (ival-lo x) (ival-lo y))
        (rnd 'up endpoint-min2 (ival-hi x) (ival-hi y))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (ival-fmax x y)
  (ival (rnd 'down endpoint-max2 (ival-lo x) (ival-lo y))
        (rnd 'up endpoint-max2 (ival-hi x) (ival-hi y))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (ival-copysign x y)
  (match-define (ival xlo xhi xerr? xerr) (ival-fabs x))
  (define can-zero (or (bfzero? (ival-lo-val y)) (bfzero? (ival-hi-val y))))
  ;; 0 is both positive and negative because we don't handle signed zero well
  (define can-neg (or (= (mpfr-sign (ival-lo-val y)) -1) can-zero))
  (define can-pos (or (= (mpfr-sign (ival-hi-val y)) 1) can-zero))
  (define err? (or (ival-err? y) xerr?))
  (define err (or (ival-err y) xerr))
  (match* (can-neg can-pos)
    [(#t #t) (ival (rnd 'down epfn bfneg xhi) (rnd 'up epfn bfcopy xhi) err? err)]
    [(#t #f) (ival (rnd 'down epfn bfneg xhi) (rnd 'up epfn bfneg xlo) err? err)]
    [(#f #t) (ival xlo xhi err? err)]
    [(#f #f)
     (unless (ival-err y)
       (error 'ival-copysign "Strange interval ~a" y))
     ival-illegal]))

(define (ival-sort ivs cmp)
  (define upper (sort (map ival-hi-val ivs) cmp))
  (define lower (sort (map ival-lo-val ivs) cmp))
  (define err? (ormap (lambda (iv) (ival-err? iv)) ivs))
  (define err (ormap (lambda (iv) (ival-err iv)) ivs))
  (define hi! (andmap (lambda (iv) (ival-hi-fixed? iv)) ivs))
  (define lo! (andmap (lambda (iv) (ival-lo-fixed? iv)) ivs))
  (for/list ([u upper]
             [l lower])
    (ival (endpoint l lo!) (endpoint u hi!) err? err)))
