#lang racket

(require math/private/bigfloat/mpfr)
(require "syntax/types.rkt")

(struct endpoint (val immovable?) #:transparent)
(struct ival (lo hi err? err) #:transparent)
(define (ival-hi-val ival)
  (endpoint-val (ival-hi ival)))
(define (ival-lo-val ival)
  (endpoint-val (ival-lo ival)))


(provide (contract-out
          [struct endpoint ([val bigvalue?] [immovable? boolean?])]
          [struct ival ([lo endpoint?] [hi endpoint?] [err? boolean?] [err boolean?])]
          [mk-ival (-> bigvalue? ival?)]
          [ival-hi-val (-> ival? bigvalue?)]
          [ival-lo-val (-> ival? bigvalue?)]
          [ival-pi (-> ival?)]
          [ival-e  (-> ival?)]
          [ival-bool (-> boolean? ival?)]
          [ival-add (-> ival? ival? ival?)]
          [ival-sub (-> ival? ival? ival?)]
          [ival-neg (-> ival? ival?)]
          [ival-mult (-> ival? ival? ival?)]
          [ival-div (-> ival? ival? ival?)]
          [ival-fma (-> ival? ival? ival? ival?)]
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
          [ival-if (-> ival? ival? ival? ival?)]))

(define (mk-ival x)
  (match x
    [(? bigfloat?)
     (ival (endpoint x #t) (endpoint x #t) #f #f)]
    [(? boolean?)
     (ival (endpoint x #t) (endpoint x #t) #f #f)]
    [_
     (error "Invalid exact value for interval arithmetic" x)]))

(define (equal-or-bf=? a b)
  (if
   (and (bigfloat? a) (bigfloat? b))
   (or (equal? a b) (bf=? a b))
   (equal? a b)))

(define -inf.bf (bf -inf.0))
(define -1.bf (bf -1))
(define 0.bf (bf 0))
(define half.bf (bf 0.5))
(define 1.bf (bf 1))
(define 2.bf (bf 2))
(define +inf.bf (bf +inf.0))
(define +nan.bf (bf +nan.0))

;; movable because the check that led to their use might change
(define -inf.endpoint (endpoint -inf.bf #f))
(define -1.endpoint (endpoint -1.bf #f))
(define 0.endpoint (endpoint 0.bf #f))
(define half.endpoint (endpoint half.bf #f))
(define 1.endpoint (endpoint 1.bf #f))
(define 2.endpoint (endpoint 2.bf #f))
(define +inf.endpoint (endpoint +inf.bf #f))
(define +nan.endpoint (endpoint +nan.bf #f))

(define (and-2 a b)
  (and a b))
(define (or-2 a b)
  (or a b))

(define (ival-pi)
  (ival (rnd-endpoint 'down pi.bf) (rnd-endpoint 'up pi.bf) #f #f))

(define (ival-e)
  (ival (rnd-endpoint 'down bfexp 1.endpoint) (rnd-endpoint 'up bfexp 1.endpoint) #f #f))

(define (ival-bool b)
  (ival (endpoint b #t) (endpoint b #t) #f #f))

(define ival-true (ival-bool #t))


(define-syntax-rule (rnd mode op args ...)
  (parameterize ([bf-rounding-mode mode])
    (op args ...)))

(define-syntax-rule (rnd-endpoint mode op args ...)
  (parameterize ([bf-rounding-mode mode])
    (e-compute op args ...)))


(define (e-compute-no-check op . args)
  (let ([result (apply op (map (lambda (x)
                                 (endpoint-val x)) args))])
    (endpoint result #f)))


;; This function computes and propagates the immovable? flag for endpoints
;; Only do this when precision is low, since it has to compute twice
(define (e-compute op . args)
  (cond
    [(> (bf-precision) 80)
     (apply e-compute-no-check (cons op args))]
    [else
     (define args-bf (map (lambda (x)
                            (endpoint-val x)) args))
     (define result (apply op args-bf))
     (define other-result
       (parameterize ([bf-rounding-mode
                       (if (equal? (bf-rounding-mode) 'up) 'down 'up)])
         (apply op args-bf)))
     (endpoint result
               ;; if one of the values is special, the result is always immovable
               (and (andmap endpoint-immovable? args)
                    ;; make sure not affected by rounding
                    (equal-or-bf=? result other-result)))]))





(define (ival-neg x)
  ;; No rounding, negation is exact
  (ival
   (e-compute bfneg (ival-hi x))
   (e-compute bfneg (ival-lo x))
   (ival-err? x) (ival-err x)))

(define (ival-add x y)
  (ival
   (rnd-endpoint 'down bfadd (ival-lo x) (ival-lo y))
   (rnd-endpoint 'up bfadd (ival-hi x) (ival-hi y))
   (or (ival-err? x) (ival-err? y))
   (or (ival-err x) (ival-err y))))

(define (ival-sub x y)
  (ival (rnd-endpoint 'down bfsub (ival-lo x) (ival-hi y))
        (rnd-endpoint 'up bfsub (ival-hi x) (ival-lo y))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (bfmin* a . as)
  (if (null? as) a (apply bfmin* (bfmin2 a (car as)) (cdr as))))

(define (endpoint-min2 e1 e2)
  (if (bfgt? (endpoint-val e1) (endpoint-val e2))
      e2
      e1))

(define (endpoint-min* a . as)
  (if (null? as)
      a
      (apply endpoint-min*
             (endpoint-min2 a (car as))
             (cdr as))))

(define (endpoint-max2 e1 e2)
  (if (bfgt? (endpoint-val e1) (endpoint-val e2))
      e1
      e2))

(define (endpoint-max* a . as)
  (if (null? as)
      a
      (apply endpoint-max*
             (endpoint-max2 a (car as))
             (cdr as))))

(define (bfmax* a . as)
  (if (null? as) a (apply bfmax* (bfmax2 a (car as)) (cdr as))))

(define (ival-mult x y)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))

  (match* ((classify-ival x) (classify-ival y))
    [(1 1)
     (ival (rnd-endpoint 'down bfmul (ival-lo x) (ival-lo y))
           (rnd-endpoint 'up bfmul (ival-hi x) (ival-hi y)) err? err)]
    [(1 -1)
     (ival (rnd-endpoint 'down bfmul (ival-hi x) (ival-lo y))
           (rnd-endpoint 'up bfmul (ival-lo x) (ival-hi y)) err? err)]
    [(1 0)
     (ival (rnd-endpoint 'down bfmul (ival-hi x) (ival-lo y))
           (rnd-endpoint 'up bfmul (ival-hi x) (ival-hi y)) err? err)]
    [(-1 0)
     (ival (rnd-endpoint 'down bfmul (ival-lo x) (ival-hi y))
           (rnd-endpoint 'up bfmul (ival-lo x) (ival-lo y)) err? err)]
    [(-1 1)
     (ival (rnd-endpoint 'down bfmul (ival-lo x) (ival-hi y))
           (rnd-endpoint 'up bfmul (ival-hi x) (ival-lo y)) err? err)]
    [(-1 -1)
     (ival (rnd-endpoint 'down bfmul (ival-hi x) (ival-hi y))
           (rnd-endpoint 'up bfmul (ival-lo x) (ival-lo y)) err? err)]
    [(0 1)
     (ival (rnd-endpoint 'down bfmul (ival-lo x) (ival-hi y))
           (rnd-endpoint 'up bfmul (ival-hi x) (ival-hi y)) err? err)]
    [(0 -1)
     (ival (rnd-endpoint 'down bfmul (ival-hi x) (ival-lo y))
           (rnd-endpoint 'up bfmul (ival-lo x) (ival-lo y)) err? err)]
    [(0 0) ; The "else" case is always correct, but is slow
     ;; We round only down, and approximate rounding up with bfnext below
     (define opts
      (rnd 'down list
           (e-compute bfmul (ival-lo x) (ival-lo y))
           (e-compute bfmul (ival-hi x) (ival-lo y))
           (e-compute bfmul (ival-lo x) (ival-hi y))
           (e-compute bfmul (ival-hi x) (ival-hi y))))
    (ival (apply endpoint-min* opts) (e-compute bfnext (apply endpoint-max* opts)) err? err)]))

(define (ival-div x y)
  (define err? (or (ival-err? x) (ival-err? y) (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err (or (ival-err x) (ival-err y) (and (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))
  
    ;; We round only down, and approximate rounding up with bfnext below
  (match* ((classify-ival x) (classify-ival y))
    [(_ 0)
     (ival -inf.endpoint +inf.endpoint err? err)]
    [(1 1)
     (ival (rnd-endpoint 'down bfdiv (ival-lo x) (ival-hi y))
           (rnd-endpoint 'up bfdiv (ival-hi x) (ival-lo y)) err? err)]
    [(1 -1)
     (ival (rnd-endpoint 'down bfdiv (ival-hi x) (ival-hi y))
           (rnd-endpoint 'up bfdiv (ival-lo x) (ival-lo y)) err? err)]
    [(-1 1)
     (ival (rnd-endpoint 'down bfdiv (ival-lo x) (ival-lo y))
           (rnd-endpoint 'up bfdiv (ival-hi x) (ival-hi y)) err? err)]
    [(-1 -1)
     (ival (rnd-endpoint 'down bfdiv (ival-hi x) (ival-lo y))
           (rnd-endpoint 'up bfdiv (ival-lo x) (ival-hi y)) err? err)]
    [(0 1)
     (ival (rnd-endpoint 'down bfdiv (ival-lo x) (ival-lo y))
           (rnd-endpoint 'up bfdiv (ival-hi x) (ival-lo y)) err? err)]
    [(0 -1)
     (ival (rnd-endpoint 'down bfdiv (ival-hi x) (ival-hi y))
           (rnd-endpoint 'up bfdiv (ival-lo x) (ival-hi y)) err? err)]
    [(_ _)
     (define opts
       (rnd 'down list
            (e-compute bfdiv (ival-lo x) (ival-lo y))
            (e-compute bfdiv (ival-hi x) (ival-lo y))
            (e-compute bfdiv (ival-lo x) (ival-hi y))
            (e-compute bfdiv (ival-hi x) (ival-hi y))))
     (ival (apply endpoint-min* opts)
           (e-compute bfnext (apply endpoint-max* opts)) err? err)]))

(define-syntax-rule (define-monotonic name bffn)
  (define (name x)
    (let ([low (rnd-endpoint 'down bffn (ival-lo x))]
          [high (rnd-endpoint 'up bffn (ival-hi x))])
      (ival (endpoint (endpoint-val low)
                      (or (endpoint-immovable? low) (bf=? (endpoint-val low) -inf.bf)))
            (endpoint (endpoint-val high)
                      (or (endpoint-immovable? high) (bf=? (endpoint-val high) +inf.bf)))
            (ival-err? x) (ival-err x)))))

(define-monotonic ival-exp bfexp)

(define-monotonic ival-exp2 bfexp2)

(define-monotonic ival-expm1 bfexpm1)

(define-syntax-rule (define-monotonic-positive name bffn)
  (define (name x)
    (define err (or (ival-err x) (bflte? (ival-hi-val x) 0.bf)))
    (define err? (or err (ival-err? x) (bflte? (ival-lo-val x) 0.bf)))
    (ival (rnd-endpoint 'down bffn (ival-lo x)) (rnd-endpoint 'up bffn (ival-hi x)) err? err)))

(define-monotonic-positive ival-log bflog)
(define-monotonic-positive ival-log2 bflog2)
(define-monotonic-positive ival-log10 bflog10)

(define (ival-log1p x)
  (define err (or (ival-err x) (bflte? (ival-hi-val x) -1.bf)))
  (define err? (or err (ival-err? x) (bflte? (ival-lo-val x) -1.bf)))
  (ival (rnd-endpoint 'down bflog1p (ival-lo x)) (rnd-endpoint 'up bflog1p (ival-hi x))
        err? err))

(define (ival-sqrt x)
  (define err (or (ival-err x) (bflt? (ival-hi-val x) 0.bf)))
  (define err? (or err (ival-err? x) (bflt? (ival-lo-val x) 0.bf)))
  (ival (rnd-endpoint 'down bfsqrt (ival-lo x)) (rnd-endpoint 'up bfsqrt (ival-hi x))
        err? err))

(define (ival-cbrt x)
  (ival (rnd-endpoint 'down bfcbrt (ival-lo x)) (rnd-endpoint 'up bfcbrt (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-hypot x y)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))
  (define x* (ival-fabs x))
  (define y* (ival-fabs y))
  (ival (rnd-endpoint 'down bfhypot (ival-lo x*) (ival-lo y*))
        (rnd-endpoint 'up bfhypot (ival-hi x*) (ival-hi y*))
        err? err))

(define (ival-pow x y)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))
  (cond
    [(bfgte? (ival-lo-val x) 0.bf)
     (let ([lo
            (if (bflt? (ival-lo-val x) 1.bf)
                (rnd-endpoint 'down bfexpt (ival-lo x) (ival-hi y))
                (rnd-endpoint 'down bfexpt (ival-lo x) (ival-lo y)))]
           [hi
            (if (bfgt? (ival-hi-val x) 1.bf)
                (rnd-endpoint 'up bfexpt (ival-hi x) (ival-hi y))
                (rnd-endpoint 'up bfexpt (ival-hi x) (ival-lo y)))])
       (ival lo hi err? err))]
    [(and (bf=? (ival-lo-val y) (ival-hi-val y)) (bfinteger? (ival-lo-val y)))
     (ival (rnd-endpoint 'down bfexpt (ival-lo x) (ival-lo y))
           (rnd-endpoint 'up bfexpt (ival-lo x) (ival-lo y))
           err? err)]
    [else
     ;; In this case, the base range includes negatives and the exp range includes reals
     ;; Focus first on just the negatives in the base range
     ;; All the reals in the exp range just make NaN a possible output
     ;; If there are no integers in the exp range, those NaNs are the only output
     ;; If there are, the min of the negative base values is from the biggest odd integer in the range
     ;;  and the max is from the biggest even integer in the range
     (define a (e-compute bfceiling (ival-lo y)))
     (define b (e-compute bffloor (ival-hi y)))
     (define lo (ival-lo x))
     (define neg-range
       (cond
         [(bflt? (endpoint-val b) (endpoint-val a))
          (ival +nan.endpoint +nan.endpoint #t #t)]
         [(bf=? (endpoint-val a) (endpoint-val b))
          (ival (rnd-endpoint 'down bfexpt (ival-lo x) a) (rnd-endpoint 'up bfexpt (ival-hi x) a) err? err)]
         [(bfodd? (endpoint-val b))
          (ival (rnd-endpoint 'down bfexpt (ival-lo x) b)
                (rnd-endpoint 'up bfmax2
                              (e-compute bfexpt (ival-hi x) (e-compute bfsub b 1.endpoint))
                              (e-compute bfexpt (ival-lo x) (e-compute bfsub b 1.endpoint))) err? err)]
         [(bfeven? (endpoint-val b))
          (ival (rnd-endpoint 'down bfexpt (ival-lo x) (e-compute  bfsub b 1.endpoint))
                (rnd-endpoint 'up bfmax2 (e-compute bfexpt (ival-hi x) b) (e-compute bfexpt (ival-lo x) b)) err? err)]
         [else (ival +nan.endpoint +nan.endpoint #t #t)]))
     (if (bfgt? (ival-hi-val x) 0.bf)
         (ival-union neg-range (ival-pow (ival 0.endpoint (ival-hi x) err? err) y))
         neg-range)]))

(define (ival-fma a b c)
  (ival-add (ival-mult a b) c))

(define (ival-and . as)
  (ival (endpoint (andmap ival-lo-val as) (andmap (lambda (x) (endpoint-immovable? (ival-lo x))) as))
        (endpoint (andmap ival-hi-val as) (andmap (lambda (x) (endpoint-immovable? (ival-hi x))) as))
        (ormap ival-err? as) (ormap ival-err as)))

(define (ival-or . as)
  (ival (endpoint (ormap ival-lo-val as) (andmap (lambda (x) (endpoint-immovable? (ival-lo x))) as))
        (endpoint (ormap ival-hi-val as) (andmap (lambda (x) (endpoint-immovable? (ival-hi x))) as))
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
    (ival (rnd-endpoint 'down bfcos (ival-hi x)) (rnd-endpoint 'up bfcos (ival-lo x)) (ival-err? x) (ival-err x))]
   [(and (bf=? a b) (bfodd? a))
    (ival (rnd-endpoint 'down bfcos (ival-lo x)) (rnd-endpoint 'up bfcos (ival-hi x)) (ival-err? x) (ival-err x))]
   [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
    (ival -1.endpoint
          (rnd-endpoint 'up bfmax2 (e-compute bfcos (ival-lo x))
                        (e-compute bfcos (ival-hi x)))
          (ival-err? x) (ival-err x))]
   [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
    (ival (rnd-endpoint 'down bfmin2
                        (e-compute bfcos (ival-lo x))
                        (e-compute bfcos (ival-hi x)))
          1.endpoint (ival-err? x) (ival-err x))]
   [else
    (ival -1.endpoint 1.endpoint (ival-err? x) (ival-err x))]))

(define (ival-sin x)
  (define lopi (rnd 'down pi.bf))
  (define hipi (rnd 'up pi.bf))
  (define a (rnd 'down bffloor (bfsub (bfdiv (ival-lo-val x) (if (bflt? (ival-lo-val x) 0.bf) lopi hipi)) half.bf))) ; half.bf is exact
  (define b (rnd 'up bffloor (bfsub (bfdiv (ival-hi-val x) (if (bflt? (ival-hi-val x) 0.bf) hipi lopi)) half.bf)))
  (cond
    [(and (bf=? a b) (bfeven? a))
     (ival (rnd-endpoint 'down bfsin (ival-hi x)) (rnd-endpoint 'up bfsin (ival-lo x)) (ival-err? x) (ival-err x))]
    [(and (bf=? a b) (bfodd? a))
     (ival (rnd-endpoint 'down bfsin (ival-lo x)) (rnd-endpoint 'up bfsin (ival-hi x)) (ival-err? x) (ival-err x))]
    [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
     (ival -1.endpoint (rnd-endpoint 'up bfmax2 (e-compute bfsin (ival-lo x)) (e-compute bfsin (ival-hi x))) (ival-err? x) (ival-err x))]
    [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
     (ival (rnd-endpoint 'down bfmin2 (e-compute bfsin (ival-lo x)) (e-compute bfsin (ival-hi x))) 1.endpoint (ival-err? x) (ival-err x))]
    [else
     (ival -1.endpoint 1.endpoint (ival-err? x) (ival-err x))]))

(define (ival-tan x)
  (ival-div (ival-sin x) (ival-cos x)))

(define (ival-atan x)
  (ival (rnd-endpoint 'down bfatan (ival-lo x)) (rnd-endpoint 'up bfatan (ival-hi x)) (ival-err? x) (ival-err x)))

(define (classify-ival x)
  (cond [(bfgte? (ival-lo-val x) 0.bf) 1] [(bflte? (ival-hi-val x) 0.bf) -1] [else 0]))

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
      (ival (rnd-endpoint 'down bfatan2 (first a-lo) (second a-lo))
            (rnd-endpoint 'up bfatan2 (first a-hi) (second a-hi)) err? err)
      (ival (endpoint (rnd 'down bfneg (pi.bf)) #f) (endpoint (rnd 'up pi.bf) #f)
            (or err? (bfgte? (ival-hi-val x) 0.bf))
            (or err (and (bf=? (ival-lo-val x) 0.bf) (bf=? (ival-hi-val x) 0.bf) (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))))

(define (ival-asin x)
  (ival (rnd-endpoint 'down bfasin (ival-lo x)) (rnd-endpoint 'up bfasin (ival-hi x))
        (or (ival-err? x) (bflt? (ival-lo-val x) -1.bf) (bfgt? (ival-hi-val x) 1.bf))
        (or (ival-err x) (bflt? (ival-hi-val x) -1.bf) (bfgt? (ival-lo-val x) 1.bf))))

(define (ival-acos x)
  (ival (rnd-endpoint 'down bfacos (ival-hi x)) (rnd-endpoint 'up bfacos (ival-lo x))
        (or (ival-err? x) (bflt? (ival-lo-val x) -1.bf) (bfgt? (ival-hi-val x) 1.bf))
        (or (ival-err x) (bflt? (ival-hi-val x) -1.bf) (bfgt? (ival-lo-val x) 1.bf))))

(define (ival-fabs x)
  (cond
   [(bfgt? (ival-lo-val x) 0.bf) x]
   [(bflt? (ival-hi-val x) 0.bf)
    (ival (e-compute bfneg (ival-hi x)) (e-compute bfneg (ival-lo x)) (ival-err? x) (ival-err x))]
   [else ; interval stradles 0
    (define top
      (if (bfgt? (bfneg (ival-lo-val x)) (ival-hi-val x))
          (e-compute bfneg (ival-lo x))
          (ival-hi x)))
    (ival 0.endpoint top
          (ival-err? x) (ival-err x))]))

(define (ival-sinh x)
  (ival (rnd-endpoint 'down bfsinh (ival-lo x)) (rnd-endpoint 'up bfsinh (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-cosh x)
  (define y (ival-fabs x))
  (ival (rnd-endpoint 'down bfcosh (ival-lo y)) (rnd-endpoint 'up bfcosh (ival-hi y)) (ival-err? y) (ival-err y)))

(define (ival-tanh x)
  (ival (rnd-endpoint 'down bftanh (ival-lo x)) (rnd-endpoint 'up bftanh (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-asinh x)
  (ival (rnd-endpoint 'down bfasinh (ival-lo x)) (rnd-endpoint 'up bfasinh (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-acosh x)
  (ival (rnd-endpoint 'down bfacosh (e-compute bfmax2 (ival-lo x) 1.endpoint)) (rnd-endpoint 'up bfacosh (ival-hi x))
        (or (bflte? (ival-lo-val x) 1.bf) (ival-err? x)) (or (bflt? (ival-hi-val x) 1.bf) (ival-err x))
))

(define (ival-atanh x)
  (ival (rnd-endpoint 'down bfatanh (ival-lo x)) (rnd-endpoint 'up bfatanh (ival-hi x)) (ival-err? x) (ival-err x)
))

(define (ival-fmod x y)
  (define y* (ival-fabs y))
  (define quot (ival-div x y*))
  (define a (rnd-endpoint 'down bftruncate (ival-lo quot)))
  (define b (rnd-endpoint 'up bftruncate (ival-hi quot)))
  (define err? (or (ival-err? x) (ival-err? y) (bf=? (ival-lo-val y*) 0.bf)))
  (define err (or (ival-err x) (ival-err y) (bf=? (ival-hi-val y*) 0.bf)))
  (define tquot (ival a b err? err))

  (cond
   [(bf=? (endpoint-val a) (endpoint-val b)) (ival-sub x (ival-mult tquot y*))]
   [(bflte? (endpoint-val b) 0.bf) (ival (e-compute bfneg (ival-hi y*)) 0.endpoint err? err)]
   [(bfgte? (endpoint-val a) 0.bf) (ival 0.endpoint (ival-hi y*) err? err)]
   [else (ival (e-compute bfneg (ival-hi y*)) (ival-hi y*) err? err)]))

(define (ival-remainder x y)
  (define y* (ival-fabs y))
  (define quot (ival-div x y*))
  (define a (rnd-endpoint 'down bfround (ival-lo quot)))
  (define b (rnd-endpoint 'up bfround (ival-hi quot)))
  (define err? (or (ival-err? x) (ival-err? y) (bf=? (ival-lo-val y*) 0.bf)))
  (define err (or (ival-err x) (ival-err y) (bf=? (ival-hi-val y*) 0.bf)))

  (if (bf=? (endpoint-val a) (endpoint-val b))
      (ival-sub x (ival-mult (ival a b err? err) y*))
      (ival (endpoint (bfneg (bfdiv (ival-hi-val y*) 2.bf)) #f)
            (endpoint (bfdiv (ival-hi-val y*) 2.bf) #f)
            err? err)))

(define-monotonic ival-rint bfrint)
(define-monotonic ival-round bfround)
(define-monotonic ival-ceil bfceiling)
(define-monotonic ival-floor bffloor)
(define-monotonic ival-trunc bftruncate)

(define (ival-erf x)
  (ival (rnd-endpoint 'down bferf (ival-lo x)) (rnd-endpoint 'up bferf (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-erfc x)
  (ival (rnd-endpoint 'down bferfc (ival-hi x)) (rnd-endpoint 'up bferfc (ival-lo x)) (ival-err? x) (ival-err x)))

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
  (ival (and-2 (e-compute not c<) (e-compute not c>))
        (or-2 (e-compute not m<) (e-compute not m>))
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
  (ival (e-compute or-2 c< c>) (e-compute or-2 m< m>) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

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
    (ival (e-compute bfmin2 (ival-lo x) (ival-lo y)) (e-compute bfmax2 (ival-hi x) (ival-hi y))
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


(module+ test
  (require rackunit math/flonum)
  (require "common.rkt")

  (define num-tests 1000)

  (define (sample-interval)
    (if (= (random 0 2) 0)
        (let ([v1 (sample-double)] [v2 (sample-double)])
          (ival (endpoint (bf (min v1 v2)) #t) (endpoint (bf (max v1 v2)) #t) (or (nan? v1) (nan? v2)) (and (nan? v1) (nan? v2))))
        (let* ([v1 (bf (sample-double))] [exp (random 0 31)] [mantissa (random 0 (expt 2 exp))] [sign (- (* 2 (random 0 2)) 1)])
          (define v2 (bfstep v1 (* sign (+ exp mantissa))))
          (if (= sign -1)
              (ival (endpoint v2 #t) (endpoint v1 #t) (or (bfnan? v1) (bfnan? v2)) (and (bfnan? v1) (bfnan? v2)))
              (ival (endpoint v1 #t) (endpoint v2 #t) (or (bfnan? v1) (bfnan? v2)) (and (bfnan? v1) (bfnan? v2)))))))

  ;; limit how big interval can be
  (define (sample-interval-sized [size-limit 1])
    (let* ([v1 (bf (sample-double))] [exp (random 0 31)] [mantissa (random 0 (expt 2 exp))] [sign (- (* 2 (random 0 2)) 1)])
          (define v2 (bfstep v1 (exact-floor (* (* sign (+ exp mantissa)) size-limit))))
          (if (= sign -1)
              (ival (endpoint v2 #t) (endpoint v1 #t) (or (bfnan? v1) (bfnan? v2)) (and (bfnan? v1) (bfnan? v2)))
              (ival (endpoint v1 #t) (endpoint v2 #t) (or (bfnan? v1) (bfnan? v2)) (and (bfnan? v1) (bfnan? v2))))))

  (define (sample-bf)
    (bf (sample-double)))

  (define (sample-from ival)
    (if (bigfloat? (ival-lo-val ival))
        (let ([p (random)])
          (bfadd (bfmul (bf p) (ival-lo-val ival)) (bfmul (bfsub 1.bf (bf p)) (ival-hi-val ival))))
        (let ([p (random 0 2)])
          (if (= p 0) (ival-lo-val ival) (ival-hi-val ival)))))

  (define (ival-valid? ival)
    (or (boolean? (ival-lo-val ival))
        (bfnan? (ival-lo-val ival))
        (bfnan? (ival-hi-val ival))
        (bflte? (ival-lo-val ival) (ival-hi-val ival))))

  (define (ival-contains? ival pt)
    (or (ival-err? ival)
        (if (bigfloat? pt)
            (if (bfnan? pt)
                (or (ival-err? ival)
                    (and (bfnan? (ival-lo-val ival)) (bfnan? (ival-hi-val ival))))
                (and (bflte? (ival-lo-val ival) pt) (bflte? pt (ival-hi-val ival))))
            (or (equal? pt (ival-lo-val ival)) (equal? pt (ival-hi-val ival))))))

  (check ival-contains? (ival-bool #f) #f)
  (check ival-contains? (ival-bool #t) #t)
  (check ival-contains? (ival-pi) (pi.bf))
  (check ival-contains? (ival-e) (bfexp 1.bf))
  (test-case "mk-ival"
    (for ([i (in-range num-tests)])
      (define pt (sample-double))
      (void)
      #;(with-check-info (['point pt])
        (check-pred ival-valid? (mk-ival (bf pt)))
        #;(check ival-contains? (mk-ival (bf pt)) (bf pt)))))
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
          (cons ival-sin   bfsin)
          (cons ival-cos   bfcos)
          (cons ival-tan   bftan)
          (cons ival-asin  bfasin)
          (cons ival-acos  bfacos)
          (cons ival-atan  bfatan)
          (cons ival-sinh  bfsinh)
          (cons ival-cosh  bfcosh)
          (cons ival-tanh  bftanh)
          (cons ival-rint  bfrint)
          (cons ival-round bfround)
          (cons ival-ceil  bfceiling)
          (cons ival-trunc bftruncate)
          (cons ival-floor bffloor)))

  (for ([(ival-fn fn) (in-dict arg1)])
    (test-case (~a (object-name ival-fn))
       (for ([n (in-range num-tests)])
         (define i (sample-interval))
         (define x (sample-from i))
         (with-check-info (['fn ival-fn] ['interval i] ['point x] ['number n])
           (check-pred ival-valid? (ival-fn i))
           (check ival-contains? (ival-fn i) (fn x))))))
  
  (define arg2
    (list (cons ival-add bfadd)
          (cons ival-sub bfsub)
          (cons ival-mult bfmul)
          (cons ival-div bfdiv)
          (cons ival-hypot bfhypot)
          (cons ival-atan2 bfatan2)
          (cons ival-< bflt?)
          (cons ival-> bfgt?)
          (cons ival-<= bflte?)
          (cons ival->= bfgte?)
          (cons ival-== bf=?)
          (cons ival-!= (compose not bf=?))))

  (for ([(ival-fn fn) (in-dict arg2)])
    (test-case (~a (object-name ival-fn))
       (for ([n (in-range num-tests)])
         (define i1 (sample-interval))
         (define i2 (sample-interval))
         (define x1 (sample-from i1))
         (define x2 (sample-from i2))

         (with-check-info (['fn ival-fn] ['interval1 i1] ['interval2 i2] ['point1 x1] ['point2 x2] ['number n])
           (define iy (ival-fn i1 i2))
           (check-pred ival-valid? iy)
           (check ival-contains? iy (fn x1 x2))))))

  (define (bffmod x y)
    (bfsub x (bfmul (bftruncate (bfdiv x y)) y)))

  (define (bfremainder x mod)
    (bfsub x (bfmul (bfround (bfdiv x mod)) mod)))

  (define weird (list (cons ival-fmod bffmod) (cons ival-remainder bfremainder)))
  (for ([(ival-fn fn) (in-dict weird)])
    (test-case (~a (object-name ival-fn))
      (for ([n (in-range num-tests)])
        (define i1 (sample-interval))
        (define i2 (sample-interval))
        (define x1 (sample-from i1))
        (define x2 (sample-from i2))

        (define y (parameterize ([bf-precision 8000]) (fn x1 x2)))

        ;; Known bug in bffmod where rounding error causes invalid output
        (unless (or (bflte? (bfmul y x1) 0.bf) (bfgt? (bfabs y) (bfabs x2)))
          (with-check-info (['fn ival-fn] ['interval1 i1] ['interval2 i2]
                                          ['point1 x1] ['point2 x2] ['number n])
            (define iy (ival-fn i1 i2))
            (check-pred ival-valid? iy)
            (check ival-contains? iy y))))))
  
  ;; ##################################################### tests for endpoint-immovable
  (define num-immovable-tests (/ num-tests 2))
  
  (define (check-endpoints-consistant result result2)
    (if (endpoint-immovable? (ival-lo result))
        (check equal-or-bf=? (ival-lo-val result) (ival-lo-val result2))
        (void))
    (if (endpoint-immovable? (ival-hi result))
        (check equal-or-bf=? (ival-hi-val result) (ival-hi-val result2))
        (void)))
  
  (define (test-function-overflows ival-fn fn num-of-arguments iterations)
    (parameterize ([bf-precision 80])
      (test-case (~a (object-name ival-fn))
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

