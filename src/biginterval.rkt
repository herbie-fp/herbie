#lang racket

(require math/bigfloat)
(require "common.rkt" "syntax/types.rkt")

(struct ival (lo hi err? err) #:transparent)

(provide (contract-out
          [struct ival ([lo bigvalue?] [hi bigvalue?] [err? boolean?] [err boolean?])]
          [mk-ival (-> real? ival?)]
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
          [ival-exp (-> ival? ival?)]
          [ival-expm1 (-> ival? ival?)]
          [ival-log (-> ival? ival?)]
          [ival-log1p (-> ival? ival?)]
          [ival-pow (-> ival? ival? ival?)]
          [ival-sin (-> ival? ival?)]
          [ival-cos (-> ival? ival?)]
          [ival-tan (-> ival? ival?)]
          [ival-asin (-> ival? ival?)]
          [ival-atan (-> ival? ival?)]
          [ival-atan2 (-> ival? ival? ival?)]
          [ival-sinh (-> ival? ival?)]
          [ival-cosh (-> ival? ival?)]
          [ival-tanh (-> ival? ival?)]
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
  (define err? (or (nan? x) (infinite? x)))
  (define x* (bf x)) ;; TODO: Assuming that float precision < bigfloat precision
  (ival x* x* err? err?))

(define (ival-pi)
  (ival (rnd 'down identity pi.bf) (rnd 'up identity pi.bf) #f #f))

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
  (ival (bf- (ival-hi x)) (bf- (ival-lo x)) (ival-err? x) (ival-err x)))

(define (ival-add x y)
  (ival (rnd 'down bf+ (ival-lo x) (ival-lo y))
        (rnd 'up bf+ (ival-hi x) (ival-hi y))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (ival-sub x y)
  (ival (rnd 'down bf- (ival-lo x) (ival-hi y))
        (rnd 'up bf- (ival-hi x) (ival-lo y))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (bfmin* a . as)
  (if (null? as) a (apply bfmin* (bfmin a (car as)) (cdr as))))

(define (bfmax* a . as)
  (if (null? as) a (apply bfmax* (bfmax a (car as)) (cdr as))))

(define (ival-mult x y)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))
  (ival (rnd 'down bfmin*
             (bf* (ival-lo x) (ival-lo y)) (bf* (ival-hi x) (ival-lo y))
             (bf* (ival-lo x) (ival-hi y)) (bf* (ival-hi x) (ival-hi y)))
        (rnd 'up bfmax*
             (bf* (ival-lo x) (ival-lo y)) (bf* (ival-hi x) (ival-lo y))
             (bf* (ival-lo x) (ival-hi y)) (bf* (ival-hi x) (ival-hi y)))
        err? err))

(define (ival-div x y)
  (define err? (or (ival-err? x) (ival-err? y) (and (bf<= (ival-lo y) 0.bf) (bf>= (ival-hi y) 0.bf))))
  (define err (or (ival-err x) (ival-err y) (and (bf= (ival-lo y) 0.bf) (bf= (ival-hi y) 0.bf))))
  (ival (rnd 'down bfmin*
             (bf/ (ival-lo x) (ival-lo y)) (bf/ (ival-hi x) (ival-lo y))
             (bf/ (ival-lo x) (ival-hi y)) (bf/ (ival-hi x) (ival-hi y)))
        (rnd 'up bfmax*
             (bf/ (ival-lo x) (ival-lo y)) (bf/ (ival-hi x) (ival-lo y))
             (bf/ (ival-lo x) (ival-hi y)) (bf/ (ival-hi x) (ival-hi y)))
        err? err))

(define (ival-exp x)
  (ival (rnd 'down bfexp (ival-lo x)) (rnd 'up bfexp (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-expm1 x)
  (ival (rnd 'down bfexpm1 (ival-lo x)) (rnd 'up bfexpm1 (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-log x)
  (define err (or (ival-err x) (bf<= (ival-hi x) 0.bf)))
  (define err? (or err (ival-err? x) (bf<= (ival-lo x) 0.bf)))
  (ival (rnd 'down bflog (ival-lo x)) (rnd 'up bflog (ival-hi x))
        err? err))

(define (ival-log1p x)
  (define err (or (ival-err x) (bf<= (ival-hi x) -1.bf)))
  (define err? (or err (ival-err? x) (bf<= (ival-lo x) -1.bf)))
  (ival (rnd 'down bflog1p (ival-lo x)) (rnd 'up bflog1p (ival-hi x))
        err? err))

(define (ival-sqrt x)
  (define err (or (ival-err x) (bf<= (ival-hi x) 0.bf)))
  (define err? (or err (ival-err? x) (bf<= (ival-lo x) 0.bf)))
  (ival (rnd 'down bfsqrt (ival-lo x)) (rnd 'up bfsqrt (ival-hi x))
        err? err))

(define (ival-cbrt x)
  (define err (or (ival-err x) (bf<= (ival-hi x) 0.bf)))
  (define err? (or err (ival-err? x) (bf<= (ival-lo x) 0.bf)))
  (ival (rnd 'down bfcbrt (ival-lo x)) (rnd 'up bfcbrt (ival-hi x))
        err? err))

(define (ival-pow x y)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))
  (cond
   [(bf>= (ival-lo x) 0.bf)
    (let ([lo
           (if (bf< (ival-lo x) 1.bf)
               (rnd 'down bfexpt (ival-lo x) (ival-hi y))
               (rnd 'down bfexpt (ival-lo x) (ival-lo y)))]
          [hi
           (if (bf> (ival-hi x) 1.bf)
               (rnd 'up bfexpt (ival-hi x) (ival-hi y))
               (rnd 'up bfexpt (ival-hi x) (ival-lo y)))])
      (ival lo hi err? err))]
   [(and (bf= (ival-lo y) (ival-hi y)) (bfinteger? (ival-lo y)))
    (ival (rnd 'down bfexpt (ival-lo x) (ival-lo y))
          (rnd 'up bfexpt (ival-lo x) (ival-lo y))
          err? err)]
   [else
    ;; In this case, the base range includes negatives and the exp range includes reals
    ;; Focus first on just the negatives in the base range
    ;; All the reals in the exp range just make NaN a possible output
    ;; If there are no integers in the exp range, those NaNs are the only output
    ;; If there are, the min of the negative base values is from the biggest odd integer in the range
    ;;  and the max is from the biggest even integer in the range
    (define a (bfceiling (ival-lo y)))
    (define b (bffloor (ival-hi y)))
    (define lo (ival-lo x))
    (define neg-range
      (cond
       [(bf< b a)
        (ival +nan.0 +nan.0 #t #t)]
       [(bf= a b)
        (ival (rnd 'down bfexpt (ival-lo x) a) (rnd 'up bfexpt (ival-hi x) a) err? err)]
       [(bfodd? b)
        (ival (rnd 'down bfexpt (ival-lo x) b)
              (rnd 'up bfmax (bfexpt (ival-hi x) (bf- b 1.bf)) (bfexpt (ival-lo x) (bf- b 1.bf))) err? err)]
       [(bfeven? b)
        (ival (rnd 'down bfexpt (ival-lo x) (bf- b 1.bf))
              (rnd 'up bfmax (bfexpt (ival-hi x) b) (bfexpt (ival-lo x) b)) err? err)]))
    (if (bf> (ival-hi x) 0.bf)
        (ival-union neg-range (ival-pow (ival 0.bf (ival-hi x) err? err) y))
        neg-range)]))

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
  (define a (rnd 'down bffloor (bf/ (ival-lo x) pi.bf)))
  (define b (rnd 'up bffloor (bf/ (ival-hi x) pi.bf)))
  (cond
   [(and (bf= a b) (bfeven? a))
    (ival (rnd 'down bfcos (ival-hi x)) (rnd 'up bfcos (ival-lo x)) (ival-err? x) (ival-err x))]
   [(and (bf= a b) (bfodd? a))
    (ival (rnd 'down bfcos (ival-lo x)) (rnd 'up bfcos (ival-hi x)) (ival-err? x) (ival-err x))]
   [(and (bf= (bf- b a) 1.bf) (bfeven? a))
    (ival -1.bf (rnd 'up bfmax (bfcos (ival-lo x)) (bfcos (ival-hi x))) (ival-err? x) (ival-err x))]
   [(and (bf= (bf- b a) 1.bf) (bfodd? a))
    (ival (rnd 'down bfmin (bfcos (ival-lo x)) (bfcos (ival-hi x))) 1.bf (ival-err? x) (ival-err x))]
   [else
    (ival -1.bf 1.bf (ival-err? x) (ival-err x))]))

(define half.bf (bf/ 1.bf 2.bf))

(define (ival-sin x)
  (define a (rnd 'down bffloor (bf- (bf/ (ival-lo x) pi.bf) half.bf)))
  (define b (rnd 'up bffloor (bf- (bf/ (ival-hi x) pi.bf) half.bf)))
  (cond
   [(and (bf= a b) (bfeven? a))
    (ival (rnd 'down bfsin (ival-hi x)) (rnd 'up bfsin (ival-lo x)) (ival-err? x) (ival-err x))]
   [(and (bf= a b) (bfodd? a))
    (ival (rnd 'down bfsin (ival-lo x)) (rnd 'up bfsin (ival-hi x)) (ival-err? x) (ival-err x))]
   [(and (bf= (bf- b a) 1.bf) (bfeven? a))
    (ival -1.bf (rnd 'up bfmax (bfsin (ival-lo x)) (bfsin (ival-hi x))) (ival-err? x) (ival-err x))]
   [(and (bf= (bf- b a) 1.bf) (bfodd? a))
    (ival (rnd 'down bfmin (bfsin (ival-lo x)) (bfsin (ival-hi x))) 1.bf (ival-err? x) (ival-err x))]
   [else
    (ival -1.bf 1.bf (ival-err? x) (ival-err x))]))

(define (ival-tan x)
  (ival-div (ival-sin x) (ival-cos x)))

(define (ival-atan x)
  (ival (rnd 'down bfatan (ival-lo x)) (rnd 'up bfatan (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-atan2 y x)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))
  (cond
   [(bf> (ival-lo y) 0.bf)
    (ival (rnd 'down bfatan2 (ival-lo y) (ival-hi x)) (rnd 'up bfatan2 (ival-lo y) (ival-lo x)) err? err)]
   [(bf< (ival-hi y) 0.bf)
    (ival (rnd 'down bfatan2 (ival-hi y) (ival-lo x)) (rnd 'down bfatan2 (ival-hi y) (ival-hi x)) err? err)]
   [(bf> (ival-lo x) 0.bf)
    (ival (rnd 'down bfatan2 (ival-lo y) (ival-lo x)) (rnd 'down bfatan2 (ival-lo y) (ival-hi x)) err? err)]
   ;; TODO: Not sufficiently accurate in the equality cases of the above
   [else
    (ival (rnd 'down bf- pi.bf) (rnd 'up identity pi.bf)
          (or err? (bf>= (ival-hi x) 0.bf))
          (or err (and (bf= (ival-lo x) 0.bf) (bf= (ival-hi x) 0.bf) (bf= (ival-lo y) 0.bf) (bf= (ival-hi y) 0.bf))))]))

(define (ival-asin x)
  (ival (rnd 'down bfasin (ival-lo x)) (rnd 'up bfasin (ival-hi x))
        (or (ival-err? x) (bf< (ival-lo x) -1.bf) (bf> (ival-hi x) 1.bf))
        (or (ival-err x) (bf< (ival-hi x) -1.bf) (bf> (ival-lo x) 1.bf))))

(define (ival-fabs x)
  (cond
   [(bf> (ival-lo x) 0.bf) x]
   [(bf< (ival-hi x) 0.bf)
    (ival (bf- (ival-hi x)) (bf- (ival-lo x)) (ival-err? x) (ival-err x))]
   [else ; interval stradles 0
    (ival 0.bf (bfmax (bf- (ival-lo x)) (ival-hi x) (ival-err? x) (ival-err x)))]))

(define (ival-sinh x)
  (ival (rnd 'down bfsinh (ival-lo x)) (rnd 'up bfsinh (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-cosh x)
  (define y (ival-fabs x))
  (ival (rnd 'down bfcosh (ival-lo y)) (rnd 'up bfcosh (ival-hi y)) (ival-err? y) (ival-err y)))

(define (ival-tanh x)
  (ival (rnd 'down bftanh (ival-lo x)) (rnd 'up bftanh (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-cmp x y)
  (define can-< (bf< (ival-lo x) (ival-hi y)))
  (define must-< (bf< (ival-hi x) (ival-lo y)))
  (define can-> (bf> (ival-hi x) (ival-lo y)))
  (define must-> (bf> (ival-lo x) (ival-hi y)))
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

(define (ival-comparator f)
  (λ as
    (if (null? as)
        ival-true
        (let loop ([head (car as)] [tail (cdr as)] [acc ival-true])
          (match tail
            ['() acc]
            [(cons next rest)
             (loop next rest (ival-and (f head next) ival-true))])))))

(define ival-<  (ival-comparator ival-<2))
(define ival-<= (ival-comparator ival-<=2))
(define ival->  (ival-comparator ival->2))
(define ival->= (ival-comparator ival->=2))
(define ival-== (ival-comparator ival-==2))

(define (ival-!=2 x y)
  (define-values (c< m< c> m>) (ival-cmp x y))
  (ival (or c< c>) (or m< m>) (or (ival-err? x) (ival-err? y)) (or (ival-err x) (ival-err y))))

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
    (ival (bfmin (ival-lo x) (ival-lo y)) (bfmax (ival-hi x) (ival-hi y))
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
