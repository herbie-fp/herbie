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
          [ival-sqrt (-> ival? ival?)]
          [ival-exp (-> ival? ival?)]
          [ival-log (-> ival? ival?)]
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
  (ival (rnd 'down (λ () pi.bf)) (rnd 'up (λ () pi.bf)) #f #f))

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

(define (ival-log x)
  (define err (or (ival-err x) (bf<= (ival-hi x) 0.bf)))
  (define err? (or err (ival-err? x) (bf<= (ival-lo x) 0.bf)))
  (ival (rnd 'down bflog (ival-lo x)) (rnd 'up bflog (ival-hi x))
        err? err))

(define (ival-sqrt x)
  (define err (or (ival-err x) (bf<= (ival-hi x) 0.bf)))
  (define err? (or err (ival-err? x) (bf<= (ival-lo x) 0.bf)))
  (ival (rnd 'down bfsqrt (ival-lo x)) (rnd 'up bfsqrt (ival-hi x))
        err? err))

(define (ival-and . as)
  (ival (andmap ival-lo as) (andmap ival-hi as)
        (ormap ival-err? as) (ormap ival-err as)))

(define (ival-or . as)
  (ival (ormap ival-lo as) (ormap ival-hi as)
        (ormap ival-err? as) (ormap ival-err as)))

(define (ival-not x)
  (ival (not (ival-hi x)) (not (ival-lo x)) (ival-err? x) (ival-err x)))

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
