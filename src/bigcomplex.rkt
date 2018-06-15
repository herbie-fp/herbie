#lang racket

(require math/bigfloat)

(struct bigcomplex (re im) #:transparent)

(provide (contract-out
          [struct bigcomplex ((re bigfloat?) (im bigfloat?))]
          [bf-complex-add (-> bigcomplex? bigcomplex? bigcomplex?)]
          [bf-complex-sub (-> bigcomplex? bigcomplex? bigcomplex?)]
          [bf-complex-neg (-> bigcomplex? bigcomplex?)]
          [bf-complex-mult (-> bigcomplex? bigcomplex? bigcomplex?)]
          [bf-complex-conjugate (-> bigcomplex? bigcomplex?)]
          [bf-complex-sqr (-> bigcomplex? bigcomplex?)]
          [bf-complex-exp (-> bigcomplex? bigcomplex?)]
          [bf-complex-log (-> bigcomplex? bigcomplex?)]
          [bf-complex-sqrt (-> bigcomplex? bigcomplex?)]
          [bf-complex-pow (-> bigcomplex? bigcomplex? bigcomplex?)]
          [bf-complex-div (-> bigcomplex? bigcomplex? bigcomplex?)])
         exact+ exact- exact* exact/ exact-sqr exact-log exact-pow exact-sqrt exact-exp)

(define (bf-complex-add x y)
  (bigcomplex (bf+ (bigcomplex-re x) (bigcomplex-re y)) (bf+ (bigcomplex-im x) (bigcomplex-im y))))

(define (bf-complex-sub x [y #f])
  (if y
      (bf-complex-add x (bf-complex-neg y))
      (bf-complex-neg x)))

(define (bf-complex-neg x)
  (bigcomplex (bf- (bigcomplex-re x)) (bf- (bigcomplex-im x))))

(define (bf-complex-mult x y)
  (bigcomplex (bf+ (bf* (bigcomplex-re x) (bigcomplex-re y)) (bf- (bf* (bigcomplex-im x) (bigcomplex-im y))))
        (bf+ (bf* (bigcomplex-im x) (bigcomplex-re y)) (bf* (bigcomplex-re x) (bigcomplex-im y)))))

(define (bf-complex-conjugate x)
  (bigcomplex (bigcomplex-re x) (bf- (bigcomplex-im x))))

(define (bf-complex-sqr x)
  (bf-complex-mult x x))

(define (bf-complex-exp x)
  (match-define (bigcomplex re im) x)
  (define scale (bfexp re))
  (bigcomplex (bf* scale (bfcos im)) (bf* scale (bfsin im))))

(define (bf-complex-log x)
  (match-define (bigcomplex re im) x)
  (define mag (bfhypot re im))
  (define arg (bfatan2 im re))
  (bigcomplex (bflog mag) arg))

(define (bf-complex-sqrt x)
  (bf-complex-pow x (bigcomplex (bf 0.5) 0.bf)))

(define (bf-complex-pow x n)
  (bf-complex-exp (bf-complex-mult n (bf-complex-log x))))

(define (bf-complex-div x y)
  (define numer (bf-complex-mult x (bf-complex-conjugate y)))
  (define denom (bf-complex-mult y (bf-complex-conjugate y)))
  (bigcomplex (bf/ (bigcomplex-re numer) (bigcomplex-re denom)) (bf/ (bigcomplex-im numer) (bigcomplex-re denom))))

(define (make-exact-fun bf-fun bf-complex-fun)
  (lambda args
    (match args
      [(list (? bigfloat?) ...)
       (apply bf-fun args)]
      [(list (? bigcomplex?) ...)
       (apply bf-complex-fun args)])))

(require (only-in racket/base [exp e]))

(define exact+ (make-exact-fun bf+ bf-complex-add))
(define exact- (make-exact-fun bf- bf-complex-sub))
(define exact* (make-exact-fun bf* bf-complex-mult))
(define exact/ (make-exact-fun bf/ bf-complex-div))
(define exact-exp (make-exact-fun bfexp bf-complex-exp))
(define exact-log (make-exact-fun bflog bf-complex-log))
(define exact-pow (make-exact-fun bfexpt bf-complex-pow))
(define exact-sqr (make-exact-fun bfsqr bf-complex-sqr))
(define exact-sqrt (make-exact-fun bfsqrt bf-complex-sqrt))

(module+ test
  (define (bf-complex-eq-approx bf1 bf2)
    (check-equal? (bfround (bigcomplex-re bf1)) (bigcomplex-re bf2))
    (check-equal? (bfround (bigcomplex-im bf1)) (bigcomplex-im bf2)))
  (require rackunit)
  (check-equal? (bf-complex-mult (bigcomplex (bf 5) (bf 2)) (bigcomplex (bf 7) (bf 12))) (bigcomplex (bf 11) (bf 74)))
  (check-equal? (bf-complex-div (bigcomplex (bf 5) (bf 2)) (bigcomplex (bf 7) (bf 4))) (bigcomplex (bf 43/65) (bf -6/65)))
  (bf-complex-eq-approx (bf-complex-pow (bigcomplex (bf 2) (bf 3)) (bigcomplex (bf 3) (bf 0))) (bigcomplex (bf (- 46)) (bf 9)))
  (bf-complex-eq-approx (bf-complex-pow (bigcomplex (bf 2) (bf 3)) (bigcomplex (bf 4) (bf 0))) (bigcomplex (bf (- 119)) (bf (- 120)))))
