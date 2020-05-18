#lang racket

(require math/bigfloat)

(struct bigcomplex (re im) #:transparent)

(provide (contract-out
          [struct bigcomplex ((re bigfloat?) (im bigfloat?))]
          [bigcomplex->complex (-> bigcomplex? complex?)]
          [complex->bigcomplex (-> complex? bigcomplex?)]
          [bf-complex-add (-> bigcomplex? bigcomplex? bigcomplex?)]
          [bf-complex-sub (-> bigcomplex? bigcomplex? bigcomplex?)]
          [bf-complex-neg (-> bigcomplex? bigcomplex?)]
          [bf-complex-mult (-> bigcomplex? bigcomplex? bigcomplex?)]
          [bf-complex-conjugate (-> bigcomplex? bigcomplex?)]
          [bf-complex-exp (-> bigcomplex? bigcomplex?)]
          [bf-complex-log (-> bigcomplex? bigcomplex?)]
          [bf-complex-sqrt (-> bigcomplex? bigcomplex?)]
          [bf-complex-pow (-> bigcomplex? bigcomplex? bigcomplex?)]
          [bf-complex-div (-> bigcomplex? bigcomplex? bigcomplex?)]))

(define (bigcomplex->complex x)
  (make-rectangular (bigfloat->flonum (bigcomplex-re x)) (bigfloat->flonum (bigcomplex-im x))))

(define (complex->bigcomplex val)
  (bigcomplex (bf (real-part val)) (bf (imag-part val))))

(define (bf-complex-neg x)
  (bigcomplex (bf- (bigcomplex-re x)) (bf- (bigcomplex-im x))))

(define (bf-complex-add x y)
  (bigcomplex (bf+ (bigcomplex-re x) (bigcomplex-re y))
              (bf+ (bigcomplex-im x) (bigcomplex-im y))))

(define (bf-complex-sub x y)
  (bf-complex-add x (bf-complex-neg y)))

(define (bf-complex-mult x y)
  (bigcomplex (bf+ (bf* (bigcomplex-re x) (bigcomplex-re y)) (bf- (bf* (bigcomplex-im x) (bigcomplex-im y))))
              (bf+ (bf* (bigcomplex-im x) (bigcomplex-re y)) (bf* (bigcomplex-re x) (bigcomplex-im y)))))

(define (bf-complex-conjugate x)
  (bigcomplex (bigcomplex-re x) (bf- (bigcomplex-im x))))

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

(module+ test
  (define (bf-complex-eq-approx bf1 bf2)
    (check-equal? (bfround (bigcomplex-re bf1)) (bigcomplex-re bf2))
    (check-equal? (bfround (bigcomplex-im bf1)) (bigcomplex-im bf2)))
  (require rackunit)
  (check-equal? (bf-complex-mult (bigcomplex (bf 5) (bf 2)) (bigcomplex (bf 7) (bf 12))) (bigcomplex (bf 11) (bf 74)))
  (check-equal? (bf-complex-div (bigcomplex (bf 5) (bf 2)) (bigcomplex (bf 7) (bf 4))) (bigcomplex (bf 43/65) (bf -6/65)))
  (bf-complex-eq-approx (bf-complex-pow (bigcomplex (bf 2) (bf 3)) (bigcomplex (bf 3) (bf 0))) (bigcomplex (bf (- 46)) (bf 9)))
  (bf-complex-eq-approx (bf-complex-pow (bigcomplex (bf 2) (bf 3)) (bigcomplex (bf 4) (bf 0))) (bigcomplex (bf (- 119)) (bf (- 120)))))
