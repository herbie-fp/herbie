#lang racket

(require math/bigfloat math/flonum "bigcomplex.rkt")

(provide (struct-out representation) get-representation)
(module+ internals (provide define-representation))

(struct representation
  (name
   bf->repr repr->bf ordinal->repr repr->ordinal
   total-bits special-values exact->repr)
  #:methods gen:custom-write
  [(define (write-proc repr port mode)
     (fprintf port "#<representation ~a>" (representation-name repr)))])

(define representations (make-hash))
(define (get-representation name)
  (hash-ref representations name
            (λ () (error 'get-representation "Unknown representation ~a" name))))

(define-syntax-rule (define-representation name args ...)
  (begin
    (define name (representation 'name args ...))
    (hash-set! representations 'name name)))

(define-representation bool
  identity
  identity
  (λ (x) (= x 0))
  (λ (x) (if x 1 0))
  1
  null
  identity)

(define-representation binary64
  bigfloat->flonum
  bf
  ordinal->flonum
  flonum->ordinal
  64
  '(+nan.0 +inf.0 -inf.0)
  real->double-flonum)

(define (single-flonum->bit-field x)
  (integer-bytes->integer (real->floating-point-bytes x 4) #f))

(define (single-flonum->ordinal x)
  (cond
    [(< x 0.0f0) (- (single-flonum->bit-field (- 0.0f0 x)))]
    [else (single-flonum->bit-field (abs x))]))

(define (bit-field->single-flonum x)
  (real->single-flonum (floating-point-bytes->real (integer->integer-bytes x 4 #f) #f)))

(define (ordinal->single-flonum x)
  (cond
    [(< x 0) (- (bit-field->single-flonum (- x)))]
    [else (bit-field->single-flonum x)]))

(define-representation binary32
  (compose real->single-flonum bigfloat->flonum)
  bf
  ordinal->single-flonum
  single-flonum->ordinal
  32
  '(+nan.f +inf.f -inf.f)
  real->single-flonum)

(define-representation complex
  (λ (x) (make-rectangular (bigfloat->flonum (bigcomplex-re x)) (bigfloat->flonum (bigcomplex-im x))))
  (λ (x) (bigcomplex (bf (real-part x)) (bf (imag-part x))))
  (λ (x) (make-rectangular (ordinal->flonum (quotient x (expt 2 64))) (ordinal->flonum (modulo x (expt 2 64)))))
  (λ (x) (+ (* (expt 2 64) (flonum->ordinal (real-part x))) (flonum->ordinal (imag-part x))))
  128
  ;; TODO(interface): note that these values for special-values are incorrect;
  ;; any value that includes +nan.0 should be a special value, but because
  ;; types and representations are not cleanly separated, this is not reasonable to
  ;; express. Once types and representations are separated, fix this.
  '(+nan.0 +inf.0)
  real->double-flonum)
