#lang racket

(require math/bigfloat math/flonum)
(require "syntax/types.rkt")

(provide (struct-out representation) get-representation *output-repr* *var-reprs* representation-type)
(module+ internals (provide define-representation))

;; Global precision tacking
(define *output-repr* (make-parameter '()))
(define *var-reprs* (make-parameter '()))

;; Structs

(struct representation
  (name type
   bf->repr repr->bf ordinal->repr repr->ordinal
   total-bits special-values)
  #:methods gen:custom-write
  [(define (write-proc repr port mode)
     (fprintf port "#<representation ~a>" (representation-name repr)))])

(define representations (make-hash))
(define (get-representation name)
  (hash-ref representations name
            (λ () (error 'get-representation "Unknown representation ~a" name))))

(define-syntax-rule (define-representation (name type) args ...)
  (begin
    (define name (representation 'name (get-type 'type) args ...))
    (hash-set! representations 'name name)))

(define-representation (bool bool)
  identity
  identity
  (λ (x) (= x 0))
  (λ (x) (if x 1 0))
  1
  null)

(define-representation (binary64 real)
  bigfloat->flonum
  bf
  ordinal->flonum
  flonum->ordinal
  64
  '(+nan.0 +inf.0 -inf.0))

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

(define-representation (binary32 real)
  (compose real->single-flonum bigfloat->flonum)
  bf
  ordinal->single-flonum
  single-flonum->ordinal
  32
  '(+nan.f +inf.f -inf.f)
  real->single-flonum)

