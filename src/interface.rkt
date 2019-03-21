#lang racket

(require math/bigfloat)
(require math/flonum)

(provide (struct-out representation) binary64 binary32
         single-flonum->ordinal)

(define-struct representation
  (name
   bf->repr
   repr->bf
   ordinal->repr
   repr->ordinal
   total-bits
   special-values))

(define binary64 (representation
  'binary64
  bigfloat->flonum
  bf
  ordinal->flonum
  flonum->ordinal
  64
  '(+nan.0 +inf.0 -inf.0)))

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

(define binary32 (representation
  'binary32
  (compose real->single-flonum bigfloat->flonum)
  bf
  ordinal->single-flonum
  single-flonum->ordinal
  32
  '(+nan.f +inf.f -inf.f)))

(define posit8 (representation
  'posit8
  (compose double->posit8 bifloat->flonum)
  (compose bf posit8->double)
  ordinal->posit8
  posit8->ordinal
  8
  (list posit8-nar)))

(define posit16 (representation
  'posit16
  (compose double->posit16 bifloat->flonum)
  (compose bf posit16->double)
  ordinal->posit16
  posit16->ordinal
  16
  (list posit16-nar)))

(define posit32 (representation
  'posit32
  (compose double->posit32 bifloat->flonum)
  (compose bf posit32->double)
  ordinal->posit32
  posit32->ordinal
  32
  (list posit32-nar)))


