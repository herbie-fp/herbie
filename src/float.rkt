#lang racket

(require math/flonum math/bigfloat)
(require "config.rkt")
(require "common.rkt")

(provide ulp-difference *bit-width* ulps->bits bit-difference)

(define (single-flonum->bit-field x)
  (integer-bytes->integer (real->floating-point-bytes x 4) #f))

(define (single-flonum->ordinal x)
  (cond [(x . < . 0.0f0) (- (single-flonum->bit-field (- 0.0f0 x)))]
	[else            (single-flonum->bit-field (abs x))]))

(define (single-flonums-between x y)
  (- (single-flonum->ordinal y) (single-flonum->ordinal x)))

(define (ulp-difference x y)
  (match* (x y)
    [((? real?) (? real?))
     (((flag 'precision 'double) flonums-between single-flonums-between) x y)]
    [((? complex?) (? complex?))
     (+ (ulp-difference (real-part x) (real-part y))
        (ulp-difference (imag-part x) (imag-part y)))]))

(define (*bit-width*) ((flag 'precision 'double) 64 32))

(define (ulps->bits x)
  (cond
   [(nan? x) +nan.0]
   [(infinite? x) (*bit-width*)]
   [else (log2 x)]))

(define (bit-difference x y)
  (ulps->bits (+ 1 (abs (ulp-difference x y)))))
