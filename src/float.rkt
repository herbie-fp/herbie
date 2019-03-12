#lang racket

(require math/flonum math/bigfloat)
(require "config.rkt")
(require "common.rkt")
(require "syntax/softposit.rkt")

(provide midpoint-float ulp-difference *bit-width* ulps->bits bit-difference sample-float sample-double)

(define (single-flonums-between x y)
  (- (single-flonum->ordinal y) (single-flonum->ordinal x)))

(define (ulp-difference x y)
  (match* (x y)
    [((? real?) (? real?))
     (if (flag-set? 'precision 'double) (flonums-between x y) (single-flonums-between x y))]
    [((? complex?) (? complex?))
     (+ (ulp-difference (real-part x) (real-part y))
        (ulp-difference (imag-part x) (imag-part y)))]
    [((? boolean?) (? boolean?))
     (if (equal? x y) 0 64)]
    [((? posit8?) (? posit8?))
     (p8s-between x y)]
    [((? posit16?) (? posit16?))
     (p16s-between x y)]
    [((? posit32?) (? posit32?))
     (p32s-between x y)]
    [((? quire8?) (? quire8?))
     (p8s-between (quire8->posit8 x) (quire8->posit8 y)) ]
    [((? quire16?) (? quire16?))
     (p16s-between (quire16->posit16 x) (quire16->posit16 y))]
    [((? quire32?) (? quire32?))
     (p32s-between (quire32->posit32 x) (quire32->posit32 y))]))

(define (midpoint-float p1 p2)
  (cond 
   [(and (double-flonum? p1) (double-flonum? p2))
    (flstep p1 (quotient (flonums-between p1 p2) 2))]
   [(and (single-flonum? p1) (single-flonum? p2))
    (floating-point-bytes->real
     (integer->integer-bytes
      (quotient
       (+ (single-flonum->ordinal p1) (single-flonum->ordinal p2))
       2)
      4 #f) #f)]
   [(and (posit8? p1) (posit8? p2))
    ;; NOTE: This isn't a binary search (just splits the difference)
    (posit8-div (posit8-add p1 p2) (double->posit8 2.0))]
   [(and (posit16? p1) (posit16? p2))
    (posit16-div (posit16-add p1 p2) (double->posit16 2.0))]
   [(and (posit32? p1) (posit32? p2))
    (posit32-div (posit32-add p1 p2) (double->posit32 2.0))]
   [else
    (error "Mixed precisions in binary search")]))

(define (*bit-width*) (if (flag-set? 'precision 'double) 64 32))

(define (ulps->bits x)
  (cond
   [(nan? x) +nan.0]
   [(infinite? x) (*bit-width*)]
   [else (/ (log x) (log 2))]))

(define (bit-difference x y)
  (ulps->bits (+ 1 (abs (ulp-difference x y)))))

(define (sample-float)
  (floating-point-bytes->real (integer->integer-bytes (random-exp 32) 4 #f)))

(define (sample-double)
  (floating-point-bytes->real (integer->integer-bytes (random-exp 64) 8 #f)))
