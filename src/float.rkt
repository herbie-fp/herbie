#lang racket

(require math/flonum math/bigfloat)
(require "config.rkt")
(require "common.rkt")
(require "syntax/softposit.rkt")
(require "interface.rkt")
;; TODO remove references to interface.rkt for single-flonum->ordinal

(provide midpoint ulp-difference *bit-width* ulps->bits bit-difference sample-float sample-double)

(define (single-flonums-between x y)
  (- (single-flonum->ordinal y) (single-flonum->ordinal x)))

(define (ulp-difference x y)
  (define ->ordinal (representation-repr->ordinal (match* (x y)
    [((? real?) (? real?))
     (if (flag-set? 'precision 'double) binary64 binary32)]
    [((? complex?) (? complex?))
     (+ (ulp-difference (real-part x) (real-part y))
        (ulp-difference (imag-part x) (imag-part y)))]
    [((? boolean?) (? boolean?)) bool ]
    [((? posit8?) (? posit8?)) posit8]
    [((? posit16?) (? posit16?)) posit16]
    [((? posit32?) (? posit32?)) posit32]
    [((? quire8?) (? posit8?)) posit8]
    [((? quire16?) (? posit16?)) posit16]
    [((? quire32?) (? posit32?)) posit32])))
  (- (->ordinal y) (->ordinal x)))

;; Returns the midpoint of ordinals, not the real-valued midpoint
(define (midpoint p1 p2)
  (define rep (cond
    [(and (double-flonum? p1) (double-flonum? p2)) binary64]
    [(and (single-flonum? p1) (single-flonum? p2)) binary32]
    [(and (posit8? p1) (posit8? p2)) posit8]
    [(and (posit16? p1) (posit16? p2)) posit16]
    [(and (posit32? p1) (posit32? p2)) posit32]
    [else (error "Mixed precisions in binary search")]))
  ((representation-ordinal->repr repr) (floor (/ (+
    ((representation-repr->ordinal repr) x)
    ((representation-repr->ordinal repr) y)) 2))))

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
