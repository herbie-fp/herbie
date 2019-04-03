#lang racket

(require math/flonum math/bigfloat)
(require "config.rkt")
(require "common.rkt")
(require "syntax/softposit.rkt")
(require "interface.rkt")
(module+ test (require rackunit))

(provide midpoint ulp-difference *bit-width* ulps->bits bit-difference
         sample-double </total <=/total =-or-nan? nan?-all-types ordinary-value?)

(define (ulp-difference x y)
  (if (and (complex? x) (complex? y) (not (real? x)) (not (real? y)))
    (+ (ulp-difference (real-part x) (real-part y))
       (ulp-difference (imag-part x) (imag-part y)))
    (let ([->ordinal (representation-repr->ordinal (match* (x y)
        [((? real?) (? real?)) (if (flag-set? 'precision 'double) binary64 binary32)]
        [((? boolean?) (? boolean?)) bool]
        [((? posit8?) (? posit8?)) posit8]
        [((? posit16?) (? posit16?)) posit16]
        [((? posit32?) (? posit32?)) posit32]
        [((? quire8?) (? quire8?)) quire8]
        [((? quire16?) (? quire16?)) quire16]
        [((? quire32?) (? quire32?)) quire32]))])
      (- (->ordinal y) (->ordinal x)))))

;; Returns the midpoint of the representation's ordinal values,
;; not the real-valued midpoint
(define (midpoint p1 p2)
  (define repr (cond
    [(and (double-flonum? p1) (double-flonum? p2)) binary64]
    [(and (single-flonum? p1) (single-flonum? p2)) binary32]
    [(and (posit8? p1) (posit8? p2)) posit8]
    [(and (posit16? p1) (posit16? p2)) posit16]
    [(and (posit32? p1) (posit32? p2)) posit32]
    [else (error "Mixed precisions in binary search")]))
  ((representation-ordinal->repr repr) (floor (/ (+
    ((representation-repr->ordinal repr) p1)
    ((representation-repr->ordinal repr) p2)) 2))))

(define (*bit-width*) (if (flag-set? 'precision 'double) 64 32))

(define (ulps->bits x)
  (cond
   [(nan? x) +nan.0]
   [(infinite? x) (*bit-width*)]
   [else (/ (log x) (log 2))]))

(define (bit-difference x y)
  (ulps->bits (+ 1 (abs (ulp-difference x y)))))

(define (ordinary-value? x)
  (match x
    [(? real?)
     (not (or (set-member? (representation-special-values binary64) x)
              (set-member? (representation-special-values binary32) x)))]
    [(? complex?)
     (and (ordinary-value? (real-part x)) (ordinary-value? (imag-part x)))]
    [(? boolean?)
     (not (set-member? (representation-special-values bool) x))]
    [(? posit8?)
     (not (set-member? (representation-special-values posit8) x))]
    [(? posit16?)
     (not (set-member? (representation-special-values posit16) x))]
    [(? posit32?)
     (not (set-member? (representation-special-values posit32) x))]
    [_ true]))

(module+ test
  (check-true (ordinary-value? 2.5))
  (check-false (ordinary-value? +nan.0))
  (check-false (ordinary-value? -inf.f)))

(define (=-or-nan? x1 x2)
  (cond
    [(and (number? x1) (number? x2))
     (or (= x1 x2)
         (and (nan? x1) (nan? x2)))]
    [(and (posit8? x1) (posit8? x2))
     (= ((representation-repr->ordinal posit8) x1)
        ((representation-repr->ordinal posit8) x2))]
    [(and (posit16? x1) (posit16? x2))
     (= ((representation-repr->ordinal posit16) x1)
        ((representation-repr->ordinal posit16) x2))]
    [(and (posit32? x1) (posit32? x2))
     (= ((representation-repr->ordinal posit32) x1)
        ((representation-repr->ordinal posit32) x2))]
    ;; TODO once we have real ->ordinal for quires, fix this
    [(and (quire8? x1) (quire8? x2))
     (= ((representation-repr->ordinal quire8) x1)
        ((representation-repr->ordinal quire8) x2))]
    [(and (quire16? x1) (quire16? x2))
     (= ((representation-repr->ordinal quire16) x1)
        ((representation-repr->ordinal quire16) x2))]
    [(and (quire32? x1) (quire32? x2))
     (= ((representation-repr->ordinal quire32) x1)
        ((representation-repr->ordinal quire32) x2))]))

(module+ test
  (check-true (=-or-nan? 2.3 2.3))
  (check-false (=-or-nan? 2.3 7.8))
  (check-true (=-or-nan? +nan.0 -nan.f))
  (check-false (=-or-nan? 2.3 +nan.f)))

(define (</total x1 x2)
  (cond
    [(or (real? x1) (complex? x1))
     (cond
       [(nan? x1) #f]
       [(nan? x2) #t]
       [else (< x1 x2)])]
    [(posit8? x1)
     (cond
       [(set-member? (representation-special-values posit8) x1) #f]
       [(set-member? (representation-special-values posit8) x2) #t]
       [else (< ((representation-repr->ordinal posit8) x1)
                ((representation-repr->ordinal posit8) x2))])]
    [(posit16? x1)
     (cond
       [(set-member? (representation-special-values posit16) x1) #f]
       [(set-member? (representation-special-values posit16) x2) #t]
       [else (< ((representation-repr->ordinal posit16) x1)
                ((representation-repr->ordinal posit16) x2))])]
    [(posit32? x1)
     (cond
       [(set-member? (representation-special-values posit32) x1) #f]
       [(set-member? (representation-special-values posit32) x2) #t]
       [else (< ((representation-repr->ordinal posit32) x1)
                ((representation-repr->ordinal posit32) x2))])]
    [(quire8? x1)
     (cond
       [(set-member? (representation-special-values quire8) x1) #f]
       [(set-member? (representation-special-values quire8) x2) #t]
       [else (< ((representation-repr->ordinal quire8) x1)
                ((representation-repr->ordinal quire8) x2))])]
    [(quire16? x1)
     (cond
       [(set-member? (representation-special-values quire16) x1) #f]
       [(set-member? (representation-special-values quire16) x2) #t]
       [else (< ((representation-repr->ordinal quire16) x1)
                ((representation-repr->ordinal quire16) x2))])]
    [(quire32? x1)
     (cond
       [(set-member? (representation-special-values quire32) x1) #f]
       [(set-member? (representation-special-values quire32) x2) #t]
       [else (< ((representation-repr->ordinal quire32) x1)
                ((representation-repr->ordinal quire32) x2))])]))

(define (nan?-all-types x)
  (cond
    [(or (real? x) (complex? x)) (nan? x)]
    [(posit8? x) (set-member? (representation-special-values posit8) x)]
    [(posit16? x) (set-member? (representation-special-values posit16) x)]
    [(posit32? x) (set-member? (representation-special-values posit32) x)]))

(define (<=/total x1 x2)
  (or (</total x1 x2) (=-or-nan? x1 x2)))

(define (sample-double)
  (floating-point-bytes->real (integer->integer-bytes (random-exp 64) 8 #f)))
