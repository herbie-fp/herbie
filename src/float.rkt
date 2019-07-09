#lang racket

(require math/flonum math/bigfloat)
(require "config.rkt" "common.rkt" "interface.rkt" "syntax/types.rkt" "bigcomplex.rkt" "syntax/syntax.rkt")
(module+ test (require rackunit))

(provide midpoint ulp-difference *bit-width* ulps->bits bit-difference
         </total <=/total =-or-nan? nan?-all-types ordinary-value?
         exact-value? val-to-type flval
         infer-representation infer-double-representation
         ->flonum ->bf random-generate fl->repr repr->fl
         <-all-precisions mk-<= special-value?
         get-representation*)

(define (infer-representation x)
  (get-representation
   (for/first ([(type rec) (in-hash type-dict)] #:when ((car rec) x))
     (if (equal? type 'real)
         (if (flag-set? 'precision 'double) 'binary64 'binary32)
         type))))

(define (infer-double-representation x y)
  (define repr1 (infer-representation x))
  (define repr2 (infer-representation y))
  (unless (equal? repr1 repr2)
    (error 'infer-representation "Different representations: ~a for ~a and ~a for ~a"
           repr1 x repr2 y))
  repr1)

(define (get-representation* x)
  (match x
    ['real (get-representation (if (flag-set? 'precision 'double) 'binary64 'binary32))]
    [x (get-representation x)]))

(define (ulp-difference x y)
  (if (and (complex? x) (complex? y) (not (real? x)) (not (real? y)))
    (+ (ulp-difference (real-part x) (real-part y))
       (ulp-difference (imag-part x) (imag-part y)))
    (let ([->ordinal (representation-repr->ordinal (infer-double-representation x y))])
      (- (->ordinal y) (->ordinal x)))))

;; Returns the midpoint of the representation's ordinal values,
;; not the real-valued midpoint
(define (midpoint p1 p2)
  (define repr (infer-double-representation p1 p2))
  ((representation-ordinal->repr repr)
   (floor (/ (+ ((representation-repr->ordinal repr) p1)
                ((representation-repr->ordinal repr) p2))
             2))))

(define (*bit-width*) (if (flag-set? 'precision 'double) 64 32))

(define (ulps->bits x)
  (cond
   [(nan? x) +nan.0]
   [(infinite? x) (*bit-width*)]
   [else (/ (log x) (log 2))]))

(define (bit-difference x y)
  (ulps->bits (+ 1 (abs (ulp-difference x y)))))

(define (random-generate repr)
  ((representation-ordinal->repr repr) (random-exp (representation-total-bits repr))))

(define (special-value? x)
  (define repr (infer-representation x))
  (set-member? (representation-special-values repr) x))

(define (ordinary-value? x)
  (if (and (complex? x) (not (real? x)))
      (and (not (and (real? x) (nan? x))) (not (and (real? x) (infinite? x))))
      (not (special-value? x))))

(module+ test
  (check-true (ordinary-value? 2.5))
  (check-false (ordinary-value? +nan.0))
  (check-false (ordinary-value? -inf.0)))

(define (=-or-nan? x1 x2)
  (cond
    [(and (number? x1) (number? x2))
     (or (= x1 x2) (and (nan? x1) (nan? x2)))]
    [else
     (define repr (infer-double-representation x1 x2))
     (= ((representation-repr->ordinal repr) x1)
        ((representation-repr->ordinal repr) x2))]))

(module+ test
  (check-true (=-or-nan? 2.3 2.3))
  (check-false (=-or-nan? 2.3 7.8))
  (check-true (=-or-nan? +nan.0 -nan.f))
  (check-false (=-or-nan? 2.3 +nan.f)))

(define (</total x1 x2)
  (cond
   [(and (complex? x1) (complex? x2) (not (real? x1)) (not (real? x2)))
    (error "Complex numbers are unordered")]
   [(and (real? x1) (real? x2))
    (cond [(nan? x1) #f] [(nan? x2) #t] [else (< x1 x2)])]
   [else
    (define repr (infer-double-representation x1 x2))
    (cond
     [(set-member? (representation-special-values repr) x1) #f]
     [(set-member? (representation-special-values repr) x2) #t]
     [else (< ((representation-repr->ordinal repr) x1)
              ((representation-repr->ordinal repr) x2))])]))

(define (nan?-all-types x)
  (if (or (real? x) (complex? x))
      (nan? x)
      (set-member? (representation-special-values (infer-representation x)) x)))

(define (<=/total x1 x2)
  (or (</total x1 x2) (=-or-nan? x1 x2)))

(define (exact-value? type val)
  (match type
    ['real (exact? val)]
    ['complex (exact? val)]
    ['boolean true]
    [_ false]))

(define (val-to-type type val)
  (match type
    ['real val]
    ['complex (if (real? val) `(complex ,val 0) val)]
    ['boolean (if val 'TRUE 'FALSE)]
    [_ (error "Unknown type" type)]))

(define (flval x)
  (match x
    [(? real?) x]
    [(? complex?) (hash 'type "complex" 'real (real-part x) 'imag (real-part x))]
    [_
     (define repr (infer-representation x))
     (hash 'type (~a repr) 'ordinal (~a ((representation-repr->ordinal repr) x)))]))

(define/contract (->flonum x)
  (-> any/c value?)
  (cond
   [(and (complex? x) (not (real? x)))
    (make-rectangular (->flonum (real-part x)) (->flonum (imag-part x)))]
   [(bigcomplex? x)
    (make-rectangular (->flonum (bigcomplex-re x)) (->flonum (bigcomplex-im x)))]
   [(and (symbol? x) (constant? x))
    (->flonum ((constant-info x 'fl)))]
   [else
    (define repr (infer-representation x))
    (if (and (real? x) (exact? x)) (exact->inexact x) x)]))

(define (fl->repr x repr)
  ((representation-exact->repr repr) x))

(define (repr->fl x repr)
  (bigfloat->flonum ((representation-repr->bf repr) x)))

(define/contract (->bf x)
  (-> any/c bigvalue?)
  (cond
   [(and (symbol? x) (constant? x)) ((constant-info x 'bf))]
   [(and (complex? x) (not (real? x)))
    (bigcomplex (bf (real-part x)) (bf (imag-part x)))]
   [else
    ((representation-repr->bf (infer-representation x)) x)]))

(define (<-all-precisions x1 x2)
  (cond
   [(or (real? x1) (complex? x1))
    (< x1 x2)]
   [else
    (define repr (infer-double-representation x1 x2))
    (define ->ordinal (representation-repr->ordinal repr))
    (< (->ordinal x1) (->ordinal x2))]))

(define (mk-<= precision var val)
  (define repr (get-representation precision))
  (define (cast x)
    (match precision
      ['posit8 `(real->posit8 ,x)] ['posit16 `(real->posit16 ,x)] ['posit32 `(real->posit32 ,x)]
      ['quire8 `(real->quire8 ,x)] ['quire16 `(real->quire16 ,x)] ['quire32 `(real->quire32 ,x)]
      [(or 'binary64 'binary32) x]))
  (define prec-point (cast (repr->fl val repr)))
  (define <=-operator
    (match precision
      [(or 'binary64 'binary32) '<=] 
      ['posit8 `<=.p8] ['posit16 `<=.p16] ['posit32 `<=.p32]
      ['quire8 `<=.p8] ['quire16 `<=.q16] ['quire32 `<=.q32]))
  (list <=-operator var prec-point))
