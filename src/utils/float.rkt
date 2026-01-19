#lang racket

(require math/bigfloat
         math/base
         racket/flonum)
(require "../utils/common.rkt"
         "../syntax/types.rkt"
         "../utils/errors.rkt")

(provide ulp-difference
         ulps->bits
         midpoint
         two-midpoints
         random-generate
         </total
         <=/total
         value->string
         value->json
         json->value
         real->repr
         repr->real)

(define (flonum->ordinal-parts x)
  (define lo (flbit-field x 0 32))
  (define hi (flbit-field x 32 64))
  (define sign? (not (zero? (bitwise-and hi #x80000000))))
  (define hi* (bitwise-and hi #x7fffffff))
  (define negative? (and sign? (or (not (zero? hi*)) (not (zero? lo)))))
  (values negative? hi* lo))

(define (parts->int hi lo)
  (+ (arithmetic-shift hi 32) lo))

(define (binary64-ulp-diff-from-parts sx hx lx sy hy ly)
  (if (eq? sx sy)
      (parts->int (- hx hy) (- lx ly))
      (+ (parts->int hx lx) (parts->int hy ly))))

(define (binary64-ulp-diff x y)
  (define-values (sx hx lx) (flonum->ordinal-parts x))
  (define-values (sy hy ly) (flonum->ordinal-parts y))
  (binary64-ulp-diff-from-parts sx hx lx sy hy ly))

(define (ulp-difference x y repr)
  (define ->ordinal (representation-repr->ordinal repr))
  (if (eq? repr <binary64>)
      (+ 1 (abs (binary64-ulp-diff x y)))
      (+ 1 (abs (- (->ordinal y) (->ordinal x))))))

;; Returns the midpoint of the representation's ordinal values,
;; not the real-valued midpoint
(define (midpoint p1 p2 repr)
  ((representation-ordinal->repr repr) (floor (/ (+ ((representation-repr->ordinal repr) p1)
                                                    ((representation-repr->ordinal repr) p2))
                                                 2))))

(define (repr-round repr dir point)
  ((representation-repr->bf repr) (parameterize ([bf-rounding-mode dir])
                                    ((representation-bf->repr repr) point))))

(define (two-midpoints repr lo hi)
  ; Midpoint is taken in repr-space, but values are stored in bf
  (define <-ordinal (compose (representation-repr->bf repr) (representation-ordinal->repr repr)))
  (define ->ordinal (compose (representation-repr->ordinal repr) (representation-bf->repr repr)))

  (define lower (<-ordinal (floor (/ (+ (->ordinal hi) (->ordinal lo)) 2))))
  (define higher (repr-round repr 'up (bfnext lower))) ; repr-next

  (and (bf>= lower lo)
       (bf<= higher hi) ; False if lo and hi were already close together
       (cons lower higher)))

(define (ulps->bits x)
  (real->double-flonum (log x 2)))

(define (random-generate repr)
  (define bits (sub1 (representation-total-bits repr)))
  ((representation-ordinal->repr repr) (random-integer (- (expt 2 bits)) (expt 2 bits))))

(define (=/total x1 x2 repr)
  (define ->ordinal (representation-repr->ordinal repr))
  (define special? (representation-special-value? repr))
  (or (= (->ordinal x1) (->ordinal x2)) (and (special? x1) (special? x2))))

(define (</total x1 x2 repr)
  (define special? (representation-special-value? repr))
  (define ->ordinal (representation-repr->ordinal repr))
  (cond
    [(special? x1) #f]
    [(special? x2) #t]
    [else (< (->ordinal x1) (->ordinal x2))]))

(define (<=/total x1 x2 repr)
  (or (</total x1 x2 repr) (=/total x1 x2 repr)))

(define (value->json x repr)
  (match x
    [(? real?)
     (match x
       [(? rational?) x]
       [(or -inf.0 -inf.f) (hash 'type "real" 'value "-inf")]
       [(or +inf.0 +inf.f) (hash 'type "real" 'value "+inf")]
       [(or +nan.0 +nan.f) (hash 'type "real" 'value "NaN")])]
    [_
     (hash 'type
           (~a (representation-name repr))
           'ordinal
           (~a ((representation-repr->ordinal repr) x)))]))

(define (json->value x repr)
  (match x
    [(? real?) (exact->inexact x)]
    [(? hash?)
     (match (hash-ref x 'type)
       ["real"
        (match (hash-ref x 'value)
          ["-inf" -inf.0]
          ["+inf" +inf.0]
          ["NaN" +nan.0]
          [_ +nan.0])]
       [_ ((representation-ordinal->repr repr) (string->number (hash-ref x 'ordinal)))])]))

(define (value->string n repr)
  ;; Prints a number with relatively few digits
  (define ->bf (representation-repr->bf repr))
  (define <-bf (representation-bf->repr repr))
  ;; Linear search because speed not an issue
  (let loop ([precision 16])
    (cond
      [(> precision (*max-mpfr-prec*))
       (warn 'value-to-string #:url "faq.html#value-to-string" "Could not uniquely print ~a" n)
       n]
      [else
       (parameterize ([bf-precision precision])
         (define bf (->bf n))
         (if (=/total n (<-bf bf) repr)
             (match (bigfloat->string bf)
               ["-inf.bf" "-inf.0"]
               ["+inf.bf" "+inf.0"]
               ["+nan.bf" "+nan.0"]
               [x x])
             (loop (+ precision 4))))]))) ; 2^4 > 10

(define (real->repr x repr)
  (parameterize ([bf-precision (representation-total-bits repr)])
    ((representation-bf->repr repr) (bf x))))

(define (repr->real x repr)
  (match x
    [(? boolean?) x]
    [_ (bigfloat->real ((representation-repr->bf repr) x))]))
