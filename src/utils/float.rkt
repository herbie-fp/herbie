#lang racket

(require math/base
         math/bigfloat
         math/flonum)
(require "../utils/common.rkt"
         "../syntax/types.rkt"
         "../utils/errors.rkt")

(provide repr-ulps
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

(define (repr-ulps repr)
  (match (representation-type repr)
    ['array
     (define elem-repr (array-representation-elem repr))
     (define elem-ulps (repr-ulps elem-repr))
     (lambda (x y)
       (unless (and (vector? x) (vector? y))
         (raise-herbie-error "Expected vector array values, got ~a and ~a" x y))
       (unless (= (vector-length x) (vector-length y))
         (raise-herbie-error "Mismatched array lengths for repr-ulps: ~a vs ~a"
                             (vector-length x)
                             (vector-length y)))
       (for/sum ([x1 (in-vector x)] [y1 (in-vector y)]) (elem-ulps x1 y1)))]
    [_
     (define ->ordinal (representation-repr->ordinal repr))
     (define special? (representation-special-value? repr))
     (define max-error (+ 1 (expt 2 (representation-total-bits repr))))
     (if (eq? repr <binary64>)
         (lambda (x y)
           (if (or (special? x) (special? y))
               max-error
               (+ 1 (abs (flonums-between x y)))))
         (lambda (x y)
           (if (or (special? x) (special? y))
               max-error
               (+ 1 (abs (- (->ordinal y) (->ordinal x)))))))]))

;; Returns the midpoint of the representation's ordinal values,
;; not the real-valued midpoint
(define (midpoint p1 p2 repr)
  (match (representation-type repr)
    ['array
     (define elem-repr (array-representation-elem repr))
     (unless (and (vector? p1) (vector? p2))
       (raise-herbie-error "Expected vector array values, got ~a and ~a" p1 p2))
     (unless (= (vector-length p1) (vector-length p2))
       (raise-herbie-error "Mismatched array lengths for midpoint: ~a vs ~a"
                           (vector-length p1)
                           (vector-length p2)))
     (for/vector #:length (vector-length p1)
                 ([x1 (in-vector p1)]
                  [y1 (in-vector p2)])
       (midpoint x1 y1 elem-repr))]
    [_
     ((representation-ordinal->repr repr) (floor (/ (+ ((representation-repr->ordinal repr) p1)
                                                       ((representation-repr->ordinal repr) p2))
                                                    2)))]))

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
  (match (representation-type repr)
    ['array
     (define elem-repr (array-representation-elem repr))
     (define len (apply * (array-representation-dims repr)))
     (for/vector #:length len
                 ([i (in-range len)])
       (random-generate elem-repr))]
    ['bool (zero? (random-integer 0 2))]
    [_
     (define bits (sub1 (representation-total-bits repr)))
     ((representation-ordinal->repr repr) (random-integer (- (expt 2 bits)) (expt 2 bits)))]))

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
    (match (representation-type repr)
      ['array
       (define len (apply * (array-representation-dims repr)))
       (define vals
         (cond
           [(vector? x)
            (unless (= (vector-length x) len)
              (error 'real->repr
                     "Expected vector of length ~a for ~a, got ~a"
                     len
                     (representation-name repr)
                     x))
            x]
           [(real? x) (make-vector len x)]
           [else
            (error 'real->repr
                   "Expected real or vector input for ~a, got ~a"
                   (representation-name repr)
                   x)]))
       ((representation-bf->repr repr) (for/vector #:length len
                                                   ([v (in-vector vals)])
                                         (bf v)))]
      [_ ((representation-bf->repr repr) (bf x))])))

(define (repr->real x repr)
  (match x
    [(? boolean?) x]
    [_
     (match (representation-type repr)
       ['array
        (for/vector ([v (in-vector ((representation-repr->bf repr) x))])
          (bigfloat->real v))]
       [_ (bigfloat->real ((representation-repr->bf repr) x))])]))
