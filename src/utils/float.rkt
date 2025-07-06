#lang racket

(require math/bigfloat
         math/base
         math/flonum)
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
         repr->real
         shift
         unshift
         <bool>
         <binary32>
         <binary64>
         fl32-
         fl32+
         fl32*
         fl32/)

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(define (ulp-difference x y repr)
  (define ->ordinal (representation-repr->ordinal repr))
  (+ 1 (abs (- (->ordinal y) (->ordinal x)))))

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
  ((representation-ordinal->repr repr) (random-bits (representation-total-bits repr))))

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

(module hairy racket/base
  (require (only-in math/private/bigfloat/mpfr get-mpfr-fun _mpfr-pointer _rnd_t bf-rounding-mode))
  (require ffi/unsafe)
  (provide bigfloat->float32)
  (define mpfr-get-flt (get-mpfr-fun 'mpfr_get_flt (_fun _mpfr-pointer _rnd_t -> _float)))
  (define (bigfloat->float32 x)
    (mpfr-get-flt x (bf-rounding-mode))))
(require (submod "." hairy))

(module+ test
  (require rackunit))

(define (float32->bit-field x)
  (integer-bytes->integer (real->floating-point-bytes x 4) #f #f))

(define (float32->ordinal x)
  (if (negative? x)
      (- (float32->bit-field (- x)))
      (float32->bit-field (abs x))))

(define (bit-field->float32 x)
  (floating-point-bytes->real (integer->integer-bytes x 4 #f #f) #f))

(define (ordinal->float32 x)
  (if (negative? x)
      (- (bit-field->float32 (- x)))
      (bit-field->float32 x)))

;; Wrapping arithmetic operations with `flsingle` introduces a
;; possible double-rounding problem but, perhaps surprisingly, this
;; double-rounding problem never actually causes error; see:
;;
;;   https://hal.science/hal-01091186/document

(define fl32+ (compose flsingle +))
(define fl32- (compose flsingle -))
(define fl32* (compose flsingle *))
(define fl32/ (compose flsingle /))

(module+ test
  (check-equal? (fl32+ 1.0 2.0) 3.0)
  (check-equal? (fl32- 1.0 2.0) -1.0)
  (check-equal? (fl32* 1.0 2.0) 2.0)
  (check-equal? (fl32/ 1.0 2.0) 0.5))

(define <bool>
  (make-representation #:name 'bool
                       #:type 'bool
                       #:repr? boolean?
                       #:bf->repr identity
                       #:repr->bf identity
                       #:ordinal->repr (λ (x) (= x 0))
                       #:repr->ordinal (λ (x) (if x 1 0))
                       #:total-bits 1
                       #:special-value? (const #f)))

(define <binary32>
  (make-representation #:name 'binary32
                       #:type 'real
                       #:repr? flonum?
                       #:bf->repr bigfloat->float32
                       #:repr->bf (λ (x)
                                    (parameterize ([bf-precision 24])
                                      (bf x)))
                       #:ordinal->repr (shift 31 ordinal->float32)
                       #:repr->ordinal (unshift 31 float32->ordinal)
                       #:total-bits 32
                       #:special-value? nan?))

(define <binary64>
  (make-representation #:name 'binary64
                       #:type 'real
                       #:repr? flonum?
                       #:bf->repr bigfloat->flonum
                       #:repr->bf (λ (x)
                                    (parameterize ([bf-precision 53])
                                      (bf x)))
                       #:ordinal->repr (shift 63 ordinal->flonum)
                       #:repr->ordinal (unshift 63 flonum->ordinal)
                       #:total-bits 64
                       #:special-value? nan?))
