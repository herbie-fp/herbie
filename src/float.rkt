#lang racket

(require math/bigfloat)
(require "common.rkt" "interface.rkt" "syntax/types.rkt"
         "syntax/syntax.rkt" "errors.rkt")
(module+ test (require rackunit))

(provide midpoint ulp-difference ulps->bits bit-difference
         </total <=/total =-or-nan? nan?-all-types ordinary-value?
         exact-value? val-to-type flval
         ->flonum ->bf random-generate fl->repr repr->fl value->string
         <-all-precisions mk-<= special-value? get-representation*)

(define (get-representation* x)
  (match x
    ['real (get-representation (if (flag-set? 'precision 'double) 'binary64 'binary32))]
    [x (get-representation x)]))

(define (ulp-difference x y repr)
  (if (and (complex? x) (complex? y) (not (real? x)) (not (real? y)))
    (+ (ulp-difference (real-part x) (real-part y) (get-representation 'binary64))
       (ulp-difference (imag-part x) (imag-part y) (get-representation 'binary64)))
    (let ([->ordinal (representation-repr->ordinal repr)])
      (- (->ordinal y) (->ordinal x)))))

;; Returns the midpoint of the representation's ordinal values,
;; not the real-valued midpoint
(define (midpoint p1 p2 repr)
  ((representation-ordinal->repr repr)
   (floor (/ (+ ((representation-repr->ordinal repr) p1)
                ((representation-repr->ordinal repr) p2))
             2))))

(define/contract (ulps->bits x)
  (-> finite? finite?)
  (log x 2))

(define (bit-difference x y repr)
  (ulps->bits (+ 1 (abs (ulp-difference x y repr))) repr))

(define (random-generate repr)
  ((representation-ordinal->repr repr) (random-exp (representation-total-bits repr))))

(define (special-value? x repr)
  (set-member? (representation-special-values repr) x))

(define (ordinary-value? x repr)
  (if (and (complex? x) (not (real? x)))
      ;; TODO: Once complex is a separate type rather than a repr, check to see
      ;; what repr the complex implementation is using
      (and (ordinary-value? (real-part x) (get-representation 'binary64))
           (ordinary-value? (imag-part x) (get-representation 'binary64)))
      (not (special-value? x repr))))

(module+ test
  (define binary64 (get-representation 'binary64))
  (check-true (ordinary-value? 2.5 binary64))
  (check-false (ordinary-value? +nan.0 binary64))
  (check-false (ordinary-value? -inf.0 binary64)))

(define (=-or-nan? x1 x2 repr)
  (define ->ordinal (representation-repr->ordinal repr))
  (or (= (->ordinal x1) (->ordinal x2))
      (if (real? x1) ; Infinities are considered special values for real reprs for some reason
          (and (nan? x1) (nan? x2))
          (and (special-value? x1 repr) (special-value? x2 repr)))))

(define (</total x1 x2 repr)
  (cond
   [(and (complex? x1) (complex? x2) (not (real? x1)) (not (real? x2)))
    (error "Complex numbers are unordered")]
   [(and (real? x1) (real? x2))
    (cond [(nan? x1) #f] [(nan? x2) #t] [else (< x1 x2)])]
   [else
    (cond
     [(set-member? (representation-special-values repr) x1) #f]
     [(set-member? (representation-special-values repr) x2) #t]
     [else (< ((representation-repr->ordinal repr) x1)
              ((representation-repr->ordinal repr) x2))])]))

(define (nan?-all-types x repr)
  (if (or (real? x) (complex? x))
      (nan? x)
      (set-member? (representation-special-values repr) x)))

(define (<=/total x1 x2 repr)
  (or (</total x1 x2 repr) (=-or-nan? x1 x2 repr)))

(define (exact-value? type val)
  (match type
    [(or 'real 'complex) (exact? val)]
    ['boolean true]
    [_ false]))

(define (val-to-type type val)
  (match type
    ['real val]
    ['complex (if (real? val) `(complex ,val 0) val)]
    ['boolean (if val 'TRUE 'FALSE)]
    [_ (error "Unknown type" type)]))

(define (flval x repr)
  (match x
    [(? real?)
     (match x
       [(? rational?) x]
       [(or -inf.0 -inf.f) (hash 'type "real" 'value "-inf")]
       [(or +inf.0 +inf.f) (hash 'type "real" 'value "+inf")]
       [(or +nan.0 +nan.f) (hash 'type "real" 'value "NaN")])]
    [(? complex?) (hash 'type "complex" 'real (real-part x) 'imag (imag-part x))]
    [_ (hash 'type (~a repr) 'ordinal (~a ((representation-repr->ordinal repr) x)))]))

(define/contract (->flonum x repr)
  (-> any/c representation? value?)
  (define type (representation-type repr))
  (match x
   [(? (type-exact? type))
    ((type-inexact->exact? type) ((type-exact->inexact type) x))]
   [(? (type-inexact? type))
    ((type-inexact->exact type) x)]
   [(and (? symbol?) (? constant?))
    (->flonum ((constant-info x 'fl)) repr)]))

(define (fl->repr x repr)
  ((representation-exact->repr repr) x))

(define (repr->fl x repr)
  (bigfloat->flonum ((representation-repr->bf repr) x)))

(define (value->string n repr)
  ;; Prints a number with relatively few digits
  (define n* (if (and (number? n) (exact? n)) (exact->inexact n) n))
  (define ->bf (representation-repr->bf repr))
  (define <-bf (representation-bf->repr repr))
  ;; Linear search because speed not an issue
  (let loop ([precision 16])
    (if (> precision (*max-mpfr-prec*))
      (begin
        (warn 'value-to-string #:url "faq.html#value-to-string"
               "Could not uniquely print ~a" n)
        n)
      (parameterize ([bf-precision precision])
        (define bf (->bf n*))
        (if (=-or-nan? n* (<-bf bf) repr)
            (match (bigfloat->string bf)
              ["-inf.bf" "-inf.0"]
              ["+inf.bf" "+inf.0"]
              ["+nan.bf" "+nan.0"]
              [x x])
            (loop (+ precision 4))))))) ; 2^4 > 10

(define/contract (->bf x repr)
  (-> any/c representation? bigvalue?)
  (cond
   [(and (symbol? x) (constant? x)) ((constant-info x 'bf))]
   [(and (complex? x) (not (real? x)))
    (bigcomplex (bf (real-part x)) (bf (imag-part x)))]
   [else
    ;; TODO(interface): Once we have complex numbers as types rather than
    ;; reprs, we don't have to do this additional check abd we can just use
    ;; repr->bf for everything.
    (if (eq? (representation-name repr) 'complex)
      (bf x)
      ((representation-repr->bf repr) x))]))

(define (<-all-precisions x1 x2 repr)
  (cond
   [(or (real? x1) (complex? x1))
    (< x1 x2)]
   [else
    (define ->ordinal (representation-repr->ordinal repr))
    (< (->ordinal x1) (->ordinal x2))]))

(define (mk-<= repr var val)
  (define (cast x)
    (match (representation-name repr)
      ['posit8 `(real->posit8 ,x)] ['posit16 `(real->posit16 ,x)] ['posit32 `(real->posit32 ,x)]
      ['quire8 `(real->quire8 ,x)] ['quire16 `(real->quire16 ,x)] ['quire32 `(real->quire32 ,x)]
      [(or 'binary64 'binary32) x]))
  (define prec-point (cast (repr->fl val repr)))
  (define <=-operator
    (match (representation-name repr)
      [(or 'binary64 'binary32) '<=] 
      ['posit8 `<=.p8] ['posit16 `<=.p16] ['posit32 `<=.p32]
      ['quire8 `<=.p8] ['quire16 `<=.q16] ['quire32 `<=.q32]))
  (list <=-operator var prec-point))
