#lang racket


(require math/bigfloat math/base)
(require "common.rkt" "interface.rkt" "syntax/types.rkt"
         "syntax/syntax.rkt" "errors.rkt")
(module+ test (require rackunit))

(provide 
 ordinary-value?
 largest-ordinary-value bound-ordinary-values
 ulp-difference ulps->bits
 midpoint random-generate
 </total <=/total =-or-nan?
 exact-value? value->code
 value->string value->json
 ->flonum ->bf
 fl->repr repr->fl)

(define (ulp-difference x y repr)
  (if (and (complex? x) (complex? y) (not (real? x)) (not (real? y)))
    (+ (ulp-difference (real-part x) (real-part y) (get-representation 'binary64))
       (ulp-difference (imag-part x) (imag-part y) (get-representation 'binary64)))
    (let ([->ordinal (representation-repr->ordinal repr)])
      (+ 1 (abs (- (->ordinal y) (->ordinal x)))))))

;; Returns the midpoint of the representation's ordinal values,
;; not the real-valued midpoint
(define (midpoint p1 p2 repr)
  ((representation-ordinal->repr repr)
   (floor (/ (+ ((representation-repr->ordinal repr) p1)
                ((representation-repr->ordinal repr) p2))
             2))))

(define (ulps->bits x) (log x 2))

(define (random-generate repr)
  ((representation-ordinal->repr repr) (random-bits (representation-total-bits repr))))

(define (special-value? x repr)
  (if (set-member? '(binary32 binary64) (representation-name repr))
      (or (infinite? x) (nan? x))
      (set-member? (representation-special-values repr) x)))

(define (ordinary-value? x repr)
  (if (and (complex? x) (not (real? x)))
      ;; TODO: Once complex is a separate type rather than a repr, check to see
      ;; what repr the complex implementation is using
      (and (ordinary-value? (real-part x) (get-representation 'binary64))
           (ordinary-value? (imag-part x) (get-representation 'binary64)))
      (not (special-value? x repr))))


(define (largest-ordinary-value repr)
  ((representation-repr->bf repr)
   ((representation-ordinal->repr repr)
    (- ((representation-repr->ordinal repr) ((representation-bf->repr repr) +inf.bf)) 1))))

(define (bound-ordinary-values repr)
  (parameterize ([bf-rounding-mode 'nearest])
    (let loop ([ordinal (bigfloat->ordinal (largest-ordinary-value repr))] [stepsize 1])
      (define bfval (ordinal->bigfloat ordinal))
      (if (bfinfinite? ((representation-repr->bf repr) ((representation-bf->repr repr) bfval)))
          bfval
          (loop (+ ordinal stepsize) (* stepsize 2))))))

(module+ test
  (define binary64 (get-representation 'binary64))
  (check-true (ordinary-value? 2.5 binary64))
  (check-false (ordinary-value? +nan.0 binary64))
  (check-false (ordinary-value? -inf.0 binary64))
  (define binary32 (get-representation 'binary32))
  (check-true (ordinary-value? 2.5f0 binary32))
  (check-false (ordinary-value? +nan.f binary32))
  (check-false (ordinary-value? -inf.f binary32)))

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

(define (<=/total x1 x2 repr)
  (or (</total x1 x2 repr) (=-or-nan? x1 x2 repr)))

(define (exact-value? type val)
  (match type
    [(or 'real 'complex) (exact? val)]
    ['boolean true]
    [_ false]))

(define (value->code type val)
  (match type
    ['real val]
    ['complex (list 'complex (real-part val) (imag-part val))]
    ['boolean (if val 'TRUE 'FALSE)]))

(define (value->json x repr)
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
    ((type-inexact->exact type) ((type-exact->inexact type) x))]
   [(? (type-inexact? type))
    ((type-inexact->exact type) x)]))

(define (fl->repr x repr)
  ((representation-bf->repr repr) (bf x)))

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
  (define type (representation-type repr))
  (cond
   [(and ((type-exact? type) x) (equal? (type-name type) 'complex)) ;; HACK
    ((type-exact->inexact type) x)]
   [else
    ;; TODO(interface): ->bf is used to convert syntactic numbers to
    ;; bf values. For 'complex' type, syntactic numbers are still
    ;; reals, so we need to call `bf` here
    (if (eq? (representation-name repr) 'complex)
      (bf x)
      ((representation-repr->bf repr) x))]))
