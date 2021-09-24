#lang racket


(require math/bigfloat math/base)
(require "common.rkt" "interface.rkt" "syntax/types.rkt" "errors.rkt")
(module+ test (require rackunit "load-plugin.rkt"))

(provide 
 ordinary-value?
 largest-ordinary-value bound-ordinary-values
 ulp-difference ulps->bits
 midpoint random-generate
 </total <=/total =-or-nan?
 value->string value->json)

(define (special-value? x repr)
  ((representation-special-values repr) x))

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

(define (ordinary-value? x repr)
  (if (and (complex? x) (not (real? x)))
      ;; TODO: Once complex is a separate type rather than a repr, check to see
      ;; what repr the complex implementation is using
      (and (ordinary-value? (real-part x) (get-representation 'binary64))
           (ordinary-value? (imag-part x) (get-representation 'binary64)))
      (not (special-value? x repr))))


(define (largest-ordinary-value repr)
  (define inf-in-repr ((representation-bf->repr repr) +inf.bf))
  ((representation-repr->bf repr)
   ((representation-ordinal->repr repr)
    (if (bfinfinite? ((representation-repr->bf repr) inf-in-repr))
        (- ((representation-repr->ordinal repr) inf-in-repr) 1)  ;; actually corresponds to +inf.0 (binary64)
        ((representation-repr->ordinal repr) inf-in-repr))))) ;; just the max val (fixed point)
        
(define (has-infinite-value? repr)
  (let ([repr->bf (representation-repr->bf repr)]
        [bf->repr (representation-bf->repr repr)])
    (bfinfinite? (repr->bf (bf->repr +inf.bf)))))

;; If the representation uses saturation instead of overflow
;; if x * x != inf when x is +max, probably saturated
(define (probably-saturated? ordinal repr)
  (define bfval (ordinal->bigfloat ordinal))
  (define bfval2 (bf* bfval bfval)) ;; squaring is probably good enough
  (define ->repr (representation-bf->repr repr))
  (define ->ordinal (representation-repr->ordinal repr))
  (= (->ordinal (->repr bfval))
     (->ordinal (->repr bfval2))))

(define (bound-ordinary-values repr)
  (if (has-infinite-value? repr) ; bail if representation does not have +inf
      (parameterize ([bf-rounding-mode 'nearest])
        (define hi-ordinal (bigfloat->ordinal (largest-ordinary-value repr)))
        (if (probably-saturated? hi-ordinal repr) ; bail if representation uses saturation
            (ordinal->bigfloat hi-ordinal)
            (let loop ([ordinal hi-ordinal] [stepsize 1])
              (define bfval (ordinal->bigfloat ordinal))
              (define ->repr (representation-bf->repr repr))
              (define ->bf (representation-repr->bf repr))
              (if (bfinfinite? (->bf (->repr bfval)))
                  bfval
                  (loop (+ ordinal stepsize) (* stepsize 2))))))
      (largest-ordinary-value repr)))

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
     [(special-value? x1 repr) #f]
     [(special-value? x2 repr) #t]
     [else (< ((representation-repr->ordinal repr) x1)
              ((representation-repr->ordinal repr) x2))])]))

(define (<=/total x1 x2 repr)
  (or (</total x1 x2 repr) (=-or-nan? x1 x2 repr)))

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

(define (value->string n repr)
  ;; Prints a number with relatively few digits
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
        (define bf (->bf n))
        (if (=-or-nan? n (<-bf bf) repr)
            (match (bigfloat->string bf)
              ["-inf.bf" "-inf.0"]
              ["+inf.bf" "+inf.0"]
              ["+nan.bf" "+nan.0"]
              [x x])
            (loop (+ precision 4))))))) ; 2^4 > 10
