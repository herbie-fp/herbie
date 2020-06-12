#lang racket
(require math/bigfloat rival)
(require "common.rkt" "programs.rkt" "interface.rkt")

(module+ test (require rackunit))

(provide find-intervals)

(define (repr-round repr dir point)
  ((representation-repr->bf repr)
   (parameterize ([bf-rounding-mode dir])
     ((representation-bf->repr repr) point))))

(define (get-midpoints lo hi repr)
  ; Midpoint is taken in repr-space, but values are stored in bf
  (define <-ordinal (compose (representation-repr->bf repr) (representation-ordinal->repr repr)))
  (define ->ordinal (compose (representation-repr->ordinal repr) (representation-bf->repr repr)))

  (define lower (<-ordinal (floor (/ (+ (->ordinal hi) (->ordinal lo)) 2))))
  (define higher (repr-round repr 'up (bfnext lower))) ; repr-next

  (and (bf>= lower lo) (bf<= higher hi) ; False if lo and hi were already close together
       (cons lower higher)))

(define (find-intervals ival-fn ranges callback #:reprs reprs #:fuel [depth 128])
  (if (empty? ranges)
      empty
      (let loop ([ranges ranges] [n 0])
        (define n* (remainder n (length ranges)))
        (define range (list-ref ranges n*))
        (define ival-res (apply ival-fn ranges))
        (define-values (ylo yhi yerr? yerr)
          (values (ival-lo ival-res) (ival-hi ival-res) (ival-err? ival-res) (ival-err ival-res)))
        
        (cond
          [yerr
           (void 'error)]
          [(and (not yerr?) ylo)
           (callback (cons ranges 'true))]
          [(not yhi)
           (void 'false)] ; Necessarily false, possibly erroneous
          [(> n depth)
           (callback (cons ranges 'other))]
          [else
           (define repr (list-ref reprs n*))
           (match (get-midpoints (ival-lo range) (ival-hi range) repr)
             [(cons midleft midright)
              (define range-lo (ival (ival-lo range) midleft))
              (define range-hi (ival midright (ival-hi range)))
              (loop (list-set ranges n* range-lo) (add1 n))
              (loop (list-set ranges n* range-hi) (add1 n))]
             [#f
              (callback (cons ranges 'other))])]))))  
          
