#lang racket
(require math/bigfloat rival)
(require "common.rkt" "programs.rkt" "interface.rkt")

(module+ test (require rackunit))

(provide find-ranges)

(define (done? <-bf iv)
  (match-define (ival (app <-bf lo) (app <-bf hi)) iv)
  (< (- hi lo) 0.5)
  #;(or (and (number? lo) (= lo hi)) (equal? lo hi)))

(define (find-ranges prog repr #:depth [depth 128] #:initial [initial #f]  #:eval-fn [eval-fn #f] #:rounding-repr [rounding-repr #f])
  (define vars (program-variables prog))
  (define body (program-body prog))

  (unless eval-fn
    (set! eval-fn 
          (parameterize ([*var-reprs* (map (λ (x) (cons x repr)) (program-variables prog))])
            (eval-prog `(λ ,vars ,body) 'ival repr))))
  
  (unless initial
    (set! initial (map (const (ival -inf.bf +inf.bf)) (program-variables prog))))
  (define <-bf (representation-bf->repr repr))
  (reap [sow] (find-intervals eval-fn initial #:fuel depth #:true sow #:unknown sow #:rounding-repr rounding-repr)))


(define (round-midpoint midpoint lo hi repr)
  
  (define (round point dir)
    ((representation-repr->bf repr)
     (parameterize ([bf-rounding-mode dir])
       ((representation-bf->repr repr) point))))
  
  (define lower (round midpoint 'down))
  (define higher
    (cond
      [(equal? lower (round midpoint 'up))
       (round (bfnext midpoint) 'up)]
      [else (round midpoint 'up)]))

  (when (equal? lower higher)
    (error 'round-midpoint "Bigfloat at precision 80 not refinement of representation"))
  
  (if (and (bf>= lower lo) (bf<= higher hi))
      (cons lower higher)
      #f))

(define (find-intervals ival-fn ranges #:fuel [depth 128]
                        #:true [true! void] #:false [false! void] #:error [error! void]
                        #:unknown [other! void] #:rounding-repr [rounding-repr #f])
  (if (empty? ranges)
      empty
      (let loop ([ranges ranges] [n 0])
        (define n* (remainder n (length ranges)))
        (define range (list-ref ranges n*))
        (define ival-res (apply ival-fn ranges))
        (define-values (ylo yhi yerr? yerr) (values (ival-lo ival-res) (ival-hi ival-res)
                                                    (ival-err? ival-res) (ival-err ival-res)))
        
        ;(printf "~a. ~a -> (~a ~a ~a ~a)\n" (make-string n #\space) ranges ylo yhi yerr? yerr)
        (cond
          [yerr
           (error! ranges)]
          [(and (not yerr?) ylo)
           (true! ranges)]
          [(and (not yerr?) (not yhi))
           (false! ranges)]
          [(> n depth)
           (other! ranges)]
          [else
           (define midpoint-unrounded
             (ordinal->bigfloat
              (floor (/ (+ (bigfloat->ordinal (ival-lo range))
                           (bigfloat->ordinal (ival-hi range)))
                        2))))

           (define midpoints
             (if rounding-repr
                 (round-midpoint midpoint-unrounded (ival-lo range) (ival-hi range) rounding-repr)
                 (cons midpoint-unrounded midpoint-unrounded)))
           (cond
             [midpoints
              (define range-lo (ival (ival-lo range) (car midpoints)))
              (define range-hi (ival (cdr midpoints) (ival-hi range)))
              (loop (list-set ranges n* range-lo) (add1 n))
              (loop (list-set ranges n* range-hi) (add1 n))]
             [else (other! ranges)])]))))  
          
