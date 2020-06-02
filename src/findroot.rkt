#lang racket
(require math/bigfloat)
(require "biginterval.rkt" "common.rkt" "programs.rkt" "interface.rkt"
         "range-analysis.rkt")


(provide find-ranges)

(define (done? <-bf iv)
  (match-define (ival (app <-bf lo) (app <-bf hi) _ _) iv)
  (< (- hi lo) 0.5)
  #;(or (and (number? lo) (= lo hi)) (equal? lo hi)))

(define (find-ranges prog repr #:depth [depth 128] #:initial [initial #f])
  (define vars (program-variables prog))
  (define body (program-body prog))
  (define fn
    (parameterize ([*var-reprs* (map (λ (x) (cons x repr)) (program-variables prog))])
      (eval-prog `(λ ,vars ,body) 'ival repr)))
  (unless initial
    (set! initial (map (const (ival -inf.bf +inf.bf #f #f)) (program-variables prog))))
  (define <-bf (representation-bf->repr repr))
  (reap [sow] (find-intervals fn initial #:fuel depth #:true sow #:unknown sow)))

(define (find-intervals ival-fn ranges #:fuel [depth 128]
                        #:true [true! void] #:false [false! void] #:error [error! void]
                        #:unknown [other! void])
  (let loop ([ranges ranges] [n 0])
    (define n* (remainder n (length ranges)))
    (define range (list-ref ranges n*))
    (match-define (ival ylo yhi yerr? yerr) (apply ival-fn ranges))
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
      (define midpoint
        (ordinal->bigfloat
         (floor (/ (+ (bigfloat->ordinal (ival-lo range))
                      (bigfloat->ordinal (ival-hi range)))
                   2))))
      (define range-lo (struct-copy ival range [hi midpoint]))
      (define range-hi (struct-copy ival range [lo midpoint]))
      (loop (list-set ranges n* range-lo) (add1 n))
      (loop (list-set ranges n* range-hi) (add1 n))])))

(define (range-table->intervals range-table variables)
  (range-combinations
   (for/list ([var-name variables])
     (hash-ref range-table var-name))))

(define (range-combinations range-lists)
  (cond
    [(empty? range-lists) empty]
    [else
     (define rest-combinations (range-combinations (rest range-lists)))
     (append*
      (for/list ([combo rest-combinations])
        (for/list ([plain-interval (first range-lists)])
          (assert (interval-l? plain-interval))
          (assert (interval-r? plain-interval))
          (cons 
           
    
