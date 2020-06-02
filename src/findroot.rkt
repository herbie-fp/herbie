#lang racket
(require math/bigfloat rival
         (only-in fpbench interval))
(require "common.rkt" "programs.rkt" "interface.rkt")

(module+ test (require rackunit))

(provide find-ranges range-table->intervals)

(define (done? <-bf iv)
  (match-define (ival (app <-bf lo) (app <-bf hi)) iv)
  (< (- hi lo) 0.5)
  #;(or (and (number? lo) (= lo hi)) (equal? lo hi)))

(define (find-ranges prog repr #:depth [depth 128] #:initial [initial #f])
  (define vars (program-variables prog))
  (define body (program-body prog))
  (define fn
    (parameterize ([*var-reprs* (map (λ (x) (cons x repr)) (program-variables prog))])
      (eval-prog `(λ ,vars ,body) 'ival repr)))
  (unless initial
    (set! initial (map (const (ival -inf.bf +inf.bf)) (program-variables prog))))
  (define <-bf (representation-bf->repr repr))
  (reap [sow] (find-intervals fn initial #:fuel depth #:true sow #:unknown sow)))

(define (find-intervals ival-fn ranges #:fuel [depth 128]
                        #:true [true! void] #:false [false! void] #:error [error! void]
                        #:unknown [other! void])
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
      (define midpoint
        (ordinal->bigfloat
         (floor (/ (+ (bigfloat->ordinal (ival-lo range))
                      (bigfloat->ordinal (ival-hi range)))
                   2))))
      (define range-lo (ival (ival-lo range) midpoint))
      (define range-hi (ival midpoint (ival-hi range)))
      (loop (list-set ranges n* range-lo) (add1 n))
      (loop (list-set ranges n* range-hi) (add1 n))])))

(define (range-table->intervals range-table variables repr)
  (apply cartesian-product
   (for/list ([var-name variables])
     (map (curry fpbench-ival->ival repr) (hash-ref range-table var-name)))))

(define (fpbench-ival->ival repr fpbench-interval)
  (define <-exact (compose (representation-bf->repr repr) bf))
  (match-define (interval (app <-exact lo) (app <-exact hi) lo? hi?) fpbench-interval)
  (when (not (and lo? hi?))
    (raise (error "not inclusive intervals")))
  
  (ival (bf lo) (bf hi)))
           
(module+ test
  (define test-table-simple
    (make-hash
     `((a . (,(interval 0.0 1.0 #t #t)))
       (b . (,(interval 0.0 1.0 #t #t))))))
  (check-equal? (range-table->intervals test-table-simple `(a b) (get-representation 'binary64))
                (list (list (ival (bf 0.0) (bf 1.0)) (ival (bf 0.0) (bf 1.0))))))
                
          
