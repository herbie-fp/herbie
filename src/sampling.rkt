#lang racket
(require math/bigfloat rival math/base
         (only-in fpbench interval range-table-ref condition->range-table [expr? fpcore-expr?]))
(require "searchreals.rkt" "common.rkt" "programs.rkt" "config.rkt" "errors.rkt" "float.rkt"
         "interface.rkt" "timeline.rkt")

(module+ test (require rackunit))

(module+ internals (provide make-sampler))

(provide make-sampler)

(define (range-table->hyperrects range-table variables reprs)
  (map (lambda (rect) (cons rect 'other))
       (apply cartesian-product
              (for/list ([var-name variables] [repr reprs])
                (map (lambda (interval) (fpbench-ival->ival repr interval))
                     (range-table-ref range-table var-name))))))

(define (fpbench-ival->ival repr fpbench-interval)
  (match-define (interval lo hi lo? hi?) fpbench-interval)
  (ival (bfstep (bf lo) (if lo? 0 1)) (bfstep (bf hi) (if hi? 0 -1))))

(define (ival-ordinal-size repr interval)
  (define ->ordinal (compose (representation-repr->ordinal repr) (representation-bf->repr repr)))
  (+ 1 (- (->ordinal (ival-hi interval)) (->ordinal (ival-lo interval)))))

(define (hyperrects->weights repr hyperrects)
  (let loop ([current 0] [hyperrects hyperrects])
    (cond
      [(empty? hyperrects) empty]
      [else
       (let ([new-val (+ current (apply * (map (curry ival-ordinal-size repr) (car (first hyperrects)))))])
         (cons new-val
               (loop new-val (rest hyperrects))))])))

(define (rect-space-sum repr hyperrects)
  (define weights (hyperrects->weights repr hyperrects))
  (if
   (empty? weights)
   0
   (last weights)))


;; we want a index i such that vector[i] > num and vector[i-1] <= num
;; assumes vector strictly increasing
(define (binary-search vector num)
  (binary-search-recursive vector num 0 (- (vector-length vector) 1)))


(define (binary-search-recursive vector num left-inclusive right-inclusive)
  (cond
    [(>= left-inclusive right-inclusive)
     (min left-inclusive (- (vector-length vector) 1))]
    [else
     (define mid (floor (/ (+ left-inclusive right-inclusive) 2)))
     (define val (vector-ref vector mid))
     (if
      (<= val num)
      (binary-search-recursive vector num (+ 1 mid) right-inclusive)
      (binary-search-recursive vector num left-inclusive mid))]))

(define (choose-hyperrect hyperrects weights)
  (define weight-max (vector-ref weights (- (vector-length weights) 1)))
  (define rand-ordinal (random-ranges (cons 0 weight-max)))
  (vector-ref hyperrects (binary-search weights rand-ordinal)))    

(define (sample-multi-bounded hyperrects weights reprs)
  (cond
    [(equal? (length reprs) 0)
     empty]
    [else
     (define hyperrect (choose-hyperrect hyperrects weights))
  
     (for/list ([interval (car hyperrect)] [repr reprs])
       (define ->ordinal (compose (representation-repr->ordinal repr) (representation-bf->repr repr)))
       (define <-ordinal (representation-ordinal->repr repr))
       (<-ordinal (random-ranges (cons (->ordinal (ival-lo interval))
                                       (+ 1 (->ordinal (ival-hi interval)))))))]))


(define (make-valid-search precondition programs repr)
  
  ;; max noninfinite repr value in bigfloat
  (define max-repr-noninfinite
    ((representation-repr->bf repr)
     ((representation-ordinal->repr repr)
      (- ((representation-repr->ordinal repr) ((representation-bf->repr repr) +inf.bf)) 1))))

  (define (rounds-infinite? ordinal)
    (define rounded ((representation-repr->bf repr) ((representation-bf->repr repr) (ordinal->bigfloat ordinal))))
    (or (equal? rounded +inf.bf) (equal? rounded -inf.bf)))
  
  (define rounds-to-infinite-repr
    (parameterize ([bf-rounding-mode 'nearest])
      (let loop ([ordinal (bigfloat->ordinal max-repr-noninfinite)] [stepsize 1])
            (if (rounds-infinite? ordinal)
                (ordinal->bigfloat ordinal)
                (loop (+ ordinal stepsize) (* stepsize 2))))))
  (define rounds-to-neg-infinite-repr
    (parameterize ([bf-rounding-mode 'nearest])
      (let loop ([ordinal (- (bigfloat->ordinal rounds-to-infinite-repr))] [stepsize 1])
            (if (rounds-infinite? ordinal)
                (ordinal->bigfloat ordinal)
                (loop (- ordinal stepsize) (* stepsize 2))))))

  (define (is-noninfinite-interval interval)
    (define not-number? (or (boolean? (ival-lo interval)) (bfnan? (ival-lo interval)) (bfnan? (ival-hi interval))))
    (if not-number?
        (ival #t #t)
        (ival-and (ival-< interval (ival rounds-to-infinite-repr rounds-to-infinite-repr))
                  (ival-> interval (ival rounds-to-neg-infinite-repr rounds-to-neg-infinite-repr)))))
        
    
  (parameterize ([*var-reprs* (map (λ (x) (cons x repr)) (program-variables precondition))])
    (compose
     (lambda (ival-vec)
       (define ival-list (vector->list ival-vec))
       (apply ival-and (first ival-list)
              (append (map is-noninfinite-interval (rest ival-list))
                      (map ival-not (map ival-error? (rest ival-list))))))
     (batch-eval-progs (cons precondition programs) 'ival repr))))


(define (log-space-improvement hyperrects from-fpcore repr)
  (cond
    [(empty? hyperrects)
     void]
    [else
     (define true-hyperrects (filter (lambda (rect) (equal? (cdr rect) 'true)) hyperrects))
     (define bf->ordinal (compose (representation-repr->ordinal repr) (representation-bf->repr repr)))
     
     (define total (expt (- (bf->ordinal +inf.bf) (bf->ordinal -inf.bf)) (length (car (first hyperrects)))))
     (define fpcore (rect-space-sum repr from-fpcore))
     (define after (rect-space-sum repr hyperrects))
     (define good (rect-space-sum repr true-hyperrects))


     (define fpcore-percent (exact->inexact (/ fpcore total)))
     (define after-percent (exact->inexact (/ after total)))
     (define good-chance (exact->inexact (/ good after)))
     
     (timeline-log! 'sampling (list (list fpcore-percent after-percent good-chance)))]))


(define (get-hyperrects precondition programs reprs repr log)
  (define range-table
    (condition->range-table
     (if (and (fpcore-expr? (program-body precondition)) (flag-set? 'setup 'search))
         (program-body precondition)
         'TRUE)))
  (define hyperrects-from-fpcore (range-table->hyperrects range-table (program-variables precondition) reprs)) 

  (for ([var (program-variables precondition)])
      (when (null? (range-table-ref range-table var))
        (raise-herbie-sampling-error "No valid values of variable ~a" var
                                     #:url "faq.html#no-valid-values")))
  (cond
    [(or (not (flag-set? 'setup 'search))
         (not (andmap
               (compose (curryr expr-supports? 'ival) program-body)
               (cons precondition programs))))
     hyperrects-from-fpcore]
    [else
     (define adjusted-search-depth
       (max 0 (- (*max-find-range-depth*) (floor (log (length hyperrects-from-fpcore) 2)))))

     (define search-func (make-valid-search precondition programs repr))
     (define hyperrects
       (apply append
              (for/list ([rect hyperrects-from-fpcore])
                (find-ranges precondition repr #:initial (car rect) #:depth adjusted-search-depth
                             #:eval-fn search-func #:rounding-repr repr))))
     
     (when (and (not (equal? (length (program-variables precondition)) 0))
                (empty? hyperrects))
       (raise-herbie-sampling-error "No valid values."
                                    #:url "faq.html#no-valid-values"))

     (log-space-improvement hyperrects hyperrects-from-fpcore repr)
     
     hyperrects]))




; These definitions in place, we finally generate the points.
(define (make-sampler repr precondition . programs)
  
  (define reprs
    (map (curry dict-ref (*var-reprs*)) (program-variables precondition)))

  (parameterize ([bf-precision 80])
    ;; TODO(interface): range tables do not handle representations right now
    ;; They produce +-inf endpoints, which aren't valid values in generic representations
    (define hyperrects (get-hyperrects precondition programs reprs repr log))

    (define weights (list->vector (hyperrects->weights repr hyperrects)))
    
    (define hyperrect-vector
      (list->vector hyperrects))

    (if (set-member? '(binary32 binary64) (representation-name repr))
        (λ ()
          (sample-multi-bounded hyperrect-vector weights reprs))
        (λ () (map random-generate reprs)))))



(module+ test
  (define repr (get-representation 'binary64))
  (check-true
   (andmap (curry set-member? '(0.0 1.0))
           (sample-multi-bounded (list->vector (list (cons (list (ival (bf 0) (bf 0))
                                                                 (ival (bf 1) (bf 1))) 'other)))
                                 (list->vector (list 1)) (list repr repr))))

  (define rand-list
    (let loop ([current 0])
      (if (> current 200)
          empty
          (let ([r (+ current (random-integer 1 10))])
            (cons r (loop r))))))
  (define arr
    (list->vector rand-list))
  (for ([i (range 0 20)])
    (define max-num (vector-ref arr (- (vector-length arr) 1)))
    (define search-for (random-integer 0 max-num))
    (define search-result (binary-search arr search-for))
    (check-true (> (vector-ref arr search-result) search-for))
    (when (> search-result 0)
      (check-true (<= (vector-ref arr (- search-result 1)) search-for))))

  (define test-table-simple
    (make-hash
     `((a . (,(interval 0.0 1.0 #t #t)))
       (b . (,(interval 0.0 1.0 #t #t))))))
  (check-equal? (range-table->hyperrects test-table-simple `(a b)
                                         (list (get-representation 'binary64) (get-representation 'binary64)))
                (list (cons (list (ival (bf 0.0) (bf 1.0)) (ival (bf 0.0) (bf 1.0))) 'other))))
