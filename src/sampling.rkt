#lang racket
(require math/bigfloat rival math/base
         (only-in fpbench interval range-table-ref condition->range-table [expr? fpcore-expr?]))
(require "searchreals.rkt" "common.rkt" "programs.rkt" "config.rkt" "errors.rkt" "float.rkt"
         "interface.rkt" "timeline.rkt" "syntax/types.rkt")

(module+ test (require rackunit))

(module+ internals (provide make-sampler))

(provide make-sampler)

(define (precondition->hyperrects precondition reprs)
  (define precondition*
    (if (and (fpcore-expr? (program-body precondition)) (flag-set? 'setup 'search))
        (program-body precondition)
        'TRUE))
  (define range-table (condition->range-table precondition*))

  (define hyperrects
    (apply cartesian-product
           (for/list ([var-name (program-variables precondition)] [repr reprs])
             (map (lambda (interval) (fpbench-ival->ival repr interval))
                  (range-table-ref range-table var-name)))))

  (map (curryr cons 'other) hyperrects))

(define (fpbench-ival->ival repr fpbench-interval)
  (match-define (interval lo hi lo? hi?) fpbench-interval)
  (match (type-name (representation-type repr))
    ['real
     (define bound (bound-ordinary-values repr))
     (define bflo (match lo [-inf.0 (bf- bound)] [+inf.0 bound] [x (bf x)]))
     (define bfhi (match hi [-inf.0 (bf- bound)] [+inf.0 bound] [x (bf x)]))
     (ival (bfstep bflo (if lo? 0 1)) (bfstep bfhi (if hi? 0 -1)))]
    ['bool
     (ival #f #t)]))

(define (hyperrect-weight hyperrect reprs)
  (apply * (for/list ([interval (in-list (car hyperrect))] [repr (in-list reprs)])
             (define ->ordinal (compose (representation-repr->ordinal repr)
                                        (representation-bf->repr repr)))
             (+ 1 (- (->ordinal (ival-hi interval)) (->ordinal (ival-lo interval)))))))

(define (partial-sums v)
  (define out (make-vector (vector-length v) 0))
  (let loop ([sum 0] [i 0])
    (if (< i (vector-length v))
        (let ([sum* (+ sum (vector-ref v i))])
          (vector-set! out i sum*)
          (loop sum* (+ i 1)))
        out)))

;; we want a index i such that vector[i] > num and vector[i-1] <= num
;; assumes vector strictly increasing
(define (binary-search vector num)
  (let loop ([left 0] [right (- (vector-length vector) 1)])
    (cond
     [(>= left right)
      (min left (- (vector-length vector) 1))]
     [else
      (define mid (floor (/ (+ left right) 2)))
      (define pivot (vector-ref vector mid))
      (if (<= pivot num) (loop (+ 1 mid) right) (loop left mid))])))

(define (choose-hyperrect hyperrects weights)
  (define weight-max (vector-ref weights (- (vector-length weights) 1)))
  (define rand-ordinal (random-integer 0 weight-max))
  (vector-ref hyperrects (binary-search weights rand-ordinal)))

(define (sample-multi-bounded hyperrects weights reprs)
  (define hyperrect (choose-hyperrect hyperrects weights))

  (for/list ([interval (car hyperrect)] [repr reprs])
    (define ->ordinal (compose (representation-repr->ordinal repr) (representation-bf->repr repr)))
    (define <-ordinal (representation-ordinal->repr repr))
    (<-ordinal (random-integer (->ordinal (ival-lo interval))
                               (+ 1 (->ordinal (ival-hi interval)))))))

(define (is-finite-interval repr)
  (define bound (bound-ordinary-values repr))
  (match (type-name (representation-type repr))
    ['real
     (λ (interval)
       (define not-number? (and (or (bfnan? (ival-lo interval)) (bfnan? (ival-hi interval)))))
       (ival-or (ival-bool not-number?) (ival-< (mk-ival (bf- bound)) interval (mk-ival bound))))]
    ['bool (const (ival-bool #t))]))

(define (is-samplable-interval repr)
  (define <-bf (representation-bf->repr repr))
  (match (type-name (representation-type repr))
    ['real
     (λ (interval)
       (match-define (ival (app <-bf lo) (app <-bf hi)) interval)
       (define lo! (ival-lo-fixed? interval))
       (define hi! (ival-hi-fixed? interval))
       (define lo=hi (or (equal? lo hi) (and (number? lo) (= lo hi))))
       (define v
         (and (not (ival-err? interval))
              (or (and lo! hi! (not lo=hi))
                  (and lo! (bigfloat? (ival-lo interval)) (bfinfinite? (ival-lo interval)))
                  (and hi! (bigfloat? (ival-hi interval)) (bfinfinite? (ival-hi interval))))))
       (ival-bool (not v)))]
    ['bool (const (ival-bool #t))]))

(define (valid-result? repr)
  (define ival-finite? (is-finite-interval repr))
  (define ival-samplable? (is-samplable-interval repr))
  (λ (ival-vec)
    (match-define (list ival-pre ival-bodies ...) (vector->list ival-vec))
    (define x
      (apply ival-and
             ival-pre
             (append
              (map ival-finite? ival-bodies)
              (map ival-samplable? ival-bodies) ; Arguable whether this should be here
              (map (compose ival-not ival-error?) ival-bodies))))
    x))

(define/contract (log-space-improvement from-search from-analysis reprs)
  (-> (non-empty-listof any/c) any/c (listof representation?) void?)

  (define (total-weight hyperrects)
    (exact->inexact (apply + (map (curryr hyperrect-weight reprs) hyperrects))))

  (define total (expt 2 (apply + (map representation-total-bits reprs))))
  (define fpcore (total-weight from-analysis))
  (define after (total-weight from-search))
  (define good (total-weight (filter (lambda (rect) (equal? (cdr rect) 'true)) from-search)))

  (timeline-push! 'sampling (/ fpcore total) (/ after total) (/ good after)))

(define (get-hyperrects precondition programs reprs repr)
  (define hyperrects-analysis (precondition->hyperrects precondition reprs))

  (define fuel (floor (max 0 (- (*max-find-range-depth*) (log (length hyperrects-analysis) 2)))))

  (define search-func
    (compose (valid-result? repr) (batch-eval-progs (cons precondition programs) 'ival repr)))

  (define hyperrects-search
    (reap [sow]
          (for ([(rect outcome) (in-dict hyperrects-analysis)])
            (find-intervals search-func rect sow #:reprs reprs #:fuel fuel))))

  (when (empty? hyperrects-search)
    (raise-herbie-sampling-error "No valid values." #:url "faq.html#no-valid-values"))

  (log-space-improvement hyperrects-search hyperrects-analysis reprs)

  hyperrects-search)




; These definitions in place, we finally generate the points.
(define (make-sampler repr precondition . programs)
  (define reprs (map (curry dict-ref (*var-reprs*)) (program-variables precondition)))

  (unless (for/and ([repr reprs]) (> (bf-precision) (representation-total-bits repr)))
    (error 'make-sampler "Bigfloat precision ~a not sufficient to refine representations ~a"
           (bf-precision) reprs))

  (cond
   ;; TODO: unclear why we need to make these representations special
   [(and (set-member? '(binary32 binary64) (representation-name repr))
         (flag-set? 'setup 'search)
         (expr-supports? (program-body precondition) 'ival)
         (andmap (compose (curryr expr-supports? 'ival) program-body) programs)
         (not (empty? reprs)))

    (define hyperrects (list->vector (get-hyperrects precondition programs reprs repr)))
    (define weights (partial-sums (vector-map (curryr hyperrect-weight reprs) hyperrects)))
    (λ () (sample-multi-bounded hyperrects weights reprs))]
   [else
    (λ () (map random-generate reprs))]))

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

  (check-equal? (precondition->hyperrects '(λ (a b) (and (<= 0 a 1) (<= 0 b 1))) (list repr repr))
                (list (cons (list (ival (bf 0.0) (bf 1.0)) (ival (bf 0.0) (bf 1.0))) 'other))))
