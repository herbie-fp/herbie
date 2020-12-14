#lang racket
(require math/bigfloat rival math/base
         (only-in fpbench interval range-table-ref condition->range-table [expr? fpcore-expr?]))
(require "searchreals.rkt" "programs.rkt" "config.rkt" "errors.rkt" "float.rkt"
         "interface.rkt" "timeline.rkt" "syntax/types.rkt")
(module+ test (require rackunit))
(provide make-sampler)

(define (precondition->hyperrects precondition reprs repr)
  ;; FPBench needs unparameterized operators
  (define range-table 
    (condition->range-table  
      (resugar-program (program-body precondition) repr #:full #f)))

  (apply cartesian-product
         (for/list ([var-name (program-variables precondition)] [repr reprs])
           (map (lambda (interval) (fpbench-ival->ival repr interval))
                (range-table-ref range-table var-name)))))

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

  (for/list ([interval hyperrect] [repr reprs])
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

(define (get-hyperrects precondition programs reprs repr)
  (define hyperrects-analysis (precondition->hyperrects precondition reprs repr))
  (cond
    [(flag-set? 'setup 'search)
     (define search-func
       (compose (valid-result? repr) (batch-eval-progs (cons precondition programs) 'ival repr)))
     (find-intervals search-func hyperrects-analysis #:reprs reprs #:fuel (*max-find-range-depth*))]
    [else
     hyperrects-analysis]))

(define (list-set-multiple list indicies values)
  (let loop ([current list] [indicies indicies] [values values] [index 0])
    (cond
      [(empty? current)
       empty]
      [(and (not (empty? indicies)) (equal? (first indicies) index))
       (cons (first values) (loop (rest current) (rest indicies) (rest values) (+ index 1)))]
      [else
       (cons (first current) (loop (rest current) indicies values (+ index 1)))])))

(define (sort-group variables point sort-group)
  (define indicies
    (sort (map (lambda (var) (index-of variables var)) (symmetry-group-variables sort-group)) <))
  (define sorted
    (sort (map (curry list-ref point) indicies) <))
  (list-set-multiple point indicies sorted))

(define (apply-preprocess variables sampled-point preprocess-structs)
  (cond
    [(empty? preprocess-structs)
     sampled-point]
    ;; Add more preprocess cases here- for now, only symmetry-group exists
    [else
     (apply-preprocess variables (sort-group variables sampled-point (first preprocess-structs)) (rest preprocess-structs))]))

; These definitions in place, we finally generate the points.
(define (make-sampler repr precondition programs preprocess-structs)
  (define reprs (map (curry dict-ref (*var-reprs*)) (program-variables precondition)))

  (unless (for/and ([repr reprs]) (> (bf-precision) (representation-total-bits repr)))
    (error 'make-sampler "Bigfloat precision ~a not sufficient to refine representations ~a"
           (bf-precision) reprs))

  (cond
   [(and (andmap (curry equal? 'real) (map (compose type-name representation-type) (cons repr reprs)))
         (expr-supports? (program-body precondition) 'ival)
         (andmap (compose (curryr expr-supports? 'ival) program-body) programs)
         (not (empty? reprs)))
    (timeline-push! 'method "search")
    (define hyperrects (list->vector (get-hyperrects precondition programs reprs repr)))
    (when (empty? hyperrects)
      (raise-herbie-sampling-error "No valid values." #:url "faq.html#no-valid-values"))
    (define weights (partial-sums (vector-map (curryr hyperrect-weight reprs) hyperrects)))
    (λ () (apply-preprocess (program-variables precondition) (sample-multi-bounded hyperrects weights reprs) preprocess-structs))]
   [else
    (timeline-push! 'method "random")
    (λ () (apply-preprocess (program-variables precondition) (map random-generate reprs) preprocess-structs))]))

(module+ test
  (define repr (get-representation 'binary64))
  (check-true
   (andmap (curry set-member? '(0.0 1.0))
           (sample-multi-bounded
            (vector (list (ival (bf 0) (bf 0)) (ival (bf 1) (bf 1))))
            (vector 1)
            (list repr repr))))

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

  (check-equal? (precondition->hyperrects '(λ (a b) (and (<=.f64 0 a 1) (<=.f64 0 b 1))) (list repr repr) repr)
                (list (list (ival (bf 0.0) (bf 1.0)) (ival (bf 0.0) (bf 1.0))))))
