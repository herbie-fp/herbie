#lang racket
(require math/bigfloat rival math/base
         (only-in fpbench interval range-table-ref condition->range-table [expr? fpcore-expr?]))
(require "searchreals.rkt" "errors.rkt" "common.rkt"
         "float.rkt" "syntax/types.rkt" "timeline.rkt" "config.rkt"
         "syntax/sugar.rkt" "ground-truth.rkt")

(provide batch-prepare-points
         ival-eval
         make-sampler 
         sample-points)

;; Part 1: use FPBench's condition->range-table to create initial hyperrects

(define (precondition->hyperrects pre ctx)
  ;; FPBench needs unparameterized operators
  (define range-table (condition->range-table pre))
  (apply cartesian-product
         (for/list ([var-name (context-vars ctx)] [var-repr (context-var-reprs ctx)])
           (map (lambda (interval) (fpbench-ival->ival var-repr interval))
                (range-table-ref range-table var-name)))))

(define (fpbench-ival->ival repr fpbench-interval)
  (match-define (interval lo hi lo? hi?) fpbench-interval)
  (match (representation-type repr)
    ['real (ival (bfstep (bf lo) (if lo? 0 1)) (bfstep (bf hi) (if hi? 0 -1)))]
    ['bool (ival #f #t)]))

(module+ test
  (require rackunit "load-plugin.rkt")
  (load-herbie-builtins)

  (check-equal? (precondition->hyperrects
                 '(and (and (<= 0 a) (<= a 1))
                       (and (<= 0 b) (<= b 1)))
                 (make-debug-context '(a b)))
                (list (list (ival (bf 0.0) (bf 1.0)) (ival (bf 0.0) (bf 1.0))))))

;; Part 2: using subdivision search to find valid intervals

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

(module+ test
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
      (check-true (<= (vector-ref arr (- search-result 1)) search-for)))))

(define (sample-ival interval repr)
  (define ->ordinal (compose (representation-repr->ordinal repr) (representation-bf->repr repr)))
  (define <-ordinal (representation-ordinal->repr repr))
  (<-ordinal (random-integer (->ordinal (ival-lo interval))
                             (+ 1 (->ordinal (ival-hi interval))))))

(define (make-hyperrect-sampler hyperrects* reprs)
  (when (null? hyperrects*)
    (raise-herbie-sampling-error "No valid values." #:url "faq.html#no-valid-values"))
  (define hyperrects (list->vector hyperrects*))
  (define weights (partial-sums (vector-map (curryr hyperrect-weight reprs) hyperrects)))
  (define weight-max (vector-ref weights (- (vector-length weights) 1)))
  (λ ()
    (define rand-ordinal (random-integer 0 weight-max))
    (define hyperrect (vector-ref hyperrects (binary-search weights rand-ordinal)))
    (map sample-ival hyperrect reprs)))

(module+ test
  (define two-point-hyperrects (list (list (ival (bf 0) (bf 0)) (ival (bf 1) (bf 1)))))
  (define repr (get-representation 'binary64))
  (check-true
   (andmap (curry set-member? '(0.0 1.0))
           ((make-hyperrect-sampler two-point-hyperrects (list repr repr))))))

(define (make-sampler ctx pre search-func)
  (define repr (context-repr ctx))
  (define reprs (context-var-reprs ctx))
  (cond
   [(and (flag-set? 'setup 'search) (not (empty? reprs))
         (andmap (compose (curry equal? 'real) representation-type) (cons repr reprs)))
    (timeline-push! 'method "search")
    (define hyperrects-analysis (precondition->hyperrects pre ctx))
    (match-define (cons hyperrects sampling-table)
      (find-intervals search-func hyperrects-analysis
                      #:ctx ctx #:fuel (*max-find-range-depth*)))
    (cons (make-hyperrect-sampler hyperrects reprs) sampling-table)]
   [else
    (timeline-push! 'method "random")
    (cons (λ () (map random-generate reprs)) (hash 'unknown 1.0))]))

;; Part 3: computing exact values by recomputing at higher precisions

(define (batch-prepare-points fn ctx sampler)
  ;; If we're using the bf fallback, start at the max precision
  (define repr (context-repr ctx))
  (define starting-precision (*starting-prec*))
  (define <-bf (representation-bf->repr repr))
  (define outcomes (make-hash))
  (define start (current-inexact-milliseconds))

  (define-values (points exactss)
    (let loop ([sampled 0] [skipped 0] [points '()] [exactss '()])
      (define pt (sampler))

      (define-values (status precision out)
        (ival-eval repr fn pt #:precision starting-precision))

      (when (equal? status 'exit)
        (warn 'ground-truth #:url "faq.html#ground-truth"
              "could not determine a ground truth"
              #:extra (for/list ([var (context-vars ctx)] [val pt])
                        (format "~a = ~a" var val))))

      (hash-update! outcomes status (curry + 1) 0)
      (define now (current-inexact-milliseconds))
      (timeline-push!/unsafe 'outcomes precision (~a status) (- now start) 1)
      (set! start now)

      (cond
       [(and (list? out) (not (ormap (representation-special-value? repr) pt)))
        (define exs (map (compose <-bf ival-lo) out))
        (if (>= (+ 1 sampled) (*num-points*))
            (values (cons pt points) (cons exs exactss))
            (loop (+ 1 sampled) 0 (cons pt points) (cons exs exactss)))]
       [else
        (when (>= skipped (*max-skipped-points*))
          (raise-herbie-error "Cannot sample enough valid points."
                              #:url "faq.html#sample-valid-points"))
        (loop sampled (+ 1 skipped) points exactss)])))
  (cons outcomes (cons points (flip-lists exactss))))


(define (combine-tables t1 t2)
  (define t2-total (apply + (hash-values t2)))
  (define t1-base (+ (hash-ref t1 'unknown 0) (hash-ref t1 'valid 0)))
  (for/fold ([t1 (hash-remove (hash-remove t1 'unknown) 'valid)])
      ([(k v) (in-hash t2)])
    (hash-set t1 k (+ (hash-ref t1 k 0) (* (/ v t2-total) t1-base)))))

(define (sample-points pre exprs ctxs)
  (timeline-event! 'analyze)
  (define fn (make-search-func pre exprs ctxs))
  (match-define (cons sampler table)
    (parameterize ([ground-truth-require-convergence #f])
      ;; TODO: Should make-sampler allow multiple contexts?
      (make-sampler (first ctxs) pre fn)))
  (timeline-event! 'sample)
  ;; TODO: should batch-prepare-points allow multiple contexts?
  (match-define (cons table2 results) (batch-prepare-points fn (first ctxs) sampler))
  (define total (apply + (hash-values table2)))
  (when (> (hash-ref table2 'infinite 0.0) (* 0.2 total))
   (warn 'inf-points #:url "faq.html#inf-points"
    "~a of points produce a very large (infinite) output. You may want to add a precondition." 
    (format-accuracy (- total (hash-ref table2 'infinite)) total #:unit "%")))
  (cons (combine-tables table table2) results))
