#lang racket
(require math/bigfloat rival math/base
         (only-in fpbench interval range-table-ref condition->range-table))

(require "searchreals.rkt" "errors.rkt" "common.rkt" "float.rkt"
         "syntax/types.rkt" "timeline.rkt" "config.rkt" "core/rival.rkt")

(provide batch-prepare-points
         eval-progs-real
         make-sampler
         sample-points)

;; Part 1: use FPBench's condition->range-table to create initial hyperrects

(define (precondition->hyperrects pre vars var-reprs)
  ;; FPBench needs unparameterized operators
  (define range-table (condition->range-table pre))
  (apply cartesian-product
         (for/list ([var-name vars] [var-repr var-reprs])
           (map (lambda (interval) (fpbench-ival->ival var-repr interval))
                (range-table-ref range-table var-name)))))

(define (fpbench-ival->ival repr fpbench-interval)
  (match-define (interval lo hi lo? hi?) fpbench-interval)
  (match (representation-type repr)
    ['real (ival (bfstep (bf lo) (if lo? 0 1)) (bfstep (bf hi) (if hi? 0 -1)))]
    ['bool (ival #f #t)]))

(module+ test (require rackunit))

#;(module+ test
  (require  "load-plugin.rkt")
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

(define (make-hyperrect-sampler hyperrects* reprs)
  (when (null? hyperrects*)
    (raise-herbie-sampling-error "No valid values." #:url "faq.html#no-valid-values"))
  (define hyperrects (list->vector hyperrects*))
  (define lo-ends
    (for/vector #:length (vector-length hyperrects)
                ([hyperrect (in-vector hyperrects)])
      (for/list ([interval (in-list hyperrect)] [repr (in-list reprs)])
        ((representation-repr->ordinal repr)
         ((representation-bf->repr repr)
          (ival-lo interval))))))
  (define hi-ends
    (for/vector #:length (vector-length hyperrects)
                ([hyperrect (in-vector hyperrects)])
      (for/list ([interval (in-list hyperrect)] [repr (in-list reprs)])
        (+ 1 
           ((representation-repr->ordinal repr)
            ((representation-bf->repr repr)
             (ival-hi interval)))))))
  (define weights (partial-sums (vector-map (curryr hyperrect-weight reprs) hyperrects)))
  (define weight-max (vector-ref weights (- (vector-length weights) 1)))
  (λ ()
    (define rand-ordinal (random-integer 0 weight-max))
    (define idx (binary-search weights rand-ordinal))
    (define los (vector-ref lo-ends idx))
    (define his (vector-ref hi-ends idx))
    (for/list ([lo (in-list los)] [hi (in-list his)] [repr (in-list reprs)])
      ((representation-ordinal->repr repr) (random-integer lo hi)))))

#;(module+ test
  (define two-point-hyperrects (list (list (ival (bf 0) (bf 0)) (ival (bf 1) (bf 1)))))
  (define repr (get-representation 'binary64))
  (check-true
   (andmap (curry set-member? '(0.0 1.0))
           ((make-hyperrect-sampler two-point-hyperrects (list repr repr))))))

(define (make-sampler evaluator)
  (match-define (real-evaluator pre vars var-reprs _ reprs _) evaluator)
  (cond
   [(and (flag-set? 'setup 'search)
         (not (empty? var-reprs))
         (for/and ([repr (in-list (append var-reprs reprs))])
           (equal? (representation-type repr) 'real)))
    (timeline-push! 'method "search")
    (define hyperrects-analysis (precondition->hyperrects pre vars var-reprs))
    (match-define (cons hyperrects sampling-table)
      (find-intervals evaluator
                      hyperrects-analysis
                      #:fuel (*max-find-range-depth*)))
    (cons (make-hyperrect-sampler hyperrects var-reprs) sampling-table)]
   [else
    (timeline-push! 'method "random")
    (cons (λ () (map random-generate var-reprs)) (hash 'unknown 1.0))]))

(define (unify-contexts! proc-name exprs ctxs)
  (unless (= (length exprs) (length ctxs))
    (error proc-name
           "number of expressions and contexts are different: ~a and ~a"
           (length exprs) (length ctxs)))
  (when (null? exprs)
    (error proc-name "must have at least one expression"))
  (define ctx (car ctxs))
  (for ([ctx* (in-list (cdr ctxs))])
    (unless (and (equal? (context-vars ctx) (context-vars ctx*))
                 (equal? (context-var-reprs ctx) (context-var-reprs ctx*)))
      (error proc-name "contexts don't have matching variables/representations ~a" ctxs)))
  (values (context-vars ctx) (context-var-reprs ctx)))

;; Returns an evaluator for a list of expressions.
(define (eval-progs-real specs ctxs)
  (define-values (vars var-reprs) (unify-contexts! 'eval-progs-real specs ctxs))
  (define reprs (map context-repr ctxs))
  (define evaluator (make-real-evaluator vars var-reprs specs reprs))
  (define bad-pt 
    (for/list ([ctx* (in-list ctxs)])
      ((representation-bf->repr (context-repr ctx*)) +nan.bf)))
  (define (<eval-prog-real> . pt)
    (define-values (_ exs) (run-real-evaluator evaluator pt))
    (or exs bad-pt))
  <eval-prog-real>)

;; Part 3: compute exact values using Rival's algorithm

(define (batch-prepare-points evaluator sampler)
  ;; If we're using the bf fallback, start at the max precision
  (define outcomes (make-hash))
  (define vars (real-evaluator-vars evaluator))
  (define var-reprs (real-evaluator-var-reprs evaluator))
  (define reprs (real-evaluator-reprs evaluator))

  (real-evaluator-clear! evaluator) ; Clear profiling vector
  (define-values (points exactss)
    (let loop ([sampled 0] [skipped 0] [points '()] [exactss '()])
      (define pt (sampler))

      (define-values (status exs) (run-real-evaluator evaluator pt))
      (case status
        [(exit)
         (warn 'ground-truth #:url "faq.html#ground-truth"
               "could not determine a ground truth"
               #:extra (for/list ([var vars] [val pt])
                         (format "~a = ~a" var val)))]
        [(valid)
         (for ([ex (in-list exs)] [repr (in-list reprs)])
           ; The `bool` representation does not produce bigfloats
           (define maybe-bf ((representation-repr->bf repr) ex))
           (when (and (bigfloat? maybe-bf) (bfinfinite? maybe-bf))
             (set! status 'infinite)))])

      (hash-update! outcomes status (curry + 1) 0)

      (define is-bad?
        (for/or ([input (in-list pt)] [repr (in-list var-reprs)])
          ((representation-special-value? repr) input)))

      (cond
       [(and (list? exs) (not is-bad?))
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
  (define-values (vars var-reprs) (unify-contexts! 'sample-points exprs ctxs))
  (define reprs (map context-repr ctxs))
  (define evaluator (make-real-evaluator vars var-reprs exprs reprs #:pre pre))
  (match-define (cons sampler table) (make-sampler evaluator))
  (timeline-event! 'sample)
  (match-define (cons table2 results) (batch-prepare-points evaluator sampler))
  (define total (apply + (hash-values table2)))
  (when (> (hash-ref table2 'infinite 0.0) (* 0.2 total))
   (warn 'inf-points #:url "faq.html#inf-points"
    "~a of points produce a very large (infinite) output. You may want to add a precondition." 
    (format-accuracy (- total (hash-ref table2 'infinite)) total #:unit "%")))
  (cons (combine-tables table table2) results))
