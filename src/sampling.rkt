#lang racket
(require math/bigfloat rival math/base
         (only-in fpbench interval range-table-ref condition->range-table [expr? fpcore-expr?]))
(require "searchreals.rkt" "errors.rkt" "common.rkt"
         "float.rkt" "syntax/types.rkt" "timeline.rkt" "config.rkt"
         "syntax/sugar.rkt")

(provide batch-prepare-points
         eval-progs-real
         ival-eval
         make-sampler
         make-search-func
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

(define bool-discretization
  (discretization identity
                  (lambda (x y) (if (eq? x y) 0 1))))

(define (representation->discretization repr)
  (discretization
   (representation-bf->repr repr)
   (lambda (x y) (- (ulp-difference x y repr) 1))))

(define (expr-size expr)
  (if (list? expr)
      (apply + 1 (map expr-size (cdr expr)))
      1))

;; Returns a function that maps an ival to a list of ivals
;; The first element of that function's output tells you if the input is good
;; The other elements of that function's output tell you the output values
(define (make-search-func pre specs ctxs)
  (define vars (context-vars (car ctxs)))
  (define var-reprs (context-var-reprs (car ctxs)))
  (define discs (map (compose representation->discretization context-repr) ctxs))
  (define machine (rival-compile (cons `(assert ,pre) specs) vars (cons bool-discretization discs)))
  (timeline-push! 'compiler
                  (apply + 1 (expr-size pre) (map expr-size specs))
                  (+ (length vars) (rival-profile machine 'instructions)))
  machine)

(define (ival-eval machine ctxs pt [iter 0])
  (define start (current-inexact-milliseconds))
  (define pt*
    (for/vector ([val (in-list pt)] [repr (in-list (context-var-reprs (car ctxs)))])
      ((representation-repr->bf repr) val)))
  (define-values (status value)
    (with-handlers
      ([exn:rival:invalid? (lambda (e) (values 'invalid #f))]
       [exn:rival:unsamplable? (lambda (e) (values 'exit #f))])
      (parameterize ([*rival-max-precision* (*max-mpfr-prec*)]
                     [*rival-max-iterations* 5])
        (values 'valid (rest (vector->list (rival-apply machine pt*))))))) ; rest = drop precondition
  (when (> (rival-profile machine 'bumps) 0)
    (warn 'ground-truth "Could not converge on a ground truth"
          #:extra (for/list ([var (in-list (context-vars (car ctxs)))] [val (in-list pt)])
                    (format "~a = ~a" var val))))
  (define executions (rival-profile machine 'executions))
  (when (>= (vector-length executions) (*rival-profile-executions*))
    (warn 'profile "Rival profile vector overflowed, profile may not be complete"))
  (define prec-threshold (exact-floor (/ (*rival-max-precision*) 25)))
  (for ([execution (in-vector executions)])
    (define name (symbol->string (execution-name execution)))
    (define precision (- (execution-precision execution)
                         (remainder (execution-precision execution) prec-threshold)))
    (timeline-push! 'mixsample (execution-time execution) name precision))
  (timeline-push!/unsafe 'outcomes (- (current-inexact-milliseconds) start)
                         (rival-profile machine 'iterations) (~a status) 1)
  (values status value))

; ENSURE: all contexts have the same list of variables
(define (eval-progs-real progs ctxs)
  (define fn (make-search-func '(TRUE) progs ctxs))
  (define bad-pt 
    (for/list ([ctx* (in-list ctxs)])
      ((representation-bf->repr (context-repr ctx*)) +nan.bf)))
  (define (<eval-prog-real> . pt)
    (define-values (result exs) (ival-eval fn ctxs pt))
    (or exs bad-pt))
  <eval-prog-real>)


;; Part 3: computing exact values by recomputing at higher precisions

(define (batch-prepare-points fn ctxs sampler)
  ;; If we're using the bf fallback, start at the max precision
  (define outcomes (make-hash))

  (define-values (points exactss)
    (let loop ([sampled 0] [skipped 0] [points '()] [exactss '()])
      (define pt (sampler))

      (define-values (status exs) (ival-eval fn ctxs pt))

      (when (equal? status 'exit)
        (warn 'ground-truth #:url "faq.html#ground-truth"
              "could not determine a ground truth"
              #:extra (for/list ([var (context-vars (first ctxs))] [val pt])
                        (format "~a = ~a" var val))))

      (when (equal? status 'valid)
        (for ([ex (in-list exs)])
          (when (and (flonum? ex) (infinite? ex))
            (set! status 'infinite))))

      (hash-update! outcomes status (curry + 1) 0)

      (define is-bad?
        (for/or ([input (in-list pt)] [repr (in-list (context-var-reprs (car ctxs)))])
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
  (define fn (make-search-func pre exprs ctxs))
  (match-define (cons sampler table)
    (make-sampler (first ctxs) pre fn))
  (timeline-event! 'sample)
  (match-define (cons table2 results) (batch-prepare-points fn ctxs sampler))
  (define total (apply + (hash-values table2)))
  (when (> (hash-ref table2 'infinite 0.0) (* 0.2 total))
   (warn 'inf-points #:url "faq.html#inf-points"
    "~a of points produce a very large (infinite) output. You may want to add a precondition." 
    (format-accuracy (- total (hash-ref table2 'infinite)) total #:unit "%")))
  (cons (combine-tables table table2) results))
