#lang racket
(require math/bigfloat rival math/base
         (only-in fpbench interval range-table-ref condition->range-table [expr? fpcore-expr?]))
(require "searchreals.rkt" "errors.rkt" "common.rkt"
         "float.rkt" "syntax/types.rkt" "timeline.rkt" "config.rkt"
         "syntax/sugar.rkt")

(provide make-sampler batch-prepare-points ival-eval)

;; Part 1: use FPBench's condition->range-table to create initial hyperrects

(define (precondition->hyperrects pre ctx)
  ;; FPBench needs unparameterized operators
  (define range-table (condition->range-table (prog->spec pre)))
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

  (define repr (get-representation 'binary64))
  (check-equal? (precondition->hyperrects
                 '(and (and (<=.f64 0 a) (<=.f64 a 1))
                       (and (<=.f64 0 b) (<=.f64 b 1)))
                 (context '(a b) repr (list repr repr)))
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

(define (point-logger name vars)
  (define start (current-inexact-milliseconds))
  (define (log! status precision pt)
    (define now (current-inexact-milliseconds))
    (when (equal? status 'exit)
      (warn 'ground-truth #:url "faq.html#ground-truth"
               "could not determine a ground truth"
               #:extra (for/list ([var vars] [val pt])
                         (format "~a = ~a" var val))))
    (define dt (- now start))
    (timeline-push! 'outcomes (~a name) precision (~a status) dt 1)
    (set! start now))
  log!)

(define (ival-eval repr fn pt #:precision [precision (*starting-prec*)])
  (let loop ([precision precision])
    (define exs (parameterize ([bf-precision precision]) (apply fn pt)))
    (match-define (ival err err?) (apply ival-or (map ival-error? exs)))
    (define precision* (exact-floor (* precision 2)))
    (cond
     [err
      (values err precision +nan.0)]
     [(not err?)
      (define infinite?
      (ival-lo (is-infinite-interval repr (apply ival-or exs))))
      (values (if infinite? 'infinite 'valid) precision exs)
     ]
     [(> precision* (*max-mpfr-prec*))
      (values 'exit precision +nan.0)]
     [else
      (loop precision*)])))

(define (is-infinite-interval repr interval)
  (define <-bf (representation-bf->repr repr))
  (define ->bf (representation-repr->bf repr))
  ;; HACK: the comparisons to 0.bf is just about posits, where right now -inf.bf
  ;; rounds to the NaR value, which then represents +inf.bf, which is positive.
  (define (positive-inf? x)
    (parameterize ([bf-rounding-mode 'nearest])
      (and (bigfloat? x) (bf> x 0.bf) (bf= (->bf (<-bf x)) +inf.bf))))
  (define (negative-inf? x)
    (parameterize ([bf-rounding-mode 'nearest])
      (and (bigfloat? x) (bf< x 0.bf) (bf= (->bf (<-bf x)) -inf.bf))))
  (define ival-positive-infinite (monotonic->ival positive-inf?))
  (define ival-negative-infinite (comonotonic->ival negative-inf?))
  (ival-or (ival-positive-infinite interval)
           (ival-negative-infinite interval)))

(define (batch-prepare-points fn ctx sampler)
  ;; If we're using the bf fallback, start at the max precision
  (define repr (context-repr ctx))
  (define starting-precision (*starting-prec*))
  (define <-bf (representation-bf->repr repr))
  (define logger (point-logger 'body (context-vars ctx)))
  (define outcomes (make-hash))

  (define-values (points exactss)
    (let loop ([sampled 0] [skipped 0] [points '()] [exactss '()])
      (define pt (sampler))

      (define-values (status precision out)
        (ival-eval repr fn pt #:precision starting-precision))
      (hash-update! outcomes status (curry + 1) 0)
      (logger status precision pt)

      (cond
       [(and (list? out) (not (ormap (representation-special-value? repr) pt)))
        (define exs (map (compose <-bf ival-lo) out))
        (if (>= (+ 1 sampled) (*num-points*))
            (values (cons pt points) (cons exs exactss))
            (loop (+ 1 sampled) 0 (cons pt points) (cons exs exactss)))]
       [else
        (when (>= skipped (*max-skipped-points*))
          (timeline-compact! 'outcomes)
          (raise-herbie-error "Cannot sample enough valid points."
                              #:url "faq.html#sample-valid-points"))
        (loop sampled (+ 1 skipped) points exactss)])))
  (timeline-compact! 'outcomes)
  (cons outcomes (cons points (flip-lists exactss))))
