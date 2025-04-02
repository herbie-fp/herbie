#lang racket
(require math/bigfloat
         rival
         math/base
         (only-in fpbench interval range-table-ref condition->range-table [expr? fpcore-expr?]))
(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt"
         "../syntax/types.rkt"
         "searchreals.rkt"
         "rival.rkt")

(provide batch-prepare-points
         sample-points)

;; Part 1: use FPBench's condition->range-table to create initial hyperrects

(define (precondition->hyperrects pre vars var-reprs)
  (define range-table (condition->range-table pre))
  (apply cartesian-product
         (for/list ([var-name (in-list vars)]
                    [var-repr (in-list var-reprs)])
           (map (lambda (interval) (fpbench-ival->ival var-repr interval))
                (range-table-ref range-table var-name)))))

(define (fpbench-ival->ival repr fpbench-interval)
  (match-define (interval lo hi lo? hi?) fpbench-interval)
  (match (representation-type repr)
    ['real (ival (bfstep (bf lo) (if lo? 0 1)) (bfstep (bf hi) (if hi? 0 -1)))]
    ['bool (ival #f #t)]))

(module+ test
  (require rackunit)
  (require "../syntax/load-plugin.rkt")
  (load-herbie-builtins)
  (define binary64 (get-representation 'binary64))
  (define pre '(and (and (<= 0 a) (<= a 1)) (and (<= 0 b) (<= b 1))))

  (check-equal? (precondition->hyperrects pre '(a b) (list binary64 binary64))
                (list (list (ival 0.bf 1.bf) (ival 0.bf 1.bf)))))

;; Part 2: using subdivision search to find valid intervals

;; we want a index i such that vector[i] > num and vector[i-1] <= num
;; assumes vector strictly increasing
(define (binary-search vector num)
  (let loop ([left 0]
             [right (- (vector-length vector) 1)])
    (cond
      [(>= left right) (min left (- (vector-length vector) 1))]
      [else
       (define mid (quotient (+ left right) 2))
       (define pivot (vector-ref vector mid))
       (if (<= pivot num)
           (loop (+ 1 mid) right)
           (loop left mid))])))

(module+ test
  (define arr (partial-sums (build-vector 50 (lambda (_) (random-integer 1 100)))))
  (define max-num (vector-ref arr (- (vector-length arr) 1)))
  (for ([search-for (in-range max-num)])
    (define search-result (binary-search arr search-for))
    (check-true (> (vector-ref arr search-result) search-for))
    (check-true (or (zero? search-result) (<= (vector-ref arr (- search-result 1)) search-for)))))

(define (make-hyperrect-sampler hyperrects* hints* reprs)
  (when (null? hyperrects*)
    (raise-herbie-sampling-error "No valid values." #:url "faq.html#no-valid-values"))
  (define hints (list->vector hints*))
  (define hyperrects (list->vector hyperrects*))
  (define lo-ends
    (for/vector #:length (vector-length hyperrects)
                ([hyperrect (in-vector hyperrects)])
      (for/list ([interval (in-list hyperrect)]
                 [repr (in-list reprs)])
        ((representation-repr->ordinal repr) ((representation-bf->repr repr) (ival-lo interval))))))
  (define hi-ends
    (for/vector #:length (vector-length hyperrects)
                ([hyperrect (in-vector hyperrects)])
      (for/list ([interval (in-list hyperrect)]
                 [repr (in-list reprs)])
        (+ 1
           ((representation-repr->ordinal repr)
            ((representation-bf->repr repr) (ival-hi interval)))))))
  (define weights (partial-sums (vector-map (curryr hyperrect-weight reprs) hyperrects)))
  (define weight-max (vector-ref weights (- (vector-length weights) 1)))

  ;; returns pt and hint
  (λ ()
    (define rand-ordinal (random-natural weight-max))
    (define idx (binary-search weights rand-ordinal))
    (define los (vector-ref lo-ends idx))
    (define his (vector-ref hi-ends idx))
    (define hint (vector-ref hints idx))
    (values (for/list ([lo (in-list los)]
                       [hi (in-list his)]
                       [repr (in-list reprs)])
              ((representation-ordinal->repr repr) (random-integer lo hi)))
            hint)))

(define (make-sampler compiler)
  (match-define (real-compiler pre vars var-reprs _ reprs _ _) compiler)
  (cond
    [(and (flag-set? 'setup 'search)
          (not (empty? var-reprs))
          (for/and ([repr (in-list (append var-reprs reprs))])
            (equal? (representation-type repr) 'real)))
     (timeline-push! 'method "search")
     (define hyperrects-analysis (precondition->hyperrects pre vars var-reprs))
     ; hints-hyperrects is a (listof '(hint hyperrect))
     (match-define (list hyperrects hints sampling-table)
       (find-intervals compiler hyperrects-analysis #:fuel (*max-find-range-depth*)))
     (values (make-hyperrect-sampler hyperrects hints var-reprs) sampling-table)]
    [else
     (timeline-push! 'method "random")
     ; sampler return false hint since rival-analyze has not been called in random method
     (values (λ () (cons (map random-generate var-reprs) #f)) (hash 'unknown 1.0))]))

;; Returns an evaluator for a list of expressions.
;; Part 3: compute exact values using Rival's algorithm

(define (batch-prepare-points compiler sampler)
  ;; If we're using the bf fallback, start at the max precision
  (define outcomes (make-hash))
  (define vars (real-compiler-vars compiler))
  (define var-reprs (real-compiler-var-reprs compiler))
  (define reprs (real-compiler-reprs compiler))

  (real-compiler-clear! compiler) ; Clear profiling vector
  (define-values (points exactss)
    (let loop ([sampled 0]
               [skipped 0]
               [points '()]
               [exactss '()])
      (define-values (pt hint) (sampler))
      (define-values (status exs) (real-apply compiler pt hint))
      (case status
        [(exit)
         (warn 'ground-truth
               "could not determine a ground truth"
               #:url "faq.html#ground-truth"
               #:extra (map (curry format "~a = ~a") vars pt))]
        [(valid)
         (for ([ex (in-list exs)]
               [repr (in-list reprs)])
           ; The `bool` representation does not produce bigfloats
           (define maybe-bf ((representation-repr->bf repr) ex))
           (when (and (bigfloat? maybe-bf) (bfinfinite? maybe-bf))
             (set! status 'infinite)))])

      (hash-update! outcomes status add1 0)

      (define is-bad?
        (for/or ([input (in-list pt)]
                 [repr (in-list var-reprs)])
          ((representation-special-value? repr) input)))

      (cond
        [(and (list? exs) (not is-bad?))
         (if (>= (+ 1 sampled) (*num-points*))
             (values (cons pt points) (cons exs exactss))
             (loop (+ 1 sampled) 0 (cons pt points) (cons exs exactss)))]
        [else
         (when (>= skipped (*max-skipped-points*))
           (raise-herbie-sampling-error "Cannot sample enough valid points."
                                        #:url "faq.html#sample-valid-points"))
         (loop sampled (+ 1 skipped) points exactss)])))
  (values (cons points (flip-lists exactss)) outcomes))

(define (combine-tables t1 t2)
  (define t2-total (apply + (hash-values t2)))
  (define t1-base (+ (hash-ref t1 'unknown 0) (hash-ref t1 'valid 0)))
  (for/fold ([t1 (hash-remove (hash-remove t1 'unknown) 'valid)]) ([(k v) (in-hash t2)])
    (hash-set t1 k (+ (hash-ref t1 k 0) (* (/ v t2-total) t1-base)))))

(define (sample-points pre specs ctxs)
  (timeline-event! 'analyze)
  (define compiler (make-real-compiler specs ctxs #:pre pre))
  (define-values (sampler table) (make-sampler compiler))
  (timeline-event! 'sample)
  (define-values (results table2) (batch-prepare-points compiler sampler))
  (define total (apply + (hash-values table2)))
  (when (> (hash-ref table2 'infinite 0.0) (* 0.2 total))
    (warn 'inf-points
          #:url "faq.html#inf-points"
          "~a of points produce a very large (infinite) output. You may want to add a precondition."
          (format-accuracy (- total (hash-ref table2 'infinite)) total #:unit "%")))
  (timeline-push! 'bogosity (combine-tables table table2))
  results)
