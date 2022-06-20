#lang racket
(require math/bigfloat rival math/base
         (only-in fpbench interval range-table-ref condition->range-table [expr? fpcore-expr?]))
(require "searchreals.rkt" "programs.rkt" "errors.rkt" "common.rkt"
         "float.rkt" "syntax/types.rkt" "timeline.rkt"
         "syntax/sugar.rkt")

(provide make-sampler batch-prepare-points)

;; Much of this code assumes everything supports intervals. Almost
;; everything does---we're still missing support for the Gamma and
;; Bessel functions. But at least none of the benchmarks use those.
(module+ test
  (require "syntax/read.rkt")
  (require racket/runtime-path)
  (require rackunit "load-plugin.rkt")

  (define-runtime-path benchmarks "../bench/")
  (load-herbie-builtins)
  
  (define exprs
    (let ([tests (load-tests benchmarks)])
      (append (map test-program tests) (map test-precondition tests))))
  (check-true (andmap (compose (curryr expr-supports? 'ival) program-body) exprs)))

;; Part 1: use FPBench's condition->range-table to create initial hyperrects

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
  (match (representation-type repr)
    ['real (ival (bfstep (bf lo) (if lo? 0 1)) (bfstep (bf hi) (if hi? 0 -1)))]
    ['bool (ival #f #t)]))

(module+ test
  (require rackunit "load-plugin.rkt")
  (load-herbie-builtins)

  (define repr (get-representation 'binary64))
  (check-equal? (precondition->hyperrects
                 '(λ (a b) (and (and (<=.f64 0 a) (<=.f64 a 1))
                                (and (<=.f64 0 b) (<=.f64 b 1)))) (list repr repr) repr)
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

(define (make-sampler repr precondition programs how search-func)
  (define reprs (map (curry dict-ref (*var-reprs*)) (program-variables precondition)))
  (cond
   [(and (flag-set? 'setup 'search) (equal? how 'ival) (not (empty? reprs))
         (andmap (compose (curry equal? 'real) representation-type) (cons repr reprs)))
    (timeline-push! 'method "search")
    (define hyperrects-analysis (precondition->hyperrects precondition reprs repr))
    (define hyperrects
      (find-intervals (compose car search-func) hyperrects-analysis
                      #:reprs reprs #:fuel (*max-find-range-depth*)))
    (make-hyperrect-sampler hyperrects reprs)]
   [else
    (timeline-push! 'method "random")
    (λ () (map random-generate reprs))]))

;; Part 3: computing exact values by recomputing at higher precisions

(define (point-logger name vars)
  (define start (current-inexact-milliseconds))
  (define (log! . args)
    (define now (current-inexact-milliseconds))
    (match-define (list category prec)
      (match args
        [`(exit ,prec ,pt)
         (define key (list 'exit prec))
         (warn 'ground-truth #:url "faq.html#ground-truth"
               "could not determine a ground truth for program ~a" name
               #:extra (for/list ([var vars] [val pt])
                         (format "~a = ~a" var val)))
         key]
        [`(unsamplable ,prec ,pt) (list 'overflowed prec)]
        [`(sampled ,prec ,pt) (list 'valid prec)]
        [`(invalid ,prec ,pt) (list 'invalid prec)]))
    (define dt (- now start))
    (timeline-push! 'outcomes (~a name) prec (~a category) dt 1)
    (set! start now))
  log!)

(define (ival-stuck-false? v)
  (define (close-enough x y) x)
  (define ival-close-enough? (close-enough->ival close-enough))
  (not (ival-hi (ival-close-enough? v))))

(define (ival-eval fn pt #:precision [precision 80])
  (let loop ([precision precision])
    (match-define (list valid exs ...) (parameterize ([bf-precision precision]) (apply fn pt)))
    (define precision* (exact-floor (* precision 2)))
    (cond
     [(not (ival-hi valid))
      (values 'invalid precision +nan.0)]
     [(ival-stuck-false? valid)
      (values 'unsamplable precision +nan.0)]
     [(ival-lo valid)
      (values 'sampled precision exs)]
     [(> precision* (*max-mpfr-prec*))
      (values 'exit precision +nan.0)]
     [else
      (loop precision*)])))

(define (batch-prepare-points how fn repr sampler)
  ;; If we're using the bf fallback, start at the max precision
  (define starting-precision
    (match how ['ival (bf-precision)] ['bf (*max-mpfr-prec*)]))
  (define <-bf (representation-bf->repr repr))
  (define logger (point-logger 'body (map car (*var-reprs*))))

  (define-values (points exactss)
    (let loop ([sampled 0] [skipped 0] [points '()] [exactss '()])
      (define pt (sampler))

      (define-values (status precision out)
        (ival-eval fn pt #:precision starting-precision))
      (logger status precision pt)

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
  (timeline-compact! 'outcomes)
  (cons points (flip-lists exactss)))
