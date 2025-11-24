#lang racket

(require "../src/core/egg-herbie.rkt"
         "../src/core/batch.rkt"
         "../src/syntax/types.rkt"
         "../src/syntax/load-platform.rkt"
         "../src/syntax/platform.rkt"
         "../src/core/programs.rkt"
         "../src/core/rules.rkt"
         "../src/syntax/read.rkt")

(define *iters* (make-parameter 3))

(module+ main
  (command-line
   #:once-each
   [("--iters") iters "How many iterations to analyze" (*iters* (string->number iters))]
   #:args ([dir "bench/"]) (run-analysis dir)))

(define (run-analysis dir)
  (activate-platform! "c")
  (define tests (load-tests dir))

  (printf "Loaded ~a tests from ~a\n" (length tests) dir)

  ;; Store accumulated impact: iter -> rule-name -> total-percentage
  (define impact (make-vector (*iters*)))
  (define iter-sizes (make-vector (*iters*) '())) ; iter -> list of final sizes

  (for ([i (in-range (*iters*))])
    (vector-set! impact i (make-hash)))

  (for ([test (in-list tests)]
        [i (in-naturals)])
    (printf "Processing test ~a/~a: ~a\n" (+ i 1) (length tests) (test-name test))
    (define-values (batch brfs) (progs->batch (list (test-input test))))

    (for ([iter (in-range (*iters*))])
      (define-values (initial-size final-size sorted-results)
        (egraph-analyze-rewrite-impact batch brfs (test-context test) iter))

      (vector-set! iter-sizes iter (cons final-size (vector-ref iter-sizes iter)))
      (for ([(rule delta) (in-dict sorted-results)])
        (define pct (/ (* 100.0 delta) final-size))
        (define iter-hash (vector-ref impact iter))
        (hash-update! iter-hash (rule-name rule) (curry + pct) 0.0))))

  (define count (length tests))
  (for ([iter (in-range (*iters*))])
    (define sizes (vector-ref iter-sizes iter))
    (define log-sum (apply + (map log sizes)))
    (define geomean (exp (/ log-sum count)))
    (printf "=== Iteration ~a (Geomean: ~a) ===\n" iter (exact-floor geomean))

    (define iter-impact (vector-ref impact iter))
    ;; Average the percentages
    (define sorted-avg (sort (hash->list iter-impact) > #:key cdr))
    (for ([item (in-list sorted-avg)]
          [_ (in-range 10)])
      (printf
       "~a ~a%\n"
       (~a (car item) #:width 30)
       (~a (~r (/ (cdr item) count) #:precision '(= 2) #:min-width 6) #:width 7 #:align 'right)))
    (newline)))
