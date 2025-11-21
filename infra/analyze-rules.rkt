#lang racket

(require "../src/core/egg-herbie.rkt"
         "../src/core/batch.rkt"
         "../src/syntax/types.rkt"
         "../src/syntax/load-platform.rkt"
         "../src/syntax/platform.rkt"
         "../src/core/programs.rkt"
         "../src/core/rules.rkt"
         "../src/syntax/read.rkt")

(define *num-iters* 5)

(module+ main
  (command-line #:args ([dir "bench/"])
                (run-analysis dir)))

(define (run-analysis dir)
  (activate-platform! "c")
  (define tests (load-tests dir))
  
  (printf "Loaded ~a tests from ~a\n" (length tests) dir)
  
  ;; Store accumulated impact: iter -> rule-name -> total-percentage
  (define impact (make-hash))
  (define iter-counts (make-hash)) ; iter -> count of valid runs (final-size > 0)
  (define iter-sizes (make-hash)) ; iter -> list of final sizes

  (for ([test (in-list tests)]
        [i (in-naturals)])
    (printf "Processing test ~a/~a: ~a\n" (+ i 1) (length tests) (test-name test))
    (define input-expr (test-input test))
    
    (define ctx (test-context test))
    (define-values (batch brfs) (progs->batch (list input-expr)))
    
    (for ([iter (in-range *num-iters*)])
      (with-handlers ([exn:fail? (lambda (e) (printf "Error in test ~a iter ~a: ~a\n" (test-name test) iter (exn-message e)))])
        (define-values (initial-size final-size sorted-results) 
          (egraph-analyze-rewrite-impact batch brfs ctx iter))
        
        (when (> final-size 0)
           (hash-update! iter-counts iter add1 0)
           (hash-update! iter-sizes iter (lambda (l) (cons final-size l)) '())
           (for ([(rule delta) (in-dict sorted-results)])
             (define pct (/ (* 100.0 delta) final-size))
             (define rname (rule-name rule))
             (define iter-hash (hash-ref! impact iter (thunk (make-hash))))
             (hash-update! iter-hash rname (curry + pct) 0.0))))))
  
  (for ([iter (in-range *num-iters*)])
    (define count (hash-ref iter-counts iter 0))
    (if (zero? count)
        (printf "=== Iteration ~a: No data ===\n" iter)
        (let ()
           (define sizes (hash-ref iter-sizes iter))
           (define log-sum (for/sum ([s sizes]) (log s)))
           (define geomean (exp (/ log-sum count)))
           (printf "=== Iteration ~a (Geomean: ~a) ===\n" iter (exact-floor geomean))

           (define iter-impact (hash-ref impact iter (make-hash)))
           ;; Average the percentages
           (define avg-impact
             (for/list ([(rule total-pct) (in-hash iter-impact)])
               (cons rule (/ total-pct count))))
           (define sorted-avg (sort avg-impact > #:key cdr))
           (for ([item (in-list (take sorted-avg (min 10 (length sorted-avg))))])
             (printf "~a ~a%\n" 
                     (~a (car item) #:width 30) 
                     (~a (~r (cdr item) #:precision '(= 2) #:min-width 6) #:width 7 #:align 'right)))
           (newline)))))
