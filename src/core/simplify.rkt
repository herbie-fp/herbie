#lang racket

(require "../common.rkt" "../programs.rkt" "../timeline.rkt" "../errors.rkt"
         "../syntax/rules.rkt" "../alternative.rkt" "egg-herbie.rkt")

(provide simplify-batch)

(module+ test
  (require rackunit "../load-plugin.rkt")
  (load-herbie-plugins))

;; for each expression, returns a list of simplified versions corresponding to egraph iterations
;; the last expression is the simplest unless something went wrong due to unsoundness
;; if the input specifies proofs, it instead returns proofs for these expressions
(define/contract (simplify-batch input)
  (-> egraph-query? (listof (listof expr?)))
  (timeline-push! 'inputs (map ~a (egraph-query-exprs input)))
  (timeline-push! 'method "egg-herbie")
  (match-define (cons results _) (run-egg input #f))

  (define out
    (for/list ([result results] [expr (egraph-query-exprs input)])
      (remove-duplicates (cons expr result))))
  (timeline-push! 'outputs (map ~a (apply append out)))
    
  out)

(module+ test
  (require "../syntax/types.rkt" "../syntax/rules.rkt")

  ;; set parameters
  (define vars '(x a b c))
  (*context* (make-debug-context vars))
  (define all-simplify-rules (*simplify-rules*))

  (define (test-simplify . args)
    (map last (simplify-batch (make-egg-query args (*simplify-rules*)))))

  (define test-exprs
    '((1 . 1)
      (0 . 0)
      ((+.f64 1 0) . 1)
      ((+.f64 1 5) . 6)
      ((+.f64 x 0) . x)
      ((-.f64 x 0) . x)
      ((*.f64 x 1) . x)
      ((/.f64 x 1) . x)
      ((-.f64 (*.f64 1 x) (*.f64 (+.f64 x 1) 1)) . -1)
      ((-.f64 (+.f64 x 1) x) . 1)
      ((-.f64 (+.f64 x 1) 1) . x)
      ((/.f64 (*.f64 x 3) x) . 3)
      ((-.f64 (*.f64 (sqrt.f64 (+.f64 x 1)) (sqrt.f64 (+.f64 x 1)))
              (*.f64 (sqrt.f64 x) (sqrt.f64 x))) . 1)
      ((+.f64 1/5 3/10) . 1/2)
      ((cos.f64 (PI.f64)) . -1)
      ((pow.f64 (E.f64) 1) . (E.f64))
      ;; this test is problematic and runs out of nodes currently
      ;;((/ 1 (- (/ (+ 1 (sqrt 5)) 2) (/ (- 1 (sqrt 5)) 2))) . (/ 1 (sqrt 5)))
      ))

  (*timeline-disabled* true)
  (define outputs (apply test-simplify (dict-keys test-exprs)))
  (for ([(original target) (in-dict test-exprs)] [output outputs])
    (with-check-info (['original original])
       (check-equal? output target)))

  (check set-member? '((*.f64 x 6) (*.f64 6 x)) 
                     (first (test-simplify '(+.f64 (+.f64 (+.f64 (+.f64 (+.f64 x x) x) x) x) x))))

  (define no-crash-exprs
    '((exp.f64 (/.f64 (/.f64 (*.f64 (*.f64 c a) 4) 
                      (-.f64 (neg.f64 b) (sqrt.f64 (-.f64 (*.f64 b b) (*.f64 4 (*.f64 a c)))))) (*.f64 2 a)))))

  (for ([expr no-crash-exprs])
    (with-check-info (['original expr])
       (check-not-exn (λ () (test-simplify expr))))))
