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
  (require "../syntax/types.rkt" "../syntax/rules.rkt" "../syntax/sugar.rkt")

  ;; set parameters
  (define vars '(x a b c))
  (*context* (make-debug-context vars))
  (define all-simplify-rules (*simplify-rules*))

  ;; check that no rules in simplify match on bare variables
  ;; this would be bad because we don't want to match type-specific operators on a value of a different type
  (for ([rule all-simplify-rules])
    (check-true
     (not (symbol? (rule-input rule)))
     (string-append "Rule failed: " (symbol->string (rule-name rule)))))
  
  (define (test-simplify . args)
    (map prog->spec (map last (simplify-batch (make-egg-query (map (curryr spec->prog (*context*)) args) (*simplify-rules*))))))

  (define test-exprs
    '((1 . 1)
      (0 . 0)
      ((+ 1 0) . 1)
      ((+ 1 5) . 6)
      ((+ x 0) . x)
      ((- x 0) . x)
      ((* x 1) . x)
      ((/ x 1) . x)
      ((- (* 1 x) (* (+ x 1) 1)) . -1)
      ((- (+ x 1) x) . 1)
      ((- (+ x 1) 1) . x)
      ((/ (* x 3) x) . 3)
      ((- (* (sqrt (+ x 1)) (sqrt (+ x 1))) (* (sqrt x) (sqrt x))) . 1)
      ((+ 1/5 3/10) . 1/2)
      ((cos (PI)) . -1)
      ((pow (E) 1) . (E))
      ;; this test is problematic and runs out of nodes currently
      ;;((/ 1 (- (/ (+ 1 (sqrt 5)) 2) (/ (- 1 (sqrt 5)) 2))) . (/ 1 (sqrt 5)))
      ))

  (*timeline-disabled* true)
  (define outputs (apply test-simplify (dict-keys test-exprs)))
  (for ([(original target) (in-dict test-exprs)] [output outputs])
    (with-check-info (['original original])
       (check-equal? output target)))

  (check set-member? '((* x 6) (* 6 x)) 
                     (first (test-simplify '(+ (+ (+ (+ (+ x x) x) x) x) x))))

  (define no-crash-exprs
    '((exp (/ (/ (* (* c a) 4) 
                 (- (neg b) (sqrt (- (* b b) (* 4 (* a c)))))) (* 2 a)))))

  (for ([expr no-crash-exprs])
    (with-check-info (['original expr])
       (check-not-exn (λ () (test-simplify expr))))))
