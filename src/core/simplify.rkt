#lang racket

(require pkg/lib)

(require "../common.rkt" "../programs.rkt" "../timeline.rkt")
(require "../syntax/rules.rkt")

;; fall back on racket egraph if rust package unavailable
(require "herbie-egraph.rkt")

(provide simplify-expr simplify-batch)
(module+ test (require rackunit))

;;################################################################################
;;
;;# One module to rule them all, the great simplify. This makes use of the other
;;# modules in this directory to simplify an expression as much as possible without
;;# making unecessary changes. We do this by creating an egraph, saturating it
;;# partially, then extracting the simplest expression from it.
;;#
;;# Simplify attempts to make only one strong guarantee:
;;# that the input is mathematically equivalent to the output; that is, for any
;;# exact x, evalutating the input on x will yield the same expression as evaluating
;;# the output on x.
;;#
;;################################################################################
;

(define use-egg-math?
  (hash-has-key? (installed-pkg-table) "egg-math"))


(define/contract (simplify-expr expr
                                #:rules rls
                                #:precompute [precompute? true]
                                #:prune [prune? true])
  (->* (expr? #:rules (listof rule?))
       (#:precompute boolean? #:prune boolean?)
       expr?)
  (first (simplify-batch (list expr) #:rules rls #:precompute precompute? #:prune prune?)))

(define/contract (simplify-batch exprs
                                 #:rules rls
                                 #:precompute [precompute? true]
                                 #:prune [prune? true])
  (->* (expr? #:rules (listof rule?))
       (#:precompute boolean? #:prune boolean?)
       expr?)
  (if use-egg-math?
      (simplify-batch-herbie-egraph exprs #:rules rls #:precompute precompute?)
      (simplify-batch-egg exprs #:rules rls #:precompute precompute?)))

(define/contract (simplify-batch-egg exprs #:rules rls #:precompute precompute?)
  (-> (listof expr?) #:rules (listof rule?) #:precompute boolean? (listof expr?))
  (debug #:from 'simplify (format "Simplifying:\n  ~a" (string-join (map ~a exprs) "\n  ")))

  (local-require "eggmath.rkt")

  (define start-time (current-inexact-milliseconds))
  (define res
    (egraph-run
     (lambda (egg-graph)
       (egraph-add-exprs
        egg-graph
        exprs
        (lambda (node-ids)
          (define start-time-inner (current-inexact-milliseconds))
          (egg-run-rules egg-graph (*node-limit*) rls node-ids precompute?)
          (for/list ([id node-ids])
            (egg-expr->expr (egraph-get-simplest egg-graph id) egg-graph)))))))
  res)

(define (egg-run-rules egg-graph node-limit rules node-ids precompute?)
  (local-require "eggmath.rkt")
  (define ffi-rules (make-ffi-rules rules))
  (define start-time (current-inexact-milliseconds))
  (define old-cnt 0)
  (for/and ([iter (in-naturals 0)])
    (egraph-run-iter egg-graph node-limit ffi-rules precompute?)
    (define cnt (egraph-get-size egg-graph))
    (define cost
      (apply +
             (for/list ([node-id node-ids])
               (egraph-get-cost egg-graph node-id))))
    (debug #:from 'simplify #:depth 2 "iteration " iter ": " cnt " enodes " "(cost " cost ")")
    (timeline-push! 'egraph iter cnt cost (- (current-inexact-milliseconds) start-time))
    (define is_stop (or (>= cnt node-limit) (<= cnt old-cnt)))
    (set! old-cnt cnt)
    (not is_stop))

  (define cost
      (apply +
             (for/list ([node-id node-ids])
               (egraph-get-cost egg-graph node-id))))
  (timeline-push! 'egraph "done" old-cnt cost (- (current-inexact-milliseconds) start-time)))


(module+ test
  (define test-exprs
    #hash([1 . 1]
          [0 . 0]
          [(+ 1 0) . 1]
          [(+ 1 5) . 6]
          [(+ x 0) . x]
          [(- x 0) . x]
          [(* x 1) . x]
          [(/ x 1) . x]
          [(- (* 1 x) (* (+ x 1) 1)) . -1]
          [(- (+ x 1) x) . 1]
          [(- (+ x 1) 1) . x]
          [(/ (* x 3) x) . 3]
          [(- (* (sqrt (+ x 1)) (sqrt (+ x 1)))
              (* (sqrt x) (sqrt x))) . 1]
          [(re (complex a b)) . a]
          ;; this test is problematic and runs out of nodes currently
          ;[(/ 1 (- (/ (+ 1 (sqrt 5)) 2) (/ (- 1 (sqrt 5)) 2))) . (/ 1 (sqrt 5))]
          ))

  (*timeline-disabled* true)
  (define outputs (simplify-batch (hash-keys test-exprs) #:rules (*simplify-rules*)))
  (for ([(original target) test-exprs] [output outputs])
    (with-check-info (['original original])
       (check-equal? output target)))

  (check set-member?
         '((* x 6) (* 6 x))
         (simplify-expr '(+ (+ (+ (+ (+ x x) x) x) x) x) #:rules (*simplify-rules*)))

  (check-equal?
         '5/10
         (simplify-expr '(+ 1/5 3/10) #:rules (*simplify-rules*)))

  (define no-crash-exprs
    '((exp (/ (/ (* (* c a) 4) (- (- b) (sqrt (- (* b b) (* 4 (* a c))))))(* 2 a)))))

  (for ([expr no-crash-exprs])
    (with-check-info (['original expr])
       (check-not-exn (λ () (simplify-expr expr #:rules (*simplify-rules*)))))))
