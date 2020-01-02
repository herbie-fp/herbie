#lang racket

(require pkg/lib)

(require "../common.rkt" "../programs.rkt" "../timeline.rkt" "../errors.rkt"
         "../syntax/rules.rkt" "herbie-egraph.rkt")

(provide simplify-expr simplify-batch)
(module+ test (require rackunit))

;; One module to rule them all, the great simplify. It uses egg-herbie
;; to simplify an expression as much as possible without making
;; unnecessary changes. We do this by creating an egraph, saturating
;; it partially, then extracting the simplest expression from it.
;;
;; If egg-herbie is not available, simplify uses regraph instead.
;;
;; Simplify makes only one guarantee: that the input is mathematically
;; equivalent to the output. For any exact x, evaluating the input on
;; x will yield the same expression as evaluating the output on x.

;; fall back on herbie-egraph if egg-herbie is unavailable
(define use-egg-math?
  (or
   (hash-has-key? (installed-pkg-table) "egg-herbie")
   (hash-has-key? (installed-pkg-table) "egg-herbie-windows")
   (hash-has-key? (installed-pkg-table) "egg-herbie-osx")
   (hash-has-key? (installed-pkg-table) "egg-herbie-linux")))


(define/contract (simplify-expr expr #:rules rls #:precompute [precompute? false])
  (->* (expr? #:rules (listof rule?)) (#:precompute boolean?) expr?)
  (first (simplify-batch (list expr) #:rules rls #:precompute precompute?)))

(define/contract (simplify-batch exprs #:rules rls #:precompute [precompute? false])
  (->* (expr? #:rules (listof rule?)) (#:precompute boolean?) expr?)

  (define driver
    (cond
     [use-egg-math?
      simplify-batch-egg]
     [else
      (warn 'simplify #:url "faq.html#egg-herbie"
            "Falling back on racket egraph because egg-herbie package not installed")
      simplify-batch-regraph]))

  (debug #:from 'simplify "Simplifying using " driver ":\n  " (string-join (map ~a exprs) "\n  "))
  (define out (driver exprs #:rules rls #:precompute precompute?))
  (debug #:from 'simplify "Simplified to:\n  " (string-join (map ~a out) "\n  "))
    
  out)

(define/contract (simplify-batch-regraph exprs #:rules rls #:precompute precompute?)
  (-> (listof expr?) #:rules (listof rule?) #:precompute boolean? (listof expr?))

  (define start-time (current-inexact-milliseconds))
  (define (log rg iter)
    (define cnt (regraph-count rg))
    (define cost (regraph-cost rg))
    (debug #:from 'simplify #:depth 2 "iteration " iter ": " cnt " enodes " "(cost " cost ")")
    (timeline-push! 'egraph iter cnt cost (- (current-inexact-milliseconds) start-time)))

  (define rg (make-regraph (map munge exprs) #:limit (*node-limit*)))

  (define phases
    (list (rule-phase (map (compose munge rule-input) rls)
                      (map (compose munge rule-output) rls))
          (and precompute? (precompute-phase eval-application))
          prune-phase
          extractor-phase))

  (for/and ([iter (in-naturals 0)])
    (log rg iter)
    (define initial-cnt (regraph-count rg))
    ;; Iterates the egraph by applying each of the given rules to the egraph
    (for ([phase phases] #:when phase) (phase rg))
    (and (< initial-cnt (regraph-count rg) (*node-limit*))))

  (log rg 'done)
  (map unmunge (regraph-extract rg)))

(define/contract (simplify-batch-egg exprs #:rules rls #:precompute precompute?)
  (-> (listof expr?) #:rules (listof rule?) #:precompute boolean? (listof expr?))
  (timeline-log! 'method 'egg-herbie)

  (local-require "eggmath.rkt")

  (egraph-run
   (lambda (egg-graph)
     (egraph-add-exprs
      egg-graph
      exprs
      (lambda (node-ids)
        (egg-run-rules egg-graph (*node-limit*) rls node-ids (and precompute? true))
        (map
         (lambda (id) (egg-expr->expr (egraph-get-simplest egg-graph id) egg-graph))
         node-ids))))))

(define (egg-run-rules egg-graph node-limit rules node-ids precompute?)
  (local-require "eggmath.rkt")
  (define ffi-rules (make-ffi-rules rules))
  (define start-time (current-inexact-milliseconds))

  (define (timeline-cost iter)
    (define cost
      (apply +
             (map (lambda (node-id) (egraph-get-cost egg-graph node-id)) node-ids)))
    (define cnt (egraph-get-size egg-graph))
    (debug #:from 'simplify #:depth 2 "iteration " iter ": " cnt " enodes " "(cost " cost ")")
    (timeline-push! 'egraph iter cnt cost (- (current-inexact-milliseconds) start-time)))
  
  (for/and ([iter (in-naturals 0)])
    (define old-cnt (egraph-get-size egg-graph))
    (egraph-run-iter egg-graph node-limit ffi-rules precompute?)
    (timeline-cost iter)
    (define cnt (egraph-get-size egg-graph))
    (define is_stop (or (>= cnt node-limit) (<= cnt old-cnt)))
    (not is_stop)))

(define (munge expr)
  ;; Despite the name, `expr` might be an expression OR a pattern
  (match expr
    [(? constant?)
     (if (symbol? expr)
         (list expr)
         expr)]
    [(? variable?)
     expr]
    [(list head subs ...)
     (cons head (map munge subs))]))

(define (unmunge expr)
  ;; Despite the name, `expr` might be an expression OR a pattern
  (match expr
    [(list constant)
     constant]
    [(list head subs ...)
     (cons head (map unmunge subs))]
    [_ expr]))

(module+ test
  (define (test-simplify . args)
    (simplify-batch args #:rules (*simplify-rules*) #:precompute true))

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
          [(+ 1/5 3/10) . 1/2]
          [(cos PI) . -1]
          ;; this test is problematic and runs out of nodes currently
          ;[(/ 1 (- (/ (+ 1 (sqrt 5)) 2) (/ (- 1 (sqrt 5)) 2))) . (/ 1 (sqrt 5))]
          ))

  (*timeline-disabled* true)
  (define outputs (apply test-simplify (hash-keys test-exprs)))
  (for ([(original target) test-exprs] [output outputs])
    (with-check-info (['original original])
       (check-equal? output target)))

  (check set-member? '((* x 6) (* 6 x)) (first (test-simplify '(+ (+ (+ (+ (+ x x) x) x) x) x))))

  (define no-crash-exprs
    '((exp (/ (/ (* (* c a) 4) (- (- b) (sqrt (- (* b b) (* 4 (* a c)))))) (* 2 a)))))

  (for ([expr no-crash-exprs])
    (with-check-info (['original expr])
       (check-not-exn (λ () (test-simplify expr))))))
