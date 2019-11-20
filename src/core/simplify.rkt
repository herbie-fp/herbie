#lang racket

(require "../common.rkt" "../programs.rkt" "../float.rkt" "../timeline.rkt")
(require "../syntax/rules.rkt" "../syntax/types.rkt")
(require "egraph.rkt" "ematch.rkt" "extraction.rkt" "matcher.rkt" "eggmath.rkt")

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

(define/contract (simplify-expr expr
                                #:rules rls
                                #:precompute [precompute? true]
                                #:prune [prune? true])
  (->* (expr? #:rules (listof rule?))
       (#:precompute boolean? #:prune boolean?)
       expr?)
  (first (simplify-batch (list expr) #:rules rls)))

(define/contract (simplify-batch exprs
                                 #:rules rls
                                 #:precompute [precompute? true]
                                 #:prune [prune? true])
  (->* (expr? #:rules (listof rule?))
       (#:precompute boolean? #:prune boolean?)
       expr?)
  ;(with-handlers ([egg-add-exn?
  ;                 (lambda (e)
  ;                  (println "Falling back on simplify-batch-herbie-egraph")
  ;                 (simplify-batch-herbie-egraph exprs #:rules rls))])
  (simplify-batch-egg exprs #:rules rls))

(define/contract (simplify-batch-egg exprs #:rules rls #:precompute [precompute? true])
  (-> (listof expr?) #:rules (listof rule?) (listof expr?))
  (debug #:from 'simplify (format "Simplifying:\n  ~a" (string-join (map ~a exprs) "\n  ")))

  (define start-time (current-inexact-milliseconds))
  (define res
    (egraph-run
     (lambda (egg-graph)
       (egraph-add-exprs
        egg-graph
        exprs
        (lambda (node-ids)
          (define start-time-inner (current-inexact-milliseconds))
          (egg-run-rules egg-graph (*node-limit*) rls node-ids)
          (for/list ([id node-ids])
            (egg-expr->expr (egraph-get-simplest egg-graph id) egg-graph)))))))
  ;; (println (- (current-inexact-milliseconds) start-time))
  res)

(define (egg-run-rules egg-graph node-limit rules node-ids)
  (define ffi-rules (make-ffi-rules rules))
  (define start-time (current-inexact-milliseconds))
  (define old-cnt 0)
  (for/and ([iter (in-naturals 0)])
    (egraph-run-iter egg-graph node-limit ffi-rules)
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

(define/contract (simplify-batch-herbie-egraph exprs
                                 #:rules rls
                                 #:precompute [precompute? true]
                                 #:prune [prune? true])
  (->* (expr? #:rules (listof rule?))
       (#:precompute boolean? #:prune boolean?)
       expr?)
  (debug #:from 'simplify (format "Simplifying:\n  ~a" (string-join (map ~a exprs) "\n  ")))

  (define start-time (current-inexact-milliseconds))
  (define eg (mk-egraph))
  (define ens (for/list ([expr exprs]) (mk-enode-rec! eg expr)))
  (define ex (apply mk-extractor ens))

  (define phases
    (filter identity
            (list (rule-phase rls)
                  (and precompute? precompute-phase)
                  (and prune? prune-phase))))

  (for/and ([iter (in-naturals 0)])
    (extractor-iterate ex)
    (define cost (apply extractor-cost ex ens))
    (define initial-cnt (egraph-cnt eg))
    (debug #:from 'simplify #:depth 2 "iteration " iter ": " (egraph-cnt eg) " enodes " "(cost " cost ")")
    (timeline-push! 'egraph iter (egraph-cnt eg) cost (- (current-inexact-milliseconds) start-time))

    ;; Iterates the egraph by applying each of the given rules to the egraph
    (for ([phase phases]) (phase eg))

    (< initial-cnt (egraph-cnt eg) (*node-limit*)))

  (extractor-iterate ex)
  (define cost (apply extractor-cost ex ens))
  (debug #:from 'simplify #:depth 2
         "iteration done: " (egraph-cnt eg) " enodes " "(cost " cost ")")
  (timeline-push! 'egraph "done" (egraph-cnt eg) cost (- (current-inexact-milliseconds) start-time))

  (define out (map cdr (apply extractor-extract ex ens)))
  (debug #:from 'simplify (format "Simplified to:\n  ~a" (string-join (map ~a out) "\n  ")))
  out)

(define (rule-applicable? rl en)
  (equal? (rule-otype rl) (enode-type en)))

;; Tries to match the rules against the given enodes, and returns a
;; list of matches found. Matches are of the form:
;; 
;; (rule enode . bindings)
;;
;; where bindings is a list of different matches between the rule and
;; the enode.

(define (find-matches ens rls)
  (reap [sow]
        (for* ([rl rls] [en ens]
               #:when (rule-applicable? rl en))
          (define bindings (match-e (rule-input rl) en))
          (unless (null? bindings)
            (sow (list* rl en bindings))))))

(define ((rule-phase rls) eg)
  (for* ([m (find-matches (egraph-leaders eg) rls)]
         #:break (>= (egraph-cnt eg) (*node-limit*)))
    (match-define (list rl en bindings ...) m)
    (for ([binding bindings] #:break (>= (egraph-cnt eg) (*node-limit*)))
      (define expr* (pattern-substitute (rule-output rl) binding))
      (define en* (mk-enode-rec! eg expr*))
      (merge-egraph-nodes! eg en en*))))

(define (precompute-phase eg)
  (for ([en (egraph-leaders eg)]
        #:break (>= (egraph-cnt eg) (*node-limit*)))
    (set-precompute! eg en)))

(define (prune-phase eg)
  (for ([en (egraph-leaders eg)] #:break (>= (egraph-cnt eg) (*node-limit*)))
    (reduce-to-single! eg en)))

(define (set-precompute! eg en)
  (define type (enode-type en))
  (for ([var (enode-vars en)] #:when (list? var))
    (define constexpr
      (cons (car var)
            (map (compose (curry setfindf constant?) enode-vars) (cdr var))))
    (when (andmap identity constexpr)
      (with-handlers ([exn:fail:contract:divide-by-zero? void])
        (define res (eval-const-expr constexpr))
        (when (and ((value-of type) res) (exact-value? type res))
          (define en* (mk-enode-rec! eg (val-to-type type res)))
          (merge-egraph-nodes! eg en en*))))))

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
