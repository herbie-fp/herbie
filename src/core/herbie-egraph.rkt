#lang racket

(require (only-in "../common.rkt" debug *node-limit* reap setfindf)
         (only-in "../timeline.rkt" timeline-push!)
         (only-in "matcher.rkt" pattern-substitute rule? rule-output rule-input))
(require "enode.rkt" "egraph.rkt" "ematch.rkt" "extraction.rkt")

(provide simplify-batch-herbie-egraph)

(define/contract (simplify-batch-herbie-egraph exprs
                                 #:rules rls
                                 #:precompute [precompute? true]
                                 #:prune [prune? true])
  (->* (any/c #:rules (listof rule?))
       (#:precompute (or/c #f procedure?) #:prune boolean?)
       any/c)
  (debug #:from 'simplify (format "Simplifying:\n  ~a" (string-join (map ~a exprs) "\n  ")))

  (define start-time (current-inexact-milliseconds))
  (define eg (mk-egraph))
  (define ens (for/list ([expr exprs]) (mk-enode-rec! eg expr)))
  (define ex (apply mk-extractor ens))

  (define phases
    (filter identity
            (list (rule-phase rls)
                  (and precompute? (precompute-phase precompute?))
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

;; Tries to match the rules against the given enodes, and returns a
;; list of matches found. Matches are of the form:
;; 
;; (rule enode . bindings)
;;
;; where bindings is a list of different matches between the rule and
;; the enode.

(define (find-matches ens rls)
  (reap [sow]
        (for* ([rl rls] [en ens])
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

(define ((precompute-phase fn) eg)
  (for ([en (egraph-leaders eg)]
        #:break (>= (egraph-cnt eg) (*node-limit*)))
    (set-precompute! eg en fn)))

(define (prune-phase eg)
  (for ([en (egraph-leaders eg)] #:break (>= (egraph-cnt eg) (*node-limit*)))
    (reduce-to-single! eg en)))

(define (set-precompute! eg en fn)
  (for ([var (enode-vars en)] #:when (list? var))
    (define op (car var))
    (define args (map (compose (curry setfindf (λ (x) (not (or (list? x) (symbol? x))))) enode-vars) (cdr var)))
    (when (andmap identity args)
      (define constant (apply fn op args))
      (when constant
        (define en* (mk-enode-rec! eg constant))
        (merge-egraph-nodes! eg en en*)))))

