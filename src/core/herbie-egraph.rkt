#lang racket

(require "../common.rkt" "../programs.rkt" "../float.rkt" "../timeline.rkt")
(require "../syntax/rules.rkt" "../syntax/types.rkt")
(require "egraph.rkt" "ematch.rkt" "extraction.rkt" "matcher.rkt")

(provide simplify-batch-herbie-egraph)

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
