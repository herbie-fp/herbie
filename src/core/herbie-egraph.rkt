#lang racket

(require (only-in "matcher.rkt" pattern-substitute rule? rule-output rule-input))
(require "enode.rkt" "egraph.rkt" "ematch.rkt" "extraction.rkt")

(provide make-regraph regraph-cost regraph-count regraph-extract
         rule-phase precompute-phase prune-phase extractor-phase)

(struct regraph (egraph extractor ens limit))

(define (make-regraph exprs #:limit [limit #f])
  (define eg (mk-egraph))
  (define ens (for/list ([expr exprs]) (mk-enode-rec! eg expr)))
  (define ex (apply mk-extractor ens))
  (extractor-iterate ex)
  (regraph eg ex ens limit))

(define (regraph-cost rg)
  (apply extractor-cost (regraph-extractor rg) (regraph-ens rg)))

(define (regraph-count rg)
  (egraph-cnt (regraph-egraph rg)))

(define (regraph-extract rg)
  (map cdr (apply extractor-extract (regraph-extractor rg) (regraph-ens rg))))

;; Tries to match the rules against the given enodes, and returns a
;; list of matches found. Matches are of the form:
;; 
;; (rule enode . bindings)
;;
;; where bindings is a list of different matches between the rule and
;; the enode.

(define (find-matches ens rls)
  (define out '())
  (for* ([rl rls] [en ens])
    (define bindings (match-e (rule-input rl) en))
    (unless (null? bindings)
      (set! out (cons (list* rl en bindings) out))))
  out)

(define ((rule-phase rls) rg)
  (define eg (regraph-egraph rg))
  (define limit (regraph-limit rg))
  (for* ([m (find-matches (egraph-leaders eg) rls)]
         #:break (>= (egraph-cnt eg) limit))
    (match-define (list rl en bindings ...) m)
    (for ([binding bindings] #:break (>= (egraph-cnt eg) limit))
      (define expr* (pattern-substitute (rule-output rl) binding))
      (define en* (mk-enode-rec! eg expr*))
      (merge-egraph-nodes! eg en en*))))

(define ((precompute-phase fn) rg)
  (define eg (regraph-egraph rg))
  (define limit (regraph-limit rg))
  (for ([en (egraph-leaders eg)]
        #:break (>= (egraph-cnt eg) limit))
    (set-precompute! eg en fn)))

(define (set-precompute! eg en fn)
  (for ([var (enode-vars en)] #:when (list? var))
    (define op (car var))
    (define args (map enode-atom (cdr var)))
    (when (andmap identity args)
      (define constant (apply fn op args))
      (when constant
        (define en* (mk-enode-rec! eg constant))
        (merge-egraph-nodes! eg en en*)))))

(define (prune-phase rg)
  (define eg (regraph-egraph rg))
  (define limit (regraph-limit rg))
  (for ([en (egraph-leaders eg)] #:break (>= (egraph-cnt eg) limit))
    (reduce-to-single! eg en)))

(define (extractor-phase rg)
  (extractor-iterate (regraph-extractor rg)))
