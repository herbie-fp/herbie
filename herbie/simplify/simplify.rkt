#lang racket

(require "egraph.rkt")
(require "ematch.rkt")
(require "enode.rkt")
(require (rename-in "backup-simplify.rkt" [simplify backup-simplify]))
(require "../rules.rkt")
(require "../matcher.rkt")
(require "../alternative.rkt")
(require "../programs.rkt")
(require "../common.rkt")

(provide simplify-expr simplify *max-egraph-iters*)
(provide (all-defined-out) (all-from-out "egraph.rkt" "../rules.rkt" "ematch.rkt"))

;;################################################################################;;
;;# One module to rule them all, the great simplify. This makes use of the other
;;# modules in this directory to simplify an expression as much as possible without
;;# making unecessary changes. We do this by creating an egraph, saturating it
;;# partially, then extracting the simplest expression from it.
;;#
;;# Simplify attempts to make only one strong guarantee:
;;# that the input is mathematically equivilent to the output; that is, for any
;;# exact x, evalutating the input on x will yield the same expression as evaluating
;;# the output on x.
;;#
;;################################################################################;;

;; Cap the number of iterations to try at this.
(define *max-egraph-iters* (make-parameter 6))
(define *node-limit* (make-parameter 500))

(define (make-simplify-change program loc replacement)
  (change (rule 'simplify (location-get loc program) replacement)
          loc
          (for/list ([var (program-variables program)])
            (cons var var))))

(define (simplify altn)
  (define prog (alt-program altn))
  (cond
   [(or (not (alt-change altn)) (null? (change-location (alt-change altn))))
    (define prog* (simplify-expr (program-body prog)))
    (if ((num-nodes (program-body prog)) . > . (num-nodes prog*))
        (list (make-simplify-change prog '(2) prog*))
        '())]
   [else
    (match-define (change rule loc _) (alt-change altn))
    (define expr (location-get loc prog))
    ;; We want to avoid simplifying if possible, so we only simplify
    ;; things produced by function calls in the rule pattern. This means
    ;; no simplification if the rule output as a whole is not a function
    ;; call pattern, and no simplifying subexpressions that don't
    ;; correspond to function call patterns.
    (define pattern (rule-output rule))
    (cond
     [(not (list? pattern)) '()]
     [(not (list? expr)) '()]
     [else
      (reap [sow]
            (for ([pos (in-naturals 1)] [arg (cdr expr)] [arg-pattern (cdr pattern)])
              (when (and (list? arg-pattern) (list? arg))
                (define arg* (simplify-expr arg))
                (debug #:from 'simplify #:tag 'exit (format "Simplified to ~a" arg*))
                (when ((num-nodes arg) . > . (num-nodes arg*)) ; Simpler
                  (sow (make-simplify-change prog (append loc (list pos)) arg*))))))])]))

(define (simplify-expr expr)
  (debug #:from 'simplify #:tag 'enter (format "Simplifying ~a" expr))
  (if (has-nan? expr) +nan.0
      (let* ([iters (min (*max-egraph-iters*) (iters-needed expr))]
	     [eg (mk-egraph expr)])
	(iterate-egraph! eg iters)
	(extract-smallest eg))))

(define (num-nodes expr)
  (if (not (list? expr)) 1
      (add1 (apply + (map num-nodes (cdr expr))))))

(define (has-nan? expr)
  (or (and (number? expr) (nan? expr))
      (and (list? expr)
	   (ormap has-nan? (cdr expr)))))

;; Returns the worst-case iterations needed to simplify this expression
(define (iters-needed expr)
  (if (not (list? expr)) 0
      (let ([sub-iters-needed (apply max (map iters-needed (cdr expr)))])
	(if (let ([op (car expr)]) (or (eq? op '*) (eq? op '+) (eq? op '-) (eq? op '/)))
	    (+ 2 sub-iters-needed)
	    (+ 1 sub-iters-needed)))))

(define (iterate-egraph! eg iters #:rules [rls (*simplify-rules*)])
  (let ([start-cnt (egraph-cnt eg)])
    (debug #:from 'simplify #:depth 2 (format "iters left: ~a (~a enodes)" iters start-cnt))
    (one-iter eg rls)
    (when (and (> (egraph-cnt eg) start-cnt)
	       (> iters 1)
	       (< (egraph-cnt eg) (*node-limit*)))
      (iterate-egraph! eg (sub1 iters) #:rules rls))))

;; Iterates the egraph by applying each of the given rules in parallel
;; to the egraph nodes.
(define (one-iter eg rls)
  ;; Tries to match the rules against the given enodes, and returns a
  ;; list of matches found. Matches are of the form:
  ;; 
  ;; (rule enode . bindings)
  ;;
  ;; where bindings is a list of different matches between the rule
  ;; and the enode.
  (define (find-matches ens)
    (filter (negate null?)
	    (for*/list ([rl rls]
			[en ens])
	      (if (rule-applied? en rl) '()
		  (let ([bindings (match-e (rule-input rl) en)])
		    (if (null? bindings) '()
			(list* rl en bindings)))))))
  (define (apply-match match)
    (match-let* ([`(,rl ,en . ,bindings) match]
                 ;; These next two lines are here because an earlier
                 ;; match application may have pruned the tree,
                 ;; invalidating the this one. Luckily, a pruned
                 ;; enode will still point to it's old leader, so we
                 ;; just get the leader, and then double check the
                 ;; bindings to make sure our match hasn't
                 ;; changed. While it may be aggressive to
                 ;; invalidate any change in bindings, it seems like
                 ;; the right thing to do for now.
                 [en (pack-leader en)])
	(when (equal? (match-e (rule-input rl) en) bindings)
          ;; Apply the match for each binding.
          (for ([binding bindings])
            (merge-egraph-nodes! eg en (substitute-e eg (rule-output rl) binding)))
          ;; Prune the enode if we can.
          (try-prune-enode en)
          ;; Mark this node as having this rule applied so that we don't try
          ;; to apply it again.
          (rule-applied! en rl))))
  (define (try-prune-enode en)
    ;; If one of the variations of the enode is a single variable or
    ;; constant, reduce to that.
    (reduce-to-single! eg en)
    ;; If one of the variations of the enode chains back to itself,
    ;; prune it away. Loops in the egraph coorespond to identity
    ;; functions.
    #;(elim-enode-loops! eg en))
  (let ([matches (find-matches (egraph-leaders eg))])
    (for-each apply-match matches))
  (map-enodes (curry set-precompute! eg) eg))

(define-syntax-rule (matches? expr pattern)
  (match expr
    [pattern #t]
    [_ #f]))

(define (set-precompute! eg en)
  (for ([var (enode-vars en)])
    (when (list? var)
      (let ([constexpr
	     (cons (car var)
		   (map (compose (curry setfindf constant?) enode-vars)
			(cdr var)))])
	(when (and (not (matches? constexpr `(/ ,a 0)))
		   (not (matches? constexpr `(log 0)))
		   (not (matches? constexpr `(/ 0)))
		   (andmap real? (cdr constexpr)))
	  (let ([res (eval-const-expr constexpr)])
	    (when (and (ordinary-float? res) (exact? res))
	      (reduce-to-new! eg en res))))))))

(define (hash-set*+ hash assocs)
  (for/fold ([h hash]) ([assoc assocs])
    (hash-set h (car assoc) (cdr assoc))))

(define (extract-smallest eg)
  (define (resolve en ens->exprs)
    (let ([possible-resolutions
	   (filter identity
	     (for/list ([var (enode-vars en)])
	       (if (not (list? var)) var
		   (let ([expr (cons (car var)
				     (for/list ([en (cdr var)])
				       (hash-ref ens->exprs (pack-leader en) #f)))])
		     (if (andmap identity (cdr expr))
			 expr
			 #f)))))])
      (if (null? possible-resolutions) #f
	  (argmin expression-cost possible-resolutions))))
  (define (pass ens ens->exprs)
    (let-values ([(pairs left)
		  (partition pair?
			     (for/list ([en ens])
			       (let ([resolution (resolve en ens->exprs)])
				 (if resolution
				     (cons en resolution)
				     en))))])
      (list (hash-set*+ ens->exprs pairs)
	    left)))
  (let loop ([todo-ens (egraph-leaders eg)]
	     [ens->exprs (hash)])
    (match-let* ([`(,ens->exprs* ,todo-ens*)
		  (pass todo-ens ens->exprs)]
		 [top-expr (hash-ref ens->exprs* (pack-leader (egraph-top eg)) #f)])
      (cond [top-expr top-expr]
            [((length todo-ens*) . = . (length todo-ens))
             (error "failed to extract: infinite loop.")]
            [#t (loop todo-ens* ens->exprs*)]))))
