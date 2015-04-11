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

(define (simplify altn)
  (let* ([chng (alt-change altn)]
	 [slocs (if (and chng (alt-delta? altn))
		    (map (curry append (change-location chng))
			 (rule-slocations (change-rule (alt-change altn))))
		   '((2)))])
    (filter
     identity
     (for/list ([loc slocs])
       (let* ([in (location-get loc (alt-program altn))]
	      [out (simplify-expr in)])
	 (debug #:from 'simplify #:tag 'exit (format "Simplified to ~a" out))
	 (assert (let valid? ([expr out])
		   (if (or (symbol? expr) (real? expr))
		       #t
		       (and (symbol? (car expr))
			    (andmap valid? (cdr expr))))))
	 (if ((num-nodes in) . <= . (num-nodes out))
	     #f
	     (change (rule 'simplify in out '())
		     loc
		     (map (λ (var) (cons var var))
			 (program-variables (alt-program altn))))))))))

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
  (if (not (list? expr)) 1
      (let ([sub-iters-needed (apply max (map iters-needed (cdr expr)))])
	(if (let ([op (car expr)]) (or (eq? op '*) (eq? op '+)))
	    (+ 2 sub-iters-needed)
	    (+ 1 sub-iters-needed)))))

(define (iterate-egraph! eg iters #:rules [rls *simplify-rules*])
  (let ([start-cnt (egraph-cnt eg)])
    (debug #:from 'simplify #:depth 2 (format "iters left: ~a" iters))
    (one-iter eg rls)
    (when (and (> (egraph-cnt eg) start-cnt)
	       (> iters 1)
	       (< (egraph-cnt eg) (*node-limit*)))
      (iterate-egraph! eg (sub1 iters) #:rules rls))))

(define (one-iter eg rls)
  (define (iterate-nodes ens rls)
    (let* ([matches
	    (filter (negate null?)
		    (for*/list ([rl rls]
				[en ens])
		      (if (rule-applied? en rl) '()
			  (let ([binds (match-e (rule-input rl) en)])
			    (if (null? binds) '()
				(list* rl en binds))))))])
      (let loop ([rest-matches matches])
	(when (not (null? rest-matches))
	  (let* ([match (car rest-matches)]
		 [rl (first match)]
		 [en (second match)]
		 [binds (cddr match)])
	    (rule-applied! en rl)
	    (for ([bind binds])
	      (merge-egraph-nodes!
	       eg en
	       (substitute-e eg (rule-output rl) bind))
	      ;; When elim-enode-loops! returns true, it means that we
	      ;; did an egraph rewrite to get rid of a looping path,
	      ;; so some of our previous matches might not be valid
	      ;; anymore. So, filter out the ones that are no longer
	      ;; valid before continuing.
	      (if (elim-enode-loops! eg en)
		  (loop (filter (λ (match)
				  (let ([rl (first match)]
					[en (second match)]
					[binds (cddr match)])
				    (equal? binds (match-e (rule-input rl) en))))
				(cdr rest-matches)))
		  (loop (cdr rest-matches)))))))))
  (iterate-nodes (egraph-leaders eg) rls)
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
	  (let ([res (common-eval constexpr)])
	    (when (and (ordinary-float? res) (exact? res))
	      (enode-override-expr! en res))))))))

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
				       (hash-ref ens->exprs en #f)))])
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
		 [top-expr (hash-ref ens->exprs* (egraph-top eg) #f)])
      (or top-expr (loop todo-ens* ens->exprs*)))))
