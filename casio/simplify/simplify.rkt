#lang racket

(require casio/simplify/egraph)
(require casio/simplify/ematch)
(require casio/simplify/enode)
(require casio/simplify/util)
(require casio/rules)
(require casio/matcher)
(require casio/alternative)
(require casio/programs)
(require casio/common)

(provide simplify-expr simplify)

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

;; A rule must reduce the expression size (judged by pattern) this much to
;; be considered a victory rule.
(define *reduce-ratio* 2/3)
;; The number of iterations of the egraph is the maximum depth times this custom
(define *iters-depth-ratio* 1.35)

(define (simplify altn)
  (let* ([chng (alt-change altn)]
	 [slocs (if chng
		    (map (curry append (change-location chng))
			 (rule-slocations (change-rule (alt-change altn))))
		   '((2)))])
    (filter
     identity
     (for/list ([loc slocs])
       (let* ([in (location-get loc (alt-program altn))]
	      [out (simplify-expr in)])
	 (if (equal? in out)
	     #f
	     (change (rule 'simplify in out '())
		     loc
		     (map (λ (var) (cons var var))
			 (program-variables (alt-program altn))))))))))

(define (simplify-expr expr)
  (debug #:from 'simplify #:tag 'enter (format "Simplifying ~a" expr))
  (let ([eg (mk-egraph expr)]
	[expr-depth (max-depth expr)])
    (iterate-egraph! eg (inexact->exact (floor (* *iters-depth-ratio* expr-depth))))
    (extract-smallest eg)))

(define (max-depth expr)
  (if (not (list? expr)) 1
      (add1 (apply max (map max-depth (cdr expr))))))

(define (iterate-egraph! eg iters #:rules [rls *simplify-rules*])
  (let ([start-cnt (egraph-cnt eg)])
    (debug #:from 'simplify #:depth 2 (format "iters left: ~a" iters))
    (one-iter eg rls)
    (when (and (> (egraph-cnt eg) start-cnt)
	       (> iters 1))
      (iterate-egraph! eg (sub1 iters) #:rules rls))))

(define (one-iter eg rls)
  (let* ([realcdr? (compose (negate null?) cdr)]
	 [matches
	  (filter realcdr?
		  (map
		   (λ (rl)
		     (cons rl
			   (filter realcdr?
				   (map-enodes (λ (en)
						 (cons en
						       (if (rule-applied? en rl) '()
							   (match-e (rule-input rl) en))))
					       eg))))
		   rls))])
    (for ([rmatch matches])
      (let ([rl (first rmatch)])
	(for ([ematch (rest rmatch)])
	  (let ([en (first ematch)]
		[binds (cdr ematch)])
	    (rule-applied! en rl)
	    (for ([bind binds])
	      (merge-egraph-nodes!
	       eg en
	       (substitute-e eg (rule-output rl) bind
			     #:victory? (victory-rule? rl))))))))
    (map-enodes (curry add-precompute eg) eg)))

(define (add-precompute eg en)
  (for ([var (enode-vars en)])
    (when (list? var)
      (let ([constexpr
	     (cons (car var)
		   (map (compose (curry setfindf constant?) enode-vars)
			(cdr var)))])
	(when (andmap identity (cdr constexpr))
	  (merge-egraph-nodes!
	   eg en
	   (mk-enode! eg (safe-eval constexpr) #:victory? #t)))))))

(define (victory-rule? rl)
  ;; Rules whose inputs are at least *reduce-ratio* smaller than their outputs
  ;; are considered victory rules.
  (<= (/ (expr-size (rule-output rl))
	 (expr-size (rule-input rl)))
      *reduce-ratio*))

(define (extract-simplest eg max-depth)
  (debug #:from 'simplify #:depth 2 "extracting...")
  (let pick ([en (egraph-top eg)] [depth 0])
    (let ([flat-expr (enode-flat-expr en)]
	  [expr (enode-expr en)]
	  [victor (pick-victory en)])
      (cond [(> depth max-depth) (error "Loop?")]
	    [(not (list? expr)) expr]
	    [(eq? victor en)
	     (cons (car expr)
		   (map (curryr pick (add1 depth))
			(cdr expr)))]
	    [victor (pick victor (add1 depth))]
	    [flat-expr
	     (cons (car expr)
		   (map (λ (en subexpr)
			  (pick (pick-matching-flat en subexpr) (add1 depth)))
			(cdr expr)
			(cdr flat-expr)))]
	    [#t
	     (cons (car expr)
		   (map (curryr pick (add1 depth))
			(cdr expr)))]))))

(define (extract-smallest eg)
  (define (resolve en ens->exprs)
    (let/ec return
      (for ([var (enode-vars en)])
	(when (not (list? var))
	  (return var))
	(let ([expr (cons (car var) (map (λ (en) (hash-ref ens->exprs en #f)) (cdr var)))])
	  (when (andmap identity (cdr expr))
	    (return expr))))
      #f))
  (let loop ([rest-ens (egraph-leaders eg)] [todo-ens (egraph-leaders eg)]
	     [ens->exprs (hash)])
    (let* ([en (car rest-ens)]
	   [rest-ens* (if (null? (cdr rest-ens))
			  todo-ens
			  (cdr rest-ens))]
	   [resolution (resolve en ens->exprs)])
      (if resolution
	  (if (equal? en (egraph-top eg))
	      resolution
	      (loop rest-ens* (remove en todo-ens) (hash-set ens->exprs en resolution)))
	  (loop rest-ens*
		todo-ens
		ens->exprs)))))

(define (expr-size expr)
  (if (not (list? expr)) 1
      (apply + 1 (map expr-size (cdr expr)))))
