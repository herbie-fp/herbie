#lang racket

(require casio/simplify/egraph)
(require casio/simplify/ematch)
(require casio/simplify/enode)
(require casio/rules)
(require casio/matcher)
(require casio/alternative)
(require casio/programs)

;;(provide simplify-expr)
(provide (all-defined-out))

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

(define (simplify altn)
  (let ([slocs (if (alt-change altn)
		   (rule-slocations (change-rule (alt-change altn)))
		   '((2)))])
    (for/list ([loc slocs])
      (let* ([in (location-get loc (alt-program altn))]
	     [out (simplify-expr in)])
	(change (rule 'simplify in out '())
		loc
		(map (λ (var) (cons var var))
		     (program-variables (alt-program altn))))))))

(define (simplify-expr expr)
  (let ([eg (mk-egraph expr)])
    (iterate-egraph! eg (floor (* 1.35 (max-depth expr))))
    (extract-simplest eg)))

(define (max-depth expr)
  (if (not (list? expr)) 1
      (add1 (apply max (map max-depth (cdr expr))))))

(define (iterate-egraph! eg iters #:rules [rls *simplify-rules*])
  (let ([start-cnt (egraph-cnt eg)])
    (printf "iters left: ~a~n" iters)
    (one-iter eg rls)
    (when (and (> (egraph-cnt eg) start-cnt)
	       (> iters 1))
      (iterate-egraph! eg (sub1 iters) #:rules rls))))

(define *reduce-ratio* 2/3)

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
						       (match-e (rule-input rl) en)))
					       eg))))
		   rls))])
    (for ([match matches])
      (let ([rl (first match)]
	    [en (first (second match))]
	    [binds (cdr (second match))])
	(for ([bind binds])
	  (merge-egraph-nodes!
	   eg en
	   (substitute-e eg (rule-output rl) bind
			 #:victory? (victory-rule? rl))))))))

(define (victory-rule? rl)
  ;; Rules whose inputs are at least *reduce-ratio* smaller than their outputs
  ;; are considered victory rules.
  (<= (/ (expr-size (rule-output rl))
	 (expr-size (rule-input rl)))
      *reduce-ratio*))

(define (extract-simplest eg)
  (printf "extracting...~n")
  (let pick ([en (egraph-top eg)] [depth 0])
    (let ([flat-expr (enode-flat-expr en)]
	  [expr (enode-expr en)]
	  [victor (pick-victory en)])
      (cond [(> depth 10) (error "Loop?")]
	    [(not (list? expr)) expr]
	    [(eq? victor en)
	     (cons (car expr)
		   (map (curryr pick (add1 depth))
			(cdr expr)))]
	    [victor (pick victor (add1 depth))]
	    [#t (cons (car expr)
		      (map (λ (en subexpr)
			     (pick (pick-matching-flat en subexpr) (add1 depth)))
			   (cdr expr)
			   (cdr flat-expr)))]))))

(define (expr-size expr)
  (if (not (list? expr)) 1
      (apply + 1 (map expr-size (cdr expr)))))
