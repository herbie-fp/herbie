#lang racket

(require casio/simplify/egraph)
(require casio/simplify/ematch)
(require casio/simplify/enode)
(require casio/rules)

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

(define (simplify-expr expr)
  (let ([eg (mk-egraph expr)])
    (iterate-egraph! eg (max-depth expr))
    (extract-simplest eg)))

(define (max-depth expr)
  (if (not (list? expr)) 1
      (add1 (apply max (map max-depth (cdr expr))))))

(define (iterate-egraph! eg iters)
  (printf "iters: ~a~n" iters)
  (let ([start-cnt (egraph-cnt eg)])
    (for ([srule *simplify-rules*])
      (apply-rule! eg srule))
    (finalize-egraph-iter! eg)
    (when (and (> (egraph-cnt eg) start-cnt)
	       (> iters 1))
      (iterate-egraph! eg (sub1 iters)))))

(define (apply-rule! eg r)
  (printf "using rule ~a~n" (rule-name r))
  (map-enodes
   (λ (en)
     (printf "applying to ~a~n" en)
     (let ([binds (match-e (rule-input r) en)])
       (for ([bind binds])
	 (merge-egraph-nodes! eg en (substitute-e eg (rule-output r) bind)))))
   eg))

(define (victory-rule? rl)
  ;; Rules whose inputs are at least *reduce-ratio* smaller than their outputs
  ;; are considered victory rules.
  (<= (/ (expr-size (rule-output rl))
	 (expr-size (rule-input rl)))
      *reduce-ratio*))

(define (extract-simplest eg)
  (let pick ([en (egraph-top eg)])
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
