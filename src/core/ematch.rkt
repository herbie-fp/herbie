#lang racket

(require "../programs.rkt" "enode.rkt" "egraph.rkt")
(provide match-e)

;;################################################################################;;
;;# The matcher module that allows us to match enode structure against patterns,
;;# giving you the bindings between enodes and variables in the pattern, and
;;# substitute those bindings into patterns, creating new enodes.
;;#
;;# This is copy-pasted from the old egraphs file, with minor modifications,
;;# so it might not always work correctly.
;;#
;;################################################################################;;

(define (merge . bindings)
  ;; (list bindings) -> binding
  (foldl merge2 '() bindings))

(define (merge2 binding1 binding2)
  ;; binding binding -> binding
  (if (and binding1 binding2)
      (let loop ([acc binding1] [rest binding2])
	(if (null? rest)
	    acc
	    (let* ([curr (car rest)]
		   [lookup (assoc (car curr) acc)])
	      (if lookup
		  (if (equal? (cdr lookup) (cdr curr))
		      (loop acc (cdr rest))
		      #f)
		  (loop (cons curr acc) (cdr rest))))))
      #f))

(define (match-e pat e)
  (cond
   [(constant? pat)
    (call/ec
     (λ (k)
       (for ([var (in-set (enode-vars e))])
	 (when (and (constant? var) (equal? pat var))
	   (k '(()))))
       '()))]
   [(variable? pat)
    `(((,pat . ,e)))]
   [(list? pat)
    (apply append
	   (for/list ([var (in-set (enode-vars e))])
	     (if (and (list? var) (eq? (car var) (car pat))
		      (= (length var) (length pat)))
		 (filter identity
			 (map (curry apply merge)
			      (apply cartesian-product
				     (for/list ([subpat (in-list (cdr pat))] [sube (in-list (cdr var))])
				       (match-e subpat sube)))))
		 '())))]
   [else
    (error "WTF" pat)]))
