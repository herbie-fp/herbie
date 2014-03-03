#lang racket

(require casio/rules)
(require casio/alternative)
(require casio/programs)
(require casio/points)
(require casio/common)

(provide (all-defined-out))

(define (simplify altn)
  (let ([slocations (if (alt-prev altn)
			(rule-slocations (change-rule (alt-change altn)))
			'(()))]
	 [location (if (alt-prev altn)
		       (change-location (alt-change altn))
		       '(cdr cdr car))])
    (when (*debug*) (println "Simplifying " (alt-program altn)))
    (define (simplify-at-locations slocations prog preerrors changes)
      (if (null? slocations)
	  changes
	  (let* ([full-location (append location (car slocations))]
		 [partly-simplified-prog (location-do full-location
						      prog
						      simplify-expression)]
		 [post-errors (errors partly-simplified-prog (*points*) (*exacts*))])
	    (if (< 0 (apply + (errors-difference post-errors preerrors)))
		(simplify-at-locations (cdr slocations)
					partly-simplified-prog
					post-errors
					(let* ([new-rule (rule 'simplify
							       (location-get full-location
									     prog)
							       (location-get full-location
									     partly-simplified-prog)
							       '())]
					       [new-change (change new-rule full-location (map (lambda (x) (cons x x))
											       (get-contained-vars prog)))])
					  (cons new-change changes)))
		(simplify-at-locations (cdr slocations)
					prog
					preerrors
					changes)))))
    (let* ([simplifying-changes (simplify-at-locations slocations
						     (alt-program altn)
						     (alt-errors altn)
						     '())])
      (apply-changes altn simplifying-changes))))

(define (simplify-expression expr)
  (rm-fns-&-invs expr))

(define (get-contained-vars expr)
  (define (get-duplicated-vars expr)
    (cond [(null? expr) '()]
	  [(symbol? expr) (list expr)]
	  [(list? expr) (apply append (map get-duplicated-vars
					   (cdr expr)))]))
  (remove-duplicates (get-duplicated-vars expr)))

(define func-inverses
  '((exp . log)
    (square . sqrt)))

(define (rm-fns-&-invs prog)
  (define (rm-fn-&-inv expr)
;;    (when (*debug*) (println "Attempting to remove functions with their inverses from: " expr))
    (define (reverse-alist l)
      (map (lambda (pair) (cons (cdr pair) (car pair)))
	   l))
    (if (pair? expr)
	(let* ([all-invs (append func-inverses (reverse-alist func-inverses))]
	       [inv (let ([item (assoc (car expr) all-invs)]) (when item (cdr item)))])
	  (if (and inv (pair? (cadr expr)) (eq? (caadr expr) inv))
	      (cadadr expr)
	      expr))
	expr))
  (for-lists prog rm-fn-&-inv))

(define (for-lists top-expr f)
  (let ([expr* (f top-expr)])
    (if (list? expr*)
	(cons (car expr*) (map (lambda (x) (for-lists x f))
			       (cdr expr*)))
	expr*)))
