#lang racket

(require casio/rules)
(require casio/alternative)
(require casio/programs)
(require casio/points)

(provide simplify)

(define (simplify altn)
  (let* ([change (alt-change altn)]
	 [location (change-location change)]
	 [rule (change-rule change)])
    (define (simplify-at-locations slocations prog preerrors changes)
      (if (null? slocations)
	  changes
	  (let* ([full-location (append location (car slocations))]
		 [partly-simplified-prog (location-do full-location
						      prog
						      simplify-expression)]
		 [post-errors (errors partly-simplified-prog)])
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
    (let* ([simplifying-changes (simplify-at-locations (rule-slocations rule)
						     (alt-program altn)
						     (alt-errors altn)
						     '())])
      (apply-changes altn simplifying-changes))))

(define (simplify-expression expr)
  expr) ;TODO

(define (get-contained-vars expr)
  (define (get-duplicated-vars expr)
    (cond [(null? expr) '()]
	  [(symbol? expr) (list expr)]
	  [(list? expr) (map append (map get-contained-vars
					 (cdr expr)))]))
  (remove-duplicates (get-duplicated-vars expr)))
