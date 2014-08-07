#lang racket

(require casio/common)
(require casio/points)
(require casio/alternative)
(require casio/analyze-local-error)
(require casio/simplify)
(require casio/combine-alts)
(require casio/locations)
(require casio/programs)
(require casio/periodicity)
(require casio/pareto-alts)
(require casio/matcher)

(provide improve improve-alt
	 *flags* toggle-flag! flag)

(define *flags*
  (make-parameter
   #hash([generate . (simplify rm)]
	 [evaluate . (exponent-points)]
         [reduce   . (regimes zach)]
         [setup    . (simplify periodicity)])))

(define (toggle-flag! category flag)
  (*flags*
   (hash-update (*flags*) category
		(λ (flag-list)
		  (if (member flag flag-list)
		      (remove flag flag-list)
		      (cons flag flag-list))))))

(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

(define (improve prog fuel)
  (let*-values ([(point-preparer) ((flag 'evaluate 'exponent-points)
				  prepare-points prepare-points-uniform)]
		[(pts exs) (point-preparer prog)]
		[(more-pts more-exs) (parameterize ([*num-points* 8192])
				       (prepare-points prog))])
    (parameterize ([*points* pts] [*exacts* exs]
		   [*more-points* more-pts] [*more-exacts* more-exs])
      (improve-alt (make-alt prog) fuel))))

(define (improve-alt alt fuel)
  (let ([alt-table (setup-alt alt fuel)])
    (improve-loop alt-table fuel)))

;; Implementation

(define ((flag type f) a b)
  (if (member f (hash-ref (*flags*) type
                          (λ () (error "Invalid flag type" type))))
      a
      b))

(define (setup-alt altn fuel)
  (let ([maybe-period ((flag 'setup 'periodicity)
		       (curry optimize-periodicity
			      (λ (altn)
				 ;; We call improve-loop directly because we don't want simplify or periodicity running on our
				 ;; subexpressions.
				(improve-loop (make-alt-table (*points*) altn) fuel))) identity)]
	[maybe-simplify ((flag 'setup 'simplify) simplify-alt identity)])
    (make-alt-table (*points*) (maybe-simplify (maybe-period altn)))))

(define (improve-loop table fuel)
  (cond [(<= fuel 0)
	 (debug "Ran out of fuel, reducing... " #:from 'main #:depth 2)
	 (reduce-alts (atab-all-alts table) fuel)]
	[(atab-completed? table)
	 (debug "Ran out of unexpanded alts in alt table, reducing... " #:from 'main #:depth 2)
	 (reduce-alts (atab-all-alts table) fuel)]
	[#t
	 (improve-loop
	  (let-values ([(picked table*) (atab-pick-alt table #:picking-func (curry argmin (compose errors-score alt-errors)))])
	    (atab-add-altns table* (append-map generate-alts (list picked))))
	  (- fuel 1))]))

(define (reduce-alts alts fuel)
  (let ([combine
         ((flag 'reduce 'regimes) regimes-alts (const #f))]
        [fixup
         ((flag 'reduce 'zach) zach-alt (const '()))])
    (let* ([alts* (append alts (append-map fixup alts))]
           [alts* (remove-duplicates alts* #:key alt-program)])
      (let ([combo (combine alts* fuel)])
	(if combo
	    (best-alt-final (cons combo alts*))
	    (best-alt-final alts*))))))
	#;(best-alt-final (list (or combo (best-alt alts*)) *orig-altn*))))))

(define (generate-alts altn)
  (append-map (curry generate-alts-at altn) (analyze-local-error altn)))

(define (generate-alts-at altn loc)
  (let ([rewrite
         ((flag 'generate 'rm) alt-rewrite-rm alt-rewrite-expression)]
        [cleanup
         ((flag 'generate 'simplify) simplify-alt identity)])
    (map cleanup (rewrite altn #:root loc))))

(define (simplify-alt altn)
  (apply-changes altn (simplify altn)))

(define (regimes-alts alts fuel)
  (let ([alts* (plausible-alts alts)])
    (if (> 2 (length alts*))
        #f
        (combine-alts alts*
         #:pre-combo-func (λ (altn) (improve-loop (make-alt-table (*points*) altn) (quotient fuel 2)))))))

(define (best-alt alts)
  (when (null? alts)
    (error "Trying to find the best of no alts!"))
  (argmin alt-history-length (argmins alt-cost (argmins (compose errors-score alt-errors) alts))))

(define (best-alt-final alts)
  (when (null? alts)
    (error "Something has gone horribly wrong"))
  (argmin alt-history-length
	  (argmins alt-cost
		   (argmins (compose errors-score
				     (λ (altn)
				       (errors (alt-program altn) (*more-points*) (*more-exacts*))))
			    alts))))

(define (zach-alt altn)
  (apply append
         (for/list ([loc (analyze-local-error altn)])
           (let ([sibling (location-sibling loc)])
             (if (and sibling
                      (= (length (location-get (location-parent loc)
                                               (alt-program altn))) 3))
                 (generate-alts-at altn sibling)
                 '())))))
