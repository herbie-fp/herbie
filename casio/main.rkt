#lang racket

(require casio/common)
(require casio/points)
(require casio/alternative)
(require casio/localize-error)
(require casio/simplify/simplify)
(require casio/infer-regimes)
(require casio/locations)
(require casio/programs)
(require casio/periodicity)
(require casio/taylor)
(require casio/alt-table)
(require casio/matcher)

(provide improve)

; For debugging
(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

(define (improve prog fuel #:get-context [get-context #f] #:samplers [samplers #f])
  (let* ([samplers (or samplers (map (curryr cons sample-float) (program-variables prog)))]
	 [context (prepare-points prog samplers)])
    (parameterize ([*pcontext* context]
		   [*analyze-context* ((flag 'localize 'cache) context #f)]
		   [*start-prog* prog])
      (let* ([alt-table (setup-prog prog fuel)]
	     [result-prog (main-loop alt-table fuel)])
	(if get-context (list result-prog (atab-context alt-table)) result-prog)))))
    
(define (main-loop table fuel)
  (parameterize ([*pcontext* (atab-context table)])
    (match-let* ([table* (improve-loop table fuel)]
		 [`(,tables ,splitpoints) (split-table table*)])
		(let ([result-prog (if (= (length tables) 1)
				       (extract-program (car tables))
				       (combine-programs splitpoints (map extract-program tables)))])
							 ;;((flag 'regimes 'recurse)
							 ;; (map (curryr main-loop (/ fuel 2)) tables)
							 ;; (map extract-program tables))))])
		  result-prog))))

;; Implementation

(define (setup-prog prog fuel)
  (let* ([alt (make-alt prog)]
	 [maybe-period ((flag 'setup 'periodicity)
			(curry optimize-periodicity
			       (λ (alt)
				 (make-alt (main-loop (make-alt-table (*pcontext*) alt) (/ fuel 2)))))
			identity)]
	 [maybe-simplify ((flag 'setup 'simplify) simplify-alt identity)])
    (make-alt-table (*pcontext*) (maybe-period (maybe-simplify alt)))))

(define (best-alt alts)
  (when (null? alts)
    (error "Trying to find the best of no alts!"))
  (argmin alt-history-length (argmins alt-cost (argmins (compose errors-score alt-errors) alts))))

(define extract-program (compose alt-program best-alt atab-all-alts))

(define (combine-programs splitpoints programs)
  (let ([rsplits (reverse splitpoints)])
    `(λ ,(program-variables (*start-prog*))
       ,(let loop ([rest-splits (cdr rsplits)]
		   [acc (program-body (list-ref programs (sp-cidx (car rsplits))))])
	  (if (null? rest-splits) acc
	      (loop (cdr rest-splits)
		    (let ([splitpoint (car rest-splits)])
		      `(if (< ,(list-ref (program-variables (*start-prog*)) (sp-vidx splitpoint))
			      ,(sp-point splitpoint))
			   ,(program-body (list-ref programs (sp-cidx splitpoint)))
			   ,acc))))))))

(define (improve-loop table fuel)
  (cond [(<= fuel 0)
	 (debug "Ran out of fuel, reducing... " #:from 'main #:depth 2)
	 (post-process table fuel)]
	[(atab-completed? table)
	 (debug "Ran out of unexpanded alts in alt table, reducing... " #:from 'main #:depth 2)
	 (post-process table fuel)]
	[#t
	 (improve-loop
	  (let-values ([(picked table*) (atab-pick-alt table #:picking-func (curry argmin (compose errors-score alt-errors)))])
	    (atab-add-altns table* (append-map generate-alts (list picked))))
	  (sub1 fuel))]))

(define (generate-alts altn)
  (append-map (curry generate-alts-at altn) (localize-error (alt-program altn))))

(define (generate-alts-at altn loc)
  (let ([rewrite
         ((flag 'generate 'rm) alt-rewrite-rm alt-rewrite-expression)]
        [cleanup
         ((flag 'generate 'simplify) simplify-alt identity)])
    (map cleanup (rewrite (alt-add-event altn '(start rm)) #:root loc))))

(define (simplify-alt altn)
  (apply alt-apply altn (simplify altn)))

(define (post-process table fuel)
  (let ([maybe-zach ((flag 'reduce 'zach) zach-alt (const '()))]
        [maybe-taylor ((flag 'reduce 'taylor) taylor-alt (λ (x y) x))])
    (let* ([all-alts (atab-all-alts table)]
           [locss (map (compose localize-error alt-program) all-alts)]
           [alts*
            (apply append
                   (for/list ([alt all-alts] [locs locss])
                     (append
                      (append-map (curry maybe-zach alt) locs)
                      (append-map (curry maybe-taylor alt) locs))))]
           [table* (atab-add-altns table alts*)])
      table*)))

(define (taylor-alt altn loc)
  ; BEWARE WHEN EDITING: the free variables of an expression can be null
  (list
   (alt-event
    (location-do loc (alt-program altn)
                 (λ (expr) (let ([fv (free-variables expr)])
                             (if (null? fv) expr (approximate-0 expr fv)))))
    `(taylor 0 ,loc) (list altn))
   (alt-event
    (location-do loc (alt-program altn)
                 (λ (expr) (let ([fv (free-variables expr)])
                             (if (null? fv) expr (approximate-inf expr fv)))))
    `(taylor inf ,loc) (list altn))))

(define (zach-alt altn loc)
  (let ([sibling (location-sibling loc)])
    (if (and sibling
             (= (length (location-get (location-parent loc)
                                      (alt-program altn))) 3))
        (generate-alts-at (alt-add-event altn '(start zaching)) sibling)
        '())))

(define (split-table table)
  (let* ([alts (atab-all-alts table)]
	 [splitpoints (infer-splitpoints alts)])
    (if (= 1 (length splitpoints)) (list (list table) splitpoints)
	(let ([preds (splitpoints->point-preds splitpoints (length alts))])
	  (let ([tables* (split-atab table preds)])
	    (assert (not (or (null? tables*) (null? (cdr tables*)))))
	    (list tables* splitpoints))))))

(define (splitpoints->point-preds splitpoints num-alts)
  (let* ([var-index (sp-vidx (car splitpoints))]
	 [intervals (map cons (cons (sp #f var-index -inf.0)
				    (take splitpoints (sub1 (length splitpoints))))
			 splitpoints)])
    (filter identity
	    (for/list ([i (in-range num-alts)])
	      (let ([p-intervals (filter (λ (interval) (= i (sp-cidx (cdr interval)))) intervals)])
		(if (null? p-intervals) #f
		    (λ (p)
		      (let ([var-val (list-ref p var-index)])
			(ormap (λ (p-i)
				 (and ((sp-point (car p-i)) . < . var-val)
				      (var-val . < . (sp-point (cdr p-i)))))
			       p-intervals)))))))))
