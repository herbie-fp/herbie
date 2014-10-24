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
	     [result-alt (main-loop alt-table fuel)])
	(if get-context (list result-alt (atab-context alt-table)) result-alt)))))
    
(define (main-loop table fuel)
  (parameterize ([*pcontext* (atab-context table)])
    (match-let ([table* (improve-loop table fuel)])
      (if ((flag 'reduce 'regimes) #t #f)
	  (match-let ([`(,tables ,splitpoints) (split-table table*)])
	    (let ([result-prog (if (= (length tables) 1)
				   (extract-alt (car tables))
				   (combine-alts splitpoints
						 (if ((flag 'regimes 'recurse) #t #f)
						     (map (curryr main-loop (/ fuel 2)) tables)
						     (map extract-alt tables))))])
	      result-prog))))

;; Implementation

(define (setup-prog prog fuel)
  (let* ([alt (make-alt prog)]
	 [maybe-period ((flag 'setup 'periodicity)
			(curry optimize-periodicity
			       (λ (alt)
				 (main-loop (make-alt-table (*pcontext*) alt) (/ fuel 2))))
			identity)]
	 [maybe-simplify ((flag 'setup 'simplify) simplify-alt identity)]
	 [processed (maybe-period (maybe-simplify alt))]
	 [table (make-alt-table (*pcontext*) processed)]
	 [extracted (extract-alt table)])
    (assert (eq? extracted processed)
	    #:extra-info (λ () (format "Extracted is ~a, but we gave it ~a"
				       extracted (alt-program processed))))
    table))

(define (extract-alt table)
  (parameterize ([*pcontext* (atab-context table)])
    (argmin alt-history-length
	    (argmins alt-cost
		     (argmins (compose errors-score alt-errors)
			      (atab-all-alts table))))))

(define (combine-alts splitpoints alts)
  (let ([rsplits (reverse splitpoints)])
    (make-regime-alt
     `(λ ,(program-variables (*start-prog*))
	,(let loop ([rest-splits (cdr rsplits)]
		    [acc (program-body (alt-program (list-ref alts (sp-cidx (car rsplits)))))])
	   (if (null? rest-splits) acc
	       (loop (cdr rest-splits)
		     (let ([splitpoint (car rest-splits)])
		       `(if (< ,(list-ref (program-variables (*start-prog*)) (sp-vidx splitpoint))
			       ,(sp-point splitpoint))
			    ,(program-body (alt-program (list-ref alts (sp-cidx splitpoint))))
			    ,acc))))))
     alts splitpoints)))

(define (improve-loop table fuel)
  (cond [(<= fuel 0)
	 (debug "Ran out of fuel, reducing... " #:from 'main #:depth 2)
	 (post-process table fuel)]
	[(atab-completed? table)
	 (debug "Ran out of unexpanded alts in alt table, reducing..." fuel "fuel remaining" #:from 'main #:depth 2)
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
  (let ([sibling (location-sibling loc)]
	[rewrite
         ((flag 'generate 'rm) alt-rewrite-rm alt-rewrite-expression)])
    (if (and sibling
             (= (length (location-get (location-parent loc)
                                      (alt-program altn))) 3))
	(rewrite (alt-add-event altn '(start zaching)) #:root sibling)
        '())))

(define (split-table orig-table)
  (match-let* ([(list splitpoints altns) (infer-splitpoints (atab-all-alts orig-table))])
    (if (= 1 (length splitpoints)) (list (list orig-table) splitpoints)
	(let* ([preds (splitpoints->point-preds splitpoints (length altns))]
	       [tables* (split-atab orig-table preds)])
	  (list tables* splitpoints)))))

(define (splitpoints->point-preds splitpoints num-alts)
  (let* ([var-index (sp-vidx (car splitpoints))]
	 [intervals (map cons (cons (sp #f var-index -inf.0)
				    (drop-right splitpoints 1))
			 splitpoints)])
    (for/list ([i (in-range num-alts)])
      (let ([p-intervals (filter (λ (interval) (= i (sp-cidx (cdr interval)))) intervals)])
	(debug #:from 'splitpoints "intervals are: " p-intervals)
	(λ (p)
	  (let ([var-val (list-ref p var-index)])
	    (for/or ([point-interval p-intervals])
	      (let ([lower-bound (sp-point (car point-interval))]
		    [upper-bound (sp-point (cdr point-interval))])
		(and (lower-bound . < . var-val)
		     (var-val . <= . upper-bound))))))))))

(define (verify-points-sorted point-lst vidx)
  (for ([p1 (drop-right point-lst 1)]
	[p2 (drop point-lst 1)])
    (assert ((list-ref p1 vidx) . <= . (list-ref p2 vidx))
	    #:extra-info (const (list p1 p2)))))

;; Verifies that for each splitpoint pred pair, where splitpoints and preds are paired
;; by position in their respective lists:
;; let p be the pred, and s be the splitpoint
;; all points between s and the splitpoint before it satisfy p, and no other points satisfy p.
(define (verify-point-preds splitpoints point-preds)
  (let ([sorted-points (car (sorted-context-list (*pcontext*) (sp-vidx (car splitpoints))))])
    (verify-points-sorted sorted-points (sp-vidx (car splitpoints)))
    (for/fold ([rest-pts sorted-points])
	([split splitpoints])
      (let-values ([(pred) (list-ref point-preds (sp-cidx split))]
		   [(points-before-split points-after-split)
		    (splitf-at rest-pts (λ (p) (<= (list-ref p (sp-vidx split)) (sp-point split))))])
	(assert (not (null? points-before-split)))
	(assert (andmap pred points-before-split)
		#:extra-info (λ _ (map pred points-before-split)))
	(let ([overlapping (for/first ([other-pred (remove pred point-preds)]
				       [other-split (remove split splitpoints)]
				       #:when (ormap other-pred points-before-split))
			     (list split other-split))])
	  (assert (not overlapping) #:extra-info (const overlapping)))
	points-after-split))
    (void)))
