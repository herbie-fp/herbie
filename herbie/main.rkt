#lang racket

(require "common.rkt")
(require "points.rkt")
(require "alternative.rkt")
(require "localize-error.rkt")
(require "simplify/simplify.rkt")
(require "infer-regimes.rkt")
(require "programs.rkt")
(require "periodicity.rkt")
(require "taylor.rkt")
(require "alt-table.rkt")
(require "matcher.rkt")

(provide improve
	 ;; For the shell
	 (all-defined-out))

; For debugging
(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))
(define initial-fuel '())

(define (improve prog fuel #:get-context [get-context #f] #:samplers [samplers #f])
  (debug #:from 'progress #:depth 1 "[Phase 1 of 3] Setting up.")
  (debug #:from 'progress #:depth 3 "[1/2] Preparing points")
  (set! initial-fuel fuel)
  (let* ([samplers (or samplers (map (curryr cons sample-default) (program-variables prog)))]
	 [context (prepare-points prog samplers)])
    (parameterize ([*pcontext* context]
		   [*analyze-context* context]
		   [*start-prog* prog])
      (debug #:from 'progress #:depth 3 "[2/2] Setting up program.")
      (let* ([alt-table (setup-prog prog fuel)]
	     [result-alt
	      (begin
		(debug #:from 'progress #:depth 1 "[Phase 2 of 3] Improving.")
		(main-loop alt-table fuel))])
	(if get-context (list result-alt (atab-context alt-table)) result-alt)))))

(define (main-loop table fuel)
  (parameterize ([*pcontext* (atab-context table)])
    (match-let ([table* (improve-loop table fuel)])
      (debug #:from 'progress #:depth 1 "[Phase 3 of 3] Extracting.")
      (if ((flag 'reduce 'regimes) #t #f)
	  (match-let ([`(,tables ,splitpoints) (split-table table*)])
	    (let ([result-alt (if (= (length tables) 1)
				  (extract-alt (car tables))
				  (combine-alts splitpoints
						(if ((flag 'regimes 'recurse) #t #f)
						    (map (curryr main-loop (/ fuel 2)) tables)
						    (map extract-alt tables))))])
	      (remove-pows result-alt)))
          (remove-pows (extract-alt table*))))))

;; Implementation

(define (remove-pows altn)
  (alt-event
   `(λ ,(program-variables (alt-program altn))
      ,(let loop ([cur-expr (program-body (alt-program altn))])
	 (cond [(and (list? cur-expr) (eq? 'expt (car cur-expr))
		     (let ([exponent (caddr cur-expr)])
		       (and (not (list? exponent))
                            (not (symbol? exponent))
			    (positive? exponent)
			    (integer? exponent)
			    (exponent . < . 10))))
		(let inner-loop ([pows-left (caddr cur-expr)])
		  (if (pows-left . = . 1)
		      (cadr cur-expr)
		      (list '* (cadr cur-expr) (inner-loop (sub1 pows-left)))))]
	       [(list? cur-expr)
		(cons (car cur-expr) (map loop (cdr cur-expr)))]
	       [#t cur-expr])))
   'removed-pows
   (list altn)))

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
	 [extracted (atab-all-alts table)])
    (assert (equal? extracted (list processed))
	    #:extra-info (λ () (format "Extracted is ~a, but we gave it ~a"
				       extracted processed)))
    table))

(define (extract-alt table)
  (parameterize ([*pcontext* (atab-context table)])
    (argmin alt-history-length
            (argmins alt-cost
                     (argmins (compose errors-score alt-errors)
                              (map simplify-alt (atab-all-alts table)))))))

(define (combine-alts splitpoints alts)
  (let ([rsplits (reverse splitpoints)])
    (make-regime-alt
     `(λ ,(program-variables (*start-prog*))
	,(let loop ([rest-splits (cdr rsplits)]
		    [acc (program-body (alt-program (list-ref alts (sp-cidx (car rsplits)))))])
	   (if (null? rest-splits) acc
	       (loop (cdr rest-splits)
		     (let ([splitpoint (car rest-splits)])
		       `(if (< ,(sp-bexpr splitpoint)
			       ,(sp-point splitpoint))
			    ,(program-body (alt-program (list-ref alts (sp-cidx splitpoint))))
			    ,acc))))))
     alts splitpoints)))

(define (best-alt alts)
  (argmin alt-cost
	  (argmins (compose errors-score alt-errors)
		   alts)))

(define (improve-loop table fuel)
  (cond [(<= fuel 0)
	 (debug "Ran out of fuel, reducing... " #:from 'main #:depth 2)
	 (post-process table)]
	[(atab-completed? table)
	 (debug "Ran out of unexpanded alts in alt table, reducing..." fuel "fuel remaining" #:from 'main #:depth 2)
	 (post-process table)]
	[#t
	 (debug #:from 'progress #:depth 2 "iteration" (add1 (- initial-fuel fuel)) "/" initial-fuel)
	 (debug #:from 'progress #:depth 3 "picking best candidate")
	 (improve-loop
	  (let-values ([(picked table*) (atab-pick-alt table #:picking-func best-alt)])
	    (atab-add-altns table* (generate-alts picked)))
	  (sub1 fuel))]))

(define (generate-alts altn)
  (let* ([locs-generated 0]
	 [locs (localize-error (alt-program altn))]
	 [num-locs (length locs)])
    (append-map (λ (loc)
		  (set! locs-generated (add1 locs-generated))
		  (debug #:from 'progress #:depth 3 "generating at location" locs-generated "out of" num-locs)
		  (append (taylor-alt altn loc)
			  (generate-alts-at altn loc)))
		locs)))

(define (generate-alts-at altn loc)
  (let ([rewrite
	 ((flag 'generate 'rm) alt-rewrite-rm alt-rewrite-expression)])
    (debug #:from 'progress #:depth 4 "rewriting")
    (let* ([rewritten (rewrite (alt-add-event altn '(start rm)) #:root loc)]
	   [num-rewritten (length rewritten)]
	   [simplified 0]
	   [cleanup
	    ((flag 'generate 'simplify)
	     (λ (alt)
	       (set! simplified (add1 simplified))
	       (debug #:from 'progress #:depth 4 "simplifying alt" simplified "of" num-rewritten)
	       (simplify-alt alt))
	     identity)])
    (map cleanup rewritten))))

(define (simplify-alt altn)
  (apply alt-apply altn (simplify altn)))

(define (completely-simplify-alt altn)
  (let* ([prog (alt-program altn)]
	 [prog* `(λ ,(program-variables prog) ,(parameterize ([*max-egraph-iters* (/ (*max-egraph-iters*) 2)])
						 (simplify-expr (program-body prog))))]
	 [chng (change (rule 'simplify prog prog*) '() (map cons (program-variables prog) (program-variables prog)))])
    (debug "prog is" prog*)
    (alt-add-event (alt-delta prog* chng altn) 'final-simplify)))

(define (post-process table)
  (debug #:from 'progress #:depth 2 "Final touches.")
  (let* ([all-alts (atab-all-alts table)]
	 [num-alts (length all-alts)]
	 [zached-alts 0]
	 [maybe-zach ((flag 'reduce 'zach)
		      (λ (alt locs)
			(debug #:from 'progress #:depth 3 "zaching alt" (add1 zached-alts) "of" num-alts)
			(set! zached-alts (add1 zached-alts))
			(append-map (curry zach-alt alt) locs))
		      (const '()))]
	 [taylored-alts 0]
	 [maybe-taylor ((flag 'reduce 'taylor)
			(λ (alt locs)
			  (debug #:from 'progress #:depth 3 "tayloring alt" (add1 taylored-alts) "of" num-alts)
			  (set! taylored-alts (add1 taylored-alts))
			  (append-map (curry taylor-alt alt) locs))
			(λ (x y) x))]
	 [locss (map (compose localize-error alt-program) all-alts)]
	 [alts*
	  (apply append
		 (for/list ([alt all-alts] [locs locss])
		   (append (maybe-zach alt locs) (maybe-taylor alt locs))))]
	 [num-alts* (length alts*)]
	 [simplified-alts 0]
	 [maybe-simplify ((flag 'reduce 'simplify)
			  (λ (alt)
			    (debug #:from 'progress #:depth 3 "simplifying alt" (add1 simplified-alts) "of" num-alts*)
			    (set! simplified-alts (add1 simplified-alts))
			    (completely-simplify-alt alt))
			  identity)]
	 [table* (atab-add-altns table (map maybe-simplify alts*))])
    table*))

(define transforms-to-try
  (let ([invert-x (λ (x) `(/ 1 ,x))] [exp-x (λ (x) `(exp ,x))] [log-x (λ (x) `(log ,x))]
	[ninvert-x (λ (x) `(/ 1 (- ,x)))])
    `((0 ,identity ,identity)
      (inf ,invert-x ,invert-x)
      (-inf ,ninvert-x ,ninvert-x)
      #;(exp ,exp-x ,log-x)
      #;(log ,log-x ,exp-x))))

(define (taylor-alt altn loc)
  ; BEWARE WHEN EDITING: the free variables of an expression can be null
  (for/list ([transform transforms-to-try])
    (match transform
      [(list name f finv)
       (alt-add-event
        (make-delta altn
		    (location-do loc (alt-program altn)
				 (λ (expr) (let ([fv (free-variables expr)])
					     (if (null? fv) expr
						 (approximate expr fv #:transform (map (const (cons f finv)) fv))))))
		    'taylor)
        `(taylor ,name ,loc))])))

(define (make-delta old-alt new-prog name)
  (alt-delta new-prog (change (rule name (alt-program old-alt) new-prog) '()
			      (for/list ([var (program-variables new-prog)]) (cons var var)))
	     old-alt))

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

(define (verify-points-sorted point-lst expr)
  (define (eval-at-point pt)
    ((eval-prog `(λ ,(program-variables (*start-prog*)) ,expr) mode:fl) pt))
  (for ([p1 (drop-right point-lst 1)]
	[p2 (drop point-lst 1)])
    (assert ((eval-at-point p1) . <= . (eval-at-point p2))
	    #:extra-info (const (list p1 p2)))))

;; Verifies that for each splitpoint pred pair, where splitpoints and preds are paired
;; by position in their respective lists:
;; let p be the pred, and s be the splitpoint
;; all points between s and the splitpoint before it satisfy p, and no other points satisfy p.
(define (verify-point-preds splitpoints point-preds)
  (let ([sorted-points (car (sort-context-on-expr (*pcontext*) (sp-bexpr (car splitpoints)) (program-variables (*start-prog*))))])
    (verify-points-sorted sorted-points (sp-bexpr (car splitpoints)))
    (for/fold ([rest-pts sorted-points])
	([split splitpoints])
      (let-values ([(pred) (list-ref point-preds (sp-cidx split))]
		   [(points-before-split points-after-split)
		    (splitf-at rest-pts (λ (p) (<= ((eval-prog `(λ ,(program-variables (*start-prog*)) (sp-bexpr split))) p) (sp-point split))))])
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
