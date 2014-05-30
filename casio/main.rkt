#lang racket

(require casio/common)
(require casio/points)
(require casio/programs)
(require casio/alternative)
(require casio/brute-force)
(require casio/redgreen)
(require casio/analyze-local-error)
(require casio/simplify)
(require casio/rules)
(require casio/combine-alts)

<<<<<<< HEAD
(define (zaching-changes altn locs)
  (map list (apply append
		   (for/list ([loc locs])
		     (let-values ([(parent other) (location-parent loc)])
		       (if (and
			    parent
			    (= (length (location-get parent (alt-program altn))) 3))
			   (rewrite-expression (location-get other (alt-program altn)) #:root other)
			   '()))))))

(define (analyze-and-rm altn)
  (let ([locs (map car (analyze-local-error altn))])
    (append
     (apply append
	    (for/list ([loc locs])
<<<<<<< HEAD
=======
	      (rewrite-local-error altn loc)))
     (apply append
	    (for/list ([loc locs])
	      (let-values ([(parent other) (location-parent loc)])
		(if (and
		     parent
		     (= (length (location-get parent (alt-program altn))) 3))
		    (alt-rewrite-expression altn #:root other)
		    '())))))))

=======
>>>>>>> Removed Dead Code From Main
(define (zaching-changes altn locs)
  (map list (apply append
		   (for/list ([loc locs])
		     (let-values ([(parent other) (location-parent loc)])
		       (if (and
			    parent
			    (= (length (location-get parent (alt-program altn))) 3))
			   (rewrite-expression (location-get other (alt-program altn)) #:root other)
			   '()))))))

(define (analyze-and-rm altn)
  (let ([locs (map car (analyze-local-error altn))])
    (append
     (apply append
	    (for/list ([loc locs])
>>>>>>> Added Zaching Back In, Clearly Labeled
	      (let ([subtree (location-get loc (alt-program altn))])
		(map reverse (rewrite-expression-head subtree #:root loc)))))
     (zaching-changes altn locs))))

(define (improve prog max-iters)
  (debug-reset)
  (define-values (points exacts) (prepare-points prog))
  (parameterize ([*points* points] [*exacts* exacts])
    (let ([orig (make-alt prog)])
      (values (improve-with-points *max-threshold* *min-threshold* (expt (/ *min-threshold* *max-threshold*) (/ max-iters)) orig)
	      orig))))

(define *max-threshold* 30)
<<<<<<< HEAD
<<<<<<< HEAD
(define *min-threshold* 25)
=======
(define *min-threshold* 20)
>>>>>>> Reintroduced Regime Changes, Stopped From Spinning Out
=======
(define *min-threshold* 25)
>>>>>>> Raised Min-Threshold

(define (improve-with-points max-threshold min-threshold thresh-step start-altn)
  ;; We keep track of the programs we've seen so we don't consider the same program twice.
  (let ([seen-programs (make-hash)])
    (define (recent-improvement altn)
      (errors-diff-score (alt-errors altn)
			 (alt-errors (alt-prev altn))))
    ;; Given a threshold, split the given alts into those who improve from their parent by
    ;; at least that threshold, and those that do not.
    (define (split-greens-nongreens threshold alts)
      (let-values ([(greens non-greens) (partition (λ (altn)
						     (> (recent-improvement altn)
							threshold))
						   alts)])
	(values (map remove-red greens)
		non-greens)))
    ;; Filter out alts that we've already seen.
    (define (filter-seen alts)
      (filter (λ (altn)
		(not (hash-ref seen-programs
			       (alt-program altn)
			       #f)))
	      alts))
    (define (register-alt altn)
      (hash-set! seen-programs (alt-program altn) #t))
    ;; Consider one alt from our list of alts to consider, and return
    ;; new versions of alts, maybes, and olds appropriately.
    (define (step alts maybes olds green-threshold)
      (debug "Stepping, with alts " alts ", maybes " maybes ", olds " olds ", green-threshold " green-threshold
	     #:from 'main #:depth 2)
      (let* ([next (best-alt alts)]
	     [new-alts (get-children next)])
	(let-values ([(greens non-greens) (split-greens-nongreens green-threshold new-alts)])
<<<<<<< HEAD
<<<<<<< HEAD
	  (let ([greens-filtered (filter-seen greens)])
<<<<<<< HEAD
=======
	  (let ([greens-filtered (filter-seen greens)]
		[non-greens-filtered (filter-seen non-greens)])
	    ;; Register that we've seen these programs.
	    (for ([altn (append greens-filtered non-greens-filtered)]) (hash-set! seen-programs (alt-program altn) #t))
>>>>>>> Added Cycles Into Main Loop
=======
	  (let ([greens-filtered (filter-seen greens)])
	    ;; Register that we've seen these programs.
>>>>>>> Made Duplicate Removal More Aggressive
=======
>>>>>>> Fixed Problem Where Alts That Didn't Get Simplified Were Discarded
	    (values (append greens (map alt-cycles++ (remove next alts)))
		    (append non-greens maybes)
		    (cons next olds))))))
    ;; Produces a list of alts from the given alt by analyzing local error,
    ;; recursively matching against our ruleset to eliminate the source of that
    ;; error, and simplifying the results.
    (define (get-children altn)
      (let* ([change-lists (analyze-and-rm altn)]
	     [analyze-improved-alts (for/list ([chng-lst change-lists])
				      (apply-changes altn chng-lst))])
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> Fixed Problem Where Alts That Didn't Get Simplified Were Discarded
	(let* ([unsimplified-alts (filter-seen analyze-improved-alts)]
	       [simplified-alts (filter-seen (map simplify-alt unsimplified-alts))])
	  (for/list ([ualt unsimplified-alts])
	    (register-alt ualt))
	  (for/list ([salt simplified-alts])
	    (register-alt salt))
	  simplified-alts)))
<<<<<<< HEAD
=======
	(map (λ (altn) (register-alt altn) altn)
	     (filter-seen (for/list ([unsimplified (filter-seen analyze-improved-alts)])
			    (register-alt unsimplified)
			    (let ([simplified (simplify-alt unsimplified)])
			      simplified))))))
>>>>>>> Made Duplicate Removal More Aggressive
=======
>>>>>>> Fixed Problem Where Alts That Didn't Get Simplified Were Discarded
    ;; Simplify an alternative
    (define (simplify-alt altn)
      (let ([simplifying-changes (simplify altn)])
	(apply-changes altn simplifying-changes)))
    ;; Takes all the alts we have left, and attempts to combine two into
    ;; a regime, improving the two branches if the combination was sucessful.
    (define (infer-regimes alts)
      (let ([plausible-combinors (plausible-alts alts)])
	(if (> 2 (length plausible-combinors))
	    (best-alt alts)
	    (let ([best-combo (best-combination plausible-combinors
						#:pre-combo-func (let ([pre-num-points (length (*points*))])
								   (λ (altn)
								     (let* ([post-num-points (length (*points*))]
									    [ratio (/ pre-num-points post-num-points)])
								       (improve-with-points (* ratio min-threshold) (* ratio max-threshold)
											    thresh-step altn)))))])
	      (or best-combo (best-alt alts))))))
    ;; Determine the alternative most likely to get us closer to our goal.
    (define (best-alt alts)
<<<<<<< HEAD
<<<<<<< HEAD
      (argmin (λ (altn) (avg-bits-errors (alt-errors altn))) alts))
=======
      (argmax (λ (altn) (- (errors-score (alt-errors altn)))) alts))
>>>>>>> Reintroduced Regime Changes, Stopped From Spinning Out
=======
      (argmin (λ (altn) (avg-bits-errors (alt-errors altn))) alts))
>>>>>>> Fixed Discrepency In Error Evaluation
    ;; Main loop 2.0
    (let loop ([alts (list (simplify-alt start-altn))] [maybes '()] [olds '()] [green-threshold max-threshold])
      (if (null? alts)
	  ;; Lower the green threshold
	  (let ([green-threshold* (* green-threshold thresh-step)])
	    (if (<= green-threshold* min-threshold)
		(infer-regimes (append maybes olds))
		(let-values ([(alts* maybes*) (split-greens-nongreens green-threshold* maybes)])
		  (loop alts* maybes* olds green-threshold*))))
	  (let-values ([(alts* maybes* olds*) (step alts maybes olds green-threshold)])
	    (loop alts* maybes* olds* green-threshold))))))

<<<<<<< HEAD

=======
>>>>>>> Removed Dead Code From Main
;; For usage at the REPL, we define a few helper functions.
;;
;; PROGRAM-A and PROGRAM-B are two example programs to test.
;; (explore prog iters) returns a list of alternatives found
;; (improve prog iters) prints the found alternatives
(define (print-improve prog max-iters)
  (let-values ([(end start) (improve prog max-iters)])
    (println "Started at: " start)
    (println "Ended at:   " end)
    (println "Improvement by an average of "
	     (improvement start end)
	     " bits of precision")
    (void)))

(define (improvement start end)
  (let ([diff (errors-difference (alt-errors start) (alt-errors end))])
    (/ (apply + (filter ordinary-float? diff)) (length diff))))

(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

;(define (plot-alternatives prog iterations)
;  "Return a spectrum plot of the alternatives found."
;  (let* ([alts (explore prog iterations)]
;         [logs (map (lambda (x) (- (/ (log (alternative-error x)) (log 10)))) alts)]
;         [rands (for/list ([i (range (length logs))]) (random))])
;    (display "Found program with score ")
;    (display (alternative-score (car alts)))
;    (newline)
;    (pretty-print (alternative-program (car alts)))
;    (parameterize ([plot-width 800] [plot-height 100]
;                   [plot-x-label #f] [plot-y-label #f])
;      (plot (points (map vector logs rands))))))

(provide improve program-a program-b print-improve improvement improve-with-points *max-threshold* *min-threshold*)
