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

(define (rewrite-local-error altn loc)
  (alt-rewrite-rm altn #:root loc))

(define (rewrite-brute-force altn)
  (alt-rewrite-tree altn))

(define (try-analyze altn)
  (let ([locs (analyze-local-error altn)])
    (append
     (apply append
	    (for/list ([loc locs])
	      (rewrite-local-error altn loc)))
     (apply append
	    (for/list ([loc locs])
	      (let-values ([(parent other) (location-parent loc)])
		(if (and
		     parent
		     (= (length (location-get parent (alt-program altn))) 3))
		    (alt-rewrite-expression altn #:root other)
		    '())))))))

(define (analyze-and-rm altn)
  (let ([locs (map car (analyze-local-error altn))])
    (apply append
	   (for/list ([loc locs])
	     (let ([subtree (location-get loc (alt-program altn))])
	       (map reverse (rewrite-expression-head subtree #:root loc)))))))

(define (try-simplify altn #:conservative [conservative #t])
  (simplify altn #:fitness-func (if conservative
				    (lambda (chng)
				      (much-better? (alt-apply altn chng)
						    altn))
				    (lambda (chng)
				      (not (much-better? altn (alt-apply altn chng)))))))

(define (improve prog max-iters)
  (debug-reset)
  (define-values (points exacts) (prepare-points prog))
  (parameterize ([*points* points] [*exacts* exacts])
    (let ([orig (make-alt prog)])
      (values (improve-with-points orig max-iters)
	      orig))))

(define *max-threshold* 30)
(define *min-threshold* 29)

(define (improve-with-points start-altn fuel)
  ;; We keep track of the programs we've seen so we don't consider the same program twice.
  (let ([seen-programs (make-hash)])
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
    ;; Consider one alt from our list of alts to consider, and return
    ;; new versions of alts, maybes, and olds appropriately.
    (define (step alts maybes olds green-threshold)
      (let* ([next (best-alt alts)]
	     [new-alts (get-children next)])
	(let-values ([(greens non-greens) (split-greens-nongreens green-threshold new-alts)])
	  (let ([greens-filtered (filter-seen greens)]
		[non-greens-filtered (filter-seen non-greens)])
	    ;; Register that we've seen these programs.
	    (for ([altn (append greens-filtered non-greens-filtered)]) (hash-set! seen-programs (alt-program altn) #t))
	    (values (append greens (remove next alts))
		    (append non-greens maybes)
		    (cons next olds))))))
    ;; Produces a list of alts from the given alt by analyzing local error,
    ;; recursively matching against our ruleset to eliminate the source of that
    ;; error, and simplifying the results.
    (define (get-children altn)
      (let* ([change-lists (analyze-and-rm altn)]
	     [analyze-improved-alts (for/list ([chng-lst change-lists])
				      (apply-changes altn chng-lst))])
	(for/list ([unsimplified analyze-improved-alts])
	  (simplify-alt unsimplified))))
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
						#:pre-combo-func (curry (flip-args improve-with-points) fuel))])
	      (or best-combo (best-alt alts))))))
    ;; Determine the alternative most likely to get us closer to our goal.
    (define (best-alt alts)
      (argmax (λ (altn) (- (errors-score (alt-errors altn)))) alts))
    ;; How much we reduce the green threshold by every time.
    (define threshold-reduction (expt (/ *min-threshold* *max-threshold*) (/ fuel)))
    ;; Main loop 2.0
    (let loop ([alts (list (simplify-alt start-altn))] [maybes '()] [olds '()] [green-threshold *max-threshold*])
      (if (null? alts)
	  ;; Lower the green threshold
	  (let ([green-threshold* (* green-threshold threshold-reduction)])
	    (if (<= green-threshold* *min-threshold*)
		(infer-regimes (append maybes olds))
		(let-values ([(greens* non-greens*) (split-greens-nongreens green-threshold* maybes)])
		  (loop greens* non-greens* olds green-threshold*))))
	  (let-values ([(alts* maybes* olds*) (step alts maybes olds green-threshold)])
	    (loop alts* maybes* olds* green-threshold))))))

;; This should only be called in a scope where *points* and *exacts* are
;; dynamically defined.
(define (improve-with-points-old altn max-iters)
  (define (final-result alts olds trace)
    (let* ([sorted (sort (reverse (append alts olds trace))
			 much-better?)]
	   [result (try-simplify (car sorted) #:conservative #f)])
      (debug "Done:" result #:from 'improve)
      result))
  (let loop ([alts (list (try-simplify altn))] [olds (list)] [trace (list)]
	     [iter max-iters])
    ;; Invariant: (no-duplicates? alts)
    ;; Invariant: (no-duplicates? olds)
    (cond
     [(= iter 0)
      (debug "Run out of iterations, trying combinations" #:from 'improve #:depth 2)
      (let ([plausible-combinors (plausible-alts (append alts olds trace))])
	(debug "Found the plausible-combinors: " plausible-combinors #:from 'improve #:depth 3)
	(if (> 2 (length plausible-combinors))
	    (begin (debug "Not enough plausible-combinors for combination" #:from 'improve #:depth 3)
		   (final-result alts olds trace))
	    (let ([best-combo (best-combination plausible-combinors
						#:pre-combo-func (curry (flip-args improve-with-points) max-iters))])
	      (or best-combo (final-result alts olds trace)))))]
     [(and (null? alts) (not (null? olds)))
					; We've exhausted all "intelligent" things to do
      (debug "Resorting to brute force"
	     #:from 'improve #:depth 2)
      (let* ([old (car olds)]
	     [old* (cdr olds)]
	     [alts* (rewrite-brute-force old)]
	     [greens
	      (map remove-red #;identity (filter (curryr much-better? old) alts*))])
	(cond
	 [(null? greens)
	  (debug "Produced" (length alts*) "alternatives, none green"
		 #:from 'improve #:tag 'info #:depth 3)
	  (loop alts* (map alt-cycles++ old*) (map alt-cycles++ (cons old trace)) (- iter 1))]
	 [else
	  (debug "Discovered" (length greens) "green changes"
		 #:from 'improve #:tag 'info #:depth 3)
	  (loop greens (list) (map alt-cycles++ (append olds alts alts* trace))
		(- iter 1))]))]
     [(and (null? alts) (null? olds))
      (error "(improve) cannot proceed: no olds or alts")]
     [else
      (debug "Step:" (car alts) #:from 'improve #:depth 2)
      (let* ([altn (car alts)]
	     [alts* (cdr alts)]
	     [next (map try-simplify (try-analyze altn))]
	     [greens
	      (map remove-red #;identity (filter (curryr much-better? altn) next))])
	(cond
	 [(null? greens)
	  (let ([next-alts (append alts* next)]
		[next-olds (cons altn olds)])
	    (debug "Produced" (length next) "alternatives, none green"
		   #:from 'improve #:tag 'info #:depth 3)
	    (loop next-alts (map alt-cycles++ next-olds) (map alt-cycles++ trace)(- iter 1)))]
	 [else 
	  (debug "Discovered" (length greens) "green changes"
		 #:from 'improve #:tag 'info #:depth 3)
	  (loop (sort greens alternative<?) (list)
		(map alt-cycles++ (append alts olds next)) (- iter 1))]))])))

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

(provide improve program-a program-b print-improve improvement improve-with-points)
