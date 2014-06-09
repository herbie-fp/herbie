#lang racket
(require casio/alternative)
(require casio/programs)
(require casio/matcher)
(require casio/points)
(require casio/common)
(require casio/redgreen)

(provide plausible-alts best-combination)
;; This value is entirely arbitrary and should probably be changed,
;; before it destroys something.
(define *branch-cost* 5)

;; A lexically scoped predicate that is true on the top level,
;; but if you're in code that was called because you were trying
;; to improve one of the branches of a conditional, then it is
;; a predicate returning true to points that will go to your part
;; of the branch, and false to points that won't. This predicate
;; takes the form of a racket expression which must be evaluated to be called.
(define *point-pred* (make-parameter (const #t)))

;; Basically matrix flipping, but for lists. So, if you pass it '((1 2 3) (4 5 6) (7 8 9)),
;; it returns '((1 4 7) (2 5 8) (3 6 9)).
(define (flip-lists list-list)
  (apply map list list-list))

;; This constant determines how aggressive our filtration is.
;; Higher values mean we will filter more aggresively, and might
;; accidentally filter out a good option.
(define *plausibility-min-region-size* 3)

;; Works like ormap, except instead of returning true if
;; any invocation of pred returns true, it returns true only
;; if at least reg-size invocations return true in a row.
(define (region-ormap pred reg-size . lsts)
  (let loop ([rest-arg-lists (flip-lists lsts)] [true-count 0])
    (cond [(= true-count reg-size) #t]
	  [(null? rest-arg-lists) #f]
	  [(apply pred (car rest-arg-lists))
	   (loop (cdr rest-arg-lists) (add1 true-count))]
	  [#t (loop (cdr rest-arg-lists) 0)])))

(define (first-pass-filter . alts)
  (let loop ([cur-alt (car alts)] [rest-alts (cdr alts)] [acc '()])
    (let ([rest-alts* (filter (λ (altn) (or (< (alt-cost altn) (alt-cost cur-alt))
					    (region-ormap < *plausibility-min-region-size*
							  (alt-errors altn) (alt-errors cur-alt))))
			      rest-alts)])
      (if (null? rest-alts*)
	  (begin (debug "Made it through the first filter: " acc #:from 'regime-changes #:depth 3)
		 (cons cur-alt acc))
	  (loop (car rest-alts*) (cdr rest-alts*) (cons cur-alt acc))))))

;; Determines which alternatives out of a list of alternatives
;; are plausible for use in regime combinations.
(define (plausible-alts alts)
  (debug "Looking for plausible alts out of " alts #:from 'regime-changes #:depth 3)
  ;; Returns a list of error-cost-points, which are the cost
  ;; of the program consed on to an error point.
  (define (make-cost-error-points altn)
    (map (curry cons (alt-cost altn)) (alt-errors altn)))

  ;; alts -> [error]
  (define (best-errors . alts)
    (apply (curry map min)
	   (map alt-errors alts)))
  ;; alts -> [cost-error-point]
  ;; For a list of alts, returns a list of, at each point,
  ;; an error-cost-point representing the best error our
  ;; alts have at that point, and the best cost of an alt
  ;; that has that error.
  (define (best-cost-errors . alts)
    (map (λ (ceps)
	   (let* ([min-err (apply min (map cdr ceps))]
		  [best-cost (apply min (map car (filter (λ (cep) (= (cdr cep) min-err))
							 ceps)))])
	     (cons best-cost min-err)))
	 (flip-lists (map make-cost-error-points alts))))
  ;; Determines whether this error-cost-point is better
  ;; than the other error-cost-point.
  (define (better? cep1 cep2)
    ;; If we have less error than the least error at this point,
    ;; we're better.
    (or (< (cdr cep1) (cdr cep2))
	(and (= (cdr cep1) (cdr cep2))
	     (<= (car cep1) (car cep2)))))
  ;; Determines whether an alt is equivilent in errors and cost.
  (define (same? alt1 alt2) (and (= (alt-cost alt1) (alt-cost alt2))
				 (andmap = (alt-errors alt1) (alt-errors alt2))))
  ;; Filter alts based on this predicate: If it's the better than all the other alts at some point,
  ;; keep it, otherwise discard it. Then, remove duplicate alts, where alts are considered the same
  ;; if they have the same error performance and cost.
  (if (>= 1 (length alts))
      alts
      (remove-duplicates (filter (lambda (altn) (region-ormap better?
							      *plausibility-min-region-size*
							      (make-cost-error-points altn)
							      (apply best-cost-errors (remove altn alts))))
				 (apply first-pass-filter alts))
			 same?)))

(define (best-option alts)
  ;; We want to check combinations on every variable, since we don't know
  ;; which variable would yield the best combination, so build a list
  ;; of all the variable indices.
  (let* ([var-indices (range (length (program-variables (alt-program (car alts)))))]
	 ;; Get all the options. We're going to get a list of options for each variable
	 ;; we try to split on, so append those lists together.
	 [all-options (apply append
			     ;; For each variable index,
			     (map (lambda (var-index)
				    ;; Map across every pair of alts, and
				    ;; make them into an option with our var index.
				    (map-pairs (curry make-option
						      var-index)
					       alts))
				  var-indices))]
	 ;; We reevaluate our options on a new set of points so that options
	 ;; that overfit will be less likely to be considered 'best'
	 [reevaluated-options (let-values ([(points exacts) (prepare-points
							     ;; We can just take the program from
							     ;; the first alt, since point preparation
							     ;; uses only the program variables
							     ;; and exact values, neither of which
							     ;; should vary between alts of the
							     ;; same run.
							     (alt-program (car alts)))])
				(let ([points* (filter (curry apply (safe-eval (*point-pred*))) points)]
				      [exacts* (map cdr (filter car (map (lambda (point exact)
									   (cons (apply (safe-eval (*point-pred*)) point)
										 exact))
									 points
									 exacts)))])
				  (map (curry reevaluate-option-on-points points exacts)
				       all-options)))]
	 ;; Use our best function to get the best option,
	 ;; comparing options by checking if one option is
	 ;; "green" over the other, on our reevaluated points.
	 [best-option (best reevaluated-options (lambda (opt1 opt2)
						  (let ([diff-score (- (errors-score (option-aug-errors^ opt1)) (errors-score (option-aug-errors^ opt2)))])
						    (or (< 0 diff-score)
							(and (= 0 diff-score) (> (option-cost opt1) (option-cost opt2)))))))])
    ;; Build the option struct and return, using the original errors on points.
    best-option))

;; Gets the best combination of two alts from a given list of alts.
;; If passed a pre-combo-func, will apply that function to both alts
;; that are combined, before combination. Will ONLY invoke pre-combo-func
;; on the alts in the combination that seems best, so the pre-combo-func
;; can be fairly costly.
(define (best-combination alts #:pre-combo-func [f identity])
  (debug "Attempting to find best combination from:" alts #:from 'regime-changes #:depth 2)
  (set! error-table (make-hash))
  (let ([best-opt (best-option alts)])
    (debug "Decided to use option " best-opt ", now improving sub-alts" #:from 'regime-changes #:depth 2)
    (let ([result (option->alt best-opt f)])
      (debug "Combination Result: " result #:from 'regime-changes #:depth 2)
      result)))

;; This function assumes that the alts had their errors filtered by
;; this condition func and it's negation respectively, and that
;; their order has not changed since then.
(define (knit-errors altn1-err altn2-err condition-func points)
  (let loop ([rest-points points] [errs1 altn1-err] [errs2 altn2-err] [acc '()])
    (if (null? rest-points)
	(reverse acc)
	(if (condition-func (car rest-points))
	    (loop (cdr rest-points) (cdr errs1) errs2 (cons (car errs1) acc))
	    (loop (cdr rest-points) errs1 (cdr errs2) (cons (car errs2) acc))))))

;; Turns an option into a new alternative.
(define (option->alt opt pre-combo-func)
  ;; Apply pre-combo-func to altn with the given points.
  (define (apply-with-points points exacts altn)
    ;; Parameterize the function call with *points* as the given points,
    ;; and *exacts* as the exacts made from those points.
    (when (or (null? points) (null? exacts))
      (error "Condition " (option-condition opt) " encompasses all of it's points in one of it's branches."
	     "Pointpred is " (*point-pred*) ", min point on condition var is " (argmin (curry (flip-args list-ref) (option-split-var-index opt)) (*points*))
	     ", max point on condition var is " (argmax (curry (flip-args list-ref) (option-split-var-index opt)) (*points*))))
    (parameterize [(*points* points)
		   (*exacts* exacts)
		   (*point-pred* (let ([vars (program-variables (alt-program (option-altn1 opt)))])
				   `(lambda ,vars
				      (and ,(option-condition opt)
					   ,(cons (*point-pred*) vars)))))]
      (pre-combo-func altn)))
  ;; Returns four values: A list of all points for which condition-func returns true,
  ;; A list of the exacts which coorespond to those points, a list of all points for
  ;; which condition-func returns false, and a list of exacts that coorespond to those
  ;; points.
  (define (split-points-exacts condition-func points exacts)
    (let loop ([rest-points points] [rest-exacts exacts] [pt '()] [et '()] [pf '()] [ef '()])
      (cond [(null? rest-points) (values pt et pf ef)]
	    [(condition-func (car rest-points))
	     (loop (cdr rest-points) (cdr rest-exacts) (cons (car rest-points) pt) (cons (car rest-exacts) et)
		   pf ef)]
	    [#t
	     (loop (cdr rest-points) (cdr rest-exacts) pt et
		   (cons (car rest-points) pf) (cons (car rest-exacts) ef))])))
  ;; Pull the a bunch of information from the options struct.
  (let* ([split-var (option-split-var opt)]
	 [condition (option-condition opt)]
	 [condition-func (compose (safe-eval `(lambda (,split-var) ,condition))
				  (curryr list-ref (option-split-var-index opt)))]
	 [split-var-index (option-split-var-index opt)]
	 [vars (program-variables (alt-program (option-altn1 opt)))])
    ;; If the condition is just #t, then this combination is no better than our input alts,
    ;; so return false.
    (cond [(eq? condition #t) #f]
	  ;; If the condition is '(not #t), the same thing but with alt1 being better than
	  ;; alt2 on all points.
	  [(and (eq? (car condition) 'not) (eq? (cadr condition) #t))
	   #f]
	  [#t
	   ;; Partition the points into the ones that will invoke alt1, and the ones that will invoke alt2
	   (let-values ([(points1 exacts1 points2 exacts2) (split-points-exacts condition-func
										(*points*)
										(*exacts*))])
	     ;; Get the improved versions of our two alts.
	     (let ([altn1* (apply-with-points points1 exacts1 (option-altn1 opt))]
		   [altn2* (apply-with-points points2 exacts2 (option-altn2 opt))])
	       ;; The new program is the old programs iffed together with our condition
	       (let ([program (cond [(eq? (car condition) 'not)
				     ;; If the condition is negated, it's simpler in the final program
				     ;; to just flip the branches.
				     `(lambda ,vars
					(if ,(cadr condition)
					    ,(program-body (alt-program altn2*))
					    ,(program-body (alt-program altn1*))))]
				    ;; If the programs ended up improving to the same thing, don't bother branching.
				    [(equal? (alt-program altn1*) (alt-program altn2*))
				     (alt-program altn1*)]
				    [#t `(lambda ,vars
					   (if ,condition
					       ,(program-body (alt-program altn1*))
					       ,(program-body (alt-program altn2*))))])]
		     ;; The errors are the option errors computed by make-option
		     [errs (knit-errors (alt-errors altn1*) (alt-errors altn2*) condition-func (*points*))]
		     ;; The cost is the worst case cost, the maximum cost of our different
		     ;; branches, plus a branch cost.
		     [cost (+ *branch-cost* (max (alt-cost altn1*) (alt-cost altn2*)))])
		 ;; Build the alt struct and return.
		 (alt program errs cost
                      (make-regime-change condition (option-altn1 opt) (option-altn2 opt) altn1* altn2* program)
                      #f 0))))])))

(define (make-regime-change cond lft1 rgt1 lft2 rgt2 final-prog)
  (let ([new-rule (rule 'regimes 'a final-prog '())])
    (change new-rule '() `((a . ()) (cond . ,cond) (lft ,lft1 ,lft2) (rgt ,rgt1 ,rgt2)))))

;; Given a list, and a function for comparing items in the list,
;; return the "best" item b, such that for all a in the list, (not (item<? b a))
(define (best lst item<?)
  (let loop ([best-item (car lst)] [rest (cdr lst)])
    (if (null? rest) best-item
	(if (item<? best-item (car rest))
	    (loop (car rest) (cdr rest))
	    (loop best-item (cdr rest))))))

;; A struct to represent the hypothetical combination of the two alternatives.
(struct option (altn1 altn2 condition errors split-var split-var-index cost) #:transparent
	#:methods gen:custom-write
	[(define (write-proc opt port mode)
	   (display "#<option " port)
	   (write (alt-program (option-altn1 opt)) port)
	   (display ", " port)
	   (write (alt-program (option-altn2 opt)) port)
	   (display ">" port))])

(struct option-aug option (errors^))

(define (alt-with-errors altn errors)
  (alt (alt-program altn) errors (alt-cost altn) (alt-change altn) (alt-prev altn) 0))

;; Given two alternatives, make an option struct to represent
;; the hypothetical combination of the two alternatives.
(define (make-option var-index altn1 altn2)
  ;; Both alts have the same variables presumably, since they
  ;; are different version of the same program.
  (let* ([vars (program-variables (alt-program altn1))]
	 ;; The split variable is the variable at the split-variable index.
	 [split-var (list-ref vars var-index)]
	 ;; Build our condition using our splitpoints sorted in ascending order.
	 [condition (get-condition (sort (get-splitpoints altn1 altn2 var-index) <)
				   split-var)]
	 ;; Compile our condition as a function for when we need to actually use it, instead
	 ;; of just putting it in our output.
	 [condition-func (safe-eval `(lambda (,split-var) ,condition))]
	 [error-list (flip-lists (map (λ (error1 error2 point)
					(if (condition-func (list-ref point var-index)) (list error1 error1 #f) (list error2 #f error2)))
				      (alt-errors altn1)
				      (alt-errors altn2)
				      (*points*)))]
	 [option-errors (car error-list)]
	 [altn1-errors (filter identity (cadr error-list))]
	 [altn2-errors (filter identity (caddr error-list))]
	 ;; The errors of the option are the errors of alt1 when alt1 should be used, and the errors of
	 ;; alt2 when alt2 should be used.
	 [errors (map (lambda (error1 error2 point) (if (condition-func (list-ref point var-index)) error1 error2))
				 (alt-errors altn1)
				 (alt-errors altn2)
				 (*points*))]
	 [cost (+ *branch-cost* (max (alt-cost altn1) (alt-cost altn2)))])
    ;; Finally, build the option structure.
    (option (alt-with-errors altn1 altn1-errors) (alt-with-errors altn2 altn2-errors) condition option-errors split-var var-index cost)))

;; Memoize getting errors for a particular program at a particular point, because each option
;; has to do it for both it's alternatives, so there's a lot of overlap
(define error-table (make-hash))

(define (merror-at prog point exact)
  (hash-ref error-table prog
	    (curry error-at prog point exact)))

(define (error-at prog point exact)
  (car (errors prog
	       (list point) (list exact))))

;; Given an option over one set of points, reevaluate it's errors over another set of points.
(define (reevaluate-option-on-points points exacts opt)
  (let* ([condition-func (safe-eval `(lambda (,(option-split-var opt)) ,(option-condition opt)))]
	 [errors^ (map (lambda (point exact)
			 (if (condition-func (list-ref point (option-split-var-index opt)))
			     (merror-at (alt-program (option-altn1 opt)) point exact)
			     (merror-at (alt-program (option-altn2 opt)) point exact)))
		       points exacts)])
    (option-aug (option-altn1 opt) (option-altn2 opt) (option-condition opt)
		(option-errors opt) (option-split-var opt) (option-split-var-index opt)
		(option-cost opt) errors^)))

;; Maps the given f across every unique, unordered pair of elements of lst.
(define (map-pairs f lst)
  (let loop ([rest lst] [acc '()])
    (if (null? (cdr rest))
	acc
	(loop (cdr rest) (append (map (curry f (car rest)) (cdr rest)) acc)))))

;; Given a set of splitpoints, and the name of the var we split on,
;; returns a racket expression which tests whether a given point should
;; use alt1.
(define (get-condition splitpoints var)
  ;; If the first splitpoint is +nan.0, then alt1 should come first.
  ;; In otherwords, if the +nan.0 is present, then our condition is the
  ;; opposite of what it would be if +nan.0 was not present, since the
  ;; locations of alt1 and alt2 are switched.
  ;; If we don't have any splitpoints, then alt1 is better than alt2 at
  ;; all of our points, so just always choose it.
  (cond [(null? splitpoints) #t]
	[(nan? (car splitpoints))
	 ;; We reverse the condition by appending not to a recursive call with the
	 ;; +nan.0 removed.
	 (list 'not (get-condition (cdr splitpoints) var))]
	[#t
	 ;; Our total condition will be the or of conditions that indicate
	 ;; that the point is in each region where alt1 applies.
	 ;; We start by adding the condition that the point is before the first
	 ;; splitpoint, the first region
	 (let ([conditions (cons `(> ,(car splitpoints) ,var)
				 ;; Then, loop across the rest of the splitpoints,
				 ;; accumulating conditions.
				 (let loop ([rest (cdr splitpoints)] [conds '()])
				   ;; If we have reached the end of the list, return our accumulator
				   (cond [(null? rest) conds]
					 ;; If we're at the second to last tlement of the list,
					 ;; then our last condition is the point being greater than
					 ;; the last splitpoint (since we only get here if we have
					 ;; an odd number of splitpoints.
					 [(null? (cdr rest)) (cons `(< ,(car rest) ,var) conds)]
					 ;; If we're somewhere in the list, take the next two elements,
					 ;; and add a condition that the point is between them.
					 [#t (loop (cddr rest) (cons `(and (< ,(car rest) ,var)
									   (> ,(cadr rest) ,var))
								     conds))])))])
	   ;; If there ended up being only one condition (a single, binary split), then we just return it.
	   ;; otherwise, or together all of our conditions.
	   (if (< 1 (length conditions))
	       (cons 'or conditions)
	       (car conditions)))]))

;; Given two alternatives and the index of the argument to split on,
;; return the points at which the alts should be split. A return value
;; starting with +nan.0 indicates that alt2 should come first, otherwise
;; alt1 should come first. You can also optionally pass a maximum number
;; of splitpoints to be returned.
(define (get-splitpoints alt1 alt2 arg-index #:max-splitpoints [max-splits 4])
  (let* ([xis (map (curryr list-ref arg-index) (*points*))]
         [err1* (map cdr (sort (map cons xis (alt-errors alt1)) < #:key car))]
         [err2* (map cdr (sort (map cons xis (alt-errors alt2)) < #:key car))]
         [xis* (sort (*points*) < #:key (curryr list-ref arg-index))]
         [difflist (for/list ([e1 err1*] [e2 err2*])
                     (cond [(< e1 e2) '<] [(= e1 e2) '=] [(> e1 e2) '>]))]
         [sindices (difflist->splitindices difflist #:max-splitpoints max-splits)])
    (when (has-duplicates? sindices)
      (error "Bad splitindices: Each index should only occur at maximum once. Indices found were " sindices))
    ;; Map across each splitindex, and make a splitpoint.
    (for/list ([i sindices])
      ;; If the splitindex is zero, turn it into an +nan.0, since the only place a zero
      ;; would appear is at the beginning, and if it appears, alt2 should come first, so
      ;; we want to put a +nan.0 to indicate that.
      (cond [(= 0 i) +nan.0]
            ;; If the index has an equality at it in the difflist, then at that point
            ;; the two alts are equal, so it's the perfect point to use as a splitpoint.
            ;; So just return the ascending point at that index.
            [(eq? '= (list-ref difflist i))
             (list-ref (list-ref xis* i) arg-index)]
            ;; If the index before this is equal (the one on the other side of the divide),
            ;; we can also just return the ascending point at that index.
            [(eq? '= (list-ref difflist (- i 1)))
             (list-ref (list-ref xis* (- i 1)) arg-index)]
            ;; Otherwise, one is better at the point before the split, and the other is
            ;; better at the point after the split, so we need to binary search to find
            ;; the splitpoint.
            [else
             (let* ([first-point (list-ref xis* i)]
                    [second-point (list-ref xis* (sub1 i))]
                    [p1 (list-ref first-point arg-index)] ; Get the points
                    [p2 (list-ref second-point arg-index)]
                    [pred
                     (λ (p)
                        (let* ([p* (point-with-dim arg-index first-point p)]
                               [exact (car (make-exacts (alt-program alt1) (list p*)))]
                               [e1 (error-at (alt-program alt1) p* exact)]
                               [e2 (error-at (alt-program alt2) p* exact)]
                               [sign (cond [(< e1 e2) '<] [(= e1 e2) '=] [(> e1 e2) '>])])
                          (eq? sign (list-ref difflist i))))])
               ;; Binary search the floats using an epsilon of one two-hundreth of the space in between the points.
               (binary-search-floats pred p1 p2 (/ (- p1 p2) 200)))]))))

(define (point-with-dim index point val)
  (map (λ (pval pindex) (if (= pindex index) val pval))
       point
       (range (length point))))

;; Given two points, the first of which is pred, and the second is not,
;; finds the point where pred becomes false, by calling split to binary
;; search the space until (split a b) returns a, b, or #f.
(define (binary-search split pred p1 p2)
  ;; Get the midpoint using our given split function
  (let ([midpoint (split p1 p2)])
    ;; If the split function returned false, we're done.
    (cond [(not midpoint) p1]
	  ;; If our midpoint is one of our points, we're done.
	  [(or (= p1 midpoint) (= p2 midpoint)) midpoint]
	  ;; If our predicate is still true of our midpoint, search the
	  ;; space between our midpoint and p2.
	  [(pred midpoint) (binary-search split pred midpoint p2)]
	  ;; Otherwise, search the space between our midpoint and p1.
	  [#t (binary-search split pred p1 midpoint)])))

;; Given two floating point numbers, the first of which is pred,
;; and the second is not, find where pred becomes false (within epsilon).
(define (binary-search-floats pred p1 p2 epsilon)
  (define (close-enough a b) (> epsilon (abs (- a b))))
  (binary-search (lambda (a b) (if (close-enough a b) #f
				   (/ (+ a b) 2)))
		 pred p1 p2))

;; Implemented here for example.
(define binary-search-ints (curry binary-search (compose floor (compose (curryr / 2) +))))

;; Gets the indices to split a region into. By default the only requirement of these regions is that they be the most accurate
;; regions where no region is less than three points in size, but you can pass in a minimum region size (default three), a
;; maximum number of splitindices, or a function that takes a single argument, a list of regions, and determines whether these
;; regions are general enough. 
(define (difflist->splitindices difflist #:min-region-size [min-size 20] #:max-splitpoints [max-splits +inf.0] #:fitness-func [fit? (const #t)])
  ;; First, convert the difflist into a list of regions, then swallow all '= regions, and then swallow any
  ;; regions less than the minimum size. Then, keep increasing the minimum size and swallowing until we have
  ;; no more than max-splits splitpoints, and we return true on our fitness function.
  (let loop ([regions (swallow-regions (compose (curry > min-size) car)
				       (swallow-regions (compose (curry eq? '=) cdr) (difflist->regions difflist)))]
	     [new-min-size (+ 1 min-size)])
    ;; If our number of regions that are not equals is less than or equal to the maximum allowed
    ;; (one more than the maximum number of splitpoints), and our fitness function returns true,
    ;; then we're done, and we should swallow the equals and return.
    (if (and (<= (length (filter (compose (compose not (curry eq? '=)) cdr) regions)) (+ 1 max-splits)) (fit? regions))
	(regions->splitindices regions)
	;; Otherwise swallow any less than our new min size, and recurse on a bigger minsize.
	(loop (swallow-regions (compose (curry > new-min-size) car) regions) (+ 1 new-min-size)))))

;; Verifies that the regionlist is well formed.
(define (verify-regions regions)
  (let loop ([acc 0] [rest-regions regions])
    (if (null? rest-regions)
	(begin (when (not (= acc (length (*points*))))
		 (error "Total size of regions must equal number of points."))
	       regions)
	(begin (when (< (caar rest-regions) 2)
		 (error "Regions cannot have zero size!"))
	       (loop (+ acc (caar rest-regions)) (cdr rest-regions))))))

;; Given a list of desired regions, returns the indices in the difflist to split at.
;; If the initial region should be of alt2, we start with an index of zero, so the
;; splitindices can be read as, starting with alt1, at every splitindex, switch alts.
;; For example: an output of (4 8 12) would indicate that alt1 should be used from 0 to 4,
;; and 8 to 12, and alt2 should be used from 4 to 8, and from 12 onwards. An output of
;; (0 6 8 10) would indicate that alt2 should be used from 0 to 6, and 8 to 10, and alt1
;; should be used from 6 to 8, and from 10 onwards.
(define (regions->splitindices regions)
  ;; First construct the splitindices with the leading zero.
  ;; We construct the splitindices by folding across the regions,
  ;; and for each region add it's size to the previous splitindex to
  ;; get the next splitindex.
  ;; We pull off the last splitindex since it would just be the size of the difflist.
  (let ([with-zero (reverse (cdr (foldl (lambda (reg acc)
				     (cons (+ (car reg) (car acc)) acc))
				   '(0) (verify-regions regions))))])
    ;; If the first region should be alt2, keep the leading zero
    (if (eq? '> (cdar regions))
	with-zero
	;; Otherwise, discard the leading zero.
	(cdr with-zero))))

;; Given a difflist, return a list of the regions in that difflist, in the
;; form of a regionlist.
;; A difflist is defined as a list composed of the symbols <, =, and >,
;; indicating whether at each of a set of points, alt1 is better than alt2, worse,
;; or the same.
;; A regionlist is defined as a list composed of regions, where each region
;; indicates a set of consecutive symbols in a difflist, and is of the form
;; (size . diff-symbol).
(define (difflist->regions difflist)
  ;; Loop across the difflist, keeping track of the size of the current region we're
  ;; building and the diffsymbol of that region, and accumulating the results in acc.
  (let loop ([restlist difflist] [cur-region-size 0] [cur-region (car difflist)] [acc '()])
    ;; If we have no more elements left, add the current region we've been working on
    ;; to our accumulator, and then reverse it (since it builds backwards), and return it.
    (cond [(null? restlist)
	   (reverse (cons (cons cur-region-size cur-region) acc))]
	  ;; If the next element is the same as the region we're working on, increment
	  ;; the current region size, and recurse on the rest of the list.
	  [(eq? (car restlist) cur-region)
	   (loop (cdr restlist) (+ cur-region-size 1) cur-region acc)]
	  ;; Otherwise, we've hit the end of our current region, so add the current region to
	  ;; the accumulator, set our new region to the symbol of the current element, set
	  ;; our size to one, and recurse.
	  [#t (loop (cdr restlist) 1 (car restlist) (cons (cons cur-region-size cur-region) acc))])))

;; Returns a regionlist that is the result of all regions in 'regions'
;; that return true to 'pred' being "swallowed" by the adjacent regions.
;; Swallowing follows these rules:
;; 1. A regions will always be swallowed by an adjacent region
;; 2. A region will be swallowed by the biggest available neighbor.
;; 3. If a region to be swallowed is on either end of the regionlist, it will
;;    be swallowed by it's existing neighbor.
;; 5. Regions are swallowed from the beginning of the list to the end.
;; 6. The result of one regions swallowing another has the type of the swallowing
;;    region.
;; Finally, note that behavior is undefined if a region which does not satisfy
;; pred can satisfy pred after swallowing another region which does satisfy pred.
(define (swallow-regions pred regions)
  ;; Returns the result of reg2 "swallowing" reg1.
  ;; The result has a size of (+ (size reg1) (size reg2)),
  ;; and a type of (type reg2).
  (define (merge-into reg1 reg2)
    (cons (+ (car reg1) (car reg2)) (cdr reg2)))
  ;; Returns the result of merging all adjacent regions
  ;; that have the same type.
  (define (merge-adjacent-regions regions)
    (reverse (foldl (lambda (reg acc)
		      (cond [(null? acc) (cons reg acc)]
			    [(eq? (cdr reg) (cdr (car acc)))
			     (cons (merge-into reg (car acc)) (cdr acc))]
			    [#t (cons reg acc)]))
		    '() regions)))
  ;; Loop over our regions from start to end.
  ;; Note, for any who read this code: the loop is always operating on
  ;; the second element of restlist, that way we have access to the item
  ;; before and after the current item, since either may be the swallower.
  ;; The first element, if it meets pred, is swallowed at the end.
  (let loop ([restlist regions] [acc '()])
    ;; If we are at the last element of the list, then we're done,
    ;; since we operate on the second element of the list (which is null).
    ;; Now that we're done, we check if the first element satisfies pred,
    ;; and if it does, we merge it into the second element, unless there
    ;; is only one element remaining in the list.
    (cond [(null? (cdr restlist)) (merge-adjacent-regions (let ([first-pass (reverse (cons (car restlist) acc))])
							    (if (and (pred (car first-pass)) (not (null? (cdr first-pass))))
								(cons (merge-into (car first-pass) (cadr first-pass)) (cddr first-pass))
								first-pass)))]
	  ;; If our second element satisfies pred, we have to merge it into one of it's neighbors.
	  [(pred (cadr restlist))
	   ;; If the second element is the last element in the list, we have to merge into the first element.
	   (cond [(null? (cddr restlist))
		  (loop (list (merge-into (cadr restlist) (car restlist))) acc)]
		 ;; If the first element has type =, merge the second element into the third element, and recurse.
		 [(< (caar restlist) (caaddr restlist))
		  (loop (list* (car restlist) (merge-into (cadr restlist) (caddr restlist)) (cdddr restlist)) acc)]
		 ;; Otherwise, merge the second element into the first element, and recurse.
		 [#t (loop (list* (merge-into (cadr restlist) (car restlist)) (cddr restlist))
			   acc)])]
	  ;; If our second element doesn't satisfy pred, add the first element to the accumulator, and recurse on
	  ;; the list from the second element onwards (cdr restlist)
	  [#t (loop (cdr restlist) (cons (car restlist) acc))])))

(define (ulps->bits e)
  (if (ordinary-float? e)
      (/ (log e) (log 2))
      64))

;; Takes a list of numbers, and returns the partial sum of those numbers.
;; For example, if your list is [1 4 6 3 8], then this returns [1 5 11 14 22].
(define (partial-sum lst)
  (let loop ([rest-lst (cdr lst)] [psum-acc (list (car lst))])
    (if (null? rest-lst)
	(reverse psum-acc)
	(loop (cdr rest-lst)
	      (cons (+ (car psum-acc) (car rest-lst))
		    psum-acc)))))

;; Struct representing a splitpoint
;; cidx = Candidate index: the candidate program that should be used to the left of this splitpoint
;; pidx = Point index: The index of the point to the left of which we should split.
;; weight = The total error-cost of the region to the left of this splitpoint
(struct sp (cidx pidx weight) #:transparent)

;; Struct representing a candidate set of splitpoints that we are considering.
;; cost = The total error in the region to the left of our rightmost splitpoint
;; splitpoints = The splitpoints we are considering in this candidate.
(struct cse (cost splitpoints) #:transparent)

(define (err-lsts->split-indices #:max-splits [max-splits 5] #:min-weight [min-weight 5] . err-lsts)
  (let ([num-candidates (length err-lsts)]
	[num-points (length (car err-lsts))]
	[psums (map partial-sum err-lsts)])
    (define (add-splitpoint sp-prev)
      (map (λ (point-idx point-entry)
	     (argmin cse-cost
		     (cons (list-ref sp-prev point-idx)
			   (filter (λ (cse)
				     (< min-weight (sp-weight (car (cse-splitpoints cse)))))
				   (apply append (map (λ (prev-split-idx prev-entry)
							(map (λ (cand-idx cand-psums)
							       (let ([new-cost (- (list-ref cand-psums point-idx)
										  (list-ref cand-psums prev-split-idx))])
								 (cse (+ (cse-cost prev-entry)
									 new-cost)
								      (cons (sp cand-idx (add1 point-idx) new-cost)
									    (cse-splitpoints prev-entry)))))
							     (range num-candidates)
							     psums))
						      (range point-idx)
						      (take sp-prev point-idx)))))))
	   (range num-points)
	   sp-prev))
    (let* ([sp-initial (map (λ (point-idx)
			      (argmin cse-cost
				      (map (λ (cand-idx cand-psums)
					     (let ([cost (list-ref cand-psums point-idx)])
					       (cse cost
						    (list (sp cand-idx (add1 point-idx) cost)))))
					   (range num-candidates)
					   psums)))
			    (range num-points))]
	   [sp-final (pipe sp-initial (make-list (sub1 max-splits) add-splitpoint))])
      (reverse (cse-splitpoints
		(list-ref
		 sp-final
		 (sub1 (length sp-final))))))))
