#lang racket

;; We need rules for extracting the slocations from the rules
(require casio/rules)
;; We need alternative for it's structure accessors and alt apply
(require casio/alternative)
;; We need programs for location-do and location-get
(require casio/programs)
;; We need this for print debugging, and for pipe
(require casio/common)
;; We need this to know whether a simplification caused a green change
(require casio/redgreen)
;; We use make-exacts to precompute functions of constants.
(require casio/points)
(require casio/locations)
;; We grab pattern matching for our canonicalizing rules.
(require racket/match)

(provide simplify simplify-expression)
<<<<<<< HEAD

<<<<<<< HEAD
<<<<<<< variant A
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> Fixed Small Issue With Alts Being Too Eagerly Simplified
;; Simplifies an alternative at the location specified by the most
;; recent change's rule.
=======
=======
;; Given an expression, and a root loc at which to base potential change locations,
;; returns a list of changes that would simplify that expression, based at that root
;; loc.
>>>>>>> Added Comments To simplify-expression and find-match-loc
(define (simplify-expression expr)
  (let simplify-loop ([cur-body expr] [simplifying-changes '()])
    (debug "Attempting to simplify expression: " cur-body #:from 'simplify #:depth 3)
    ;; The absolute dumbest thing: Try to find places our goal rules already apply, and apply them.
    (let rule-loop ([rules goal-rules])
      (if (null? rules)
	  (reverse simplifying-changes)
	  (let-values ([(rule-match-loc rule-match-bindings) (find-match-loc (rule-input (car rules)) cur-body)])
	    (if rule-match-loc
		(let ([new-change (change (car rules) rule-match-loc rule-match-bindings)])
		  (simplify-loop (change-apply new-change cur-body) (cons new-change simplifying-changes)))
		(rule-loop (cdr rules))))))))

;; Given a pattern and an expression, returns a values object with a location
;; within the expression at which the pattern matches, and it's bindings,
;; or (values #f #f) if there is no match. If there are multiple matches,
;; there are no guarantees about which will be returned, although in the current
;; implementation the bias is toward the top of the tree, and then left to right.
(define (find-match-loc pattern expr)
  (let loop ([loc-queue '(())])
    (if (null? loc-queue)
	(values #f #f)
	(let ([cur-exp (location-get (car loc-queue) expr)]
	      [cur-loc (car loc-queue)])
	  (let ([bindings (pattern-match pattern cur-exp)])
	    (if bindings
		(values cur-loc bindings)
		(loop (append (cdr loc-queue) (map (λ (loc-seg) (append cur-loc (list loc-seg)))
						   (cond [(not (list? cur-exp)) '()]
							 [(= (length cur-exp) 2) '(1)]
							 [#t '(1 2)]))))))))))

<<<<<<< HEAD
>>>>>>> Added find-match-loc
=======
(define (append-to-change-locations chngs loc-prefix)
  (map (λ (chng) (change (change-rule chng) (append loc-prefix (change-location chng)) (change-bindings chng)))
       chngs))

>>>>>>> Implemented Dumb Simplification
>>>>>>> variant B
####### Ancestor
;; Given an expression, and a root loc at which to base potential change locations,
;; returns a list of changes that would simplify that expression, based at that root
;; loc.
(define (simplify-expression expr)
  (let simplify-loop ([cur-body expr] [simplifying-changes '()])
    (debug "Attempting to simplify expression: " cur-body #:from 'simplify #:depth 3)
    ;; The absolute dumbest thing: Try to find places our goal rules already apply, and apply them.
    (let rule-loop ([rules goal-rules])
      (if (null? rules)
	  (reverse simplifying-changes)
	  (let-values ([(rule-match-loc rule-match-bindings) (find-match-loc (rule-input (car rules)) cur-body)])
	    (if rule-match-loc
		(let ([new-change (change (car rules) rule-match-loc rule-match-bindings)])
		  (simplify-loop (change-apply new-change cur-body) (cons new-change simplifying-changes)))
		(rule-loop (cdr rules))))))))

;; Given a pattern and an expression, returns a values object with a location
;; within the expression at which the pattern matches, and it's bindings,
;; or (values #f #f) if there is no match. If there are multiple matches,
;; there are no guarantees about which will be returned, although in the current
;; implementation the bias is toward the top of the tree, and then left to right.
(define (find-match-loc pattern expr)
  (let loop ([loc-queue '(())])
    (if (null? loc-queue)
	(values #f #f)
	(let ([cur-exp (location-get (car loc-queue) expr)]
	      [cur-loc (car loc-queue)])
	  (let ([bindings (pattern-match pattern cur-exp)])
	    (if bindings
		(values cur-loc bindings)
		(loop (append (cdr loc-queue) (map (λ (loc-seg) (append cur-loc (list loc-seg)))
						   (cond [(not (list? cur-exp)) '()]
							 [(= (length cur-exp) 2) '(1)]
							 [#t '(1 2)]))))))))))

(define (append-to-change-locations chngs loc-prefix)
  (map (λ (chng) (change (change-rule chng) (append loc-prefix (change-location chng)) (change-bindings chng)))
       chngs))

======= end
=======
;; Simplifies an alternative at the location specified by the most
;; recent change's rule.
>>>>>>> Basic Simplification Done
(define (simplify altn)
  (define (eliminate-dead-head altn)
      (let loop ([cur-alt altn] [cur-prev (alt-prev altn)])
	(cond [(not cur-prev) cur-alt]
	      [(> (alt-cost cur-alt) (alt-cost cur-prev))
	       (loop cur-prev (alt-prev cur-prev))]
	      [#t (loop cur-alt (alt-prev cur-prev))])))
  (let* ([location (if (alt-prev altn)
		       (change-location (alt-change altn))
		       '(2))]
	 [slocations (map (λ (sloc) (append location sloc))
			  (if (alt-prev altn)
			      (rule-slocations (change-rule (alt-change altn)))
			      '(())))])
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
    (debug "Simplify " altn " at locations " slocations #:from 'simplify #:tag 'enter #:depth 1)
    (let* ([unfiltered-changes (apply append (map (λ (loc) (append-to-change-locations (simplify-expression (location-get loc (alt-program altn))) loc))
						  slocations))]
	   [partially-filtered-changes (let loop ([r-changes (reverse unfiltered-changes)])
					 (if (null? r-changes)
					     '()
					     (let ([rl (change-rule (car r-changes))])
					       (if (> (rule-cost-improvement rl) *goal-cost-improvement*)
						   (reverse r-changes)
						   (loop (cdr r-changes))))))])
      ;; We set the prev pointer to null because we only care about the changes we're applying,
      ;; and we want to make sure to not have red elimination worry about any of the changes
      ;; before we simplified.
      (let loop ([cur-alt (alt-with-prev #f altn)] [rest-changes partially-filtered-changes])
	(if (null? rest-changes)
	    (alt-changes cur-alt)
	    (let ([altn* (alt-apply cur-alt (car rest-changes))])
	      (if (simpler? altn*)
		  (loop (remove-red altn* #:fitness-func reduced?) (cdr rest-changes))
		  (loop altn* (cdr rest-changes)))))))))
=======
    (if (null? slocations) altn
	(debug "Simplify " altn " at locations " slocations #:from 'simplify #:tag 'enter #:depth 1)
	(let* ([unfiltered-changes (apply append (map (λ (loc) (append-to-change-locations (simplify-expression (location-get loc (alt-program altn))) loc))
						      slocations))]
	       [partially-filtered-changes (let loop ([r-changes (reverse unfiltered-changes)])
					     (if (null? r-changes)
						 '()
						 (let ([rl (change-rule (car r-changes))])
						   (if (> (rule-cost-improvement rl) *goal-cost-improvement*)
						       (reverse r-changes)
						       (loop (cdr r-changes))))))])
	  ;; We set the prev pointer to null because we only care about the changes we're applying,
	  ;; and we want to make sure to not have red elimination worry about any of the changes
	  ;; before we simplified.
	  (let loop ([cur-alt (alt-with-prev #f altn)] [rest-changes partially-filtered-changes])
	    (if (null? rest-changes)
		(let ([result (alt-changes cur-alt)])
>>>>>>> Caused Simplify To Short Circuit On Nothing To Do
		  (debug "Simplified to " cur-alt #:from 'simplify #:depth 2)
		  result)
		(let ([altn* (alt-apply cur-alt (car rest-changes))])
		  (if (simpler? altn*)
		      (loop (remove-red altn* #:fitness-func reduced?) (cdr rest-changes))
		      (loop altn* (cdr rest-changes))))))))))
=======
    (if (null? slocations) '()
	(begin (debug "Simplify " altn " at locations " slocations #:from 'simplify #:tag 'enter #:depth 1)
<<<<<<< HEAD
	       (let* ([unfiltered-changes (apply append (map (λ (loc) (append-to-change-locations (simplify-expression (location-get loc (alt-program altn))) loc))
							     slocations))]
		      [partially-filtered-changes (let loop ([r-changes (reverse unfiltered-changes)])
						    (if (null? r-changes)
							'()
							(let ([rl (change-rule (car r-changes))])
							  (if (> (rule-cost-improvement rl) *goal-cost-improvement*)
							      (reverse r-changes)
							      (loop (cdr r-changes))))))])
=======
	       (let* ([unfiltered-changes (apply append (map (λ (loc) (append-to-change-locations
								       (simplify-expression (location-get loc (alt-program altn))) loc))
							     slocations))])
>>>>>>> Fixed Small Issue With Alts Being Too Eagerly Simplified
		 ;; We set the prev pointer to null because we only care about the changes we're applying,
		 ;; and we want to make sure to not have red elimination worry about any of the changes
		 ;; before we simplified.
<<<<<<< HEAD
		 (let loop ([cur-alt (alt-with-prev #f altn)] [rest-changes partially-filtered-changes])
		   (if (null? rest-changes)
		       (let ([result (alt-changes cur-alt)])
			 (debug "Simplified to " cur-alt #:from 'simplify #:depth 2)
			 result)
		       (let ([altn* (alt-apply cur-alt (car rest-changes))])
			 (if (simpler? altn*)
			     (loop (remove-red altn* #:fitness-func reduced?) (cdr rest-changes))
			     (loop altn* (cdr rest-changes)))))))))))
>>>>>>> Fixed API Inconsistency
=======
		 (let* ([stripped-alt (alt-with-prev #f altn)]
			[simplified-alt (apply-changes stripped-alt unfiltered-changes)]
			[re-alt (remove-red (eliminate-dead-head simplified-alt) #:fitness-func reduced? #:aggressive #f)])
		   (debug "Simplified to " re-alt #:from 'simplify #:depth 2)
		   (alt-changes re-alt)))))))
>>>>>>> Fixed Change Elimination Bug In Simplify

(define (simplify* altn)
  (let ([simplify-changes (simplify altn)])
    (apply-changes altn simplify-changes)))

(define (simpler? altn)
  (> (rule-cost-improvement (change-rule (alt-change altn)))
     *goal-cost-improvement*))

(define (simpler*? altn)
  (member (change-rule (alt-change altn))
	  goal-rules))

(define (reduced? altn)
<<<<<<< HEAD
  (> (alt-cost altn) (alt-cost (alt-prev altn))))
=======
    (debug "Simplify " altn " at locations " slocations #:from 'simplify #:tag 'enter #:depth 2)
<<<<<<< HEAD
    (apply append (map (λ (loc) (append-to-change-locations (simplify-expression (location-get loc (alt-program altn))) loc))
		       slocations))))
>>>>>>> Implemented Dumb Simplification

<<<<<<< variant A
=======
=======
    (debug "Simplify " altn " at locations " slocations #:from 'simplify #:tag 'enter #:depth 1)
>>>>>>> Tweaked Debug Info
    (let* ([unfiltered-changes (apply append (map (λ (loc) (append-to-change-locations (simplify-expression (location-get loc (alt-program altn))) loc))
						  slocations))]
	   [partially-filtered-changes (let loop ([r-changes (reverse unfiltered-changes)])
					 (if (null? r-changes)
					     '()
					     (let ([rl (change-rule (car r-changes))])
					       (if (> (rule-cost-improvement rl) *goal-cost-improvement*)
						   (reverse r-changes)
						   (loop (cdr r-changes))))))])
      ;; We set the prev pointer to null because we only care about the changes we're applying,
      ;; and we want to make sure to not have red elimination worry about any of the changes
      ;; before we simplified.
      (let loop ([cur-alt (alt-with-prev #f altn)] [rest-changes partially-filtered-changes])
	(if (null? rest-changes)
	    (alt-changes cur-alt)
	    (let ([altn* (alt-apply cur-alt (car rest-changes))])
	      (if (simpler? altn*)
		  (loop (remove-red altn* #:fitness-func reduced?) (cdr rest-changes))
		  (loop altn* (cdr rest-changes)))))))))

(define (simpler? altn)
  (> (rule-cost-improvement (change-rule (alt-change altn)))
     *goal-cost-improvement*))

<<<<<<< HEAD
>>>>>>> Basic Simplification Done
=======
(define (simpler*? altn)
  (member (change-rule (alt-change altn))
	  goal-rules))

(define (reduced? altn)
  (> (alt-cost altn) (alt-cost (alt-prev altn))))
=======
  (< (alt-cost altn) (alt-cost (alt-prev altn))))
>>>>>>> Fixed Small Issue With Alts Being Too Eagerly Simplified

>>>>>>> Changed Fitness Function For Speed
(define *goal-cost-improvement* 4)

(define (rule-cost-improvement rl)
  (let ([orig-cost (expression-cost (rule-input rl))]
	[new-cost (expression-cost (rule-output rl))])
    (if (= new-cost 0) +inf.0
	(/ orig-cost new-cost))))
<<<<<<< HEAD
=======

(define (alt-with-prev prev altn)
  (alt (alt-program altn) (alt-errors altn) (alt-cost altn) (alt-change altn) prev (alt-cycles altn)))
>>>>>>> Basic Simplification Done

(define goal-rules (sort (filter (λ (rule)
				   (> (rule-cost-improvement rule) *goal-cost-improvement*))
				 *rules*)
			 >
			 #:key rule-cost-improvement))
>>>>>>> variant B
(define (get-rule name)
  (car (filter (λ (rule) (eq? (rule-name rule) name)) *rules*)))
####### Ancestor
(define *goal-cost-improvement* 4)
(define (rule-cost-improvement rl)
  (let ([orig-cost (expression-cost (rule-input rl))]
	[new-cost (expression-cost (rule-output rl))])
    (if (= new-cost 0) +inf.0
	(/ orig-cost new-cost))))
(define goal-rules (sort (filter (λ (rule)
				   (> (rule-cost-improvement rule) *goal-cost-improvement*))
				 *rules*)
			 >
			 #:key rule-cost-improvement))
======= end

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< variant A
=======
(define goal-rules (sort (filter (λ (rule)
				   (> (rule-cost-improvement rule) *goal-cost-improvement*))
				 *rules*)
			 >
			 #:key rule-cost-improvement))

>>>>>>> Changed Fitness Function For Speed
(define (alt-with-prev prev altn)
  (alt (alt-program altn) (alt-errors altn) (alt-cost altn) (alt-change altn) prev (alt-cycles altn)))

(define (get-rule name)
  (car (filter (λ (rule) (eq? (rule-name rule) name)) *rules*)))

=======
>>>>>>> Basic Simplification Done
;; Return the variables that are in the expression
(define (get-contained-vars expr)
  ;; Get a list that of the vars, with each var repeated for each time it appears
  (define (get-duplicated-vars expr)
    (cond [(or (null? expr) (real? expr)) '()] ; If we've reached the end of a list, or we're at a constant, there are no vars.
	  [(symbol? expr) (list expr)] ; If we're at a variable, return it in a list
	  [(list? expr) (apply append (map get-duplicated-vars
					   (cdr expr)))])) ; If we're at a list, get the vars from all of it's items, and append them together.
  (remove-duplicates (get-duplicated-vars expr))) ; Get the list with duplicates, and remove the duplicates.
<<<<<<< HEAD

>>>>>>> variant B
;; Simplifies an alternative at the location specified by the most
;; recent change's rule. If passed a fitness-function, only applies
;; the simplification at any given location if fitness-func, when
;; passed the change, returns true.

####### Ancestor
;; Simplifies an alternative at the location specified by the most
;; recent change's rule. If passed a fitness-function, only applies
;; the simplification at any given location if fitness-func, when
;; passed the change, returns true.
(define (simplify-old altn #:fitness-func [fit? (const #t)])

  ;; Creates a simplifying change at the given location in the given program
  (define (make-simplification-change prog location)
    (let* ([simplified-prog (location-do location prog simplify-expression)]
	   [new-rule (rule 'simplify (location-get location prog)
			   (location-get location simplified-prog) '())])
	(change new-rule location (map (lambda (x) (cons x x)) (get-contained-vars prog)))))

  ;; Grab the simplification locations from the rule, and then the location of the
  ;; change, since the slocations are relative to the change.
  (let ([slocations (if (alt-prev altn)
			(map list (rule-slocations (change-rule (alt-change altn))))
			'(()))]
	[location (if (alt-prev altn)
		      (change-location (alt-change altn))
		      '(2))])
    (debug "Simplifying" (alt-program altn)
	   "at" (map (curry append location) slocations)
	   #:from 'simplify #:tag 'enter #:depth 2)

    (apply-changes altn
                   (filter fit?
                           (map (compose (curry make-simplification-change (alt-program altn))
                                         (curry append location))
                                slocations)))))

;; Return the variables that are in the expression
(define (get-contained-vars expr)
  ;; Get a list that of the vars, with each var repeated for each time it appears
  (define (get-duplicated-vars expr)
    (cond [(or (null? expr) (real? expr)) '()] ; If we've reached the end of a list, or we're at a constant, there are no vars.
	  [(symbol? expr) (list expr)] ; If we're at a variable, return it in a list
	  [(list? expr) (apply append (map get-duplicated-vars
					   (cdr expr)))])) ; If we're at a list, get the vars from all of it's items, and append them together.
  (remove-duplicates (get-duplicated-vars expr))) ; Get the list with duplicates, and remove the duplicates.

;; Gets all the rules which reduce an expression
(define reduction-rules
  (filter (lambda (rule) ; We filter through the total list of rules
	    (symbol? (rule-output rule)))
	  *rules*))

;;Try to apply a list of rules to an expression.
(define (attempt-apply-all rules expr)
  (pipe expr (map (lambda (rule)
		    (lambda (expr)
		      (attempt-apply rule expr)))
		  rules)))

;; Try to apply a rule to an expression. If it fails, just return the expression.
(define (attempt-apply arule expr)
  ;; Try to get the bindings.
  (let ([match  (pattern-match (rule-input arule) expr)])
    (if match ; If we have bindings, apply them. Otherwise, jut return the expression.
	(pattern-substitute (rule-output arule) match)
	expr)))

;; Given an expression and a function, apply that function to every list in the expression, working inwards.
(define (for-lists top-expr f)
  ;; First apply the function to the top level expression
  (if (pair? top-expr)
      (let ([expr* (f top-expr)])
	(if (pair? expr*)
	    ;; If we still have a list, recurse on all of it's items
	    (cons (car expr*) (map (lambda (x) (for-lists x f))
				   (cdr expr*)))
	    ;; If we've reached a non-list, or applying the function returned a list, we're done.
	    expr*))
      top-expr))

;; Take a canonicalized expression, and add all like terms
(define (resolve-terms terms)
  ;; For each term, attempt to resolve it with the others.
  (let loop ([unresolved-terms terms] [acc '()])
    ;; If we still have terms left
    (if (null? unresolved-terms)
	;; If we're out of terms, our acc is empty, just return the zero expression.
	;; If we have only one term, just return that term. If we have more terms,
	;; make an addition.
	(if (null? acc) 0 acc)
	(let* ([cur-term (car unresolved-terms)]
	       [rest-terms (cdr unresolved-terms)]
	       [mterms (filter (lambda (t)
				 (equal? (term-atoms t)
					 (term-atoms cur-term)))
			       rest-terms)]) ; Grab every term that matches the current term
	  (if (= 0 (length mterms)) ; If we didn't get any terms, just add it to the accumulator, and recurse on the rest.
	      (loop rest-terms (cons cur-term acc))
	      ;; If we did get terms, remove the matching terms from the terms we have left.
	      (loop (remove* mterms rest-terms)
		    ;; And combine the matching terms and the term they matched with, and append it to the accumulator
		    (let ([new-term (combine-like-terms (cons cur-term mterms))])
		      (if new-term
			  (cons new-term acc)
			  acc))))))))
=======
>>>>>>> Basic Simplification Done

;; Cancel appropriate factors
(define (resolve-factors factors)
  ;; Cancel factors, given some inverted factors and non-inverted factors
  (define (cancel-factors factors inverted-factors)
    (cond [(null? factors) inverted-factors] ; If there are no factors, we just return the inverted factors
	  [(null? inverted-factors) factors] ; If there are no inverted factors left, return the factors
	  [(member (caddr (car inverted-factors)) factors)
	   (cancel-factors (remove (caddr (car inverted-factors)) factors) (cdr inverted-factors))] ; If there is a factor to cancel, cancel it.
	  [#t (cancel-factors (cons (car inverted-factors) factors) (cdr inverted-factors))])) ; Otherwise, just add this non-cancelable factor to the factors we return at the end.
  (let-values ([(inverted non-inverted) (partition (lambda (element)
						     (and (not (atomic? element))
							  (eq? (car element) '/)))
						   factors)]) ; Partition the elements into inverted and non-inverted
    (cancel-factors non-inverted inverted))) ;; Return the newly cancelled factors

;; Get the atoms of a term. Terms are made up of atoms multiplied together.
(define (term-atoms expr)
  (cond [(number? expr) '()] ; All constants can be combined
	[(symbol? expr) (list expr)] ; If the expression is a single variable, then that's it's only atom
	[(number? (cadr expr)) (cddr expr)] ; If the second item is a number, return everything past it
	[(eq? '- (car expr)) (term-atoms (cadr expr))] ; If the term is negated, get the atoms from it's positive version
	[(eq? '* (car expr)) (cdr expr)] ; If we have a term of the form (* x y z ...), so just return everything past the '*
	[#t (list expr)])) ; Otherwise, we can't break down this term any further.

;; Return whether or not the expression is atomic, meaning it can't be broken down into arithmetic.
(define (atomic? expr)
  (or (real? expr) ; Numbers are atomic
      (symbol? expr) ; as are variables
      (not (or (eq? (car expr) ; Or everything that hasn't been broken down by the canonicalizing into an operator
		    '-)
	       (eq? (car expr)
		    '+)
	       (eq? (car expr)
		    '/)
	       (eq? (car expr)
		    '*)))))
;; Returns whether or not the given expression is a full program
(define (prog? expr)
  (and (not (atomic? expr))
       (or (eq? (car expr) 'lambda)
	   (eq? (car expr) 'λ))))

;; Get the constant factor of a given term.
(define (factor term)
  (cond [(real? term) term]
	[(atomic? term) 1]
	[(eq? '- (car term)) (- (factor (cadr term)))]
	[(real? (cadr term)) (cadr term)]
	[#t 1]))

;; Combine all terms that are combinable, and return a result of a list of one or zero terms
(define (combine-like-terms terms)
  ;;(debug "Combining terms" terms #:from 'combine-like-terms #:tag 'enter)
  ;; Get the combined constant factor.
  (let ([new-factor (foldr (lambda (t acc) (+ acc (factor t)))
			   0 terms)]) ; We just fold over terms, trying to combine their constant factors
    (cond [(= 0 new-factor) #f] ; If our terms canceled, return false.
	  [(real? (car terms)) new-factor] ; If the terms are constants, just return a list of that factor
	  [(symbol? (car terms)) (if (= 1 new-factor) (list (car terms)) (list '* new-factor (car terms)))]
	  [(eq? '* (caar terms)) (let ([body (if (real? (cadar terms)) (cddar terms) (cdar terms))])
				   (cond [(= 1 new-factor) (cons '* body)] ; If we have a factor of one, return the body multiplied together
					 [(= -1 new-factor) (list '- (cons '* body))] ; If we have a factor of negative one, do the same but negate it
					 [#t (list* '* new-factor body)]))] ; Otherwise, mutliply together the factor and the body.
	  [#t (list '* new-factor (car terms))])))

======= end
;; Provide sorting for symbols so that we can canonically order variables and other atoms
(define (symbol<? sym1 sym2)
  (string<? (symbol->string sym1) (symbol->string sym2)))

;; Provide sorting for atoms. This ordering doesn't necessarily make any sense, but it's canonical so that we can more easily compare terms
(define (atom<? a1 a2)
  (cond [(and (symbol? a1) (symbol? a2))
	 (symbol<? a1 a2)] ; If they're both symbols, compare as symbols
	[(symbol? a1) #f] ; If only the first is a symbol, it's "greater"
	[(symbol? a2) #t] ; If only the second is a symbol, it's "less"
	[#t (let ([vars1 (get-contained-vars a1)] ;; Compare by the vars in the expression. This ordering doesn't really matter, as long as it's the same every time.
		  [vars2 (get-contained-vars a2)])
	      (let loop ([avars vars1] [bvars vars2])
		(cond [(and (null? avars) (null? bvars)) #f]
		      [(null? avars) #t]
		      [(null? bvars) #f]
		      [#t (if (eq? (car avars) (car bvars))
			      (loop (cdr avars) (cdr bvars))
			      (symbol<? (car avars) (car bvars)))])))]))

;; Sorts expressions. Expressions are sorted by their first atom.
(define (expr<? expr1 expr2)
  ;; Get the first atom of an expression recursively
  (define (first-atom expr)
    (if (atomic? expr)
	expr
	(first-atom (car expr))))
  ;; Sort the expressions by their first atom
  (atom<? (first-atom expr1) (first-atom expr2)))

<<<<<<< HEAD
<<<<<<< variant A
=======
>>>>>>> Basic Simplification Done
;; Return whether or not the expression is atomic, meaning it can't be broken down into arithmetic.
(define (atomic? expr)
  (or (real? expr) ; Numbers are atomic
      (symbol? expr) ; as are variables
      (not (or (eq? (car expr) ; Or everything that hasn't been broken down by the canonicalizing into an operator
		    '-)
	       (eq? (car expr)
		    '+)
	       (eq? (car expr)
		    '/)
	       (eq? (car expr)
		    '*)))))

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
>>>>>>> variant B
####### Ancestor
;; Takes terms and creates an addition expression where each addition only adds two things
(define (decanonicalize-addition terms)
  (define (f terms acc)
    (if (null? terms)
	 acc
	 (f (cdr terms) (single-decanonicalize (list '+ (car terms) acc)))))
  (f (cddr terms) (single-decanonicalize `(+ ,(cadr terms) ,(car terms)))))

;; Takes a list of factors and creates a multiplication expression where each multiplication only multiplies two things.
(define (decanonicalize-multiplication factors)
  (define (f factors acc)
    (if (null? factors) acc
	(f (cdr factors) (single-decanonicalize (list '* (car factors) acc)))))
  (f (cddr factors) (single-decanonicalize `(* ,(cadr factors) ,(car factors)))))

;; Decanonicalize the top level operator of an expression
(define (single-decanonicalize expr)
  (match expr
    [`(+ ,a (- ,b)) `(- ,a ,b)]
    [`(+ (- ,a) ,b) `(- ,b ,a)]
    [`(+ ,a ,b ,c . ,n) (decanonicalize-addition (reverse (list* a b c n)))]
    [`(* ,a (/ 1 ,b)) `(/ ,a ,b)]
    [`(* (/ 1 ,a) ,b) `(/ ,b ,a)]
    [`(* ,a ,b ,c . ,n) (decanonicalize-multiplication (reverse (list* a b c n)))]
    [a a]))

;; Turn an expression outputed by the simplifier into a form more friendly to the rest of casio.
(define (decanonicalize expr)
  (if (list? expr)
      (let ([expr* (cons (car expr) (map decanonicalize (cdr expr)))])
	(single-decanonicalize expr*))
      expr))


;; Create an addition expression from a list of terms.
(define (addition terms)
  (let ([terms* (resolve-terms (sort terms expr<?))])
    (cond [(null? terms*) 0]
	  [(= 1 (length terms*)) (car terms*)]
	  [#t (cons '+ terms*)])))

;; Create a multiplication expression fro a list of factors
(define (multiplication factors)
  (let ([factors* (resolve-factors (sort factors expr<?))])
    (cond [(null? factors*) 1]
	  [(= 1 (length factors*)) (car factors*)]
	  [#t (cons '* factors*)])))

;; Memoize single simplify, it seems to cut down the total time taken by about 80-90%
(define simplification-table (make-hash))

(define (msingle-simplify expr)
  (hash-ref! simplification-table expr
	     (lambda () (single-simplify expr))))

;; Simplify an expression, with the assumption that all of it's subexpressions are already simplified.
(define (single-simplify expr)
  (let ([expr* (try-precompute (attempt-apply-all reduction-rules
						  expr))]) ; First attempt to reduce the expression using our reduction rules.
    (match expr*
      [`(- ,a ,a) 0] ; A number minus itself is zero
      [`(- ,a ,b) (inner-simplify-expression `(+ ,a (- ,b)))] ; Move the minus inwards, and make a recursive call in case the minus needs to be moved further inwards
      [`(- 0) 0] ; Negative zero is still zero
      [`(- (- ,a)) a] ; Double negate is positive
      [`(- (+ . ,as)) (inner-simplify-expression (cons '+ (map (lambda (a) (list '- a)) as)))] ; Move Subtraction inwards
      [`(/ ,a ,a) 1] ; A number divided by itself is 1
      [`(+ ,a 0) a] ; Additive Identity
      [`(+ 0 ,a) a]
      [`(* ,a 0) 0] ; Multiplying anything by zero yields zero
      [`(* 0 ,a) 0]
      [`(/ 0 ,a) 0] ; Dividing zero by anything yields zero.
      [`(* ,a 1) a] ; Multiplicitive Identity
      [`(* ,a ,a) `(sqr ,a)] ; This rule should help sqr and sqrts cancel more often.
      [`(* 1 ,a) a]
      [`(/ 1 1) 1] ; Get rid of any one-over-ones
      [`(/ 1 (/ 1 ,a)) a] ; Double inversion
      [`(/ ,a) `(/ 1 ,a)] ; A bit hacky, since we shouldn't be producing expressions of this form, but this is a valid expression in racket, so hey.
      [`(/ 1 ,a) `(/ 1 ,a)] ; Catch this case here so that it doesn't fall to the next rule, resulting in infinite recursion
      [`(/ ,a ,b) (inner-simplify-expression `(* ,a (/ 1 ,b)))] ; Move the division inwards, and make a recursive call in case the division needs to be moved further inwards
      [`(+ (+ . ,a) (+ . ,b)) (addition (append a b))] ; These next three rules flatten out addition
      [`(+ (+ . ,a) ,b) (addition (cons b a))]
      [`(+ ,a (+ . ,b)) (addition (cons a b))]
      [`(+ . ,a) (addition a)] ; Sort addition in canonical order and attempt to cancel terms
      ;; Distribute addition (and by extension, subtraction) out of multiplication. We do this before flattening multiplication,
      ;; because it would be a lot harder to write these rules for arbitrary length multiplication. We do this after flattening
      ;; addition, because it ensures we cancel all terms without the need to recurse. Finally, we do this after turning subtractions
      ;; into additions so that subtractions also get distributed.
      [`(* (+ . ,as) (+ . ,bs)) (addition (apply append (map (lambda (b) (map (lambda (a) (multiplication (list a b))) as)) bs)))]
      [`(* ,a (+ . ,bs)) (addition (map (lambda (b) (multiplication (list a b))) bs))]
      [`(* (+ . ,as) ,b) (addition (map (lambda (a) (multiplication (list a b))) as))]
      [`(* (* . ,a) (* . ,b)) (multiplication (append a b))] ; Flatten out multiplication
      [`(* (* . ,a) ,b) (multiplication (cons b a))]
      [`(* ,a (* . ,b)) (multiplication (cons a b))]
      [`(* . ,a) (multiplication a)]
      [`(log (* . ,as)) (addition (map (lambda (a) (inner-simplify-expression `(log ,a))) as))] ; Log of product is sum of logs
      [`(exp (+ . ,as)) (multiplication (map (lambda (a) (inner-simplify-expression `(exp ,a))) as))] ; Same thing for exponents
      [`(sqr (* . ,as)) (multiplication (map (lambda (a) (list 'sqr a)) as))] ;; Product of powers.
      [`(sqrt (* . ,as)) (multiplication (map (lambda (a) (list 'sqrt a)) as))]
      [a a]))) ; Finally, if we don't have any other match, return ourselves.

;; Simplify an arbitrary expression to the best of our abilities.
(define (inner-simplify-expression expr)
  ;; If the expression is a literal or a variable, there's nothing to be done
  (if (list? expr)
      ;; First, attempt to simplify all subexpressions. Then, to a top level simplify on the resulting expression.
      (let ([expr* (cons (car expr)
			 (map inner-simplify-expression (cdr expr)))])
	(msingle-simplify expr*))
      expr))

======= end
=======
>>>>>>> Basic Simplification Done
=======
(define full-namespace
  (let ([ns (make-base-namespace)])
    (eval '(require racket) ns)
    ns))

>>>>>>> New Namespace Attempt
;; Given an expression, returns a constant if that expression is just a function of constants, the original expression otherwise.
(define (try-precompute expr)
=======
(define (try-precompute expr loc)
>>>>>>> Added Precomputation
  (if (and (list? expr) (andmap number? (cdr expr)))
      (let ([value (eval expr full-namespace)])
	(if (rational? value)
	    (list (change (rule 'precompute expr value '()) loc '()))
	    '()))
      '()))

<<<<<<< variant A
(define (simplify-expression expr)
  (debug "Simplifying expression: " expr #:from 'simplify #:depth 2)
  (let* ([canon-changes (canonicalize expr)]
	 [expr* (changes-apply canon-changes expr)]
	 [resolve-changes (resolve expr*)])
    (append canon-changes resolve-changes)))

;; Simplifies an expression, and then applies the simplifying changes.
;; for debuggging.
(define (simplify-expression* expr)
  (let ([simplify-changes (simplify-expression expr)])
    (changes-apply simplify-changes expr)))

(struct s-atom (var loc) #:prefab)
(define (s-atom-has-op? op atom)
  (let ([expr (s-atom-var atom)])
    (and (list? expr) (eq? op (car expr)))))

;; Expects changes in applicative order.
;; Assumes that changes will have only one-to-one bindings,
;; which all the changes we simplify with should.
(define (translate-atom-through-changes changes atom)
  (let loop ([rest-chng changes] [loc (s-atom-loc atom)])
    (if (null? rest-chng) (s-atom (s-atom-var atom) loc)
	(let ([relative-loc (match-loc loc (change-location (car rest-chng)))])
	  (if (not relative-loc)
	      (loop (cdr rest-chng) loc)
	      (if (> (length (change-location (car rest-chng))) (length loc))
		  (error "Cannot Translate: bad locations.")
		  (let* ([new-relative-loc
			  (car (translate-location relative-loc
					      (rule-location-translations (change-rule (car rest-chng)))
					      car cadr))]
			 [new-abs-loc (append (change-location (car rest-chng)) new-relative-loc)])
		    (loop (cdr rest-chng) new-abs-loc))))))))

(define (loc->extract-changes assoc-rgt-rl assoc-lft-rl comm-rl expr loc)
  (define (apply-rule rl loc expr)
    (change rl loc (pattern-match (rule-input rl) (location-get loc expr))))
  (let loop ([cur-loc-r (reverse loc)] [expr* expr] [changes '()])
    (cond [(or (not (list? expr)) (null? cur-loc-r)) (values '() expr)]
	  [(= 1 (length cur-loc-r))
	   (if (= 1 (car cur-loc-r)) (values changes expr*)
	       (let ([new-change (apply-rule comm-rl '() expr*)])
		 (values (cons new-change changes) (change-apply new-change expr*))))]
	  [#t
	   (cond [(and (= 1 (car cur-loc-r)) (= 1 (cadr cur-loc-r)))
		  (let ([new-change (apply-rule assoc-rgt-rl (reverse (cddr cur-loc-r)) expr*)])
		    (loop (cdr cur-loc-r)
			  (change-apply new-change expr*)
			  (cons new-change changes)))]
		 [(and (= 2 (car cur-loc-r)) (= 2 (cadr cur-loc-r)))
		  (let ([new-change (apply-rule assoc-lft-rl (reverse (cddr cur-loc-r)) expr*)])
		    (loop (cdr cur-loc-r)
			  (change-apply new-change expr*)
			  (cons new-change changes)))]
		 [#t (let ([new-change (apply-rule comm-rl (reverse (cdr cur-loc-r)) expr*)])
		       (loop (cons (if (= (car cur-loc-r) 1) 2 1) (cdr cur-loc-r))
			     (change-apply new-change expr*)
			     (cons new-change changes)))])])))

(define add-extract-changes (curry loc->extract-changes (get-rule 'associate-+-rgt) (get-rule 'associate-+-lft) (get-rule '+-commutative)))
(define mul-extract-changes (curry loc->extract-changes (get-rule 'associate-*-rgt) (get-rule 'associate-*-lft) (get-rule '*-commutative)))

(define (multiplicitave-cancel vni-atom inv-atom rest-atom-lists expr loc)
  (let-values ([(extract-vni-atom-changes expr*) (mul-extract-changes expr (drop (s-atom-loc vni-atom) (length loc)))])
    (let ([inv-atom* (translate-atom-through-changes (reverse extract-vni-atom-changes) inv-atom)])
      (let-values ([(extract-inv-atom-changes caddrexpr*) (mul-extract-changes (caddr expr*)
									       (drop (s-atom-loc inv-atom*)
										     (add1 (length loc))))])
	(let* ([expr* (list (car expr*) (cadr expr*) (caddr expr*))]
	       [cancel-changes (let ([cancelling-loc (if (equal? caddrexpr* (s-atom-var inv-atom)) '() '(1))])
				 (append (if (equal? cancelling-loc '())
					     '()
					     (list (let ([rl (get-rule '*-lft-identity)])
						     (change rl '() `((a . ,(caddr caddrexpr*)))))))
					 (list (let ([rl (get-rule '*-inverses)])
						 (change rl cancelling-loc `((a . ,(s-atom-var vni-atom)))))
					       (let ([rl (get-rule 'un-div-inv)]
						     [var (s-atom-var vni-atom)])
						 (change rl cancelling-loc `((a . ,var) (b . ,var)))))
					 (if (equal? cancelling-loc '())
					     '()
					     (list (let ([rl (get-rule 'associate-*-lft)])
						     (change rl '() (pattern-match (rule-input rl) expr*)))))))]
	       [all-changes (append-to-change-locations (append cancel-changes
							       (append-to-change-locations extract-inv-atom-changes '(2))
							       extract-vni-atom-changes) loc)]
	       [new-atom-lists (map (λ (list) (map (curry translate-atom-through-changes (reverse all-changes)) list)) rest-atom-lists)])
	  (values all-changes new-atom-lists))))))

;; Positive atom should come first, then negative atom.
;; Returns a list of cancelling changes, and the new
;; atoms.
(define (additive-cancel pos-atom neg-atom rest-atom-lists expr loc)
  (let-values ([(extract-pos-atom-changes expr*) (add-extract-changes expr (drop (s-atom-loc pos-atom) (length loc)))])
    (let ([neg-atom* #|neg-atom might have moved now (probably did)|# (translate-atom-through-changes (reverse extract-pos-atom-changes) neg-atom)])
      (let-values ([(extract-neg-atom-changes caddrexpr*) (add-extract-changes (caddr expr*)
									    (drop (s-atom-loc neg-atom*)
										  (add1 (length loc))))])
	(let* ([expr* (list (car expr*) (cadr expr*) caddrexpr*)]
	       [cancel-changes (let ([cancelling-loc (if (equal? caddrexpr* (s-atom-var neg-atom)) '() '(1))])
				 (append (if (equal? cancelling-loc '())
					     '()
					     (list (let ([rl (get-rule '+-lft-identity)])
						     (change rl '() `((a . ,(caddr caddrexpr*)))))))
					 (list (let ([rl (get-rule '+-inverses)])
						 (change rl cancelling-loc `((a . ,(s-atom-var pos-atom)))))
					       (let ([rl (get-rule 'unsub-neg)])
						 (change rl cancelling-loc `((a . ,(s-atom-var pos-atom)) (b . ,(s-atom-var pos-atom))))))
					 (if (equal? cancelling-loc '())
					     '()
					     (list (let ([rl (get-rule 'associate-+-lft)])
						     (change rl '() (pattern-match (rule-input rl) expr*)))))))]
	       [all-changes (append-to-change-locations (append cancel-changes
								(append-to-change-locations extract-neg-atom-changes '(2))
								extract-pos-atom-changes) loc)]
	       [new-atom-lists (map (λ (list) (map (curry translate-atom-through-changes (reverse all-changes)) list)) rest-atom-lists)])
	    (values all-changes new-atom-lists))))))

(define (try-multiplicitave-cancel expr atoms loc)
  (let-values ([(inv-atoms vni-atoms) (partition (curry s-atom-has-op? '/) atoms)])
    (let loop ([rest-vni vni-atoms] [rest-inv inv-atoms] [cur-expr expr] [changes '()] [done-vni-atoms '()])
      (if (null? rest-vni) (values changes (append rest-inv done-vni-atoms))
	  (let ([matching-invs (filter (compose (curry equal? `(/ ,(s-atom-var (car rest-vni)))) s-atom-var) rest-inv)])
	    (if (null? matching-invs)
		(loop (cdr rest-vni) rest-inv cur-expr changes (cons (car rest-vni) done-vni-atoms))
		(let ([cancelling-inv (argmin (compose length s-atom-loc) matching-invs)])
		  (let*-values ([(new-changes atom-lists*) (multiplicitave-cancel (car rest-vni)
										  cancelling-inv
										  (list (cdr rest-vni)
											(remove cancelling-inv rest-inv)
											done-vni-atoms)
										  cur-expr
										  loc)])
		    (let ([expr* (changes-apply (reverse new-changes) cur-expr)])
		      (loop (car atom-lists*) (cadr atom-lists*) expr* (append new-changes changes) (caddr atom-lists*)))))))))))

;; Changes that come out of here are in reverse-applicative order.
(define (try-additive-cancel expr atoms loc)
  (let-values ([(neg-atoms pos-atoms) (partition (curry s-atom-has-op? '-) atoms)])
    (let loop ([rest-pos pos-atoms] [rest-neg neg-atoms] [cur-expr expr] [changes '()] [done-pos-atoms '()])
      (if (null? rest-pos) (values changes (append rest-neg done-pos-atoms))
	  (let ([matching-negs (filter (compose (curry equal? `(- ,(s-atom-var (car rest-pos)))) s-atom-var) rest-neg)])
	    (if (null? matching-negs)
		(loop (cdr rest-pos) rest-neg cur-expr changes (cons (car rest-pos) done-pos-atoms))
		(let ([cancelling-neg (argmin (compose length s-atom-loc) matching-negs)])
		  (let*-values ([(new-changes atom-lists*) (additive-cancel (car rest-pos)
								       cancelling-neg
								       (list (cdr rest-pos)
									     (remove cancelling-neg rest-neg)
									     done-pos-atoms)
								       cur-expr
								       loc)])
		    (let ([expr* (changes-apply (reverse new-changes) cur-expr)])
		      (loop (car atom-lists*) (cadr atom-lists*) expr* (append new-changes changes) (caddr atom-lists*)))))))))))

;; Gets all the rules which reduce an expression
(define reduction-rules
  (filter (lambda (rule) ; We filter through the total list of rules
	    (symbol? (rule-output rule)))
	  *rules*))

;;Try to apply a list of rules to an expression.
(define (attempt-apply-all rules expr loc)
  (ormap (curry attempt-apply expr loc) rules))

;; Try to apply a rule to an expression. On success, returns the change applied.
;; On failure, returns #f.
(define (attempt-apply expr loc arule)
  ;; Try to get the bindings.
  (let ([match (pattern-match (rule-input arule) expr)])
    (and match ; If we have bindings, apply them. Otherwise, jut return the expression.
	(change arule loc match))))

(define (resolve expr)
  (define (resolve-subexpressions loc expr)
    (let loop ([subexprs (cdr expr)] [changes-acc '()] [atoms-acc '()] [pos 1])
      (if (null? subexprs)
	  (values changes-acc atoms-acc)
	  (let-values ([(new-changes new-atoms)
			(resolve-expression (append loc (list pos)) (car subexprs))])
	    (let ([carsubexprs* (changes-apply (reverse (drop-change-location-items new-changes (add1 (length loc)))) (car subexprs))])
	      (loop (cdr subexprs) (append new-changes changes-acc)
		    (if (and (list? carsubexprs*) (eq? (car expr) (car carsubexprs*)))
			(append new-atoms atoms-acc)
			(cons (s-atom carsubexprs* (append loc (list pos))) atoms-acc))
		    (add1 pos)))))))
  (define (try-reduce expr loc)
    (let ([applicable-reduction (attempt-apply-all reduction-rules expr '())])
      (if (not applicable-reduction)
	  (values '() '())
	  (let ([expr* (change-apply applicable-reduction expr)])
	    (let-values ([(changes atoms) (resolve-expression loc expr*)])
	      (values (append (append-to-change-locations (list applicable-reduction) loc) changes) atoms))))))
  (define (safe-= expr val)
    (and (number? expr) (= expr val)))
  ;; resolve-expression returns two values: The changes that should be made at this level or below,
  ;; and a list of s-atoms that exist at this level or below, but only within the current
  ;; operator.
  (define (resolve-expression loc expr)
    (if (not (list? expr)) (values '() (list (s-atom expr loc))) ;; Base case.
  	(let-values ([(sub-changes sub-atoms) (resolve-subexpressions loc expr)])
  	  (let ([expr* (changes-apply (reverse (drop-change-location-items sub-changes (length loc))) expr)])
  	    (cond [(and (eq? (car expr*) '/) (safe-= (cadr expr*) 1))
		   (values (list (let ([rl (get-rule 'div1)])
				   (change rl loc '())))
			   (s-atom 1 loc))]
		  [(and (eq? (car expr*) '*) (safe-= (cadr expr*) 1))
		   (let-values ([(changes atoms) (resolve-expression loc (caddr expr*))])
		     (let ([rl (get-rule '*-lft-identity)])
		       (values (cons (change rl loc `((a . ,(caddr expr*)))) sub-changes) atoms)))]
		  [(and (eq? (car expr*) '*) (safe-= (caddr expr*) 1))
		   (let-values ([(changes atoms) (resolve-expression loc (cadr expr*))])
		     (let ([rl (get-rule '*-lft-identity)])
		       (values (cons (change rl loc `((a . ,(cadr expr*)))) sub-changes) atoms)))]
		  [(eq? (car expr*) '+) ;; This counld just as easily be (eq? (car expr) '+), since the operator shouldn't change.
		   (let-values ([(changes atoms) (try-additive-cancel expr* sub-atoms loc)])
		     (values (append changes sub-changes) atoms))]
		  [(eq? (car expr*) '*)
		   (let-values ([(changes atoms) (try-multiplicitave-cancel expr* sub-atoms loc)])
		     (values (append changes sub-changes) atoms))]
		  [#t
		   (let-values ([(changes atoms) (try-reduce expr* loc)])
		     (values (append changes sub-changes) atoms))])))))
  (let-values ([(changes atoms) (resolve-expression '() expr)])
    (reverse changes)))

(define (canonicalize expr)
  ;; Creates a change object for applying the given rule
  ;; at the given location to the given expression.
  (define (rule-apply->change rl loc expr)
    (change rl loc (pattern-match (rule-input rl) expr)))
  ;; rule-locs are a cons pair of a rule, and a RELATIVE location that
  ;; that rule should be applied.
  (define (rule-locs->changes base-loc rl-locs expr)
    (let loop ([rest-rl-locs rl-locs] [cur-expr expr] [acc '()])
      (if (null? rest-rl-locs) (append-to-change-locations acc base-loc)
	  (let* ([chng (rule-apply->change (caar rest-rl-locs) (cdar rest-rl-locs) cur-expr)]
		 [expr* (change-apply chng cur-expr)])
	    (loop (cdr rest-rl-locs) expr* (cons chng acc))))))
  ;; Note: Changes that come out of here are in order from last
  ;; to first.
  (define (canonicalize-expr loc cur-expr)
    (if (not (list? cur-expr)) '()
	(let* ([sub-changes (apply append (map canonicalize-expr
					       (map (λ (pos) (append loc (list pos)))
						    (range 1 (length cur-expr)))
					       (cdr cur-expr)))]
	       [cur-expr* (changes-apply (reverse (drop-change-location-items sub-changes (length loc))) cur-expr)])
	  (match cur-expr*
	    [`(- (- ,a))
	     (list* (rule-apply->change (get-rule 'remove-double-neg) loc cur-expr*)
		    sub-changes)]
	    [`(- ,a ,b)
	     (append (canonicalize-expr loc `(+ ,a (- ,b)))
		     (list (rule-apply->change (get-rule 'sub-neg) loc cur-expr*))
		     sub-changes)]
	    [`(+ ,a ,b)
	     (if (expr<? b a) (cons (rule-apply->change (get-rule '+-commutative) loc cur-expr*)
				    sub-changes)
		 sub-changes)]
	    [`(* ,a (+ ,b ,c))
	     (append (canonicalize-expr (append loc (list 1)) `(* ,a ,b))
		     (canonicalize-expr (append loc (list 2)) `(* ,a ,c))
		     (list (rule-apply->change (get-rule 'distribute-lft-in) loc cur-expr*))
		     sub-changes)]
	    [`(* (+ ,a ,b) ,c)
	     (append (canonicalize-expr (append loc (list 1)) `(* ,c ,a))
		     (canonicalize-expr (append loc (list 2)) `(* ,c ,b))
		     (rule-locs->changes loc (list (cons (get-rule '*-commutative) '() #|this is the location of the change, relative to the base location, loc|#)
						   (cons (get-rule 'distribute-lft-in) '())) cur-expr*)
		     sub-changes)]
	    [`(* (- ,a) ,b)
	     (append (canonicalize-expr (append loc (list 1)) `(* ,a ,b))
		     (list (rule-apply->change (get-rule 'distribute-lft-neg-out) loc cur-expr*))
		     sub-changes)]
	    [`(* ,a (- ,b))
	     (append (canonicalize-expr (append loc (list 1)) `(* ,a ,b))
		     (list (rule-apply->change (get-rule 'distribute-rgt-neg-out) loc cur-expr*))
		     sub-changes)]
	    [`(* ,a ,b)
	     (if (expr<? b a) (cons (rule-apply->change (get-rule '*-commutative) loc cur-expr*)
				    sub-changes)
		 sub-changes)]
	    [`(/ ,a ,b)
	     (append (canonicalize-expr loc `(* ,a (/ ,b)))
		     (list (rule-apply->change (get-rule 'div-inv) loc cur-expr*))
		     sub-changes)]
	    [`(* ,a ,a)
	     (append (canonicalize-expr loc `(sqr a))
		     (list (rule-apply->change (get-rule 'square-unmult) loc cur-expr*))
		     sub-changes)]
	    [`(sqr (* ,a ,b))
	     (append (canonicalize-expr (append loc (list 1)) `(sqr ,a))
		     (canonicalize-expr (append loc (list 2)) `(sqr ,b))
		     (list (rule-apply->change (get-rule 'square-prod) loc cur-expr*))
		     sub-changes)]
	    [`(sqrt (* ,a ,b))
	     (append (canonicalize-expr (append loc (list 1)) `(sqrt ,a))
		     (canonicalize-expr (append loc (list 2)) `(sqrt ,b))
		     (list (rule-apply->change (get-rule 'sqrt-prod) loc cur-expr*))
		     sub-changes)]
	    [`(log (* ,a ,b))
	     (append (canonicalize-expr (append loc (list 1)) `(log ,a))
		     (canonicalize-expr (append loc (list 2)) `(log ,b))
		     (list (rule-apply->change (get-rule 'log-prod) loc cur-expr*))
		     sub-changes)]
	    [`(log (/ ,a))
	     (append (canonicalize-expr loc `(- (log ,a)))
		     (list (rule-apply->change (get-rule 'log-rec) loc cur-expr*))
		     sub-changes)]
	    [`(exp (+ ,a ,b))
	     (append (canonicalize-expr (append loc (list 1)) `(exp ,a))
		     (canonicalize-expr (append loc (list 2)) `(exp ,b))
		     (list (rule-apply->change (get-rule 'exp-sum) loc cur-expr*))
		     sub-changes)]
	    [`(exp (- ,a))
	     (append (canonicalize-expr loc `(/ (exp ,a)))
		     (list (rule-apply->change (get-rule 'exp-neg) loc cur-expr*))
		     sub-changes)]
	    [a sub-changes]))))
  (reverse (canonicalize-expr '() expr)))

(define (append-to-change-locations chngs loc-prefix)
  (map (λ (chng) (change (change-rule chng) (append loc-prefix (change-location chng)) (change-bindings chng)))
       chngs))

(define (drop-change-location-items chngs num-items)
  (map (λ (chng) (change (change-rule chng) (drop (change-location chng) num-items) (change-bindings chng)))
       chngs))

;; Like canonicalize, but returns the new expression instead of the changes.
(define (canonicalize-apply expr)
  (let ([chngs (canonicalize expr)])
    (changes-apply chngs expr)))

;; A more descriptive version of canonicalize-apply*,
;; for debugging.
(define (canonicalize-apply* expr)
  (let ([chngs (canonicalize expr)])
    (let loop ([rest-chngs (reverse chngs)] [cur-expr expr])
      (if (null? rest-chngs)
	  cur-expr
	  (let ([expr* (change-apply (car rest-chngs) cur-expr)])
	    (println (car rest-chngs) "->")
	    (println expr*)
	    (loop (cdr rest-chngs) expr*))))))
>>>>>>> variant B
(define (simplify-expression expr)
  (debug "Simplifying expression: " expr #:from 'simplify #:depth 3)
  (let* ([canon-changes (canonicalize expr)]
	 [expr* (changes-apply canon-changes expr)]
	 [resolve-changes (resolve expr*)])
    (append canon-changes resolve-changes)))

(struct s-atom (var loc) #:prefab)
(define (s-atom-has-op? op atom)
  (let ([expr (s-atom-var atom)])
    (and (list? expr) (eq? op (car expr)))))

;; Expects changes in applicative order.
;; Assumes that changes will have only one-to-one bindings,
;; which all the changes we simplify with should.
(define (translate-atom-through-changes changes atom)
  (let loop ([rest-chng changes] [loc (s-atom-loc atom)])
    (if (null? rest-chng) (s-atom (s-atom-var atom) loc)
	(let ([relative-loc (match-loc loc (change-location (car rest-chng)))])
	  (if (not relative-loc)
	      (loop (cdr rest-chng) loc)
	      (if (> (length (change-location (car rest-chng))) (length loc))
		  (error "Cannot Translate: bad locations.")
		  (let* ([new-relative-loc
			  (car (translate-location relative-loc
					      (rule-location-translations (change-rule (car rest-chng)))
					      car cadr))]
			 [new-abs-loc (append (change-location (car rest-chng)) new-relative-loc)])
		    (loop (cdr rest-chng) new-abs-loc))))))))

(define (loc->extract-changes assoc-rgt-rl assoc-lft-rl comm-rl expr loc)
  (define (apply-rule rl loc expr)
    (change rl loc (pattern-match (rule-input rl) (location-get loc expr))))
  (let loop ([cur-loc-r (reverse loc)] [expr* expr] [changes '()])
    (cond [(or (not (list? expr)) (null? cur-loc-r)) (values '() expr)]
	  [(= 1 (length cur-loc-r))
	   (if (= 1 (car cur-loc-r)) (values changes expr*)
	       (let ([new-change (apply-rule comm-rl '() expr*)])
		 (values (cons new-change changes) (change-apply new-change expr*))))]
	  [#t
	   (cond [(and (= 1 (car cur-loc-r)) (= 1 (cadr cur-loc-r)))
		  (let ([new-change (apply-rule assoc-rgt-rl (reverse (cddr cur-loc-r)) expr*)])
		    (loop (cdr cur-loc-r)
			  (change-apply new-change expr*)
			  (cons new-change changes)))]
		 [(and (= 2 (car cur-loc-r)) (= 2 (cadr cur-loc-r)))
		  (let ([new-change (apply-rule assoc-lft-rl (reverse (cddr cur-loc-r)) expr*)])
		    (loop (cdr cur-loc-r)
			  (change-apply new-change expr*)
			  (cons new-change changes)))]
		 [#t (let ([new-change (apply-rule comm-rl (reverse (cdr cur-loc-r)) expr*)])
		       (loop (cons (if (= (car cur-loc-r) 1) 2 1) (cdr cur-loc-r))
			     (change-apply new-change expr*)
			     (cons new-change changes)))])])))

(define add-extract-changes (curry loc->extract-changes (get-rule 'associate-+-rgt) (get-rule 'associate-+-lft) (get-rule '+-commutative)))
(define mul-extract-changes (curry loc->extract-changes (get-rule 'associate-*-rgt) (get-rule 'associate-*-lft) (get-rule '*-commutative)))

(define (multiplicitave-cancel vni-atom inv-atom rest-atom-lists expr loc)
  (let-values ([(extract-vni-atom-changes expr*) (mul-extract-changes expr (drop (s-atom-loc vni-atom) (length loc)))])
    (let ([inv-atom* (translate-atom-through-changes (reverse extract-vni-atom-changes) inv-atom)])
      (let-values ([(extract-inv-atom-changes caddrexpr*) (mul-extract-changes (caddr expr*)
									       (drop (s-atom-loc inv-atom*)
										     (add1 (length loc))))])
	(let* ([expr* (list (car expr*) (cadr expr*) (caddr expr*))]
	       [cancel-changes (let ([cancelling-loc (if (equal? caddrexpr* (s-atom-var inv-atom)) '() '(1))])
				 (append (if (equal? cancelling-loc '())
					     '()
					     (list (let ([rl (get-rule '*-lft-identity)])
						     (change rl '() `((a . ,(caddr caddrexpr*)))))))
					 (list (let ([rl (get-rule '*-inverses)])
						 (change rl cancelling-loc `((a . ,(s-atom-var vni-atom)))))
					       (let ([rl (get-rule 'un-div-inv)]
						     [var (s-atom-var vni-atom)])
						 (change rl cancelling-loc `((a . ,var) (b . ,var)))))
					 (if (equal? cancelling-loc '())
					     '()
					     (list (let ([rl (get-rule 'associate-*-lft)])
						     (change rl '() (pattern-match (rule-input rl) expr*)))))))]
	       [all-changes (append-to-change-locations (append cancel-changes
							       (append-to-change-locations extract-inv-atom-changes '(2))
							       extract-vni-atom-changes) loc)]
	       [new-atom-lists (map (λ (list) (map (curry translate-atom-through-changes (reverse all-changes)) list)) rest-atom-lists)])
	  (values all-changes new-atom-lists))))))

;; Positive atom should come first, then negative atom.
;; Returns a list of cancelling changes, and the new
;; atoms.
(define (additive-cancel pos-atom neg-atom rest-atom-lists expr loc)
  (let-values ([(extract-pos-atom-changes expr*) (add-extract-changes expr (drop (s-atom-loc pos-atom) (length loc)))])
    (let ([neg-atom* #|neg-atom might have moved now (probably did)|# (translate-atom-through-changes (reverse extract-pos-atom-changes) neg-atom)])
      (let-values ([(extract-neg-atom-changes caddrexpr*) (add-extract-changes (caddr expr*)
									    (drop (s-atom-loc neg-atom*)
										  (add1 (length loc))))])
	(let* ([expr* (list (car expr*) (cadr expr*) caddrexpr*)]
	       [cancel-changes (let ([cancelling-loc (if (equal? caddrexpr* (s-atom-var neg-atom)) '() '(1))])
				 (append (if (equal? cancelling-loc '())
					     '()
					     (list (let ([rl (get-rule '+-lft-identity)])
						     (change rl '() `((a . ,(caddr caddrexpr*)))))))
					 (list (let ([rl (get-rule '+-inverses)])
						 (change rl cancelling-loc `((a . ,(s-atom-var pos-atom)))))
					       (let ([rl (get-rule 'unsub-neg)])
						 (change rl cancelling-loc `((a . ,(s-atom-var pos-atom)) (b . ,(s-atom-var pos-atom))))))
					 (if (equal? cancelling-loc '())
					     '()
					     (list (let ([rl (get-rule 'associate-+-lft)])
						     (change rl '() (pattern-match (rule-input rl) expr*)))))))]
	       [all-changes (append-to-change-locations (append cancel-changes
								(append-to-change-locations extract-neg-atom-changes '(2))
								extract-pos-atom-changes) loc)]
	       [new-atom-lists (map (λ (list) (map (curry translate-atom-through-changes (reverse all-changes)) list)) rest-atom-lists)])
	    (values all-changes new-atom-lists))))))

(define (try-multiplicitave-cancel expr atoms loc)
  (let-values ([(inv-atoms vni-atoms) (partition (curry s-atom-has-op? '/) atoms)])
    (let loop ([rest-vni vni-atoms] [rest-inv inv-atoms] [cur-expr expr] [changes '()] [done-vni-atoms '()])
      (if (null? rest-vni) (values changes (append rest-inv done-vni-atoms))
	  (let ([matching-invs (filter (compose (curry equal? `(/ ,(s-atom-var (car rest-vni)))) s-atom-var) rest-inv)])
	    (if (null? matching-invs)
		(loop (cdr rest-vni) rest-inv cur-expr changes (cons (car rest-vni) done-vni-atoms))
		(let ([cancelling-inv (argmin (compose length s-atom-loc) matching-invs)])
		  (let*-values ([(new-changes atom-lists*) (multiplicitave-cancel (car rest-vni)
										  cancelling-inv
										  (list (cdr rest-vni)
											(remove cancelling-inv rest-inv)
											done-vni-atoms)
										  cur-expr
										  loc)])
		    (let ([expr* (changes-apply (reverse new-changes) cur-expr)])
		      (loop (car atom-lists*) (cadr atom-lists*) expr* (append new-changes changes) (caddr atom-lists*)))))))))))

;; Changes that come out of here are in reverse-applicative order.
(define (try-additive-cancel expr atoms loc)
  (let-values ([(neg-atoms pos-atoms) (partition (curry s-atom-has-op? '-) atoms)])
    (let loop ([rest-pos pos-atoms] [rest-neg neg-atoms] [cur-expr expr] [changes '()] [done-pos-atoms '()])
      (if (null? rest-pos) (values changes (append rest-neg done-pos-atoms))
	  (let ([matching-negs (filter (compose (curry equal? `(- ,(s-atom-var (car rest-pos)))) s-atom-var) rest-neg)])
	    (if (null? matching-negs)
		(loop (cdr rest-pos) rest-neg cur-expr changes (cons (car rest-pos) done-pos-atoms))
		(let ([cancelling-neg (argmin (compose length s-atom-loc) matching-negs)])
		  (let*-values ([(new-changes atom-lists*) (additive-cancel (car rest-pos)
								       cancelling-neg
								       (list (cdr rest-pos)
									     (remove cancelling-neg rest-neg)
									     done-pos-atoms)
								       cur-expr
								       loc)])
		    (let ([expr* (changes-apply (reverse new-changes) cur-expr)])
		      (loop (car atom-lists*) (cadr atom-lists*) expr* (append new-changes changes) (caddr atom-lists*)))))))))))

;; Gets all the rules which reduce an expression
(define reduction-rules
  (filter (lambda (rule) ; We filter through the total list of rules
	    (symbol? (rule-output rule)))
	  *rules*))

;;Try to apply a list of rules to an expression.
(define (attempt-apply-all rules expr loc)
  (ormap (curry attempt-apply expr loc) rules))

;; Try to apply a rule to an expression. On success, returns the change applied.
;; On failure, returns #f.
(define (attempt-apply expr loc arule)
  ;; Try to get the bindings.
  (let ([match (pattern-match (rule-input arule) expr)])
    (and match ; If we have bindings, apply them. Otherwise, jut return the expression.
	(change arule loc match))))

(define (resolve expr)
  (define (resolve-subexpressions loc expr)
    (let loop ([subexprs (cdr expr)] [changes-acc '()] [atoms-acc '()] [pos 1])
      (if (null? subexprs)
	  (values changes-acc atoms-acc)
	  (let-values ([(new-changes new-atoms)
			(resolve-expression (append loc (list pos)) (car subexprs))])
	    (let ([carsubexprs* (changes-apply (reverse (drop-change-location-items new-changes (add1 (length loc)))) (car subexprs))])
	      (loop (cdr subexprs) (append new-changes changes-acc)
		    (if (and (list? carsubexprs*) (eq? (car expr) (car carsubexprs*)))
			(append new-atoms atoms-acc)
			(cons (s-atom carsubexprs* (append loc (list pos))) atoms-acc))
		    (add1 pos)))))))
  (define (try-reduce expr loc)
    (let ([applicable-reduction (attempt-apply-all reduction-rules expr '())])
      (if (not applicable-reduction)
	  (values '() '())
	  (let ([expr* (change-apply applicable-reduction expr)])
	    (let-values ([(changes atoms) (resolve-expression loc expr*)])
	      (values (append (append-to-change-locations (list applicable-reduction) loc) changes) atoms))))))
  (define (safe-= expr val)
    (and (number? expr) (= expr val)))
  ;; resolve-expression returns two values: The changes that should be made at this level or below,
  ;; and a list of s-atoms that exist at this level or below, but only within the current
  ;; operator.
  (define (resolve-expression loc expr)
    (if (not (list? expr)) (values '() (list (s-atom expr loc))) ;; Base case.
  	(let-values ([(sub-changes sub-atoms) (resolve-subexpressions loc expr)])
  	  (let* ([expr* (changes-apply (reverse (drop-change-location-items sub-changes (length loc))) expr)]
		 [precompute-changes (try-precompute expr* loc)])
	    (if (not (null? precompute-changes))
		(values (append precompute-changes sub-changes) (list (s-atom (changes-apply (drop-change-location-items precompute-changes
															 (length loc))
											     expr*)
									      loc)))
		(cond [(and (eq? (car expr*) '/) (safe-= (cadr expr*) 1))
		       (values (list (let ([rl (get-rule 'div1)])
				       (change rl loc '())))
			       (s-atom 1 loc))]
		      [(and (eq? (car expr*) '*) (safe-= (cadr expr*) 1))
		       (let-values ([(changes atoms) (resolve-expression loc (caddr expr*))])
			 (let ([rl (get-rule '*-lft-identity)])
			   (values (cons (change rl loc `((a . ,(caddr expr*)))) sub-changes) atoms)))]
		      [(and (eq? (car expr*) '*) (safe-= (caddr expr*) 1))
		       (let-values ([(changes atoms) (resolve-expression loc (cadr expr*))])
			 (let ([rl (get-rule '*-lft-identity)])
			   (values (cons (change rl loc `((a . ,(cadr expr*)))) sub-changes) atoms)))]
		      [(eq? (car expr*) '+) ;; This counld just as easily be (eq? (car expr) '+), since the operator shouldn't change.
		       (let-values ([(changes atoms) (try-additive-cancel expr* sub-atoms loc)])
			 (values (append changes sub-changes) atoms))]
		      [(eq? (car expr*) '*)
		       (let-values ([(changes atoms) (try-multiplicitave-cancel expr* sub-atoms loc)])
			 (values (append changes sub-changes) atoms))]
		      [#t
		       (let-values ([(changes atoms) (try-reduce expr* loc)])
			 (values (append changes sub-changes) atoms))]))))))
  (let-values ([(changes atoms) (resolve-expression '() expr)])
    (reverse changes)))

(define (canonicalize expr)
  ;; Creates a change object for applying the given rule
  ;; at the given location to the given expression.
  (define (rule-apply->change rl loc expr)
    (change rl loc (pattern-match (rule-input rl) expr)))
  ;; rule-locs are a cons pair of a rule, and a RELATIVE location that
  ;; that rule should be applied.
  (define (rule-locs->changes base-loc rl-locs expr)
    (let loop ([rest-rl-locs rl-locs] [cur-expr expr] [acc '()])
      (if (null? rest-rl-locs) (append-to-change-locations acc base-loc)
	  (let* ([chng (rule-apply->change (caar rest-rl-locs) (cdar rest-rl-locs) cur-expr)]
		 [expr* (change-apply chng cur-expr)])
	    (loop (cdr rest-rl-locs) expr* (cons chng acc))))))
  ;; Note: Changes that come out of here are in order from last
  ;; to first.
  (define (canonicalize-expr loc cur-expr)
    (if (not (list? cur-expr)) '()
	(let* ([sub-changes (apply append (map canonicalize-expr
					       (map (λ (pos) (append loc (list pos)))
						    (range 1 (length cur-expr)))
					       (cdr cur-expr)))]
	       [cur-expr* (changes-apply (reverse (drop-change-location-items sub-changes (length loc))) cur-expr)])
	  (match cur-expr*
	    [`(- (- ,a))
	     (list* (rule-apply->change (get-rule 'remove-double-neg) loc cur-expr*)
		    sub-changes)]
	    [`(- ,a ,b)
	     (append (canonicalize-expr loc `(+ ,a (- ,b)))
		     (list (rule-apply->change (get-rule 'sub-neg) loc cur-expr*))
		     sub-changes)]
	    [`(+ ,a ,b)
	     (if (expr<? b a) (cons (rule-apply->change (get-rule '+-commutative) loc cur-expr*)
				    sub-changes)
		 sub-changes)]
	    [`(* ,a (+ ,b ,c))
	     (append (canonicalize-expr (append loc (list 1)) `(* ,a ,b))
		     (canonicalize-expr (append loc (list 2)) `(* ,a ,c))
		     (list (rule-apply->change (get-rule 'distribute-lft-in) loc cur-expr*))
		     sub-changes)]
	    [`(* (+ ,a ,b) ,c)
	     (append (canonicalize-expr (append loc (list 1)) `(* ,c ,a))
		     (canonicalize-expr (append loc (list 2)) `(* ,c ,b))
		     (rule-locs->changes loc (list (cons (get-rule '*-commutative) '() #|this is the location of the change, relative to the base location, loc|#)
						   (cons (get-rule 'distribute-lft-in) '())) cur-expr*)
		     sub-changes)]
	    [`(* (- ,a) ,b)
	     (append (canonicalize-expr (append loc (list 1)) `(* ,a ,b))
		     (list (rule-apply->change (get-rule 'distribute-lft-neg-out) loc cur-expr*))
		     sub-changes)]
	    [`(* ,a (- ,b))
	     (append (canonicalize-expr (append loc (list 1)) `(* ,a ,b))
		     (list (rule-apply->change (get-rule 'distribute-rgt-neg-out) loc cur-expr*))
		     sub-changes)]
	    [`(* ,a ,b)
	     (if (expr<? b a) (cons (rule-apply->change (get-rule '*-commutative) loc cur-expr*)
				    sub-changes)
		 sub-changes)]
	    [`(/ ,a ,b)
	     (append (canonicalize-expr loc `(* ,a (/ ,b)))
		     (list (rule-apply->change (get-rule 'div-inv) loc cur-expr*))
		     sub-changes)]
	    [`(* ,a ,a)
	     (append (canonicalize-expr loc `(sqr a))
		     (list (rule-apply->change (get-rule 'square-unmult) loc cur-expr*))
		     sub-changes)]
	    [`(sqr (* ,a ,b))
	     (append (canonicalize-expr (append loc (list 1)) `(sqr ,a))
		     (canonicalize-expr (append loc (list 2)) `(sqr ,b))
		     (list (rule-apply->change (get-rule 'square-prod) loc cur-expr*))
		     sub-changes)]
	    [`(sqrt (* ,a ,b))
	     (append (canonicalize-expr (append loc (list 1)) `(sqrt ,a))
		     (canonicalize-expr (append loc (list 2)) `(sqrt ,b))
		     (list (rule-apply->change (get-rule 'sqrt-prod) loc cur-expr*))
		     sub-changes)]
	    [`(log (* ,a ,b))
	     (append (canonicalize-expr (append loc (list 1)) `(log ,a))
		     (canonicalize-expr (append loc (list 2)) `(log ,b))
		     (list (rule-apply->change (get-rule 'log-prod) loc cur-expr*))
		     sub-changes)]
	    [`(log (/ ,a))
	     (append (canonicalize-expr loc `(- (log ,a)))
		     (list (rule-apply->change (get-rule 'log-rec) loc cur-expr*))
		     sub-changes)]
	    [`(exp (+ ,a ,b))
	     (append (canonicalize-expr (append loc (list 1)) `(exp ,a))
		     (canonicalize-expr (append loc (list 2)) `(exp ,b))
		     (list (rule-apply->change (get-rule 'exp-sum) loc cur-expr*))
		     sub-changes)]
	    [`(exp (- ,a))
	     (append (canonicalize-expr loc `(/ (exp ,a)))
		     (list (rule-apply->change (get-rule 'exp-neg) loc cur-expr*))
		     sub-changes)]
	    [a sub-changes]))))
  (reverse (canonicalize-expr '() expr)))

(define (append-to-change-locations chngs loc-prefix)
  (map (λ (chng) (change (change-rule chng) (append loc-prefix (change-location chng)) (change-bindings chng)))
       chngs))

(define (drop-change-location-items chngs num-items)
  (map (λ (chng) (change (change-rule chng) (drop (change-location chng) num-items) (change-bindings chng)))
       chngs))

;; Like canonicalize, but returns the new expression instead of the changes.
(define (canonicalize-apply expr)
  (let ([chngs (canonicalize expr)])
    (changes-apply chngs expr)))

;; A more descriptive version of canonicalize-apply*,
;; for debugging.
(define (canonicalize-apply* expr)
  (let ([chngs (canonicalize expr)])
    (let loop ([rest-chngs (reverse chngs)] [cur-expr expr])
      (if (null? rest-chngs)
	  cur-expr
	  (let ([expr* (change-apply (car rest-chngs) cur-expr)])
	    (println (car rest-chngs) "->")
	    (println expr*)
	    (loop (cdr rest-chngs) expr*))))))
####### Ancestor
(define (simplify-expression-old expr)
  (decanonicalize (inner-simplify-expression expr)))
======= end
