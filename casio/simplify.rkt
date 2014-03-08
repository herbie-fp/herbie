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
;; We grab pattern matching for our canonicalizing rules.
(require racket/match)

;; Simplify is the only thing we need to export
(provide simplify simplify-expression)

;; Simplifies an alternative if simplification would result in a green change,
;; without undoing the most recent change.
(define (simplify altn)
  ;; Grab the simplification locations from the rule, and then the location of the
  ;; change, since the slocations are relative to the change.
  (let ([slocations (if (alt-prev altn)
			(rule-slocations (change-rule (alt-change altn)))
			'(()))]
	[location (if (alt-prev altn)
		      (change-location (alt-change altn))
		      '(cdr cdr car))])
    ;;(when (*debug*) (println "Simplifying " (alt-program altn) " at " (map (lambda (l) (append location l))
;;									   slocations)))
    ;; Try to create a new, simplified alt, by simplifying at all the slocations
    (define (simplify-at-locations slocations alt)
      (if (null? slocations)
	  ;; If we don't have any slocations left, just return the alt
	  alt
	  (let* ([full-location (append location (car slocations))] ; The full location for the simplification
		 [partly-simplified-prog (location-do full-location 
						      (alt-program alt)
						      simplify-expression)] ; Try to simplify the program at the slocation
		 [new-rule (rule 'simplify
				 (location-get full-location
					       (alt-program alt))
				 (location-get full-location
					       partly-simplified-prog)
				 '())] ; Create a new rule for the simplification
		 [new-change (change new-rule full-location (map (lambda (x) (cons x x))
								 (get-contained-vars (alt-program altn))))] ; Create a change from the old alt
		                                                                                            ; to a new-simplified alt
		 [new-alt (if (*debug*) (println (alt-apply alt new-change))
			      (alt-apply alt new-change))]) ; Create a new alt that's simplified.
	    ;;(when (*debug*) (println "Simplified to: " partly-simplified-prog))
	    (if (green? new-alt)
		(simplify-at-locations (cdr slocations) ; If our new alt is green-tipped, recurse on that for the rest of the slocations
					new-alt)
		(simplify-at-locations (cdr slocations) ; If our new alt isn't any better, recurse on the old alt for the rest of the slocations
					alt)))))
    (simplify-at-locations slocations altn))) ; Call the recursive function with our given altn and it's simplification locations

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
		    (append (combine-like-terms (cons cur-term mterms))
			    acc)))))))

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
  ;;(when (*debug*) (println "getting atoms for " expr))
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
  ;;(when (*debug*) (println "combining: " terms))
  ;; Get the combined constant factor.
  (let ([new-factor (foldr (lambda (t acc)
			     ;;(when (*debug*) (println "adding factor: " t))
			     (+ acc (factor t)))
			   0 terms)]) ; We just fold over terms, trying to combine their constant factors
    (cond [(= 0 new-factor) '()] ; If our terms canceled, return an empty list.
	  [(real? (car terms)) (list new-factor)] ; If the terms are constants, just return a list of that factor
	  [(symbol? (car terms)) (list (if (= 1 new-factor) (list (car terms)) (list '* new-factor (car terms))))]
	  [#t (let ([body (if (real? (cadar terms)) (cddar terms) (cdar terms))])
		(cond [(= 1 new-factor) (cons '* body)] ; If we have a factor of one, return the body multiplied together
		      [(= -1 new-factor) (list '- (cons '* body))] ; If we have a factor of negative one, do the same but negate it
		      [#t (list* '* new-factor body)]))]))) ; Otherwise, mutliply together the factor and the body.

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
  (let ([expr* (attempt-apply-all reduction-rules expr)]) ; First attempt to reduce the expression using our reduction rules.
    (match expr*
      [`(- ,a ,a) 0] ; A number minus itself is zero
      [`(- ,a ,b) (simplify-expression `(+ ,a (- ,b)))] ; Move the minus inwards, and make a recursive call in case the minus needs to be moved further inwards
      [`(- 0) 0] ; Negative zero is still zero
      [`(- (- ,a)) a] ; Double negate is positive
      [`(/ ,a ,a) 1] ; A number divided by itself is 1
      [`(+ ,a 0) a] ; Additive Identity
      [`(* ,a 0) 0] ; Multiplying anything by zero yields zero
      [`(* ,a 1) a] ; Multiplicitive Identity
      [`(/ 1 1) 1] ; Get rid of any one-over-ones
      [`(/ 1 ,a) `(/ 1 ,a)] ; Catch this case here so that it doesn't fall to the next rule, resulting in infinite recursion
      [`(/ ,a ,b) (simplify-expression `(* ,a (/ 1 ,b)))] ; Move the division inwards, and make a recursive call in case the division needs to be moved further inwards
      [`(+ (+ . ,a) (+ . ,b)) (addition (append a b))] ; These next three rules flatten out addition
      [`(+ (+ . ,a) ,b) (addition (cons b a))]
      [`(+ ,a (+ . ,b)) (addition (cons a b))]
      [`(+ . ,a) (addition a)] ; Sort addition in canonical order and attempt to cancel terms
      [`(* (* . ,a) (* . ,b)) (multiplication (append a b))] ; These next four are the same as the above four, for multiplication
      [`(* (* . ,a) ,b) (multiplication (cons b a))]
      [`(* ,a (* . ,b)) (multiplication (cons a b))]
      [`(* . ,a) (multiplication a)]
      [a a]))) ; Finally, if we don't have any other match, return ourselves.

;; Simplify an arbitrary expression to the best of our abilities.
(define (simplify-expression expr)
  ;; If the expression is a literal or a variable, there's nothing to be done
  (if (list? expr)
      ;; First, attempt to simplify all subexpressions. Then, to a top level simplify on the resulting expression.
      (let ([expr* (cons (car expr)
			 (map simplify-expression (cdr expr)))])
	(msingle-simplify expr*))
      expr))
