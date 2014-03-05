#lang racket

;; We need rules for extracting the slocations from the rules
(require casio/rules)
;; We need alternative for it's structure accessors and alt apply
(require casio/alternative)
;; We need programs for location-do and location-get
(require casio/programs)
;; We need this for print debugging
(require casio/common)
;; We need this to know whether a simplification caused a green change
(require casio/redgreen)
;; We grab pattern matching for our canonicalizing rules.
(require racket/match)

;; Simplify is the only thing we need to export
(provide simplify)

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
		 [new-alt (alt-apply alt new-change)]) ; Create a new alt that's simplified.
	    ;;(when (*debug*) (println "Simplified to: " partly-simplified-prog))
	    (if (green? new-alt)
		(simplify-at-locations (cdr slocations) ; If our new alt is green-tipped, recurse on that for the rest of the slocations
					new-alt)
		(simplify-at-locations (cdr slocations) ; If our new alt isn't any better, recurse on the old alt for the rest of the slocations
					alt)))))
    (simplify-at-locations slocations altn))) ; Call the recursive function with our given altn and it's simplification locations

;; Simplify an expression
(define (simplify-expression expr)
  ;; To simplify an expression, we first remove functions with their inverses, then we canonicalize, then resolve terms, then decanonicalize
  (decanonicalize (resolve-terms (resolve-factors (canonicalize (rm-fns-&-invs expr))))))

;; Return the variables that are in the expression
(define (get-contained-vars expr)
  ;; Get a list that of the vars, with each var repeated for each time it appears
  (define (get-duplicated-vars expr)
    (cond [(or (null? expr) (real? expr)) '()] ; If we've reached the end of a list, or we're at a constant, there are no vars.
	  [(symbol? expr) (list expr)] ; If we're at a variable, return it in a list
	  [(list? expr) (apply append (map get-duplicated-vars
					   (cdr expr)))])) ; If we're at a list, get the vars from all of it's items, and append them together.
  (remove-duplicates (get-duplicated-vars expr))) ; Get the list with duplicates, and remove the duplicates.

;; A list of functions with their inverses for removal
(define func-inverses
  '((exp . log)
    (square . sqrt)
    (- . -)))

;; Remove any pairs of functions with their inverses from a program
(define (rm-fns-&-invs prog)
  ;; Try to remove a top-level function with it's inverse.
  (define (rm-fn-&-inv expr)
    ;; (when (*debug*) (println "Attempting to remove functions with their inverses from: " expr))
    ;; Take an alist, and return a new one where the values in the old alist are mapped to the keys.
    (define (reverse-alist l)
      (map (lambda (pair) (cons (cdr pair) (car pair)))
	   l))
    (let* ([all-invs (append func-inverses (reverse-alist func-inverses))] ; Get an alist which maps every function to it's inverse.
	   [inv (let ([item (assoc (car expr) all-invs)]) (when item (cdr item)))]) ; Try to get the inverse of the outer function
      (if (and inv (pair? (cadr expr)) (eq? (caadr expr) inv))
	  ;; If we've found an inverse, there is an inner function, and the inner function matches the inverse, strip off both functions
	  (cadadr expr)
	  ;; If any of those are false, we can't remove anything from here.
	  expr)))
  ;; For every list in the program (working from the outside inwards), try to remove function inverses.
  (for-lists prog rm-fn-&-inv))

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

;; Take an expression, and turn it into canonical form. Canonical form has the additions on the outside, subtractions
;; one-level in, then multiplication, division. Inside that should be any expressions that the simplifier considers
;; atoms, or items that it can't simplify.
(define (canonicalize expr)
  (if (pair? expr)
      (let ([expr* (cons (car expr)
			 (map canonicalize (cdr expr)))]) ;; First, try to canonicalize all subexpressions.
	;; Then, try to canonicalize with our canonicalization rules
	(match expr*
	  [`(+ (+ . ,a) (+ . ,b)) (cons '+ (append a b))]
	  [`(+ ,a (+ . ,b)) (list* '+ a b)]
	  [`(+ (+ . ,a) ,b) (cons '+ (append a (list b)))]
	  [`(- (+ . ,a)) (cons '+ (map (lambda (expr) (list '- (canonicalize expr)))
       			       a))]
	  [`(- ,a ,b) (canonicalize `(+ ,a (- ,b)))]
	  [`(* (+ . ,a) (+ . ,b)) (cons '+ (apply append (map (lambda (an)
								(map (lambda (bn)
								       `(* ,an ,bn))
								     b))
							      a)))]
	  [`(* (+ . ,as) ,b) (cons '+ (map (lambda (a)
					   `(* ,a ,b))
					 as))]
	  [`(* ,a (+ ,bs)) (cons '+ (map (lambda (b)
					   `(* ,a ,b))
					 bs))]
	  [`(* (* . ,a) (* . ,b)) (cons '* (append a b))]
	  [`(* ,a (* . ,b)) (list* '* a b)]
	  [`(* (* . ,a) ,b) (cons '* (append a (list b)))]
	  [`(/ 1 (/ 1 ,a)) a]
	  [`(/ 1 ,a) `(/ 1 ,a)]
	  [`(/ ,a ,b) (canonicalize `(* ,a (/ 1 ,b)))]
	  [`(/ (* ,a) (* ,b)) (list '* (let loop ([exprs1 a] [exprs2 b])
					 (cond [(null? a) (map (lambda (v) (canonicalize `(/ 1 ,b))))]
					       [(null? b) a]
					       [#t (cons (/ (car a) (car b)) (loop (cdr a) (cdr b)))])))]
	  [`(/ ,a (* ,b . ,c)) `(* (/ ,a ,b) . ,(map (lambda (v) (canonicalize `(/ 1 ,v)))
						    c))]
	  [`(/ (* ,a . ,b) ,c) `(* (/ ,a ,c) . ,b)]
	  [`(/ (+ ,as) ,c) (cons '+ (map (lambda (v) `(/ ,v ,c))
					 as))]
	  [`(/ ,a ,b) (canonicalize `(* ,a (/ 1 ,b)))]
	  [`(square (* . ,as)) (cons '* (map (lambda (v) `(square ,v))
					   as))]
	  [`(square (/ ,a ,b)) `(/ (square ,a) (square ,b))]
	  [`(sqrt (* . ,as)) (cons '* (map (lambda (v) `(sqrt ,v))
					   as))]
	  [`(sqrt (/ ,a ,b)) `(/ (sqrt a) (sqrt b))]
	  [a a]))
      ;; If this isn't a list, than the expression is as canonical as we're going to get.
      expr))

;; Turn a expression from canonical form into a simpler more readable expression
(define (decanonicalize cexpr)
  ;; Only try to decanonicalize cexpr if it's a pair?
  (if (pair? cexpr)
      (let ([expr* (cons (car cexpr)
			 (map decanonicalize (cdr cexpr)))]) ; Decanonicalize each item in the list first
	;; Try to decanonicalize with our decanonicalizing rules
	(match expr*
	  [`(+ ,a ,b ,c . ,n) (foldl (lambda (t acc) `(+ ,acc t))
				     a
				     (list* b c n))]
	  [`(* 1 ,a) a]
	  [`(+ ,a (- ,b)) `(- ,a ,b)]
	  [`(+ (- ,a) ,b) `(- ,b ,a)]
	  [`(* ,a (/ 1 ,b)) `(/ ,a ,b)]
	  [`(* (/ 1 ,a) ,b) `(/ ,b ,a)]
	  [a a]))
      cexpr))

;; Take a canonicalized expression, and add all like terms
(define (resolve-terms expr)
  ;; For each term, attempt to resolve it with the others.
  (let loop ([terms (cdr expr)] [acc '()])
    ;; If we still have terms left
    (if (null? terms)
	;; If we're out of terms, our acc is empty, just return the zero expression.
	;; If we have only one term, just return that term. If we have more terms,
	;; make an addition.
	(cond [(null? acc) 0]
	      [(= 1 (length acc)) (car acc)]
	      [#t (cons '+ acc)])
	(let* ([cur-term (car terms)] ; Grab the first term
	       [mterms (filter (lambda (t) 
				 (equal? (term-atoms t)
					 (term-atoms cur-term)))
			       (cdr terms))]) ; Grab every term that matches the current term
	  (if (= 0 (length mterms)) ; If we didn't get any terms, just add it to the accumulator, and recurse on the rest.
	      (loop (cdr terms) (cons cur-term acc))
	      ;; If we did get terms, remove the matching terms from the terms we have left.
	      (loop (remove* mterms (cdr terms))
		    ;; And combine the matching terms and the term they matched with, and append it to the accumulator
		    (append (combine-like-terms (cons (car terms) mterms))
			    acc)))))))

;; Cancel appropriate factors
(define (resolve-factors term)
  ;; Cancel factors, given some inverted factors and non-inverted factors
  (define (cancel-factors factors inverted-factors)
    (cond [(null? factors) inverted-factors] ; If there are no factors, we just return the inverted factors
	  [(null? inverted-factors) factors] ; If there are no inverted factors left, return the factors
	  [(member (cadr (car inverted-factors)) factors)
	   (cancel-factors (remove (cadr (car inverted-factors)) factors) (cdr inverted-factors))] ; If there is a factor to cancel, cancel it.
	  [#t (cancel-factors (cons (car inverted-factors) factors) (cdr inverted-factors))])) ; Otherwise, just add this non-cancelable factor to the factors we return at the end.
  (let-values ([(inverted non-inverted) (partition (lambda (element)
						     (and (not (atomic element))
							  (eq? (car element) '/)))
						   (if (number? (cadr term))
						       (cddr term)
						       (cdr term)))]) ; Partition the elements into inverted and non-inverted
    (let ([new-factors (cancel-factors non-inverted inverted)]) ; The newly canceled factors
      (if (null? new-factors)
	  1 ; If all the factors canceled, we just return 1, the multiplicative identity
	  (if (number? (cadr term))
	      (list* '* (cadr term) new-factors) ;; Return the factors
	      (list* '* new-factors))))))
	

;; Get the atoms of a term. Terms are made up of atoms multiplied together.
(define (term-atoms expr)
  ;;(when (*debug*) (println "getting atoms for " expr))
  (if (atomic expr)
      ;; If we're already at an atom, just return a list of that atom
      (list expr)
      (let ([positive-term (if (eq? (car expr) '-)
			       (cadr expr)
			       expr)]) ; If we have a negated term, turn it positive for now.
	(if (atomic positive-term)
	    ;; If we had just started with an atom negated, just return the atom.
	    (list positive-term)
	    (sort (if (real? (cadr positive-term))
		      ;; If the term has a constant factor, return everything past it.
		      (cddr positive-term)
		      ;; If the term doesn't have a constnat factor, return everything but the plus sign.
		      (cdr positive-term)) atom<?)))))

;; Return whether or not the expression is atomic, meaning it can't be broken down into arithmetic.
(define (atomic expr)
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

;; Combine al terms that are combinable.
(define (combine-like-terms terms)
  ;;(when (*debug*) (println "combining: " terms))
  ;; Get the combined constant factor.
  (let ([new-factor (foldr (lambda (t acc)
			     ;;(when (*debug*) (println "adding factor: " t))
			     (+ acc
				(cond [(real? t) t]
				      [(atomic t) 1]
				      [(and (eq? (car t) '-) (not (pair? (cadr t))))
				       -1]
				      [(and (eq? (car t) '-) (real? (cadadr t)))
				       (- (cadadr t))]
				      [(eq? (car t) '-) -1]
				      [(real? (cadr t)) (cadr t)]
				      [#t 1])))
			   0 terms)]) ; We just fold over terms, trying to combine their constant factors
    (cond [(real? (car terms)) (list new-factor)] ; If the terms are constants, just return a list of a single constant term.
	  [(= 0 new-factor) '()] ; If our terms canceled, return an empty list.
	  [(and (atomic (car terms)) (= 1 new-factor)) (car terms)] ; If we have a single atom with a factor of one, just return that atom
	  [(atomic (car terms)) `((* ,new-factor ,(car terms)))] ; If the terms are a single atom, just return the factor multiplied by the atom
	  [(= 1 new-factor) (if (= 1 (length terms)) ; If the factor is one, just return the atoms multiplied together
				(cddar terms)
				`((* . ,(cddar terms))))]
	  [(= -1 new-factor) (list (list '- (if (= 1 (length (cddar terms))) ; If we have a factor of negate one, invert the atoms multiplied together
						(caddar terms)
						(cons '* (cddar terms)))))]
	  [#t `((* ,new-factor . ,(cddar terms)))]))) ; Otherwise, return the constant factor and the terms multiplied together

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
