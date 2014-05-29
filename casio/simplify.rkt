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

;; Simplifies an alternative at the location specified by the most
;; recent change's rule.
(define (simplify altn)
  (let* ([location (if (alt-prev altn)
		       (change-location (alt-change altn))
		       '(2))]
	 [slocations (map (λ (sloc) (append location sloc))
			  (if (alt-prev altn)
			      (rule-slocations (change-rule (alt-change altn)))
			      '(())))])
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
		  (debug "Simplified to " cur-alt #:from 'simplify #:depth 2)
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
  (> (alt-cost altn) (alt-cost (alt-prev altn))))

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

(define (alt-with-prev prev altn)
  (alt (alt-program altn) (alt-errors altn) (alt-cost altn) (alt-change altn) prev (alt-cycles altn)))

(define (get-rule name)
  (car (filter (λ (rule) (eq? (rule-name rule) name)) *rules*)))

;; Return the variables that are in the expression
(define (get-contained-vars expr)
  ;; Get a list that of the vars, with each var repeated for each time it appears
  (define (get-duplicated-vars expr)
    (cond [(or (null? expr) (real? expr)) '()] ; If we've reached the end of a list, or we're at a constant, there are no vars.
	  [(symbol? expr) (list expr)] ; If we're at a variable, return it in a list
	  [(list? expr) (apply append (map get-duplicated-vars
					   (cdr expr)))])) ; If we're at a list, get the vars from all of it's items, and append them together.
  (remove-duplicates (get-duplicated-vars expr))) ; Get the list with duplicates, and remove the duplicates.

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

(define (try-precompute expr loc)
  (if (and (list? expr) (andmap number? (cdr expr)))
      (let ([value (safe-eval expr)])
	(if (rational? value)
	    (list (change (rule 'precompute expr value '()) loc '()))
	    '()))
      '()))

(define (simplify-expression expr)
  (debug "Simplifying expression: " expr #:from 'simplify #:depth 2)
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
		(values (append precompute-changes sub-changes) (list (s-atom (changes-apply precompute-changes expr*) loc)))
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
