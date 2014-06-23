#lang racket

;; We need rules for extracting the slocations from the rules
(require casio/matcher)
;; We need alternative for it's structure accessors and alt apply
(require casio/alternative)
;; We need programs for location-do and location-get
(require casio/programs)
;; We need this for print debugging, and for pipe
(require casio/common)
;; We need this to know whether a simplification caused a green change
(require casio/redgreen)
(require casio/locations)
;; We grab pattern matching for our canonicalizing rules.
(require racket/match)

(provide simplify simplify-expression)
;; Simplifies an alternative at the location specified by the most
;; recent change's rule.
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
    (if (null? slocations) '()
	(begin (debug "Simplify " altn " at locations " slocations #:from 'simplify #:tag 'enter #:depth 1)
	       (let* ([unfiltered-changes (apply append (map (λ (loc) (append-to-change-locations
								       (simplify-expression (location-get loc (alt-program altn))) loc))
							     slocations))])
		 ;; We set the prev pointer to null because we only care about the changes we're applying,
		 ;; and we want to make sure to not have red elimination worry about any of the changes
		 ;; before we simplified.
		 (let* ([stripped-alt (alt-with-prev #f altn)]
			[simplified-alt (apply-changes stripped-alt unfiltered-changes)]
			[re-alt (remove-red (eliminate-dead-head simplified-alt) #:fitness-func reduced? #:aggressive #f)])
		   (debug "Simplified to " re-alt #:from 'simplify #:depth 2)
		   (alt-changes re-alt)))))))

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
  (< (alt-cost altn) (alt-cost (alt-prev altn))))

(define (safe-= . args)
  (and (andmap number? args)
       (apply = args)))

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

(define (s-atom<? a1 a2)
  (cond [(and (number? (s-atom-var a1)) (number? (s-atom-var a2)))
	 (< (s-atom-var a1) (s-atom-var a2))]
	[(number? (s-atom-var a1)) #t]
	[(number? (s-atom-var a2)) #f]
	[#t (atom<? (s-atom-var a1) (s-atom-var a2))]))

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

(define full-namespace
  (let ([ns (make-base-namespace)])
    (eval '(require racket) ns)
    ns))

;; Given an expression, returns a constant if that expression is just a function of constants, the original expression otherwise.
(define (try-precompute expr loc)
  (if (and (list? expr) (andmap number? (cdr expr)))
      (let ([value (with-handlers ([(const #t) (λ (e) #f)])
                       (eval expr full-namespace))])
	(if (rational? value)
	    (list (change (rule 'precompute expr value '()) loc '()))
	    '()))
      '()))

(define (simplify-expression expr)
  (debug "Simplifying expression: " expr #:from 'simplify #:depth 3)
  (let* ([canon-changes (canonicalize expr)]
	 [expr* (changes-apply canon-changes expr)]
	 [resolve-changes (cancel-terms expr*)])
    (append canon-changes resolve-changes)))

;; Simplifies an expression, and then applies the simplifying changes.
;; for debuggging.
(define (simplify-expression* expr)
  (let ([simplify-changes (simplify-expression expr)])
    (changes-apply simplify-changes expr)))

(struct s-atom (var loc) #:prefab)

(struct s-var (var pow) #:prefab)
(struct s-term (coeff vars loc) #:prefab)
;; Given an expression that does not break down,
;; such as (f x) for some unknown f, this gives
;; the canonical term representation of it.
(define (expr-term atom-expr loc)
  (s-term 1 (list (s-var atom-expr 1)) loc))

(define (term->expr term)
  (define (var->expr var)
    (cond [(= 0 (s-var-pow var))
	   1]
	  [(= (/ 1 2) (s-var-pow var))
	   `(sqrt ,(s-var-var var))]
	  [(= 1 (s-var-pow var))
	   (s-var-var var)]
	  [(= 2 (s-var-pow var))
	   `(sqr ,(s-var-var var))]
	  [#t `(expt ,(s-var-var var))]))
  (define (coeff_vars->expr coeff vars)
    (cond [(= coeff 0) 0]
	  [(null? vars) coeff]
	  [(< coeff 0) (list '- (coeff_vars->expr (- coeff) vars))]
	  [(= coeff 1) (if (= (length vars) 1)
			   (var->expr (car vars))
			   (cons '* (map var->expr vars)))]
	  [#t (list* '* coeff (map var->expr vars))]))
  (coeff_vars->expr (s-term-coeff term) (s-term-vars term)))

;; Like map, but accepts functions that return two values,
;; and returns two values, a list of all the first values returned
;; by each function application, and a list of all the second values
;; returned by the function applications.
(define (map-2values f . lsts)
  (let loop ([rest-lsts lsts] [acc1 '()] [acc2 '()])
    (if (null? (car rest-lsts)) (values (reverse acc1) (reverse acc2))
	(let-values ([(val1 val2) (apply f (map car rest-lsts))])
	  (loop (map cdr rest-lsts) (cons val1 acc1) (cons val2 acc2))))))

(define (cancel-terms expr)
  ;; Predicate to determine whether a given node is a leaf of the expression
  ;; tree.
  (define is-leaf? (compose not list?))
  ;; Takes a leaf node and the location at which that that leaf node was found,
  ;; and returns a list of changes and terms from that leaf.
  (define (handle-leaf loc leaf)
    (if (number? leaf)
	(values '() (list (s-term leaf '() loc)))
	(values '() (list (s-term 1 (list (s-var leaf 1)) loc)))))
  ;; Takes a non-leaf node, the location at which it was found, and the terms
  ;; returned by handling it's sub-nodes, and returns a list of changes and terms
  ;; resulting from handling that node.
  (define (handle-node loc expr sub-terms)
    ((hash-ref *node-handlers* (car expr) (const default-node-handler)) loc expr sub-terms))
  ;; Given a location and an expression, recursively calls bubble-changes-and-terms
  ;; on each sub-node of that expression, returning a list of resulting changes
  ;; and a list of resulting terms.
  (define (resolve-subs loc expr)
    (let-values ([(sub-change-lsts sub-term-lsts)
		  (map-2values bubble-changes-and-terms
			       (map (compose (curry append loc) list)
				    (map add1 (range (length (cdr expr)))))
			       (cdr expr))])
      (values (apply append sub-change-lsts) sub-term-lsts)))
  ;; Top level algorithm for cancelling terms in an expression tree.
  (define (bubble-changes-and-terms loc expr)
    (if (is-leaf? expr)
	(handle-leaf loc expr)
	(let*-values ([(sub-changes sub-term-lsts) (resolve-subs loc expr)]
		      [(expr*) (changes-apply (drop-change-location-items sub-changes (length loc)) expr)]
		      [(new-changes terms*) (handle-node loc expr* sub-term-lsts)])
	  (values (append sub-changes new-changes) terms*))))
  (let-values ([(final-changes final-terms) (bubble-changes-and-terms '() expr)])
    final-changes))

(define (default-node-handler loc expr sub-term-lsts)
  (values '() (list (expr-term (cons (car expr)
				     (map (λ (term-lst)
					    (let ([expr-lst (map term->expr term-lst)])
					      (if (= 1 (length expr-lst))
						  (car expr-lst)
						  (cons '+ expr-lst))))
					  sub-term-lsts)) loc))))

(define *node-handlers*
  (make-immutable-hasheq
   `(['+ (λ (loc expr sub-term-lsts)
	   (let ([sub-terms (apply append sub-term-lsts)])
	     (try-combine-+ sub-terms expr loc)))]
     ['- (λ (loc expr sub-term-lsts)
	   (define (negate-term term)
	     (s-term (- (s-term-coeff term)) (s-term-vars term) (s-term-loc term)))
	   (if (= 1 (length sub-term-lsts)) (values '() (map negate-term (car sub-term-lsts)))
	       (let* ([left-subterms (car sub-term-lsts)]
		      [right-subterms (cadr sub-term-lsts)]
		      [subterms (append left-subterms
					(map negate-term
					     right-subterms))])
		 (try-combine-+ subterms expr loc))))])))

(define (try-combine-+ terms expr loc)
  (let loop ([rest-terms terms] [cur-expr expr] [changes-acc '()])
    (let ([match (find-matching-term-pair rest-terms)])
      (if (not match) (values changes-acc rest-terms)
	  (let*-values ([(chngs term*) (combine-+-changes (car match) (cadr match) expr loc)]
			[(terms*) (map translate-term-through-changes (remove* match rest-terms))])
	    (loop (if (= 0 (s-term-coeff term*)) terms*
		      (cons term* terms*))
		  (changes-apply (drop-change-location-items chngs (length loc)) cur-expr)
		  (append chngs changes-acc)))))))

(define (late-canonicalize-term-changes loc expr)
  (let* ([atoms (let loop ([cur-expr expr] [cur-loc loc])
		  (if (and (list? cur-expr) (eq? '* (car cur-expr)))
		      (append (loop (cadr cur-expr) (append cur-loc '(1)))
			      (loop (caddr cur-expr) (append cur-loc '(2))))
		      (list (s-atom cur-expr cur-loc))))]
	 [sorted-atoms (sort atoms s-atom<?)]
	 [ordering-changes (let loop ([cur-expr expr] [rest-atoms sorted-atoms]
				      [changes-acc '()] [cur-loc loc])
			     (if (null? rest-atoms) changes-acc
				 (let*-values ([(extraction-changes expr*)
						(mul-extract-changes cur-expr (drop (s-atom-loc (car rest-atoms)) (length cur-loc)))]
					       [(rest*) (map (curry translate-atom-through-changes (reverse extraction-changes))
							     (cdr rest-atoms))])
				   (if (null? rest*) changes-acc
				       (loop (caddr expr*) (cdr rest-atoms)
					     (append (append-to-change-locations extraction-changes cur-loc) changes-acc)
					     (append cur-loc '(2)))))))]
	 [precombining-constants-changes
	  (let loop ([rest-atoms sorted-atoms] [changes-acc '()]
		     [cur-expr (changes-apply (drop-change-location-items ordering-changes (length loc))
					      expr)])
	    (if (or (= 1 (length rest-atoms)) (not (number? (s-atom-var (cadr rest-atoms)))))
		changes-acc
		(let ([new-constant (+ (s-atom-var (car rest-atoms)) (s-atom-var (cadr rest-atoms)))])
		  (loop (cons (s-atom new-constant (s-atom-loc (car rest-atoms))))
			(list* (change (rule 'precompute `(* (s-atom-var (car rest-atoms)) (s-atom-var (cadr rest-atoms))) new-constant '())
				       (append loc '(1))
				       '())
			       (change (get-rule 'associate-*-lft) loc `((a . (s-atom-var (car rest-atoms)))
									 (b . (s-atom-var (cadr rest-atoms)))
									 (c . (cadr (cadr cur-expr)))))
			       (list '* new-constant (cadr (cadr cur-expr))))))))])
    (append precombining-constants-changes ordering-changes)))

(define (combine-+-changes term1 term2 expr loc)
  ;; Returns whether or not the expression is a multiplication of
  ;; parts, where one of the parts is a constant.
  (define (proper-term? expr)
    (cond [(not (and (list? expr) (eq? (car expr) '*)))
	   #f]
	  [(or (number? (cadr expr)) (number? (caddr expr)))
	   #t]
	  [#t (or (proper-term? (cadr expr)) (proper-term? (caddr expr)))]))

  (let*-values ([(extract-term1-changes expr*) (add-extract-changes expr (drop (s-term-loc term1) (length loc)))]
		[(extract-term1-full-changes) (append-to-change-locations (reverse extract-term1-changes) loc)]
		[(term2*)  (translate-term-through-changes extract-term1-full-changes term2)]
		[(extract-term2-changes caddrexpr*) (add-extract-changes (caddr expr*) (drop (s-term-loc term2*) (add1 (length loc))))]
		[(extract-term2-full-changes) (append-to-change-locations (reverse extract-term2-changes) (append loc '(2)))]
		[(expr*) (list (car expr*) (cadr expr*) caddrexpr*)]
		[(combine-changes term-combination)
		 (let loop ([cur-expr expr*] [cur-loc loc] [changes-acc '()])
		   (cond
		    [(and (list? (caddr cur-expr)) (eq? (caaddr cur-expr) '+))
		     (loop (list (car cur-expr) (cadr cur-expr) (cadr (caddr cur-expr)))
			   (append cur-loc '(1))
			   (cons (let ([rl (get-rule 'associate-+-lft)])
				   (change rl cur-loc (pattern-match (rule-input rl) cur-expr)))
				 changes-acc))]
		    [(and (list? (cadr cur-expr)) (eq? (caadr cur-expr) '-)
			  (list? (caddr cur-expr)) (eq? (caaddr cur-expr) '-))
		     (loop (list (car cur-expr) (cadadr cur-expr) (cadr (caddr cur-expr)))
			   (append cur-loc '(1))
			   (cons (let ([rl (get-rule 'distribute-neg-out)])
				   (change rl cur-loc (pattern-match (rule-input rl) cur-expr)))
				 changes-acc))]
		    [(and (list? (cadr cur-expr)) (eq? (caadr cur-expr) '-))
		     (loop (list (car cur-expr) (caddr cur-expr) (cadr cur-expr))
			   cur-loc
			   (cons (let ([rl (get-rule '+-commutative)])
				   (change rl cur-loc (pattern-match (rule-input rl) cur-expr)))
				 changes-acc))]
		    [(and (list? (caddr cur-expr)) (eq? (caaddr cur-expr) '-))
		     (loop (list '- (cadr cur-expr) (cadr (caddr cur-expr)))
			   cur-loc
			   (cons (let ([rl (get-rule 'unsub-neg)])
				   (change rl cur-loc (pattern-match (rule-input rl) cur-expr)))
				 changes-acc))]
		    [(not (or (proper-term? (cadr cur-expr)) (proper-term? (caddr cur-expr))))
		     (if (eq? (car cur-expr) '-)
			 (values (append (list (change (get-rule '+-inverses) cur-loc `((a . ,(cadr cur-expr)))))
					 (late-canonicalize-term-changes (append cur-loc '(1)) (cadr cur-expr))
					 (late-canonicalize-term-changes (append cur-loc '(2)) (caddr cur-expr))
					 changes-acc)
				 (s-term 0 '() cur-loc))
			 (loop `(+ (* 1 ,(cadr cur-expr)) (* 1 ,(caddr cur-expr)))
			       cur-loc
			       (let ([rl (get-rule '*-un-lft-identity)])
				 (list* (change rl (append cur-loc '(1)) (pattern-match (rule-input rl) (cadr cur-expr)))
					(change rl (append cur-loc '(2)) (pattern-match (rule-input rl) (caddr cur-expr)))
					changes-acc))))]
		    [(not (proper-term? (cadr cur-expr)))
		     (loop `(,(car cur-expr) (* 1 ,(cadr cur-expr)) ,(caddr cur-expr))
			   cur-loc
			   (list* (let ([rl (get-rule '*-un-lft-identity)])
				    (change rl (append cur-loc '(1)) (pattern-match (rule-input rl) (cadr cur-expr))))
				  changes-acc))]
		    [(not (proper-term? (caddr cur-expr)))
		     (loop `(,(car cur-expr) ,(cadr cur-expr) (* 1 ,(caddr cur-expr)))
			   cur-loc
			   (list* (let ([rl (get-rule '*-un-lft-identity)])
				    (change rl (append cur-loc '(2)) (pattern-match (rule-input rl) (caddr cur-expr))))
				  changes-acc))]
		    [#t (let* ([distributed-coeffs `(,(car cur-expr) ,(cadr (cadr cur-expr)) ,(cadr (caddr cur-expr)))]
			       [new-coeff (safe-eval distributed-coeffs)])
			  (values (let* ([canonicalize-left-changes (late-canonicalize-term-changes (append loc '(1)) (cadr cur-expr))]
					 [canonicalize-right-changes (late-canonicalize-term-changes (append loc '(2)) (caddr cur-expr))]
					 [cur-expr* (changes-apply (drop-change-location-items (append canonicalize-left-changes
												       canonicalize-right-changes)
											       (length loc)) cur-expr)]
					 [distribute-out-coeffs-change
					  (let ([rl (if (eq? (car cur-expr) '+)
							(get-rule 'distribute-rgt-out)
							(get-rule 'distribute-rgt-out--))])
					    (change rl cur-loc (pattern-match (rule-input rl) cur-expr*)))]
					 [cur-expr* (changes-apply (drop-change-location-items (list distribute-out-coeffs-change)
											       (length cur-loc))
								   cur-expr*)]
					 [commute-coeff-and-vars-change
					  (let ([rl (get-rule '*-commutative)])
					    (change rl cur-loc (pattern-match (rule-input rl) cur-expr*)))]
					 [cur-expr* (changes-apply (drop-change-location-items (list commute-coeff-and-vars-change)
											       (length cur-loc))
								   cur-expr*)]
					 [precompute-coeffs-change (change (rule 'precompute distributed-coeffs new-coeff '())
								    (append cur-loc '(1))
								    '())])
				   (append (list
					    precompute-coeffs-change
					    commute-coeff-and-vars-change
					    distribute-out-coeffs-change)
					   canonicalize-right-changes
					   canonicalize-left-changes
					   changes-acc))
				  (s-term new-coeff (s-term-vars term1) cur-loc)))]))]
		[(expr*) (changes-apply
			  (drop-change-location-items (reverse combine-changes) (length loc))
			  expr*)]
		[(cleanup-changes)
		 (let loop ([cur-coeff (s-term-coeff term-combination)] [changes-acc '()]
			    [cur-loc loc] [cur-vars (s-term-vars term-combination)]
			    [cur-expr expr*])
		   (cond [(and (list? cur-expr) (eq? (car cur-expr) '+))
			  (let* ([inner-cleanup-changes (loop cur-coeff changes-acc (append cur-loc '(1)) cur-vars (cadr cur-expr))]
				 [inner-expr* (changes-apply (drop-change-location-items inner-cleanup-changes (length loc)) expr*)])
			    (if (safe-= inner-expr* 0)
				(append (list (let ([rl (get-rule '+-lft-identity)])
						(change rl cur-loc
							(pattern-match (rule-input rl)
								       (list (car cur-expr) inner-expr* (caddr cur-expr))))))
					inner-cleanup-changes
					changes-acc)
				(append inner-cleanup-changes changes-acc)))]
			 [(= -1 cur-coeff)
			  (cons (let ([rl (get-rule 'mul-1-neg)])
				  (change rl cur-loc (pattern-match (rule-input rl) cur-expr)))
				changes-acc)]
			 [(and (= 0 cur-coeff) (not (null? cur-vars)))
			  (cons (let ([rl (get-rule 'mul0)])
				  (change rl cur-loc (pattern-match (rule-input rl) cur-expr)))
				changes-acc)]
			 [(= 1 cur-coeff)
			  (cons (let ([rl (get-rule '*-lft-identity)])
				  (change rl cur-loc (pattern-match (rule-input rl) cur-expr)))
				changes-acc)]
			 [#t '()]))])
    (values (append extract-term1-full-changes
		    extract-term2-full-changes
		    (reverse combine-changes)
		    (reverse cleanup-changes))
	    term-combination)))

(define (find-matching-term-pair terms)
  (let loop ([rest-terms terms])
    (if (null? rest-terms) #f
	(let ([member-res (member (car rest-terms) (cdr rest-terms)
				  (λ (t1 t2) (equal? (s-term-vars t1) (s-term-vars t2))))])
	  (if member-res
	      (list (car rest-terms) (car member-res))
	      (loop (cdr rest-terms)))))))

(define (translate-loc-through-changes changes loc)
  (if (null? changes) loc
      (let ([relative-loc (match-loc loc (change-location (car changes)))])
	(if (not relative-loc)
	    (translate-loc-through-changes (cdr changes) loc)
	    (if (> (length (change-location (car changes))) (length loc))
		(error "Cannot Translate: bad locations.")
		(let* ([new-relative-loc
			(car (translate-location relative-loc
						 (rule-location-translations (change-rule (car changes)))
						 car cadr))]
		       [new-abs-loc (append (change-location (car changes)) new-relative-loc)])
		  (translate-loc-through-changes (cdr changes) new-abs-loc)))))))

(define (translate-term-through-changes changes term)
  (s-term (s-term-coeff term) (s-term-vars term)
	  (translate-loc-through-changes changes (s-term-loc term))))

(define (translate-atom-through-changes changes atom)
  (s-atom (s-atom-var atom)
	  (translate-loc-through-changes changes (s-atom-loc atom))))

(define (s-atom-has-op? op atom)
  (let ([expr (s-atom-var atom)])
    (and (list? expr) (eq? op (car expr)))))

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
    (let ([inv-atom* (translate-atom-through-changes (append-to-change-locations
						      (reverse extract-vni-atom-changes)
						      loc)
						     inv-atom)])
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
    (let ([neg-atom* #|neg-atom might have moved now (probably did)|# (translate-atom-through-changes
								       (append-to-change-locations (reverse extract-pos-atom-changes)
												   loc)
								       neg-atom)])
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
		    (let ([expr* (changes-apply (drop-change-location-items (reverse new-changes) (length loc)) cur-expr)])
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
		    (let ([expr* (changes-apply (drop-change-location-items (reverse new-changes) (length loc)) cur-expr)])
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
	  (values '() (list (s-atom expr loc)))
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
	    [`(- (+ ,a ,b))
	     (append (canonicalize-expr loc `(+ (- ,a) (- ,b)))
		     (list (rule-apply->change (get-rule 'distribute-neg-in) loc cur-expr*))
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
