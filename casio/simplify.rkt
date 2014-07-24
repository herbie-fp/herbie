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
  (let ([results (filter (λ (rule) (eq? (rule-name rule) name)) *rules*)])
    (if (null? results)
	(error "Could not find a rule by the name" name)
	(car results))))

;; Return the variables that are in the expression
(define (get-contained-vars expr)
  ;; Get a list that of the vars, with each var repeated for each time it appears
  (define (get-duplicated-vars expr)
    (cond [(or (null? expr) (real? expr)) '()] ; If we've reached the end of a list, or we're at a constant, there are no vars.
	  [(symbol? expr) (list expr)] ; If we're at a variable, return it in a list
	  [(list? expr) (apply append (map get-duplicated-vars
					   (cdr expr)))])) ; If we're at a list, get the vars from all of it's items, and append them together.
  (remove-duplicates (get-duplicated-vars expr))) ; Get the list with duplicates, and remove the duplicates.

(define (s-var<? v1 v2)
  (expr<? (s-var-var v1) (s-var-var v2)))

(define (s-var-equal? v1 v2)
  (and (eq? (s-var-var v1) (s-var-var v2))
       (= (s-var-pow v1) (s-var-pow v2))))

(define (s-terms-match? t1 t2)
  (and (= (length (s-term-vars t1)) (length (s-term-vars t2)))
       (andmap s-var-equal? (s-term-vars t1) (s-term-vars t2))))

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

;; Given an expression, returns a constant if that expression is just a function of constants, the original expression otherwise.
(define (try-precompute expr loc)
  (if (and (list? expr) (andmap number? (cdr expr)))
      (let ([value (with-handlers ([(const #t) (λ (e) #f)])
                       (safe-eval expr))])
	(if (rational? value)
	    (list (change (rule 'precompute expr value '()) loc '()))
	    '()))
      '()))

(define (srcloc->string sl)
  (if sl
      (string-append
       (path->string (srcloc-source sl))
       ":"
       (number->string (srcloc-line sl))
       ":"
       (number->string (srcloc-column sl)))
      "???"))

(define (simplify-expression expr)
  (debug "Simplifying expression: " expr #:from 'simplify #:depth 3)
  (with-handlers ([exn:fail? (λ (exn)
			       (debug #:from 'simplify #:depth 1
				      "!!CRASH!!"
				      "Simplifying expression: " expr
				      (exn-message exn)
				      (for/list ([tb (continuation-mark-set->context
						      (exn-continuation-marks exn))])
					(list (car tb) (srcloc->string (cdr tb)))))
			       '())])
    (let* ([canon-changes (canonicalize expr)]
	   [expr* (changes-apply canon-changes expr)]
	   [resolve-changes (cancel-terms expr*)]
	   [expr* (changes-apply resolve-changes expr*)]
	   [cleanup-changes (cleanup expr*)])
      (append canon-changes resolve-changes cleanup-changes))))

;; Simplifies an expression, and then applies the simplifying changes.
;; for debuggging.
(define (simplify-expression* expr)
  (let ([simplify-changes (simplify-expression expr)])
    (changes-apply simplify-changes expr)))

(struct s-atom (var loc) #:prefab)

(struct s-var (var pow loc inner-terms) #:prefab)
(struct s-term (coeff vars loc) #:prefab)
;; Given an expression that does not break down,
;; such as (f x) for some unknown f, this gives
;; the canonical term representation of it.
(define (expr-term atom-expr loc)
  (s-term 1 (list (s-var atom-expr 1 loc '())) loc))

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
	(values '() (list (s-term 1 (list (s-var leaf 1 loc '())) loc)))))
  ;; Takes a non-leaf node, the location at which it was found, and the terms
  ;; returned by handling it's sub-nodes, and returns a list of changes and terms
  ;; resulting from handling that node.
  (define (handle-node loc expr sub-term-lsts)
    (let ([pre-chngs (try-precompute expr loc)])
      (if (null? pre-chngs)
	  ((hash-ref *node-handlers* (car expr) (const default-node-handler)) loc expr sub-term-lsts)
	  (values pre-chngs
		  (list (s-term (changes-apply (make-chngs-rel pre-chngs loc) expr)
				'() loc))))))
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
  (values '() (list (s-term 1 (list (s-var expr 1 loc (car sub-term-lsts))) loc))))

(define (handle-expt loc expr sub-term-lsts)
  (cond [(matches? `(expt ,a 1) expr)
	 (values (list (let ([rl (get-rule 'unexpt1)])
			 (change rl loc `((a . ,(cadr expr))))))
		 (map (curry drop-term-loc (length loc) 1) (car sub-term-lsts)))]
	[(is-pow? expr)
	 (handle-pow loc expr sub-term-lsts (caddr expr))]
	[#t (values '()
		    (list (s-term 1 (list (s-var expr 1 loc (car sub-term-lsts))) loc)))]))

(define (is-pow? expr)
  (and (list? expr)
       (or (eq? (car expr) 'sqr)
	   (eq? (car expr) 'sqrt)
	   (and (eq? (car expr) 'expt)
		(number? (caddr expr))))))

(define-syntax-rule (matches? pattern expr)
  (match expr
    [pattern #t]
    [_ #f]))

(define (drop-term-loc i n term)
  (s-term (s-term-coeff term)
	  (map (curry drop-var-loc i n)
	       (s-term-vars term))
	  (append (take (s-term-loc term) i)
		  (drop (s-term-loc term) (+ i n)))))

(define (drop-var-loc i n var)
  (s-var (s-var-var var)
	 (s-var-pow var)
	 (append (take (s-var-loc var) i)
		 (drop (s-var-loc var) (+ i n)))
	 (map (curry drop-term-loc i n) (s-var-inner-terms var))))

(define (handle-pow loc expr sub-term-lsts pow)
  (cond [(is-pow? (cadr expr))
	 (let* ([canon-changes (reverse (late-canonicalize-var-changes loc expr))]
		[rel-canon-changes (drop-change-location-items canon-changes (length loc))]
		[expr* (changes-apply rel-canon-changes expr)]
		[inner-var (car (s-term-vars (caar sub-term-lsts)))])
	   (cond [(matches? `(expt ,a 1) expr*)
		  (values (append canon-changes
				  (list (let ([rl (get-rule 'unexpt1)])
					  (change rl loc `((a . ,(cadr expr*)))))))
			  (map (curry drop-term-loc (length loc) 1)
				   (map (λ (t) (s-term 1 (map (λ (v) (s-var (s-var-var v) 1 (s-var-loc v) (s-var-inner-terms v)))
							      (s-term-vars t))
						       (s-term-loc t)))
					(car sub-term-lsts))))]
		 [(matches? `(expt (expt ,a ,b) ,c) expr*)
		  (values '()
			  (list (s-term 1 (list (s-var (cadr expr) pow
						       loc (car sub-term-lsts)))
					loc)))]
		 [#t (values canon-changes
			     (list (s-term 1 (list (s-var (s-var-var inner-var) (* (s-var-pow inner-var) pow)
							  loc (map (curry drop-term-loc (length loc) 1)
								   (s-var-inner-terms inner-var))))
					   loc)))]))]
	[#t (values '() (if (< 1 (length (car sub-term-lsts)))
			    (list (s-term 1 (list (s-var (cadr expr) pow loc (car sub-term-lsts))) loc))
			    (let ([inner-vars (s-term-vars (caar sub-term-lsts))])
			      (if (and (rational? (expt (s-term-coeff (caar sub-term-lsts)) pow)) (< 1 (length inner-vars)))
				  (list (s-term 1 (map (λ (v)
							 (drop-var-loc (length loc) 1 (s-var (s-var-var v) (* pow (s-var-pow v))
											     (s-var-loc v) (s-var-inner-terms v))))
						       inner-vars) loc))
				  (list (s-term 1 (list (s-var (cadr expr) pow loc (car sub-term-lsts))) loc))))))]))

(define (negate-coeff term)
  (s-term (- (s-term-coeff term))
	  (s-term-vars term)
	  (s-term-loc term)))

(define (invert-var var)
  (s-var (s-var-var var)
	 (- (s-var-pow var))
	 (s-var-loc var)
	 (s-var-inner-terms var)))

(define (s-term-with-loc loc term)
  (s-term (s-term-coeff term)
	  (map (λ (v)
		 (s-var-with-loc
		  (append loc
			  (drop (s-var-loc v) (length (s-term-loc term))))
		  v))
	       (s-term-vars term)) loc))

;; Functionally mutate the terms location, without updating it's vars.
(define (s-term-with-loc* loc term)
  (s-term (s-term-coeff term)
	  (s-term-vars term) loc))

(define (s-var-with-loc loc var)
  (s-var (s-var-var var) (s-var-pow var)
	 loc (map (λ (t)
		    (s-term-with-loc
		     (append loc
			     (drop (s-term-loc t)
				   (length (s-var-loc var))))
		     t))
		  (s-var-inner-terms var))))

(define (s-var-with-loc* loc var)
  (s-var (s-var-var var) (s-var-pow var)
	 loc (s-var-inner-terms var)))

(define (s-term-with-vars term vars)
  (s-term (s-term-coeff term) vars (s-term-loc term)))

(define (varmap f term . lsts)
  (s-term-with-vars term (apply (curry map f (s-term-vars term)) lsts)))

(define (swap-tloc-elements i j term)
  (s-term-with-loc
   (let* ([old-loc (s-term-loc term)]
	  [old-i-item (list-ref old-loc i)]
	  [old-j-item (list-ref old-loc j)])
     (with-item i old-j-item
		(with-item j old-i-item old-loc)))
   term))

(define (swap-vloc-elements i j var)
  (s-var-with-loc
   (let* ([old-loc (s-var-loc var)]
	  [old-i-item (list-ref old-loc i)]
	  [old-j-item (list-ref old-loc j)])
     (with-item i old-j-item
		(with-item j old-i-item old-loc)))
   var))

(define (drop-range i n lst)
  (append (take lst i) (drop lst (+ i n))))
(define (dropr n lst)
  (take lst (sub1 (length lst))))

;; Inserts the elements of a into b at index i.
(define (insert i a b)
  (append (take b i) a (drop b i)))
;; These changes come out in applicative order.
(define (distribute-neg-in expr sub-terms loc)
  (match expr
    [`(- (+ ,a ,b))
     (let*-values ([(lft-terms rgt-terms) (partition (compose (curry = 1) cadr
							      (curryr drop (length loc)) s-term-loc)
						     sub-terms)]
		   [(lft-chngs lft-terms*) (distribute-neg-in `(- ,a) lft-terms (append loc '(1)))]
		   [(rgt-chngs rgt-terms*) (distribute-neg-in `(- ,b)
							      (map (curry swap-tloc-elements (length loc) (add1 (length loc)))
								   rgt-terms)
							      (append loc '(2)))])
       (values (append (list (let ([rl (get-rule 'distribute-neg-in)])
			       (change rl loc `((a . ,a)
						(b . ,b)))))
		       lft-chngs
		       rgt-chngs)
	       (append lft-terms* rgt-terms*)))]
    [`(- (- ,a))
     (values (list (let ([rl (get-rule 'remove-double-neg)])
		     (change rl loc `((a . ,a)))))
	     (if (= 1 (length sub-terms))
		 (list (varmap (λ (v)
				 (s-var-with-loc (drop-range (length loc) 2 (s-var-loc v)) v))
			       (s-term-with-loc* loc (negate-coeff (car sub-terms)))))
		 (error "Bad distribution of negatives: " expr)))]
    [`(- ,a)
     (if (number? a)
	 (values (list (let ([rl (rule 'precompute `(- ,a) (- a) '())])
			 (change rl loc '())))
		 (list (s-term (- a) '() loc)))
	 (values '()
		 (if (= 1 (length sub-terms))
		     (list (s-term-with-loc* (let ([oloc (s-term-loc (car sub-terms))])
					       (take oloc (sub1 (length oloc))))
					     (negate-coeff (car sub-terms))))
		     (error "Bad distribution of negatives: " expr))))]
    [_ (values '() sub-terms)]))

(define (distribute-mul-in expr sub-vars loc)
  (match expr
    [`(* (- ,a) (- ,b))
     (values (list (let ([rl (get-rule 'distribute-lft-neg-out)])
		     (change rl loc `((a . ,a)
				      (b . (- ,b)))))
		   (let ([rl (get-rule 'distribute-rgt-neg-out)])
		     (change rl (append loc '(1)) `((a . ,a)
						    (b . ,b))))
		   (let ([rl (get-rule 'remove-double-neg)])
		     (change rl loc `((a . (* ,a ,b))))))
	     (map (λ (v) (s-var-with-loc (drop-range (add1 (length loc)) 1 (s-var-loc v))))
		  sub-vars))]
    [`(* (- ,a) ,b)
     (values (list (let ([rl (get-rule 'distribute-lft-neg-out)])
		     (change rl loc `((a . ,a)
				      (b . ,b)))))
	     (map (λ (v) (if (= 2 (list-ref (s-var-loc v) (length loc)))
			     (s-var-with-loc (insert (length loc) '(1) (s-var-loc v))
					     v)
			     v))
		  sub-vars))]
    [`(* ,a (- ,b))
     (values (list (let ([rl (get-rule 'distribute-rgt-neg-out)])
		     (change rl loc `((a . ,a)
				      (b . ,b)))))
	     (map (λ (v) (if (= 1 (list-ref (s-var-loc v) (length loc)))
			     (s-var-with-loc (insert (length loc) '(1) (s-var-loc v))
					     v)
			     (swap-vloc-elements (length loc) (add1 (length loc)) v)))
		  sub-vars))]
    [_ (values '() sub-vars)]))

(define (distribute-inv-in expr sub-vars loc)
  (match expr
    [`(/ (* ,a ,b))
     (let*-values ([(lft-vars rgt-vars) (partition (compose (curry = 1) cadr
							    (curryr drop (length loc)) s-var-loc)
						   sub-vars)]
		   [(lft-chngs lft-vars*) (distribute-inv-in `(/ ,a) lft-vars (append loc '(1)))]
		   [(rgt-chngs rgt-vars*) (distribute-inv-in `(/ ,b)
							     (map (curry swap-vloc-elements (length loc) (add1 (length loc)))
								  rgt-vars)
							     (append loc '(2)))])
       (values (append (list (let ([rl (get-rule 'distribute-inv-in)])
			       (change rl loc `((a . ,a)
						(b . ,b)))))
		       lft-chngs
		       rgt-chngs)
	       (append lft-vars* rgt-vars*)))]
    [`(/ (- ,a))
     (let-values ([(inner-chngs inner-vars) (distribute-inv-in `(/ ,a) sub-vars (append loc '(1)))])
       (values (cons (let ([rl (get-rule 'distribute-inv-neg)])
		       (change rl loc `((a . ,a))))
		     inner-chngs)
	       inner-vars))]
    [`(/ (/ ,a))
     (values (list (let ([rl (get-rule 'remove-double-div)])
		     (change rl loc `((a . ,a)))))
	     (if (= 1 (length sub-vars))
		 (list (s-var-with-loc (let ([oloc (s-var-loc (car sub-vars))])
					   (take oloc (sub1 (length oloc))))
				       (invert-var (car sub-vars))))
		 (error "Bad distribution of inverses: " expr)))]
    [`(/ ,a)
     (if (number? a)
	 (values (list (let ([rl (rule 'precompute `(/ ,a) (/ a) '())])
			 (change rl loc '())))
		 (list (s-term (/ a) '() loc)))
	 (values '()
		 (if (= 1 (length sub-vars))
		     (list (s-var-with-loc* (let ([oloc (s-var-loc (car sub-vars))])
					      (take oloc (sub1 (length oloc))))
					    (invert-var (car sub-vars))))
		     (error "Bad distribution of inverses: " expr))))]
    [_ (values '() sub-vars)]))

(define *node-handlers*
  (make-immutable-hasheq
   `([+ . ,(λ (loc expr sub-term-lsts)
	     (let ([sub-terms (apply append sub-term-lsts)])
	       (match expr
		 [`(+ 0 ,a) (values (list (let ([rl (get-rule '+-lft-identity)])
					    (change rl loc `((a . ,a)))))
				    (map (curry drop-term-loc (length loc) 1)
					 (cadr sub-term-lsts)))]
		 [`(+ ,a 0) (values (list (let ([rl (get-rule '+-rgt-identity)])
					    (change rl loc `((a . ,a)))))
				    (map (curry drop-term-loc (length loc) 1)
					 (car sub-term-lsts)))]
		 [_ (try-combine-+ sub-terms expr loc)])))]
     [- . ,(λ (loc expr sub-term-lsts)
	     (define (negate-term term)
	       (s-term (- (s-term-coeff term))
		       (s-term-vars term)
		       (s-term-loc term)))
	     (if (= 1 (length sub-term-lsts))
		 (if (= 1 (length (car sub-term-lsts)))
		     (values '() (list (s-term-with-loc* loc (negate-term (caar sub-term-lsts)))))
		     (let-values ([(dist-neg-in-chngs terms*) (distribute-neg-in expr (car sub-term-lsts) loc)])
		       (values dist-neg-in-chngs terms*)))
		 (let* ([left-subterms (car sub-term-lsts)]
			[right-subterms (cadr sub-term-lsts)]
			[subterms (append left-subterms
					  (map negate-term
					       right-subterms))])
		   (try-combine-+ subterms expr loc))))]
     [* . ,(λ (loc expr sub-term-lsts)
	     (match expr
	       [`(* 0 ,a)
		(values (list (let ([rl (get-rule 'mul0)])
				(change rl loc `((a . ,a)))))
			'())]
	       [`(* ,a 0)
		(values (list (let ([rl (get-rule '*-commutative)])
				(change rl loc `((a . ,a) (b . 0))))
			      (let ([rl (get-rule 'mul0)])
				(change rl loc `((a . ,a)))))
			'())]
	       [`(* 1 ,a)
		(values (list (let ([rl (get-rule '*-lft-identity)])
				(change rl loc `((a . ,a)))))
			(map (curry drop-term-loc (length loc) 1) (cadr sub-term-lsts)))]
	       [`(* ,a 1)
		(values (list (let ([rl (get-rule '*-commutative)])
				(change rl loc `((a . ,a) (b . 1))))
			      (let ([rl (get-rule '*-lft-identity)])
				(change rl loc `((a . ,a)))))
			(map (curry drop-term-loc (length loc) 1) (car sub-term-lsts)))]
	       [_ (let ([vars (sort (append (if (> (length (car sub-term-lsts)) 1) '()
						(s-term-vars (caar sub-term-lsts)))
					    (if (> (length (cadr sub-term-lsts)) 1) '()
						(s-term-vars (caadr sub-term-lsts))))
				    s-var<?)]
			[coeff (* (if (> (length (car sub-term-lsts)) 1) 1
				      (s-term-coeff (caar sub-term-lsts)))
				  (if (> (length (cadr sub-term-lsts)) 1) 1
				      (s-term-coeff (caadr sub-term-lsts))))])
		    (if (= 0 coeff)
			(cancel-coeff-changes expr loc)
			(let*-values ([(dist-chngs vars*) (distribute-mul-in expr vars loc)]
				      [(expr*) (changes-apply (make-chngs-rel dist-chngs loc) expr)]
				      [(combine-chngs terms*) (if (eq? (car expr*) '*)
								  (try-combine-* vars* coeff expr* loc)
								  (try-combine-* vars* coeff (cadr expr*) (append loc '(1))))]
				      [(terms**) (if (eq? (car expr*) '*)
						     terms*
						     (map (λ (t) (s-term-with-loc* (dropr 1 (s-term-loc t)) t))
							  terms*))])
			  (values (append dist-chngs combine-chngs)
				  terms**))))]))]
     [/ . ,(λ (loc expr sub-term-lsts)
	     (define (invert-var var)
	       (s-var (s-var-var var) (- (s-var-pow var)) loc (s-var-inner-terms var)))
	     (cond [(< 1 (length sub-term-lsts))
		    (error "Binary division should not appear in canonicalized expressions!: " expr)]
		   [(= 1 (length (car sub-term-lsts)))
		    (if (< 1 (length (s-term-vars (caar sub-term-lsts))))
			(let-values ([(chngs vars*) (distribute-inv-in expr (s-term-vars (caar sub-term-lsts)) loc)])
			  (values chngs
				  (list (s-term (/ (s-term-coeff (caar sub-term-lsts)))
						vars*
						loc))))
			(values '() (list (s-term (s-term-coeff (caar sub-term-lsts))
						  (map invert-var (s-term-vars (caar sub-term-lsts)))
						  loc))))]
		   [#t (values
			'() (list (s-term 1 (list (s-var (cadr expr) -1 loc (car sub-term-lsts))) loc)))]))]
     [expt . ,handle-expt]
     [sqr . ,(λ (loc expr sub-term-lsts)
	       (handle-pow loc expr sub-term-lsts 2))]
     [sqrt . ,(λ (loc expr sub-term-lsts)
		(handle-pow loc expr sub-term-lsts 1/2))]
     [exp . ,(λ (loc expr sub-term-lsts)
	       (if (matches? `(exp (log ,a)) expr)
		   (values (list (let ([rl (get-rule 'rem-exp-log)])
				   (change rl loc `((x . ,(cadr (cadr expr)))))))
			   (map (curry drop-term-loc (length loc) 2)
				(s-var-inner-terms (car (s-term-vars (caar sub-term-lsts))))))
		   (default-node-handler loc expr sub-term-lsts)))]
     [log . ,(λ (loc expr sub-term-lsts)
	       (if (matches? `(log (exp ,a)) expr)
		   (values (list (let ([rl (get-rule 'rem-log-exp)])
				   (change rl loc `((x . ,(cadr (cadr expr)))))))
			   (map (curry drop-term-loc (length loc) 2)
				(s-var-inner-terms (car (s-term-vars (caar sub-term-lsts))))))
		   (default-node-handler loc expr sub-term-lsts)))])))

(define (cancel-coeff-changes expr loc)
  (let* ([lc-changes (late-canonicalize-term-changes loc expr)]
	 [expr* (changes-apply (drop-change-location-items lc-changes (length loc)) expr)]
	 [cancel-change (let ([rl (get-rule 'mul0)])
			  (change rl loc `((a . ,(cadr expr*)))))])
    (values (append lc-changes (list cancel-change))
	    (list (s-term 0 '() loc)))))

(define (try-combine-* vars coeff expr loc)
  (let loop ([rest-vars vars] [cur-expr expr] [changes-acc '()])
    (let ([match (find-matching-var-pair rest-vars)])
      (if (not match) (values changes-acc (list (s-term coeff rest-vars loc)))
	  (let*-values ([(chngs var*) (combine-*-changes (car match) (cadr match) expr loc)]
			[(vars*) (map (curry translate-var-through-changes chngs) (remove* match rest-vars))])
	    (loop (if (= 0 (s-var-pow var*)) vars*
		      (cons var* vars*))
		  (changes-apply (drop-change-location-items chngs (length loc)) cur-expr)
		  (append changes-acc chngs)))))))

(define (make-chngs-rel chngs loc)
  (drop-change-location-items chngs (length loc)))
(define (make-chngs-full chngs loc)
  (append-to-change-locations chngs loc))

(define (combine-*-changes var1 var2 expr loc)
  (let*-values ([(relative-var1-loc) (drop (s-var-loc var1) (length loc))]
		[(extract-var1-changes expr*1) (mul-extract-changes expr relative-var1-loc)]
		[(extract-var1-full-changes) (reverse (append-to-change-locations extract-var1-changes loc))]
		[(var2*1) (translate-var-through-changes extract-var1-full-changes var2)]
		[(relative-var2-loc) (drop (s-var-loc var2*1) (length loc))]
		[(extract-var2-changes caddrexpr*) (mul-extract-changes (caddr expr*1) (drop relative-var2-loc 1))]
		[(extract-var2-full-changes) (reverse (append-to-change-locations extract-var2-changes (append loc '(2))))]
		[(expr*2) (with-item 2 caddrexpr* expr*1)]
		[(var2*2) (translate-var-through-changes extract-var2-full-changes var2*1)]
		[(relative-var2-loc*) (drop (s-var-loc var2*2) (length loc))]
		[(canon-var1-changes) (reverse (late-canonicalize-var-changes (append loc '(1)) (cadr expr*2)))]
		[(canon-var2-changes) (reverse (late-canonicalize-var-changes (s-var-loc var2*2) (location-get relative-var2-loc* expr*2)))]
		[(expr*3) (changes-apply (make-chngs-rel (append canon-var1-changes canon-var2-changes) loc) expr*2)]
		[(combine-changes var-combination)
		 (let loop ([cur-expr expr*3] [cur-loc loc] [changes-acc '()])
		   (if (and (list? (caddr cur-expr)) (eq? (caaddr cur-expr) '*))
		       (loop (list (car cur-expr) (cadr cur-expr) (cadr (caddr cur-expr)))
			     (append cur-loc '(1))
			     (cons (let ([rl (get-rule 'associate-*-lft)])
				     (change rl cur-loc (pattern-match (rule-input rl) cur-expr)))
				   changes-acc))
		       (values (let* ([distribute-powers-change (let ([rl (get-rule 'expt-prod-up)])
								  (change rl cur-loc (pattern-match (rule-input rl) cur-expr)))]
				      [cur-expr* (changes-apply (make-chngs-rel (list distribute-powers-change) loc) cur-expr)]
				      [precompute-powers-change
				       (change (rule 'precompute `(+ ,(s-var-pow var1) ,(s-var-pow var2))
						     (+ (s-var-pow var1) (s-var-pow var2)) '())
					       (append cur-loc '(2)) '())])
				 (list* precompute-powers-change distribute-powers-change changes-acc))
			       (s-var (s-var-var var1) (+ (s-var-pow var1) (s-var-pow var2)) cur-loc (s-var-inner-terms var1)))))]
		[(expr*4) (changes-apply (reverse (drop-change-location-items combine-changes (length loc))) expr*3)]
		[(cleanup-changes)
		 (cond [(= (s-var-pow var-combination) 0)
			(list* (let ([rl (get-rule 'unexpt0)])
				 (change rl (s-var-loc var-combination) `((a . ,(s-var-var var-combination)))))
			       (if (equal? (s-var-loc var-combination) loc) '()
				   (list (let ([rl (get-rule '*-lft-identity)])
					   (change rl loc `((a . ,(caddr expr*4))))))))]
		       [(= (s-var-pow var-combination) 1)
			(list (let ([rl (get-rule 'unexpt1)])
				(change rl (s-var-loc var-combination) `((a . ,(s-var-var var-combination))))))]
		       [#t '()])])
    (values (append extract-var1-full-changes
		    extract-var2-full-changes
		    canon-var1-changes
		    canon-var2-changes
		    (reverse combine-changes)
		    cleanup-changes)
	    var-combination)))

(define (try-combine-+ terms expr loc)
  (let loop ([rest-terms terms] [cur-expr expr] [changes-acc '()])
    (let ([match (find-matching-term-pair rest-terms)])
      (if (not match) (values changes-acc rest-terms)
	  (let*-values ([(chngs term*) (combine-+-changes (car match) (cadr match) cur-expr loc)]
			[(terms*) (map (curry translate-term-through-changes chngs) (remove* match rest-terms))])
	    (loop (if (= 0 (s-term-coeff term*)) terms*
		      (cons term* terms*))
		  (changes-apply (drop-change-location-items chngs (length loc)) cur-expr)
		  (append changes-acc chngs)))))))

(define (late-canonicalize-var-changes loc expr)
  (define (get-merge-chngs loc expr)
    (match expr
      [`(/ ,a) (append (get-merge-chngs loc `(expt ,a -1))
		       (list (let ([rl (get-rule 'inv-expt)])
			       (change rl loc `((a . ,a))))))]
      [`(sqr ,a) (append (get-merge-chngs loc `(expt ,a 2))
			 (list (let ([rl (get-rule 'expt2)])
				 (change rl loc `((a . ,a))))))]
      [`(sqrt ,a) (append (get-merge-chngs loc `(expt ,a 1/2))
			  (list (let ([rl (get-rule 'expt1/2)])
				  (change rl loc `((a . ,(cadr expr)))))))]
      [_ (if (not (is-pow? expr)) '()
	     (let* ([inner-canonicalize-changes (get-merge-chngs (append loc '(1))
								 (cadr expr))]
		    [cadr-expr* (changes-apply (reverse (drop-change-location-items
							 inner-canonicalize-changes (add1 (length loc)))) (cadr expr))]
		    [expr* (with-item 1 cadr-expr* expr)]
		    [inner-pow (if (not (is-pow? cadr-expr*)) 1
				   (caddr cadr-expr*))]
		    [outer-pow (caddr expr*)]
		    [new-pow (* inner-pow outer-pow)])
	       (append (if (and (not (= 1 inner-pow))
				(or (not (integer? inner-pow))
				    (odd? inner-pow) (and (integer? new-pow)
							  (even? new-pow))))
			   (list (change (rule 'precompute `(* inner-pow outer-pow) new-pow '())
					 (append loc '(2)) '())
				 (let ([rl (get-rule 'expt-expt)])
				   (change rl loc (pattern-match (rule-input rl) expr*))))
			   '())
		       inner-canonicalize-changes)))]))
  (let* ([merge-chngs (get-merge-chngs loc expr)]
	 [merged (changes-apply (reverse (drop-change-location-items merge-chngs (length loc))) expr)])
    (if (not (list? merged))
	(cons (let ([rl (get-rule 'expt1)])
		(change rl loc `((a . ,merged))))
	      merge-chngs)
	merge-chngs)))

(define (late-canonicalize-term-changes loc expr)
  (let* ([atoms (let loop ([cur-expr expr] [cur-loc loc])
		  (if (and (list? cur-expr) (eq? '* (car cur-expr)))
		      (append (loop (cadr cur-expr) (append cur-loc '(1)))
			      (loop (caddr cur-expr) (append cur-loc '(2))))
		      (list (s-atom cur-expr cur-loc))))]
	 [canon-v-changes (apply append (map (λ (atom) (if (number? (s-atom-var atom)) '()
							   (late-canonicalize-var-changes (s-atom-loc atom)
											  (s-atom-var atom))))
					     atoms))]
	 [expr* (changes-apply (make-chngs-rel (reverse canon-v-changes) loc) expr)]
	 [atoms* (map (λ (a) (s-atom (location-get (drop (s-atom-loc a) (length loc)) expr*)
				     (s-atom-loc a)))
		      atoms)]
	 [sorted-atoms (sort atoms* s-atom<?)]
	 [ordering-changes (let loop ([cur-expr expr*] [rest-atoms sorted-atoms]
				      [changes-acc '()] [cur-loc loc])
			     (if (null? rest-atoms) changes-acc
				 (let*-values ([(extraction-changes expr*)
						(mul-extract-changes cur-expr (drop (s-atom-loc (car rest-atoms)) (length cur-loc)))]
					       [(full-extraction-changes) (append-to-change-locations extraction-changes cur-loc)]
					       [(rest*) (map (curry translate-atom-through-changes (reverse full-extraction-changes))
							     (cdr rest-atoms))])
				   (if (null? rest*) changes-acc
				       (loop (caddr expr*) rest*
					     (append full-extraction-changes changes-acc)
					     (append cur-loc '(2)))))))]
	 [precombining-constants-changes
	  (let loop ([rest-atoms sorted-atoms] [changes-acc '()]
		     [cur-expr (changes-apply (drop-change-location-items ordering-changes (length loc))
					      expr*)])
	    (if (or (= 1 (length rest-atoms)) (not (number? (s-atom-var (cadr rest-atoms)))))
		changes-acc
		(let ([new-constant (* (s-atom-var (car rest-atoms)) (s-atom-var (cadr rest-atoms)))])
		  (loop (cons (s-atom new-constant (s-atom-loc (car rest-atoms)))
			      (map (λ (atom) (s-atom (s-atom-var atom)
						     (append (take (s-atom-loc atom) (length loc))
							     (drop (s-atom-loc atom) (add1 (length loc))))))
				   (cddr rest-atoms)))
			(list* (change (rule 'precompute `(* (s-atom-var (car rest-atoms)) (s-atom-var (cadr rest-atoms))) new-constant '())
				       (append loc '(1))
				       '())
			       (change (get-rule 'associate-*-lft) loc `((a . ,(s-atom-var (car rest-atoms)))
									 (b . ,(s-atom-var (cadr rest-atoms)))
									 (c . ,(caddr (caddr cur-expr)))))
			       changes-acc)
			(list '* new-constant (caddr (caddr cur-expr)))))))]
	 [expr* (changes-apply (reverse (drop-change-location-items (append precombining-constants-changes ordering-changes) (length loc))) expr*)])
    (append precombining-constants-changes ordering-changes canon-v-changes)))

;; Returns changes in applicative order.
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
		[(expr*) (with-item 2 caddrexpr* expr*)]
		[(combine-changes term-combination)
		 (let loop ([cur-expr expr*] [cur-loc loc] [changes-acc '()])
		   (match cur-expr
		    [`(+ ,a (+ ,b ,c))
		     (loop (list '+ a b)
			   (append cur-loc '(1))
			   (cons (let ([rl (get-rule 'associate-+-lft)])
				   (change rl cur-loc `((a . ,a) (b . ,b) (c . ,c))))
				 changes-acc))]
		    [`(+ (- ,a) (- ,b))
		     (let-values ([(inner-chngs inner-term)
				   (loop `(+ ,a ,b)
					 (append cur-loc '(1))
					 (cons (let ([rl (get-rule 'distribute-neg-out)])
						 (change rl cur-loc `((a . ,a) (b . ,b))))
					       changes-acc))])
		       (values inner-chngs (s-term-with-loc* (dropr 1 (s-term-loc inner-term)) inner-term)))]
		    [`(+ (- ,a) ,b)
		     (loop `(+ ,b (- ,a))
			   cur-loc
			   (cons (let ([rl (get-rule '+-commutative)])
				   (change rl cur-loc `((a . (- ,a)) (b . ,b))))
				 changes-acc))]
		    [`(+ ,a (- ,b))
		     (loop `(- ,a ,b)
			   cur-loc
			   (cons (let ([rl (get-rule 'unsub-neg)])
				   (change rl cur-loc `((a . ,a) (b . ,b))))
				 changes-acc))]
		    [`(,op ,t1 ,t2)
		     (cond [(and (number? t1) (number? t2))
			    (let* ([val (+ t1 t2)]
				   [rl (rule 'precompute cur-expr val '())])
			      (values (cons (change rl cur-loc '())
					    changes-acc)
				      (s-term val '() cur-loc)))]
			   [(not (or (proper-term? t1) (proper-term? t2)))
			    (if (eq? op '-)
				(values (append (list (change (get-rule '+-inverses) cur-loc `((a . ,t1))))
						(late-canonicalize-term-changes (append cur-loc '(1)) t1)
						(late-canonicalize-term-changes (append cur-loc '(2)) t2)
						changes-acc)
					(s-term 0 '() cur-loc))
				(loop `(+ (* 1 ,t1) (* 1 ,t2))
				      cur-loc
				      (let ([rl (get-rule '*-un-lft-identity)])
					(list* (change rl (append cur-loc '(1)) `((a . ,t1)))
					       (change rl (append cur-loc '(2)) `((a . ,t2)))
					       changes-acc))))]
			   [(not (proper-term? t1))
			    (loop `(,op (* 1 ,t1) ,t2)
				  cur-loc
				  (list* (let ([rl (get-rule '*-un-lft-identity)])
					   (change rl (append cur-loc '(1)) `((a . ,t1))))
					 changes-acc))]
			   [(not (proper-term? t2))
			    (loop `(,op ,t1 (* 1 ,t2))
				  cur-loc
				  (list* (let ([rl (get-rule '*-un-lft-identity)])
					   (change rl (append cur-loc '(2)) `((a . ,t1))))
					 changes-acc))]
			   [#t
			    (let* ([canonicalize-left-changes (late-canonicalize-term-changes (append cur-loc '(1)) t1)]
				   [canonicalize-right-changes (late-canonicalize-term-changes (append cur-loc '(2)) t2)]
				   [cur-expr* (changes-apply (drop-change-location-items (append (reverse canonicalize-left-changes)
												 (reverse canonicalize-right-changes))
											 (length cur-loc))
							     cur-expr)]
				   [distributed-coeffs `(,(car cur-expr*) ,(cadr (cadr cur-expr*)) ,(cadr (caddr cur-expr*)))]
				   [new-coeff (safe-eval distributed-coeffs)]
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
			      (let ([chngs (append (list
						    precompute-coeffs-change
						    commute-coeff-and-vars-change
						    distribute-out-coeffs-change)
						   canonicalize-right-changes
						   canonicalize-left-changes
						   changes-acc)])
				(values chngs
					(s-term new-coeff
						(let ([tloc (s-term-loc term1)])
						  (map (λ (v)
							 (let* ([rel-vloc (drop (s-var-loc v) (length tloc))]
								[new-vloc (append cur-loc '(2) rel-vloc)])
							   (s-var (s-var-var v) (s-var-pow v) new-vloc
								  (s-var-inner-terms v))))
						       (s-term-vars term1)))
						cur-loc))))])]))]
		[(expr*) (changes-apply
			  (drop-change-location-items (reverse combine-changes) (length loc))
			  expr*)]
		[(cleanup-changes)
		 (let loop ([cur-coeff (s-term-coeff term-combination)] [changes-acc '()]
			    [cur-loc loc] [cur-vars (s-term-vars term-combination)]
			    [cur-expr expr*])
		   (cond [(and (list? cur-expr) (eq? (car cur-expr) '+))
			  (let* ([inner-cleanup-changes (loop cur-coeff changes-acc (append cur-loc '(1)) cur-vars (cadr cur-expr))]
				 [inner-expr* (changes-apply (drop-change-location-items inner-cleanup-changes (add1 (length loc))) (cadr cur-expr))])
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
	(let ([member-res (memf (curry s-terms-match? (car rest-terms))
				(cdr rest-terms))])
	  (if member-res
	      (list (car rest-terms) (car member-res))
	      (loop (cdr rest-terms)))))))

(define (find-matching-var-pair vars)
  (let loop ([rest-vars vars])
    (if (null? rest-vars) #f
	(let ([member-res (memf (λ (v2)
				  (eq? (s-var-var (car rest-vars))
				       (s-var-var v2)))
				(cdr rest-vars))])
	  (if member-res
	      (list (car rest-vars) (car member-res))
	      (loop (cdr rest-vars)))))))

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
  (s-term (s-term-coeff term) (map (curry translate-var-through-changes changes) (s-term-vars term))
	  (translate-loc-through-changes changes (s-term-loc term))))

(define (translate-atom-through-changes changes atom)
  (s-atom (s-atom-var atom)
	  (translate-loc-through-changes changes (s-atom-loc atom))))

(define (translate-var-through-changes changes var)
  (s-var (s-var-var var) (s-var-pow var)
	 (translate-loc-through-changes changes (s-var-loc var))
	 (s-var-inner-terms var)))

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
	    [`(* (- ,a) (- ,b))
	     (append (canonicalize-expr loc `(* ,a ,b))
		     (list (rule-apply->change (get-rule 'remove-double-neg) loc `(- (- (* ,a ,b))))
			   (rule-apply->change (get-rule 'distribute-lft-neg-out) (append loc (list 1)) `(* (- ,a) ,b))
			   (rule-apply->change (get-rule 'distribute-rgt-neg-out) loc cur-expr*))
		     sub-changes)]
	    [`(* (- ,a) ,b)
	     (append (canonicalize-expr (append loc (list 1)) `(* ,a ,b))
		     (list (rule-apply->change (get-rule 'distribute-lft-neg-out) loc cur-expr*))
		     sub-changes)]
	    [`(* ,a (- ,b))
	     (append (canonicalize-expr (append loc (list 1)) `(* ,a ,b))
		     (list (rule-apply->change (get-rule 'distribute-rgt-neg-out) loc cur-expr*))
		     sub-changes)]
	    [`(/ ,a ,b)
	     (append (canonicalize-expr loc `(* ,a (/ ,b)))
		     (list (rule-apply->change (get-rule 'div-inv) loc cur-expr*))
		     sub-changes)]
	    [`(/ (* ,a ,b))
	     (append (canonicalize-expr loc `(* (/ ,a) (/ ,b)))
		     (list (rule-apply->change (get-rule 'distribute-inv-in) loc cur-expr*))
		     sub-changes)]
	    [`(/ (/ ,a))
	     (cons (rule-apply->change (get-rule 'remove-double-div) loc cur-expr*)
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
	    [`(expt (* ,a ,b) ,c)
	     (append (canonicalize-expr (append loc (list 1)) `(expt ,a ,c))
		     (canonicalize-expr (append loc (list 2)) `(expt ,b ,c))
		     (list (rule-apply->change (get-rule 'unexpt-prod-down) loc cur-expr*))
		     sub-changes)]
	    [`(sqr (- ,a))
	     (cons (rule-apply->change (get-rule 'sqr-neg) loc cur-expr*)
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

;; Makes top level changes to the given expressions with the given rules, specified by name.
(define (make-changes expr . rule-names)
  (let loop ([cur-expr expr] [rest-rules (map get-rule rule-names)] [acc '()])
    (if (null? rest-rules) (reverse acc)
	(let* ([rl (car rest-rules)]
	       [chng (change rl '()
			     (pattern-match (rule-input rl) cur-expr))])
	  (loop (change-apply chng expr) (cdr rest-rules)
		(cons chng acc))))))

;; Takes a function f (expr) -> [chng] and applies it to
;; the subexpressions in this expression, appending the correct
;; change prefixes to the results.
(define (sub-apply-chng f expr)
  (apply append
	 (enumerate (λ (idx item)
		      (append-to-change-locations (f item) (list (add1 idx))))
		    (cdr expr))))

(define (cleanup expr)
  (let* ([normalize-chngs (normalize expr)]
	 [expr* (changes-apply normalize-chngs expr)]
	 [ci-chngs (coerce-inverses expr*)])
    (append normalize-chngs ci-chngs)))

(define (normalize expr)
  (let* ([sub-changes (if (list? expr) (sub-apply-chng normalize expr) '())]
	 [expr* (changes-apply sub-changes expr)]
	 [make-changes* (curry make-changes expr*)])
    (append
     sub-changes
     (match expr*
       [`(+ (- ,a) (- ,b))
	(make-changes* 'distribute-neg-out)]
       [`(+ ,a (- ,b))
	(make-changes* 'unsub-neg)]
       [`(- (, a) ,b)
	(make-changes* '+-commutative 'unsub-neg)]
       [`(* (/ ,a) (/ ,b))
	(make-changes* 'distribute-inv-out)]
       [`(* ,a (/ ,b))
	(make-changes* 'un-div-inv)]
       [`(* (/ ,a) ,b)
	(make-changes* '*-commutative 'un-div-inv)]
       [`(expt x 1)
	(make-changes* 'unexpt1)]
       [_ '()]))))

(define (coerce-inverses expr)
  (let* ([sub-changes (if (list? expr) (sub-apply-chng coerce-inverses expr) '())]
	 [expr* (changes-apply sub-changes expr)]
	 [make-changes* (curry make-changes expr*)])
    (append
     sub-changes
     (match expr*
       [`(/ ,a)
	(make-changes* '*-un-lft-identity 'un-div-inv)]
       [_ '()]))))

(define (tchngs f expr)
  (let ([changes (f expr)])
    (changes-apply changes expr)))
