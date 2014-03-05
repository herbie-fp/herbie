#lang racket

(require casio/rules)
(require casio/alternative)
(require casio/programs)
(require casio/points)
(require casio/common)
(require casio/redgreen)
(require racket/match)

(provide (all-defined-out))

(define (simplify altn)
  (let ([slocations (if (alt-prev altn)
			(rule-slocations (change-rule (alt-change altn)))
			'(()))]
	 [location (if (alt-prev altn)
		       (change-location (alt-change altn))
		       '(cdr cdr car))])
    (when (*debug*) (println "Simplifying " (alt-program altn) " at " (map (lambda (l) (append location l))
									   slocations)))
    (define (simplify-at-locations slocations prog preerrors changes)
      (if (null? slocations)
	  changes
	  (let* ([full-location (append location (car slocations))]
		 [partly-simplified-prog (location-do full-location
						      prog
						      simplify-expression)]
		 [post-errors (if (or (null? partly-simplified-prog) (not (pair? partly-simplified-prog)))
				  (map (lambda (x) 0) *points*)
				  (errors partly-simplified-prog (*points*) (*exacts*)))])
	    (when (*debug*) (println "Simplified to: " partly-simplified-prog))
	    (if (< 0 (errors-diff-score post-errors preerrors))
		(simplify-at-locations (cdr slocations)
					partly-simplified-prog
					post-errors
					(let* ([new-rule (rule 'simplify
							       (location-get full-location
									     prog)
							       (location-get full-location
									     partly-simplified-prog)
							       '())]
					       [new-change (change new-rule full-location (map (lambda (x) (cons x x))
											       (get-contained-vars prog)))])
					  (cons new-change changes)))
		(simplify-at-locations (cdr slocations)
					prog
					preerrors
					changes)))))
    (let* ([simplifying-changes (simplify-at-locations slocations
						     (alt-program altn)
						     (alt-errors altn)
						     '())])
      (apply-changes altn simplifying-changes))))

(define (simplify-expression expr)
  (decanonicalize (resolve-terms (canonicalize (rm-fns-&-invs expr)))))

(define (get-contained-vars expr)
  (define (get-duplicated-vars expr)
    (cond [(or (null? expr) (real? expr)) '()]
	  [(symbol? expr) (list expr)]
	  [(list? expr) (apply append (map get-duplicated-vars
					   (cdr expr)))]))
  (remove-duplicates (get-duplicated-vars expr)))

(define func-inverses
  '((exp . log)
    (square . sqrt)
    (- . -)))

(define (rm-fns-&-invs prog)
  (define (rm-fn-&-inv expr)
;;    (when (*debug*) (println "Attempting to remove functions with their inverses from: " expr))
    (define (reverse-alist l)
      (map (lambda (pair) (cons (cdr pair) (car pair)))
	   l))
    (if (pair? expr)
	(let* ([all-invs (append func-inverses (reverse-alist func-inverses))]
	       [inv (let ([item (assoc (car expr) all-invs)]) (when item (cdr item)))])
	  (if (and inv (pair? (cadr expr)) (eq? (caadr expr) inv))
	      (cadadr expr)
	      expr))
	expr))
  (for-lists prog rm-fn-&-inv))

(define (for-lists top-expr f)
  (let ([expr* (f top-expr)])
    (if (list? expr*)
	(cons (car expr*) (map (lambda (x) (for-lists x f))
			       (cdr expr*)))
	expr*)))

(define (canonicalize expr)
  (if (pair? expr)
      (let ([expr* (cons (car expr)
			 (map canonicalize (cdr expr)))])
	(match expr*
	  [`(+ (+ . ,a) (+ . ,b)) (cons '+ (append a b))]
	  [`(+ ,a (+ . ,b)) (list* '+ a b)]
	  [`(+ (+ . ,a) ,b) (cons '+ (append a (list b)))]
	  [`(- (+ . ,a)) (cons '+ (map (lambda (expr) (list '- (canonicalize expr)))
       			       a))]
	  [`(- ,a ,b) (canonicalize `(+ ,a (- ,b)))]
	  [a a]))
      expr))

(define (decanonicalize cexpr)
  (if (pair? cexpr)
      (let ([expr* (cons (car cexpr)
			 (map decanonicalize (cdr cexpr)))])
	(match expr*
	  [`(+ ,a ,b ,c . ,n) `(+ (+ ,a ,b) ,c ,n)]
	  [`(* 1 ,a) a]
	  [`(+ ,a (- ,b)) `(- ,a ,b)]
	  [`(+ (- ,a) ,b) `(- ,b ,a)]
	  [a a]))
      cexpr))

(define (resolve-terms expr)
  (let loop ([terms (cdr expr)] [acc '()])
    (if (null? terms)
	(cond [(null? acc) 0]
	      [(= 1 (length acc)) (car acc)]
	      [#t (cons '+ acc)])
	(let* ([cur-term (car terms)]
	       [mterms (filter (lambda (t)
				 (equal? (term-atoms t)
					 (term-atoms cur-term)))
			       (cdr terms))])
	  (if (= 0 (length mterms))
	      (loop (cdr terms) (cons cur-term acc))
	      (loop (remove* mterms (cdr terms))
		    (append (combine-like-terms (cons (car terms) mterms))
			    acc)))))))
      
(define (term-atoms expr)
  ;;(when (*debug*) (println "getting atoms for " expr))
  (if (atomic expr)
      (list expr)
      (let ([positive-term (if (eq? (car expr) '-)
			       (cadr expr)
			       expr)])
	(if (atomic positive-term)
	    (list positive-term)
	    (if (real? (cadr positive-term))
		(cddr positive-term)
		(cdr positive-term))))))

(define (atomic expr)
  (or (real? expr)
      (symbol? expr)
      (not (or (eq? (car expr)
		    '-)
	       (eq? (car expr)
		    '+)
	       (eq? (car expr)
		    '/)
	       (eq? (car expr)
		    '*)))))

(define (combine-like-terms terms)
  ;;(when (*debug*) (println "combining: " terms))
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
			   0 terms)])
    (cond [(real? (car terms)) (list new-factor)]
	  [(= 0 new-factor) '()]
	  [(atomic (car terms)) `((* ,new-factor ,(car terms)))]
	  [(= 1 new-factor) (if (= 1 (length terms))
				(cddar terms)
				`((* . ,(cddar terms))))]
	  [(= -1 new-factor) (list (list '- (if (= 1 (length (cddar terms)))
						(caddar terms)
						(cons '* (cddar terms)))))]
	  [#t `((* ,new-factor . ,(cddar terms)))])))

(define (symbol<? sym1 sym2)
  (string<? (symbol->string sym1) (symbol->string sym2)))

(define (atom<? a1 a2)
  (cond [(and (symbol? a1) (symbol? a2))
	 (symbol<? a1 a2)]
	[(symbol? a1) #f]
	[(symbol? a2) #t]
	[#t (let ([vars1 (get-contained-vars a1)]
		  [vars2 (get-contained-vars a2)])
	      (let loop ([avars vars1] [bvars vars2])
		(cond [(and (null? avars) (null? bvars)) #f]
		      [(null? avars) #t]
		      [(null? bvars) #f]
		      [#t (if (eq? (car avars) (car bvars))
			      (loop (cdr avars) (cdr bvars))
			      (symbol<? (car avars) (car bvars)))])))]))
