#lang racket

(require casio/rules)
(require casio/common)
(require racket/set)

(define prog '(- (sqrt (+ x 1)) (sqrt x)))
(provide (struct-out egraph) enode-vars mk-egraph iterate-egraph
	 graphviz-e get-rule)

(struct enode (vars) #:mutable)
(struct egraph (top exprs ens cnt) #:mutable)

(define (mk-enode eg expr)
  (if (enode? expr)
      (error "!!!")
      (let ([en (hash-ref (egraph-exprs eg) expr #f)])
        (if en
            en
            (let ([en* (enode (list expr))])
              (hash-set! (egraph-exprs eg) expr en*)
              (hash-set! (egraph-ens eg) en* (egraph-cnt eg))
              (set-egraph-cnt! eg (+ (egraph-cnt eg) 1))
              en*)))))

(define (enode-add! eg e var)
  (when (not (member var (enode-vars e)))
    (if (enode? var)
	(error "!!!")
	(hash-set! (egraph-exprs eg) var e))
    (set-egraph-cnt! eg (+ (egraph-cnt eg) 1))
    (set-enode-vars! e (cons var (enode-vars e)))))

(define (egraph-replace! eg enfind enreplace)
  (when (not (eq? enfind enreplace))
    (when (eq? (egraph-top eg) enfind)
      (set-egraph-top! eg enreplace))
    (let enode-replace! ([seen '()] [cur-en (egraph-top eg)])
      (when (not (member cur-en seen))
	(set-enode-vars!
	 cur-en
	 (for/list ([var (enode-vars cur-en)])
	   (if (list? var)
	       (list* (car var)
		      (map (λ (en)
			     (if (eq? en enfind)
				 enreplace en))
			   (cdr var)))
	       var)))
	(for ([var (enode-vars cur-en)])
	  (when (list? var)
	    (map (curry enode-replace! (cons cur-en seen))
		 (cdr var))))))
    (hash-remove! (egraph-ens eg) enfind)
    (let ([exprs (egraph-exprs eg)])
      (for ([expr (hash-keys exprs)])
	(let ([nd (hash-ref exprs expr)])
	  (cond [(eq? nd enfind)
		 (hash-set! exprs expr enreplace)]
		[(and (list? expr) (ormap (curry eq? enfind) (cdr expr)))
		 (hash-remove! exprs expr)
		 (hash-set! exprs
			    (cons
			     (car expr)
			     (for/list ([en (cdr expr)])
			       (if (eq? en enfind)
				   enreplace
				   en)))
			    nd)]
		[#t '()]))))))

(define (enode-merge! eg enfrom eninto)
  (set-enode-vars!
   eninto
   ;; This ordering is important: We're putting the enfrom vars first
   ;; because it is usually the shorter route and thus the one we want to pick.
   (remove-duplicates (append (enode-vars enfrom)
			      (enode-vars eninto))))
  (egraph-replace! eg enfrom eninto))

(define (mk-egraph p)
  (define (go eg expr)
    (if (list? expr)
        (mk-enode eg (cons (car expr) (map (curry go eg) (cdr expr))))
        (mk-enode eg expr)))

  (let* ([eg (egraph #f (make-hash) (make-hasheq) 0)]
         [en (go eg p)])
    (set-egraph-top! eg en)
    eg))

(define (list-cartesian-product . lsts)
  (if (null? lsts)
      '(())
      (let ([tails (apply list-cartesian-product (cdr lsts))])
        (for*/list ([elt (car lsts)] [tail tails])
          (cons elt tail)))))

(define (merge . bindings)
  ; (list bindings) -> binding
  (foldl merge2 '() bindings))

(define (merge2 binding1 binding2)
  ; binding binding -> binding
  (if (and binding1 binding2)
      (let loop ([acc binding1] [rest binding2])
        (if (null? rest)
            acc
            (let* ([curr (car rest)]
                   [lookup (assoc (car curr) acc)])
              (if lookup
                  (if (equal? (cdr lookup) (cdr curr))
                      (loop acc (cdr rest))
                      #f)
                  (loop (cons curr acc) (cdr rest))))))
      #f))


(define (match-e pat e)
  (cond
   [(number? pat)
    (call/ec
     (λ (k)
      (for ([var (enode-vars e)])
        (when (and (number? var) (= pat var))
          (k '(()))))
      '()))]

   [(symbol? pat)
    `(((,pat . ,e)))]

   [(list? pat)
    (apply append
      (for/list ([var (enode-vars e)])
        (if (and (list? var) (eq? (car var) (car pat))
                   (= (length var) (length pat)))
          (filter identity
                  (map (curry apply merge)
                         (apply list-cartesian-product
                                (for/list ([subpat (cdr pat)] [sube (cdr var)])
                                  (match-e subpat sube)))))
          '())))]

   [else
    (error "WTF" pat)]))

(define (substitute-e eg pat bindings)
  (cond
   ;;This is subtly wrong. We also want to count symbolic constants as numbers
   [(number? pat) pat]
   [(symbol? pat)
    (cdr (assoc pat bindings))]
   [(list? pat)
    (cons (car pat)
          (for/list ([subpat (cdr pat)])
            (let ([e (substitute-e eg subpat bindings)])
              (if (enode? e)
                  e
                  (mk-enode eg e)))))]))

(define (for-egraph eg f)
  (map f (hash-keys (egraph-ens eg))))

(define (at-node eg r en)
  (let ([binds (match-e (rule-input r) en)])
    (for ([bind binds])
      (let ([var (substitute-e eg (rule-output r) bind)])
	(if (enode? var)
	    (enode-merge!
	     eg var en)
	    (enode-add! eg en var))))))

(define (one-iter eg rules)
  (let ([start-size (egraph-cnt eg)])
    (for ([r rules])
      (for-egraph eg (λ (en) (at-node eg r en))))
    (> (egraph-cnt eg) start-size)))

(define (iterate-egraph eg rules n)
  (define (iterate k)
    (println "Size(" k "): " (egraph-cnt eg))
    (when (and (> k 0) (one-iter eg rules))
      (iterate (- k 1))))

  (iterate n)
  eg)

(define (graphviz-e e fp)
  (call-with-output-file fp #:exists 'replace
      (λ (p)
         (println #:port p "digraph {")
         (hash-for-each (egraph-ens e)
                        (λ (en nid)
                           (println #:port p "en" nid " [label=\"NODE\"] ;")
                           (enumerate (λ (vid var)
                                       (cond
                                        [(list? var)
                                         (println #:port p "var" vid "en" nid " [label=\"" (car var) "\"] ;")
                                         (enumerate (λ (idx en2)
                                                      (println #:port p
                                                               "var" vid "en" nid " -> "
                                                               "en" (hash-ref (egraph-ens e) en2 -1)
							       "[tailport=" (if (= idx 0) "sw" "se")"] ;"))
                                                   (cdr var))]
                                        [else
                                         (println #:port p "var" vid "en" nid " [label=\"" var "\"] ;")])
                                       (println #:port p "en" nid " -> var" vid "en" nid " [arrowstyle=none] ;"))
                                    (enode-vars en))))
         (println #:port p "}"))))
