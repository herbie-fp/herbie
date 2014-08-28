#lang racket

(require casio/rules)
(require casio/common)
(require racket/set)

(define prog '(- (sqrt (+ x 1)) (sqrt x)))

(define *eg* #f)

(provide (struct-out egraph) enode-vars mk-egraph iterate-egraph
	 graphviz-e get-rule)
(provide (all-defined-out))

(struct enode (children parent expr) #:mutable)
(struct egraph (top exprs ens cnt) #:mutable)

(define (mk-enode eg expr)
  (if (enode? expr)
      (error "!!!")
      (let ([en (hash-ref (egraph-exprs eg) expr #f)])
        (if en
            en
            (let ([en* (enode '() #f expr)])
              (hash-set! (egraph-exprs eg) expr en*)
              (hash-set! (egraph-ens eg) en* (egraph-cnt eg))
              (set-egraph-cnt! eg (+ (egraph-cnt eg) 1))
              en*)))))

(define (enode-rep en)
  (let ([parent (enode-parent en)])
    (if (not parent)
	en
	(let ([ancestor (let get-ancestor ([cur en])
			  (if (enode-parent cur)
			      (get-ancestor (enode-parent cur))
			      cur))])
	  (set-enode-parent en ancestor)
	  ancestor))))

(define (enode-sisters en)
  (enode-children (enode-rep en)))

(define (enode-vars en)
  (map enode-expr (enode-sisters en)))

(define (enode-merge! en1 en2)
  (let ([en-parent (enode (list* en1 en2 (append (enode-children en1) (enode-children en2))) #f #f)])
    (set-enode-parent! en1 en-parent)
    (set-enode-parent! en2 en-parent)
    en-parent))

(define (egraph-replace! eg enfind enreplace)
  (check-egraph eg #:mesg "Before replace")
  (when (not (eq? enfind enreplace))
    (when (eq? (egraph-top eg) enfind)
      (set-egraph-top! eg enreplace))
    (for-egraph eg
     (λ (en)
       (set-enode-vars!
	en
	(for/list ([var (enode-vars en)])
	  (if (list? var)
	      (list* (car var)
		     (map (λ (en)
			    (if (eq? en enfind)
				enreplace en))
			  (cdr var)))
	      var)))))
    (let ([exprs (egraph-exprs eg)])
      (for ([expr (hash-keys exprs)])
	(println "expr: " (expr->string expr))
	(let ([nd (hash-ref exprs expr)])
	  (when (eq? nd enfind)
	    (hash-set! exprs expr enreplace)
	    (set! nd enreplace))
	  (when (list? expr) #;(and (list? expr) (ormap (curry eq? enfind) (cdr expr)))
	    (println "hi " (expr->string expr))
	    (hash-remove! exprs expr)
	    (hash-set! exprs
		       (cons
			(car expr)
			(for/list ([en (cdr expr)])
			  (if (eq? en enfind)
			      enreplace
			      en)))
		       nd)))))
    (println "enfind: " (node->string enfind))
    (hash-remove! (egraph-ens eg) enfind)
    (check-egraph eg #:mesg "After replace")))

(define (node->string en)
  (with-output-to-string
    (λ _
      (display "#<enode ")
      (write (hash-ref (egraph-ens *eg*) en #f))
      (display ">"))))

(define (expr->string expr)
  (if (list? expr)
      (~a (cons (car expr)
		(map node->string
		     (cdr expr))))
      (~a expr)))
  
(define (enode-merge! eg enfrom eninto)
  (check-egraph eg #:mesg "Before set")
  (set-enode-vars!
   eninto
   ;; This ordering is important: We're putting the enfrom vars first
   ;; because it is usually the shorter route and thus the one we want to pick.
   (remove-duplicates (append (enode-vars enfrom)
			      (enode-vars eninto))))
  (check-egraph eg #:mesg "After set")
  (egraph-replace! eg enfrom eninto)
  (check-egraph eg #:mesg "After merge"))

(define (mk-egraph p)
  (define (go eg expr)
    (if (list? expr)
        (mk-enode eg (cons (car expr) (map (curry go eg) (cdr expr))))
        (mk-enode eg expr)))

  (let* ([eg (egraph #f (make-hash) (make-hasheq) 0)]
         [en (go eg p)])
    (set-egraph-top! eg en)
    (set! *eg* eg)
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
	    (enode-merge!
	     eg (mk-enode eg var) en))

(define (one-iter eg rules)
  (let ([start-size (egraph-cnt eg)])
    (for ([r rules])
      (for-egraph eg (λ (en)
		       (when (not (eq? eg *eg*))
			 (error "---"))
		       (when (not (in-table en))
			 (error "+++"))
		       (at-node eg r en))))
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

(define (in-table en)
  (hash-ref (egraph-ens *eg*) en #f))

(define (check-egraph eg #:mesg [message ""] #:die [die? #t])
  (define (enode-children en)
    (apply append
	   (for/list ([var (enode-vars en)])
	     (if (list? var)
		 (cdr var)
		 '()))))
  
  (define (fail . mesgs)
    (let ([msg (apply string-append
		      message
		      "> "
		      (for/list ([msg mesgs])
			(if (string? msg)
			    msg
			    (~a msg))))])
      (if die?
	  (error msg) (eprintf msg))))
  
  (let check-node ([seen '()] [en (egraph-top eg)])
    (when (not (in-table en))
      (fail "Found a node reachable from head that isn't in the egraph node table!"
	    " Seen: "
	    (apply append
		   (for/list ([s seen])
		     (~a " " (hash-ref (egraph-ens eg) s #f)))))
    (when (not (member en seen))
      (map (curry check-node (cons en seen)) (enode-children en))))
    (hash-map (egraph-exprs eg)
	      (λ (expr nd)
		(when (not (in-table nd))
		  (fail
		   "Expression " (expr->string expr)
		   " is associated with a node that is not in the table, "
		   (node->string nd)))
		(when (list? expr)
		  (for ([subnd (cdr expr)])
		    (when (not (in-table subnd))
		      (fail
		       "Part of expression " (expr->string expr)
		       " is not in the table!"))))))))
