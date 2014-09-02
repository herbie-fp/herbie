#lang racket

(require casio/rules)
(require casio/common)
(require racket/set)

(define prog '(- (sqrt (+ x 1)) (sqrt x)))

(provide (struct-out egraph) enode-vars mk-egraph iterate-egraph
	 graphviz-e get-rule)
(provide (all-defined-out))

(struct enode (children parent depth expr id-code) #:mutable
	#:methods gen:custom-write
	[(define (write-proc en port mode)
	   (display "#<enode " port)
	   (write (enode-id en) port)
	   (display " (" port)
	   (write (enode-id-code en) port)
	   (display ")>" port))]
	#:methods
	gen:equal+hash
	[(define (equal-proc a b equal?-recur)
	   (eq? (enode-rep a)
		(enode-rep b)))
	 (define (hash-proc a hash-recur)
	   (enode-id a))
	 (define (hash2-proc a hash-recur)
	   (hash-recur (enode-id a)))])

(struct egraph (top exprs repnds cnt) #:mutable)

(define (enode-id en)
  (enode-id-code (enode-rep en)))

(define (mk-enode eg expr)
  (if (enode? expr)
      (error "!!!")
      (let ([en (hash-ref (egraph-exprs eg) expr #f)])
        (if en
            en
            (let ([en* (enode '() #f 1 expr (egraph-cnt eg))])
              (hash-set! (egraph-exprs eg) expr en*)
	      (set-egraph-repnds! eg (cons en* (egraph-repnds eg)))
              (set-egraph-cnt! eg (add1 (egraph-cnt eg)))
              en*)))))

(define (enode-rep en)
  (let ([parent (enode-parent en)])
    (if (not parent)
	en
	(let ([ancestor (enode-rep parent)])
	  (set-enode-parent! en ancestor)
	  ancestor))))

(define (enode-sisters en)
  (let ([pc (enode-children (enode-rep en))])
    (list* (enode-rep en) pc)))

(define (enode-vars en)
  (map enode-expr (enode-sisters en)))

(define (enode-merge! eg en1 en2)
  (when (not (equal? en1 en2))
    (let-values ([(new-rep other-rep) (if (< (enode-depth en1) (enode-depth en2))
					  (values en2 en1)
					  (values en1 en2))])
      (set-egraph-repnds! eg (remq other-rep (egraph-repnds eg)))
      (set-enode-parent! other-rep new-rep)
      (set-enode-children! new-rep (append (list other-rep) (enode-children new-rep) (enode-children other-rep)))
      (when (= (enode-depth en1) (enode-depth en2))
	(set-enode-depth! new-rep (add1 (enode-depth new-rep)))))))

(define (mk-egraph p)
  (define (go eg expr)
    (if (list? expr)
        (mk-enode eg (cons (car expr) (map (curry go eg) (cdr expr))))
        (mk-enode eg expr)))

  (let* ([eg (egraph #f (make-hash) '() 0)]
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
        (if (and (list? var) (equal? (car var) (car pat))
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
  (map f (egraph-repnds eg)))

(define (at-node eg r en)
  (let ([binds (match-e (rule-input r) en)])
    (for ([bind binds])
      (let ([var (substitute-e eg (rule-output r) bind)])
	(if (enode? var)
	    (enode-merge! eg en var)
	    (enode-merge! eg (mk-enode eg var) en))))))

(define (one-iter eg rules)
  (let ([start-size (egraph-cnt eg)])
    (for ([r rules])
      (for-egraph eg (λ (en) (at-node eg r en))))
    #;(> (egraph-cnt eg) start-size)))

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
	 (for-egraph
	  e
	  (λ (en)
	    (let ([nid (enode-id en)])
	      (println #:port p "en" nid " [label=\"NODE\"] ;")
	      (enumerate (λ (vid var)
			   (cond
			    [(list? var)
			     (println #:port p "var" vid "en" nid " [label=\"" (car var) "\"] ;")
			     (enumerate (λ (idx en2)
					  (println #:port p
						   "var" vid "en" nid " -> "
						   "en" (enode-id en2)
						   "[tailport=" (if (= idx 0) "sw" "se")", arrowstyle=dot] ;"))
					(cdr var))]
			    [else
			     (println #:port p "var" vid "en" nid " [label=\"" var "\"] ;")])
			   (println #:port p "en" nid " -> var" vid "en" nid " [arrowstyle=none] ;"))
			 (enode-vars en)))))
         (println #:port p "}"))))

;; (define (check-egraph eg #:mesg [message ""] #:die [die? #t])
;;   (define (enode-children en)
;;     (apply append
;; 	   (for/list ([var (enode-vars en)])
;; 	     (if (list? var)
;; 		 (cdr var)
;; 		 '()))))
  
;;   (define (fail . mesgs)
;;     (let ([msg (apply string-append
;; 		      message
;; 		      "> "
;; 		      (for/list ([msg mesgs])
;; 			(if (string? msg)
;; 			    msg
;; 			    (~a msg))))])
;;       (if die?
;; 	  (error msg) (eprintf msg))))
  
;;   (let check-node ([seen '()] [en (egraph-top eg)])
;;     (when (not (in-table en))
;;       (fail "Found a node reachable from head that isn't in the egraph node table!"
;; 	    " Seen: "
;; 	    (apply append
;; 		   (for/list ([s seen])
;; 		     (~a " " (hash-ref (egraph-ens eg) s #f)))))
;;     (when (not (member en seen))
;;       (map (curry check-node (cons en seen)) (enode-children en))))
;;     (hash-map (egraph-exprs eg)
;; 	      (λ (expr nd)
;; 		(when (not (in-table nd))
;; 		  (fail
;; 		   "Expression " (expr->string expr)
;; 		   " is associated with a node that is not in the table, "
;; 		   (node->string nd)))
;; 		(when (list? expr)
;; 		  (for ([subnd (cdr expr)])
;; 		    (when (not (in-table subnd))
;; 		      (fail
;; 		       "Part of expression " (expr->string expr)
;; 		       " is not in the table!"))))))))
