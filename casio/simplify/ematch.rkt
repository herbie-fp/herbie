#lang racket

(require casio/simplify/util)
(require casio/simplify/enode)
(require casio/simplify/egraph)
(require casio/programs)

(provide match-e substitute-e)

;;################################################################################;;
;;# The matcher module that allows us to match enode structure against patterns,
;;# giving you the bindings between enodes and variables in the pattern, and
;;# substitute those bindings into patterns, creating new enodes.
;;#
;;# This is copy-pasted from the old egraphs file, with minor modifications,
;;# so it might not always work correctly.
;;#
;;################################################################################;;

(define (list-cartesian-product . lsts)
  (if (null? lsts)
      '(())
      (let ([tails (apply list-cartesian-product (cdr lsts))])
	(for*/list ([elt (car lsts)] [tail tails])
	  (cons elt tail)))))

(define (merge . bindings)
  ;; (list bindings) -> binding
  (foldl merge2 '() bindings))

(define (merge2 binding1 binding2)
  ;; binding binding -> binding
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
   [(constant? pat)
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

(define (substitute-e eg pat bindings #:victory? [victory #f])
  (cond
   [(constant? pat)
    (mk-enode! eg pat #:victory? victory)]
   [(symbol? pat)
    (let ([binden (cdr (assoc pat bindings))])
      ;; If we were set to victory, and this enode doesn't already have a victory
      ;; node in it's pack, set it to victory
      (when (and victory (not (pick-victory binden)))
	(set-enode-victory?! binden victory))
      binden)]
   [(list? pat)
    (mk-enode! eg (cons (car pat)
			(for/list ([subpat (cdr pat)])
			  (substitute-e eg subpat bindings)))
	       #:victory? victory)]))
