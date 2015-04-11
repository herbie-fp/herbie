#lang racket

(require "enode.rkt")
(require "../common.rkt")

(provide mk-enode! mk-egraph
	 merge-egraph-nodes!
	 egraph? egraph-cnt egraph-top
	 map-enodes draw-egraph egraph-leaders
         elim-enode-loops!
         )


;;################################################################################;;
;;# The mighty land of egraph, where the enodes reside for their entire lives.
;;# Egraphs keep track of how many enodes have been created, and keep a pointer
;;# to the top enode, defined as the enode through which all other enodes in the
;;# egraph can be reached by means of (enode-children) and (enode-expr). They
;;# also keep track of where each enode pack is referenced (expressions), and
;;# for each expression, which enode pack has it as a (enode-var).
;;#
;;# The following things should always be true of egraphs:
;;# 1. (egraph-cnt eg) is a positive integer.
;;# 2. (egraph-cnt eg) is equal to the sum of the size of each of the enode packs
;;#    whose leaders are kept as keys in leader->iexprs.
;;# 3. (egraph-top eg) is a valid enode.
;;# 4. For each enode en which is a key of leader->iexprs, en is the leader of
;;#    its own pack.
;;# 5. For every mapping (k, v) in leader->iexprs, for each expression e in v,
;;#    k is a member of (cdr e). That is, v is an expression that references k.
;;# 6. The set of values in leader->iexprs appended together is a subset of the
;;#    set of keys in expr->parent, when you consider enodes of the same pack
;;#    equal.
;;# 7. For every for any enode en, for every member of (enode-vars en) k, (k, v)
;;#    is in expr-parent.
;;# 8. For every mapping (k, v) in expr->parent, and every node referenced by k
;;#    is the leader of it's own pack.
;;# 9. No two enode packs have any vars in common, using the equal? definition of
;;#    equality.
;;#
;;#  Note: While the keys of leader->iexprs and the enodes referenced by the keys
;;#  of expr->parent are guaranteed to be leaders, the values of expr->parent,
;;#  and the enodes referenced by the values of leadders->iexprs are not.
;;#  This decision was made because it would require more state infrastructure
;;#  to update these values without having to make a pass over every mapping,
;;#  and since any member of a pack is equal? to any other member, it is not necessary
;;#  in the values of the map. For the keys however, it is necessary to update to
;;#  leaders, because if a key is changed, either because it was an enode that was
;;#  merged, or because it was an expression containing an enode that was changed,
;;#  it's hashing result will change, orphaning the key. Still, it is safest to
;;#  work with leaders whenever possible, so when aquiring a node as a value from
;;#  one of these maps, or through an enode-vars call, convert it to it's leader
;;#  through pack-leader before using it whenever possible.
;;#
;;################################################################################;;

(struct egraph (cnt top leader->iexprs expr->parent) #:mutable)

;; For debugging
(define (check-egraph-valid eg #:loc [location 'check-egraph-valid])
  (let ([leader->iexprs (egraph-leader->iexprs eg)]
	[count (egraph-cnt eg)])
    ;; The egraphs count must be a positive integer
    (assert (and (integer? count) (positive? count)) #:loc location)
    ;; The count is equal to the sum of the size of each of the enode packs.
    (assert (= count (apply + (map (compose length pack-members)
				   (hash-keys leader->iexprs))))
	    #:loc location)
    ;; The top is a valid enode. (enode validity is verified upon creation).
    (assert (enode? (egraph-top eg)) #:loc location)
    ;; Verify properties 4-6
    (hash-for-each leader->iexprs
		   (λ (leader iexprs)
		     (assert (eq? leader (pack-leader leader)) #:loc location)
		     (assert (set-mutable? iexprs) #:loc location)
		     (for ([iexpr iexprs])
		       (assert (list? iexpr) #:loc location)
		       (assert (member leader (cdr iexpr)) #:loc location)
		       (assert (hash-has-key? (egraph-expr->parent eg) iexpr) #:loc location))))
    ;; Verify property 7
    (hash-for-each (egraph-expr->parent eg)
		   (λ (k v)
		     ;; This line verifies that we didn't change the definition of hashing
		     ;; for some part of this expression without also refreshing the binding.
		     (assert (hash-has-key? (egraph-expr->parent eg) k) #:loc location)
		     
		     (when (list? k)
		       (for ([en (cdr k)])
			 (assert (eq? en (pack-leader en)) #:loc location)))))
    ;; Verify property 8
    (let loop ([seen (set)] [rest-leaders (hash-keys leader->iexprs)])
      (let ([cur-leader-vars (enode-vars (car rest-leaders))])
	(assert (set-empty? (set-intersect (set-copy-clear seen) cur-leader-vars)))
	(when (not (null? (cdr rest-leaders)))
	  (loop (set-union cur-leader-vars seen) (cdr rest-leaders)))))))

;; Takes an egraph, and an expression, as defined in enode.rkt, and returns
;; either a new enode that has been added to the graph, mutating the state of
;; of the graph to indicate the addition, or if the expression already exists
;; in the egraph it returns the node associated with it. While the node exists
;; after this call, if we are creating a new node it still must be merged into
;; an existing node or otherwise attached to the (egraph-top eg) node to be
;; completely added to the egraph.
(define (mk-enode! eg expr)
  (if (hash-has-key? (egraph-expr->parent eg) expr)
      (let ([res (hash-ref (egraph-expr->parent eg) expr)])
	res)
      (let* ([expr* (if (not (list? expr)) expr
			(cons (car expr)
			      (map pack-leader (cdr expr))))]
	     [en (new-enode expr* (egraph-cnt eg))]
	     [leader->iexprs (egraph-leader->iexprs eg)])
	(set-egraph-cnt! eg (add1 (egraph-cnt eg)))
	(hash-set! leader->iexprs en (mutable-set))
	(when (list? expr*)
	  (for ([suben (cdr expr*)])
	    (set-add! (hash-ref leader->iexprs (pack-leader suben))
		      expr*)))
	(hash-set! (egraph-expr->parent eg)
		   expr*
		   en)
	en)))

;; Takes a plain mathematical expression, quoted, and returns the egraph
;; representing that expression with no expansion or saturation.
(define (mk-egraph expr)
  (define (expr->enode eg expr)
    (if (list? expr)
	(mk-enode! eg
		   (cons (car expr)
			 (map (curry expr->enode eg)
			      (cdr expr))))
	(mk-enode! eg expr)))
  (let ([eg (egraph 0 #f (make-hash) (make-hash))])
    (set-egraph-top! eg (expr->enode eg expr))
    ;; This is an expensive check, but useful for debuggging.
    #;(check-egraph-valid eg #:loc 'constructing-egraph)
    eg))

;; Maps a given function over all the equivilency classes
;; of a given egraph (node packs).
(define (map-enodes f eg)
  (map f (egraph-leaders eg)))

;; Gets all the pack leaders in the egraph
(define (egraph-leaders eg)
  (hash-keys (egraph-leader->iexprs eg)))

;; Given an egraph and two enodes present in that egraph, merge
;; the packs of those two nodes, so that those nodes are equal?,
;; and return the same pack-leader and enode-vars (as well as enode-pid).
;; The keys of leader->iexprs and expr->parent are updated to refer
;; to the merged leader instead of the leaders of en1 and en2,
;; but the values of those mapping are not.
(define (merge-egraph-nodes! eg en1 en2)
  ;; Operate on the pack leaders in case we were passed a non-leader
  (let ([leader->iexprs (egraph-leader->iexprs eg)]
	[expr->parent (egraph-expr->parent eg)])
    (begin0
	(let inner-merge ([en1 en1] [en2 en2])
	  (let ([l1 (pack-leader en1)]
		[l2 (pack-leader en2)])
	    ;; If the leaders are the same, then these nodes are already
	    ;; part of the same pack, so do just return the leader.
	    (if (eq? l1 l2) l1
		(let* (;; Keep track of which expressions need to be updated
		       [changed-exprs
			(let ([iexprs1 (hash-ref leader->iexprs l1)]
			      [iexprs2 (hash-ref leader->iexprs l2)])
			  (set-union! iexprs1 iexprs2)
			  iexprs1)]
		       ;; Get the mappings, since they will be inaccessable after the merge.
		       [changed-mappings
			(for/list ([expr changed-exprs])
			  (cons expr (hash-ref expr->parent expr)))])
		  ;; Remove the old mappings from the table.
		  (for ([expr changed-exprs])
		    (hash-remove! expr->parent expr))
		  ;; Remove the old leader->iexprs mappings
		  (hash-remove! leader->iexprs l1)
		  (hash-remove! leader->iexprs l2)
		  (let* ([merged-en (enode-merge! l1 l2)])
		    ;; Add the new leader->iexprs mapping. We remove duplicates again,
		    ;; even though we already did that, because there might be things that
		    ;; weren't duplicates before we merged, but are now.
		    (hash-set! leader->iexprs merged-en (mutable-set-remove-duplicates changed-exprs))
		    ;; Add back the affected expr->parent mappings, with updated
		    ;; keys.
		    (define to-merge
		      (filter
		       identity
		       (for/list ([chmap changed-mappings])
			 (let* ([new-key
				 (let ([old-key (car chmap)])
				   (cons (car old-key)
					 (map (λ (en)
						(if (equal? en merged-en)
						    merged-en (pack-leader en)))
					      (cdr old-key))))]
				[new-val (pack-leader (cdr chmap))]
				[existing-val (hash-ref expr->parent new-key #f)])
			   (if existing-val
			       (cons new-val existing-val)
			       (begin
				 (hash-set! expr->parent
					    new-key
					    new-val)
				 #f))))))
		    ;; Merge all the nodes that this merge has made equivilent.
		    (for ([merge-pair to-merge])
		      (inner-merge (car merge-pair) (cdr merge-pair)))
		    merged-en))))))
    ;; Check to make sure we haven't corrupted the state.
    ;; This is an expensive check, but useful for debuggging.
    #;(check-egraph-valid eg #:loc 'merging)))

(define (mutable-set-remove-duplicates st)
  (list->mutable-set (set->list st)))

;; Eliminates looping paths in the egraph that contain en. Does not
;; work if there are other looping paths.
(define (elim-enode-loops! eg en)
  (let* ([variations (enode-vars en)]
	 ;; Keep track of the expressions that could be changed by our
	 ;; actions, and the enodes that contain those expressions so
	 ;; we can update them to point to the new leader.
         [changed-exprs (hash-ref (egraph-leader->iexprs eg) (pack-leader en))]
         [changed-nodes (for/list ([expr changed-exprs])
                          (hash-ref (egraph-expr->parent eg) expr))]
	 ;; Keep track of old state so it's easier to roll back.
	 [old-expr->parent (hash-copy (egraph-expr->parent eg))]
	 [old-leader->iexprs (hash-copy (egraph-leader->iexprs eg))])

    ;; Remove all affected expressions from our expr->parent table,
    ;; since we can't remove them once we've done the pack filter,
    ;; because their hashing code changes.
    (for ([ch-en changed-nodes])
      (for-pack!
       (λ (en)
	 (hash-remove! (egraph-expr->parent eg) (enode-expr en)))
       (pack-leader ch-en)))
    ;; Remove the old node as a leader from the leader table
    (hash-remove! (egraph-leader->iexprs eg) (pack-leader en))

    ;; Filter out any variations of the leader which loop back on
    ;; themselves, by removing those enodes from the pack. Keep track
    ;; of which enodes we filtered.
    (define filtered '())
    (define iexprs* '())
    (define leader*
      (pack-filter!
       (λ (en)
         (let ([loops? (and (list? (enode-expr en))
                            (for/or ([child (cdr (enode-expr en))])
                              (enode-subexpr? 1 en child)))])
           (when loops?
             (set! filtered (cons en filtered)))
           (not loops?)))
       (pack-leader en)))

    ;; If we didn't find any loops, skip the rest, and rollback our hash-remove!.
    (if (null? filtered)
	(begin (set-egraph-expr->parent! eg old-expr->parent)
	       (set-egraph-leader->iexprs! eg old-leader->iexprs)
	       #f)
	(begin
	  (for ([en filtered])
	    (for ([suben (cdr (enode-expr en))])
	      (when (hash-has-key? (egraph-leader->iexprs eg) suben)
		(set-subtract!
		 (hash-ref (egraph-leader->iexprs eg) suben)
		 (set (enode-expr en))))))

	  ;; Update anywhere the old enode appeared as a parent in
	  ;; expr->parent to point to the new leader.
	  (for ([expr variations])
	    (hash-set! (egraph-expr->parent eg) expr leader*))

	  ;; Loop through the expressions which contained our old leader, to update to new leader.
	  (for ([ch-en changed-nodes])
	    (for-pack!
	     (λ (en)
	       (let ([expr (enode-expr en)])
		 (when (list? expr)
		   (let ([expr* (cons (car expr)
				      (for/list ([child (cdr expr)])
					(if (member child filtered) leader* child)))])
		     ;; Keep track of all containing expressions.
		     (set! iexprs* (cons expr* iexprs*))
		     ;; Point them at the new leader, and also update their bodies if they contained the old leader.
		     (hash-set! (egraph-expr->parent eg) expr* en)
		     ;; Update the involved expressions set of all the children of this expression
		     (for ([child (cdr expr*)])
		       (hash-set! (egraph-leader->iexprs eg) (pack-leader child) (mutable-set expr*)))
		     (set-enode-expr! en expr*)))))
	     (pack-leader ch-en)))
	  ;; Update the set of involved expressions
	  (hash-set! (egraph-leader->iexprs eg) leader* (list->mutable-set iexprs*))
	  ;; Decrease the egraph count to account for the nodes that were removed
	  (set-egraph-cnt! eg (- (egraph-cnt eg) (length filtered)))
	  ;; Verify
	  #;(check-egraph-valid eg #:loc 'elimed-loops)
	  #t))))

;; Draws a representation of the egraph to the output file specified
;; in the DOT format.
(define (draw-egraph eg fp)
  (with-output-to-file
      fp #:exists 'replace
      (λ ()
	(displayln "digraph {")
	(for ([en (egraph-leaders eg)])
	  (let ([id (enode-pid en)])
	    (printf "node~a[label=\"NODE ~a\"]~n" id id)
	    (for ([varen (remove-duplicates (pack-members en) #:key enode-expr)]
		  [vid (in-naturals)])
	      (let ([var (enode-expr varen)])
		(printf "node~avar~a[label=\"~a\",shape=box,color=blue]~n"
			id vid (if (list? var) (car var) var))
		(printf "node~a -> node~avar~a[style=dashed]~n"
			id id vid)
		(cond
		 [(not (list? var)) (void)]
		 [(= (length var) 2)
		  (printf "node~avar~a -> node~a~n"
			  id vid (enode-pid (second var)))]
		 [(= (length var) 3)
		  (printf "node~avar~a -> node~a[tailport=sw]~n"
			  id vid (enode-pid (second var)))
		  (printf "node~avar~a -> node~a[tailport=se]~n"
			  id vid (enode-pid (third var)))])))))
	(displayln "}")))
  (system (format "dot -Tpng -o ~a.png ~a" fp fp)))
