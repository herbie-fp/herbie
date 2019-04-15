#lang racket

(require "../common.rkt" "../syntax/syntax.rkt" "../syntax/types.rkt" "enode.rkt")

(provide mk-enode! mk-enode-rec! mk-egraph
	 merge-egraph-nodes!
	 egraph? egraph-cnt
	 draw-egraph egraph-leaders
         elim-enode-loops! reduce-to-single!
         )

(provide (all-defined-out)
	 (all-from-out "enode.rkt"))

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
;;# 3. For each enode en which is a key of leader->iexprs, en is the leader of
;;#    its own pack.
;;# 4. For every mapping (k, v) in leader->iexprs, for each expression e in v,
;;#    k is a member of (cdr e). That is, v is an expression that references k.
;;# 5. The set of values in leader->iexprs appended together is a subset of the
;;#    set of keys in expr->parent, when you consider enodes of the same pack
;;#    equal.
;;# 6. For every for any enode en, for every member of (enode-vars en) k, (k, v)
;;#    is in expr-parent.
;;# 7. For every mapping (k, v) in expr->parent, and every node referenced by k
;;#    is the leader of it's own pack.
;;# 8. No two enode packs have any vars in common, using the equal? definition of
;;#    equality.
;;#
;;#
;;#  Note: While the keys of leader->iexprs and the enodes referenced by the keys
;;#  of expr->parent are guaranteed to be leaders, the values of expr->parent,
;;#  and the enodes referenced by the values of leadders->iexprs are not.
;;#  This decision was made because it would require more state infrastructure
;;#  to update these values without having to make a pass over every mapping.
;;#
;;################################################################################;;

;; Only ever use leaders as keys!
(struct egraph (cnt leader->iexprs expr->parent) #:mutable)

;; For debugging
(define (check-egraph-valid eg #:loc [location 'check-egraph-valid])
  (let ([leader->iexprs (egraph-leader->iexprs eg)]
	[count (egraph-cnt eg)])
    (assert (not (hash-has-key? leader->iexprs #f)))
    ;; The egraphs count must be a positive integer
    (assert (and (integer? count) (positive? count)) #:loc location)

    ;; Verify properties 4-6
    (for ([(leader iexprs) (in-hash leader->iexprs)])
      (assert (eq? leader (pack-leader leader)) #:loc location)
      (assert (set-mutable? iexprs) #:loc location)
      (for ([iexpr iexprs])
	(assert (list? iexpr) #:loc location)
	(assert (for/or ([sub (cdr iexpr)])
                  (eq? (pack-leader sub) leader))
                #:loc location)
        (assert (for/and ([sub (cdr iexpr)])
                  (eq? (pack-leader sub) sub)))
	(assert (hash-has-key? (egraph-expr->parent eg) (update-en-expr iexpr)) #:loc location)))

    ;; Verify property 7
    (for ([(k v) (in-hash (egraph-expr->parent eg))])
      ;; This line verifies that we didn't change the definition of hashing
      ;; for some part of this expression without also refreshing the binding.
      (assert (hash-has-key? (egraph-expr->parent eg) k) #:loc location)
      (when (list? k)
	(for ([en (cdr k)])
	  (assert (eq? en (pack-leader en)) #:loc location))))

    ;; Verify property 8
    (let loop ([seen (set)] [rest-leaders (hash-keys leader->iexprs)])
      (let ([cur-leader-vars (enode-vars (car rest-leaders))])
	(assert (for/and ([var cur-leader-vars])
		  (or (value? var) (symbol? var) (list? var))))
	(assert (set-empty? (set-intersect (set-copy-clear seen) cur-leader-vars)))
	(when (not (null? (cdr rest-leaders)))
	  (loop (set-union cur-leader-vars seen) (cdr rest-leaders)))))))

;; Takes an egraph, and an expression, as defined in enode.rkt, and returns
;; either a new enode that has been added to the graph, mutating the state of
;; of the graph to indicate the addition, or if the expression already exists
;; in the egraph it returns the node associated with it. While the node exists
;; after this call, if we are creating a new node it still must be merged into
;; an existing node or otherwise attached to some node to be
;; completely added to the egraph.
(define (mk-enode! eg expr)
  (if (hash-has-key? (egraph-expr->parent eg) expr)
      (let ([res (hash-ref (egraph-expr->parent eg) expr)])
	(pack-leader res))
      (let* ([expr* (if (not (list? expr)) expr
			(cons (car expr)
			      (map pack-leader (cdr expr))))]
	     [en (new-enode expr* (egraph-cnt eg))]
	     [leader->iexprs (egraph-leader->iexprs eg)])
 	(set-egraph-cnt! eg (add1 (egraph-cnt eg)))
	(hash-set! leader->iexprs en (mutable-set))
	(when (list? expr*)
	  (for ([suben (in-list (cdr expr*))])
	    (set-add! (hash-ref leader->iexprs (pack-leader suben))
		      expr*)))
	(hash-set! (egraph-expr->parent eg)
		   expr*
		   en)
	en)))

(define (mk-enode-rec! eg expr)
  (match expr
    [(list op args ...)
     (mk-enode! eg (cons op (map (curry mk-enode-rec! eg) args)))]
    [_
     (mk-enode! eg expr)]))

;; Takes a plain mathematical expression, quoted, and returns the egraph
;; representing that expression with no expansion or saturation.
(define (mk-egraph)
  (egraph 0 (make-hash) (make-hash)))

;; Gets all the pack leaders in the egraph
(define (egraph-leaders eg)
  (hash-keys (egraph-leader->iexprs eg)))

(define (dedup-vars! en)
  (update-vars! (pack-leader en) update-en-expr)
  (dedup-children! en))

;; Given an egraph and two enodes present in that egraph, merge the
;; packs of those two nodes, so that those nodes return the same
;; pack-leader and enode-vars. The keys of leader->iexprs and
;; expr->parent are updated to refer to the merged leader instead of
;; the leaders of en1 and en2, but the values of those mapping are
;; not.
(define (merge-egraph-nodes! eg en1 en2)
  (match-define (egraph _ leader->iexprs expr->parent) eg)
  ;; Operate on the pack leaders in case we were passed a non-leader
  (define l1 (pack-leader en1))
  (define l2 (pack-leader en2))

  (cond
   [(eq? l1 l2)
    ;; If the leaders are the same, then these nodes are already part
    ;; of the same pack. However, this call usually means that two
    ;; vars of this leader were found equivalent through another
    ;; merge, so we want to update the vars to remove the redundancy.
    (dedup-vars! l1)]
   [else
    ;; Hold on to these vars as they won't be the same after the
    ;; merge, but we don't yet know which one we need.
    (define old-vars1 (enode-vars l1))
    (define old-vars2 (enode-vars l2))

    ;; Merge the node packs
    (define merged-en (enode-merge! l1 l2))

    ;; Now that we know which one became leader, we can bind these.
    (define-values (leader follower follower-old-vars)
      (if (eq? l1 merged-en)
          (values l1 l2 old-vars2)
          (values l2 l1 old-vars1)))

    ;; Get the expressions which mention the follower so we can see if
    ;; their new form causes new merges.
    (define iexprs (hash-ref leader->iexprs follower))

    ;; Once we've merged these enodes, other ones might have become
    ;; equivalent. For example, if we had an enode which had the
    ;; variation (+ x 1), and an enode which had the variation (+ y
    ;; 1), and we merged x and y, then we know that these two enodes
    ;; are equivalent, and should be merged.
    (define to-merge
      (for/append ([iexpr (in-mutable-set iexprs)])
        (define replaced-iexpr (update-en-expr iexpr))
        (define other-parent (hash-ref expr->parent replaced-iexpr #f))
        (if other-parent
            (list (cons other-parent (hash-ref expr->parent iexpr)))
            '())))

    ;; Now that we have extracted all the information we need from the
    ;; egraph maps in their current state, we are ready to update
    ;; them. We need to know which one is the old leader, and which is
    ;; the new to easily do this, so we branch on which one is eq? to
    ;; merged-en.
    (update-leader! eg follower-old-vars follower leader)

    ;; Now the state is consistent for this merge, so we can tackle
    ;; the other merges.
    (for ([node-pair (in-list to-merge)])
      (merge-egraph-nodes! eg (car node-pair) (cdr node-pair)))

    ;; The other merges can have caused new things to merge with our
    ;; merged-en from before (due to loops in the egraph), so we turn
    ;; this into a leader before finally returning it.
    (pack-leader merged-en)]))

(define (update-en-expr expr)
  (if (list? expr)
      (for/list ([sub (in-list expr)])
        (if (enode? sub) (pack-leader sub) sub))
      expr))

(define (update-leader! eg old-vars old-leader new-leader)
  (when (not (eq? old-leader new-leader))
    (let* ([changed-exprs (hash-ref (egraph-leader->iexprs eg) old-leader)])
      (set-union! (hash-ref! (egraph-leader->iexprs eg) new-leader (mutable-set))
                  changed-exprs)
      (for ([ch-expr (in-mutable-set changed-exprs)])
        (for ([suben (in-list (cdr ch-expr))])
          (hash-update! (egraph-leader->iexprs eg) (pack-leader suben)
                        (λ (st)
                          (for/mutable-set ([expr (in-mutable-set st)])
                            (update-en-expr expr)))))
        (let ([old-binding (hash-ref (egraph-expr->parent eg) ch-expr)])
          (hash-remove! (egraph-expr->parent eg) ch-expr)
          (hash-set! (egraph-expr->parent eg) (update-en-expr ch-expr) (update-en-expr old-binding))))
      (hash-remove! (egraph-leader->iexprs eg) old-leader)
      (for ([variation (in-set old-vars)])
        (hash-set! (egraph-expr->parent eg)
                   (update-en-expr variation)
                   new-leader)))))

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

    ;; Filter out the first variation of the leader which loops back on
    ;; itself, by removing that enode from the pack. Keep track
    ;; of which enode we filtered.
    (define filtered #f)
    (define iexprs* '())
    (define leader*
      (pack-removef!
       (λ (en)
         (let ([loops? (and (list? (enode-expr en))
                            (for/or ([child (cdr (enode-expr en))])
                              (enode-subexpr? 1 en child)))])
           (when loops?
             (set! filtered en))
           loops?))
       (pack-leader en)))

    ;; If we didn't find any loops, skip the rest, and rollback our hash-remove!.
    (if (not filtered)
	(begin (set-egraph-expr->parent! eg old-expr->parent)
	       (set-egraph-leader->iexprs! eg old-leader->iexprs)
	       #f)
	(begin
	  (for ([suben (cdr (enode-expr filtered))])
	    (when (hash-has-key? (egraph-leader->iexprs eg) suben)
	      (set-subtract!
	       (hash-ref (egraph-leader->iexprs eg) suben)
	       (set (enode-expr filtered)))))

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
					(if (equal? child filtered) leader* child)))])
		     ;; Keep track of all containing expressions.
		     (set! iexprs* (cons expr* iexprs*))
		     ;; Point them at the new leader, and also update
		     ;; their bodies if they contained the old leader.
		     (hash-set! (egraph-expr->parent eg) expr* en)
		     ;; Update the involved expressions set of all the
		     ;; children of this expression
		     (for ([child (cdr expr*)])
		       (hash-set! (egraph-leader->iexprs eg) (pack-leader child) (mutable-set expr*)))
		     (set-enode-expr! en expr*)))))
	     (pack-leader ch-en)))
	  ;; Update the set of involved expressions
	  (hash-set! (egraph-leader->iexprs eg) leader* (list->mutable-set iexprs*))
	  ;; Decrease the egraph count to account for the nodes that were removed
	  ;; Verify
	  (check-egraph-valid eg #:loc 'elimed-loops)
	  #t))))

;; If there are any variations of this enode that are a single
;; constant or variable, prune to that.
(define (reduce-to-single! eg en)
  (when (for/or ([var (in-set (enode-vars en))])
	  (or (constant-or-num? var) (herbie-variable? var)))
    (let* ([leader (pack-leader en)]
           [old-vars (for/mutable-set ([var (in-set (enode-vars leader))])
                       (update-en-expr var))]
           [leader* (pack-filter! (λ (inner-en)
                                    (not (list? (enode-expr inner-en))))
                                  leader)])
      (when (not (eq? leader leader*))
        (update-leader! eg old-vars leader leader*)))))

;; Draws a representation of the egraph to the output file specified
;; in the DOT format.
(define (draw-egraph eg fp)
  (with-output-to-file
      fp #:exists 'replace
      (λ ()
	(printf "digraph {\n")
	(for ([en (egraph-leaders eg)])
          (define id (enode-pid en))

	  (printf "node~a[label=\"NODE ~a\"]\n" id id)
	  (for ([varen (pack-members en)] [vid (in-naturals)])
            (define var (enode-expr varen))
	    (printf "node~avar~a[label=\"~a\",shape=box,color=blue]\n"
		    id vid (if (list? var) (car var) var))
	    (printf "node~a -> node~avar~a[style=dashed]\n"
		    id id vid)
            (when (list? var)
              (define n (length (cdr var)))
              (for ([arg (cdr var)] [i (in-naturals)])
	        (printf "node~avar~a -> node~a[tailport=~a]\n"
                        id vid
                        (enode-pid arg)
                        (cond
                          [(= i 0) "sw"]
                          [(= i (- n 1)) "se"]
                          [else "s"])
                        )))))
	(printf "}\n")))
  (system (format "dot -Tpng -o ~a.png ~a" fp fp)))
