#lang racket

(require "../common.rkt")

(provide new-enode enode-merge!
	 enode-vars refresh-vars! enode-pid
	 enode?
	 enode-expr
	 pack-leader pack-members
	 rule-applied? rule-applied!
	 enode-override-expr!
	 enode-subexpr?)

;;################################################################################;;
;;# The mighty enode, one of the main lifeforms of this planet.
;;# Enodes are born solitary creatures, but can be merged to form a pack.
;;# Everytime two groups are merged, a leader is selected for the new pack
;;# from the leaders of the packs that were merged. A solitary
;;# enode is the leader of their own pack.
;;#
;;# The following things are always true of enode packs:
;;# 1. (enode-parent en) always returns either #f, or an enode that is
;;#    part of the same pack as en.
;;# 2. In each pack, there is exactly one enode for which (enode-parent en)
;;#    returns #f, and that enode is the leader of the pack.
;;# 3. The (enode-parent) pointers are non-cyclic, and always eventually
;;#    lead to the leader of the pack.
;;# 4. (enode-children en) is a list of enodes, for which each enode has a parent.
;;# 5. If en2 is the parent of en1, then en1 has the same expression as one of the
;;#    children of en2.
;;# 6. The enode-children links are also non-cyclic.
;;# 7. (enode-depth en) is a positive integer.
;;# 8. Solitary enodes have a depth of 1.
;;# 9. If en2 is the parent of en1, then (enode-depth en2) > (enode-depth en1)
;;# 10. If an enode is the leader of a pack of size n, then the depth of that
;;#     enode is at least log_2(n)
;;#
;;# The following things are true of enodes in general:
;;# 1. (enode-expr en) can take one of three forms:
;;#    a. a symbol
;;#    b. a number
;;#    c. a list starting with a symbol representing an operator,
;;#       and containing one or more enodes.
;;# 2. Each enode within a given context has a unique id-code.
;;#
;;################################################################################;;

(struct enode (expr id-code children parent depth cvars applied-rules)
	#:mutable
	#:methods gen:custom-write
	[(define (write-proc en port mode)
	   (fprintf port "#<enode ~a(~a)>" (enode-pid en) (enode-id-code en)))]
	#:methods gen:equal+hash
	[(define (equal-proc en1 en2 recurs-equal?)
	   (eq? (pack-leader en1) (pack-leader en2)))
         (define (hash-proc en recurse-hash)
	   (recurse-hash (enode-pid en)))
	 (define (hash2-proc en recurse-hash)
	   (enode-pid en))])

;; Creates a new enode. Keep in mind that this is egraph-blind,
;; and it should be wrapped in an egraph function for registering
;; with the egraph on creation.
(define (new-enode expr id-code)
  (let ([en* (enode expr id-code '() #f 1 (set expr) (mutable-set))])
    (check-valid-enode en* #:loc 'node-creation)
    en*))

;; Have one enode adopt the other as it's child.
;; To maintain invariants, the child should not be
;; deeper than the parent.
(define (adopt-enode! new-parent child)
  (assert (>= (enode-depth new-parent) (enode-depth child)))
  (assert (not (eq? new-parent child)))
  (set-enode-children! new-parent
		       (append (list child)
			       (enode-children child)
			       (enode-children new-parent)))
  (set-enode-parent! child new-parent)
  (when (<= (enode-depth new-parent) (enode-depth child))
    (set-enode-depth! new-parent (add1 (enode-depth new-parent))))
  (set-enode-cvars! new-parent (set-union (enode-cvars new-parent) (enode-cvars child)))
  (set-union! (enode-applied-rules new-parent)
	      (enode-applied-rules child))
  #;(map refresh-victory! (pack-members new-parent))
  ;; This is an expensive check, but useful for debuggging.
  #;(check-valid-parent child)
  #;(check-valid-children new-parent))

;; Merge two packs, given a node from either group.
;; Warning: Both this function and adopt-enode! change
;; the hashing and equality procedures for the enodes involved.
;; Therefore, be sure to update hash tables that use structures
;; containing these enodes as keys, and be careful about equality.
;; Returns the new leader.
(define (enode-merge! en1 en2)
  (let ([l1 (pack-leader en1)]
	[l2 (pack-leader en2)])
    (when (not (eq? l1 l2))
      (let-values ([(new-leader new-follower)
		    (if (< (enode-depth l1) (enode-depth l2))
			(values l2 l1)
			(values l1 l2))])
	(adopt-enode! new-leader new-follower)
	new-leader))))

;; Given an enode, override it's variations so that it only has one variation,
;; the given expression.
(define (enode-override-expr! en expr)
  (let ([leader (pack-leader en)])
    (set-enode-expr! leader en)
    (set-enode-cvars! leader (set expr))))

(define (check-valid-enode en #:loc [location 'check-valid-enode])
  ;; Checks that the enodes expr field is well formed.
  (let ([expr (enode-expr en)])
    (assert (or (number? expr) (symbol? expr)
		(and (list? expr) (symbol? (car expr))
		     (ormap enode? (cdr expr)))) #:loc location))
  ;; Checks that the depth is positive.
  (assert (positive? (enode-depth en)) #:loc location))

(define (check-valid-parent en #:loc [location 'check-valid-parent])
  (let ([parent (enode-parent en)])
    (when parent
      ;; Checks that we are one of our parents children.
      (assert (memf (compose (curry equal? (enode-expr en))
			     enode-expr)
		    (enode-children parent))
		    #:loc location)
      ;; Checks that the parents depth is greater than the childs.
      (assert (> (enode-depth parent) (enode-depth en)) #:loc location)
      ;; Checks that the parent links are non-cyclic
      (let check-parent-cycles ([seen (list en parent)] [cur-en parent])
	(let ([next-parent (enode-parent cur-en)])
	  (when next-parent
	    (assert (not (memq next-parent seen)) #:loc 'checking-for-parent-cycles)
	    (check-parent-cycles (cons next-parent seen) next-parent)))))))

(define (check-valid-children en #:loc [location 'check-valid-children])
  (let ([children (enode-children en)])
    (when (not (null? children))
      ;; Checks that all of our children have a parent.
      (assert (ormap enode-parent children) #:loc location)
      ;; Checks that our children have valid parent relations.
      (map check-valid-parent children)
      ;; Checks that the children links are non-cyclic
      (let check-children-cycles ([seen '()] [cur-en en])
	(assert (not (memq cur-en seen)) #:loc 'checking-for-parent-cycles)
	(map (curry check-children-cycles (cons cur-en seen))
	     (enode-children cur-en))))))

;; Gets the leader of the pack to which en belongs. A pack is defined as
;; the set of all enodes that have the same leader.
(define (pack-leader en)
  (let ([parent (enode-parent en)])
    (if (not parent) en
	(let ([leader (pack-leader parent)])
	  (set-enode-parent! en leader)
	  leader))))

;; Gets all the members of the pack that en is a part of.  The result
;; will contain at least one enode, en, and should contain every enode
;; en* for which (eq? (pack-leader en*) (pack-leader en)).
(define (pack-members en)
  (let ([l (pack-leader en)])
    (cons l (enode-children l))))

;; Returns the ENODE VARiationS, the different expressions
;; of the members of the given enodes pack.
(define (enode-vars en)
  (enode-cvars (pack-leader en)))

;; Removes duplicates from the varset of this node.
(define (refresh-vars! en)
  (set-enode-cvars! en (list->set (set->list (enode-cvars en)))))

;; Returns the pack ID of the pack of the given enode.
(define (enode-pid en)
  (enode-id-code (pack-leader en)))

;; Returns whether the given rule has already been applied to the given enode
(define (rule-applied? en rl)
  (set-member? (enode-applied-rules en) rl))

;; Marks the given enode as having the given rule applied to it.
(define (rule-applied! en rl)
 (set-add! (enode-applied-rules en) rl))

(define (check-valid-pack en #:loc [location 'check-valid-pack])
  (let ([members (pack-members en)])
    ;; Check that exactly one member of the pack is a leader.
    (assert (= 1 (length (filter (negate enode-parent) members))))
    ;; Check that all members of the pack report the same leader.
    (let check-all-equal ([leaders (map pack-leader members)])
      (when (< 1 (length leaders))
	(assert (eq? (car leaders) (cadr leaders)))
	(check-all-equal (cdr leaders))))
    ;; Check that the depth of the leader of any pack is at least log_2(n),
    ;; where n is the size of the pack.
    (assert (>= (enode-depth (pack-leader en))
		(/ (log (length members))
		   (log 2))))))

;; Searches up to a specified depth for needle-e occuring in the
;; children of haystack-e, and returns true if it can find it.
(define (enode-subexpr? depth needle-e haystack-e)
  (and (>= depth 0)
       (or (equal? haystack-e needle-e)
	   (ormap (curry enode-subexpr? (sub1 depth) needle-e)
		  (for/fold ([children '()])
		      ([var (enode-vars haystack-e)])
		    (append children (if (list? var) (cdr var) '())))))))


;; Given any enode, draws the pack structure containing that enode.
;; The blue node is the leader of the pack, and green lines indicate
;; the parent of a node, while all other lines indicate children.
(define (draw-pack en fp)
  (with-output-to-file
      fp #:exists 'replace
      (λ ()
	(displayln "digraph {")
	(let ([leader (pack-leader en)])
	  (printf "node~a[label = ~a, color=blue]~n" (enode-id-code leader) (enode-id-code leader))
	  (let draw-children ([cur-parent leader])
	    (for ([child (enode-children cur-parent)])
	      (let ([id (enode-id-code child)])
		(printf "node~a[label=~a]~n" id id)
		(printf "node~a -> node~a[color=green]~n" id (enode-id-code cur-parent))
		(printf "node~a -> node~a~n" (enode-id-code cur-parent) id)
		(draw-children child)))))
	(displayln "}"))))
