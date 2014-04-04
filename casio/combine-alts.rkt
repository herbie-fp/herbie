#lang racket
(require casio/alternative)
(require casio/programs)
(require casio/rules)
(require casio/points)
(require casio/common)
(require casio/redgreen)

(provide (all-defined-out))

;; This value is entirely arbitrary and should probably be changed,
;; before it destroys something.
(define *branch-cost* 5)

;; Depreceated, but kept around for testing and reference
(define (combine-two-alts var-index alt0 alt1 #:pre-combo-func [f identity])
  (let* ([vars (program-variables (alt-program alt0))]
	 [split-var (list-ref vars var-index)]
	 [condition (get-condition (sort (get-splitpoints alt0 alt1 var-index) <)
				   split-var)])
    (let-values ([(points0 points1) (partition (compose (eval `(lambda (,split-var) ,condition))
							(curry (flip-args list-ref) var-index))
					       (*points*))])
    `(lambda ,vars
       (if ,condition
	   ,(program-body (alt-program (parameterize [(*points* points0) (*exacts* (make-exacts (alt-program alt0) points0))] (f alt0))))
	   ,(program-body (alt-program (parameterize [(*points* points1) (*exacts* (make-exacts (alt-program alt1) points1))] (f alt1)))))))))

(define (best-combination alts #:pre-combo-func [f identity])
  (let* ([var-indices (build-list (length (program-variables (alt-program (car alts)))))]
	 [all-options (apply append
			     (map (lambda (var-index)
				    (map-pairs (curry make-option
						      var-index)
					       alts))
				  var-indices))]
	 [best-option (best all-options (compose (curry > 0) errors-diff-score))])
    (option->alt best-option f)))

(define (option->alt opt pre-combo-func)
  (define (apply-with-points points altn)
    (parameterize [(*points* points)
		   (*exacts* (make-exacts (alt-program altn) points))]
      (pre-combo-func altn)))
  (let ([split-var (option-split-var opt)]
	[condition (option-condition opt)]
	[split-var-index (option-split-var-index opt)]
	[vars (alt-program (option-altn1 opt))])
    (let-values ([(points1 points2) (partition (compose (eval `(lambda (,split-var) ,condition))
							(curry (flip-args list-ref) split-var-index))
					       (*points*))])
      (let ([altn1* (apply-with-points points1 (option-altn1 opt))]
	    [altn2* (apply-with-points points2 (option-altn2 opt))])
	(let ([program `(lambda ,vars
			  (if ,condition
			      (program-body (alt-program altn1*))
			      (program-body (alt-program altn2*))))]
	      [errs (option-errors opt)]
	      [cost (+ *branch-cost* (max (alt-cost altn1*) (alt-cost altn2*)))])
	  (alt program errs cost #f #f))))))

(define (best lst item<?)
  (let loop ([best-item (car lst)] [rest (cdr lst)])
    (if (null? rest) best-item
	(if (item<? best-item (car rest))
	    (loop (car rest) (cdr rest))
	    (loop best-item (cdr rest))))))

(struct option (altn1 altn2 condition errors split-var split-var-index) #:transparent
	#:methods gen:custom-write
	[(define (write-proc opt port mode)
	   (display "#<option " port)
	   (write (alt-program (option-altn1 opt)) port)
	   (display ", " port)
	   (write (alt-program (option-altn2 opt)) port)
	   (display ">" port))])

(define (make-option var-index altn1 altn2)
  (let* ([vars (program-variables (alt-program altn1))]
	 [split-var (list-ref vars var-index)]
	 [condition (get-condition (sort (get-splitpoints altn1 altn2 var-index) <)
				  split-var)]
	 [condition-func (eval `(lambda (,split-var) ,condition))]
	 [errors (map (lambda (error1 error2 point) (if (condition-func (list-ref point var-index)) error1 error2))
				 (alt-errors altn1)
				 (alt-errors altn2)
				 *points*)])
    (option altn1 altn2 condition errors split-var var-index)))

;; Maps the given f across every unique, unordered pair of elements of lst.
(define (map-pairs f lst)
  (let loop ([rest lst] [acc '()])
    (if (null? (cdr rest))
	acc
	(loop (cdr rest) (append (map (curry f (car rest)) (cdr rest)) acc)))))

(define (get-condition splitpoints var)
  (if (nan? (car splitpoints))
      (list 'not (get-condition (cdr splitpoints) var))
      (let ([conditions (cons `(> ,(car splitpoints) ,var)
			      (let loop ([rest (cdr splitpoints)] [conds '()])
				(cond [(null? rest) conds]
				      [(null? (cdr rest)) (cons `(< ,(car rest) ,var) conds)]
				      [#t (loop (cddr rest) (cons `(and (< ,(car rest) ,var)
									(> ,(cadr rest) ,var))
								   conds))])))])
	(if (< 1 (length conditions))
	    (cons 'or conditions)
	    (car conditions)))))

;; Given a list in point order (small-positive to large-positive, then small-negative to large-negative),
;; Reorder it into ascending order (large-negative to small-negative, small-positive to large-positive).
(define (ascending-order var-index l)
  (let* ([num-positives (length (filter (compose positive? (curry (flip-args list-ref) var-index)) (*points*)))]
	 [positives (take l num-positives)]
	 [negatives (drop l num-positives)])
    (append (reverse negatives) positives)))

(define (get-splitpoints alt1 alt2 arg-index #:max-splitpoints [max-splits 4])
  (let* ([difflist (errors-compare (ascending-order arg-index (alt-errors alt1))
				   (ascending-order arg-index (alt-errors alt2)))]
	 [sindices (difflist->splitindices difflist #:max-splitpoints max-splits)]
	 [ascending-points (ascending-order arg-index (*points*))])
    (map (lambda (i)
	   (cond [(= 0 i) +nan.0]
		 [(eq? '= (list-ref difflist i))
		  (list-ref (list-ref ascending-points i) arg-index)]
		 [(eq? '= (list-ref difflist (- i 1)))
		  (list-ref (list-ref ascending-points (- i 1)) arg-index)]
		 [#t (let ([p1 (list-ref (list-ref ascending-points i) arg-index)]
			   [p2 (list-ref (list-ref ascending-points (- i 1)) arg-index)]
			   [pred (compose (curry eq? (list-ref difflist i)) ;;Is it the same sign as the first point?
						    (lambda (p) ;; Get the sign of the given point
						      (let ([points (list (list p))])
							(errors-compare (let ([prog (alt-program alt1)])
									  (errors prog points (make-exacts prog points)))
									(let ([prog (alt-program alt2)])
									  (errors prog points (make-exacts prog points)))))))])
		       (binary-search-floats pred p1 p2 (/ (- p1 p2) 200)))]))
	 sindices)))
					   
									      
		 
;; Given two points, the first of which is pred, and the second is not,
;; finds the point where pred becomes false, by calling split to binary
;; search the space until (split a b) returns a or b.
(define (binary-search split pred p1 p2)
  (if (= p1 p2) p1
      (let ([midpoint (split p1 p2)])
	(cond [(not midpoint) p1]
	      [(or (= p1 midpoint) (= p2 midpoint)) midpoint]
	      [(pred midpoint) (binary-search split pred midpoint p2)]
	      [#t (binary-search split pred p1 midpoint)]))))

(define (flip-args f) (lambda (x y) (f y x)))

;; Given two floating point numbers, the first of which is pred,
;; and the second is not, find where pred becomes false (within epsilon).
(define (binary-search-floats pred p1 p2 epsilon)
  (define (close-enough a b) (> epsilon (abs (- a b))))
  (binary-search (lambda (a b) (if (close-enough a b) #f
				   (/ (+ a b) 2)))
		 pred p1 p2))

;; Implemented here for example.
(define binary-search-ints (curry binary-search (compose floor (compose (curry (flip-args /) 2) +))))

;; Gets the indices to split a region into. By default the only requirement of these regions is that they be the most accurate
;; regions where no region is less than three points in size, but you can pass in a minimum region size (default three), a
;; maximum number of splitindices, or a function that takes a single argument, a list of regions, and determines whether these
;; regions are general enough. 
(define (difflist->splitindices difflist #:min-region-size [min-size 3] #:max-splitpoints [max-splits +inf.0] #:fitness-func [fit? (const #t)])
  ;; First, convert the difflist into a list of regions, and swallow any regions less than the minimum size.
  ;; Then, keep increasing the minimum size and swallowing until we have no more than max-splits splitpoints,
  ;; and we return true on our fitness function. Finally, swallow all the equals.
  (let loop ([regions (swallow-regions (compose (curry > min-size) car)
				       (difflist->regions difflist))]
	     [new-min-size (+ 1 min-size)])
    ;; If our number of regions that are not equals is less than or equal to the maximum allowed
    ;; (one more than the maximum number of splitpoints), and our fitness function returns true,
    ;; then we're done, and we should swallow the equals and return.
    (if (and (<= (length (filter (compose (compose not (curry eq? '=)) cdr) regions)) (+ 1 max-splits)) (fit? regions))
	(regions->splitindices (swallow-regions (compose (curry eq? '=) cdr) regions))
	;; Otherwise swallow any less than our new min size, and recurse on a bigger minsize.
	(loop (swallow-regions (compose (curry > new-min-size) car) regions) (+ 1 new-min-size)))))

;; Given a list of desired regions, returns the indices in the difflist to split at.
;; If the initial region should be of alt2, we start with an index of zero, so the
;; splitindices can be read as, starting with alt1, at every splitindex, switch alts.
;; For example: an output of (4 8 12) would indicate that alt1 should be used from 0 to 4,
;; and 8 to 12, and alt2 should be used from 4 to 8, and from 12 onwards. An output of
;; (0 6 8 10) would indicate that alt2 should be used from 0 to 6, and 8 to 10, and alt1
;; should be used from 6 to 8, and from 10 onwards.
(define (regions->splitindices regions)
  ;; First construct the splitindices with the leading zero.
  ;; We construct the splitindices by folding across the regions,
  ;; and for each region add it's size to the previous splitindex to
  ;; get the next splitindex.
  ;; We pull off the last splitindex since it would just be the size of the difflist.
  (let ([with-zero (reverse (cdr (foldl (lambda (reg acc)
					  (cons (+ (car reg) (car acc)) acc))
					'(0) regions)))])
    ;; If the first region should be alt2, keep the leading zero
    (if (eq? '< (cdar regions))
	with-zero
	;; Otherwise, discard the leading zero.
	(cdr with-zero))))

;; Given a difflist, return a list of the regions in that difflist, in the
;; form of a regionlist.
;; A difflist is defined as a list composed of the symbols <, =, and >,
;; indicating whether at each of a set of points, alt1 is better than alt2, worse,
;; or the same.
;; A regionlist is defined as a list composed of regions, where each region
;; indicates a set of consecutive symbols in a difflist, and is of the form
;; (size . diff-symbol).
(define (difflist->regions difflist)
  ;; Loop across the difflist, keeping track of the size of the current region we're
  ;; building and the diffsymbol of that region, and accumulating the results in acc.
  (let loop ([restlist difflist] [cur-region-size 0] [cur-region (car difflist)] [acc '()])
    ;; If we have no more elements left, add the current region we've been working on
    ;; to our accumulator, and then reverse it (since it builds backwards), and return it.
    (cond [(null? restlist)
	   (reverse (cons (cons cur-region-size cur-region) acc))]
	  ;; If the next element is the same as the region we're working on, increment
	  ;; the current region size, and recurse on the rest of the list.
	  [(eq? (car restlist) cur-region)
	   (loop (cdr restlist) (+ cur-region-size 1) cur-region acc)]
	  ;; Otherwise, we've hit the end of our current region, so add the current region to
	  ;; the accumulator, set our new region to the symbol of the current element, set
	  ;; our size to one, and recurse.
	  [#t (loop (cdr restlist) 1 (car restlist) (cons (cons cur-region-size cur-region) acc))])))

;; Returns a regionlist that is the result of all regions in 'regions'
;; that return true to 'pred' being "swallowed" by the adjacent regions.
;; Swallowing follows these rules:
;; 1. A regions will always be swallowed by an adjacent region
;; 2. If one neighbor is of type =, and the other is not, then the neighbor
;;    that is not of type = will swallow the region.
;; 3. If a region to be swallowed is on either end of the regionlist, it will
;;    be swallowed by it's existing neighbor.
;; 4. Otherwise, regions will be swallowed by the region that comes after them.
;; 5. Regions are swallowed from the beginning of the list to the end.
;; 6. The result of one regions swallowing another has the type of the swallowing
;;    region, unless that type is =, in which case it has the type of the swallowed
;;    region.
;; Finally, note that behavior is undefined if a region which does not satisfy
;; pred can satisfy pred after swallowing another region which does satisfy pred.
(define (swallow-regions pred regions)
  ;; Returns the result of reg2 "swallowing" reg1.
  ;; The result has a size of (+ (size reg1) (size reg2)),
  ;; and a type of (type reg2), unless (eq? '= (type reg2)),
  ;; in which case it has a type of (type reg1).
  (define (merge-into reg1 reg2)
    (cons (+ (car reg1) (car reg2)) (if (eq? '= (cdr reg2))
					(cdr reg1)
					(cdr reg2))))
  ;; Returns the result of merging all adjacent regions
  ;; that have the same type.
  (define (merge-adjacent-regions regions)
    (reverse (foldl (lambda (reg acc)
		      (cond [(null? acc) (cons reg acc)]
			    [(eq? (cdr reg) (cdr (car acc)))
			     (cons (merge-into reg (car acc)) (cdr acc))]
			    [#t (cons reg acc)]))
		    '() regions)))
  ;; Loop over our regions from start to end.
  ;; Note, for any who read this code: the loop is always operating on
  ;; the second element of restlist, that way we have access to the item
  ;; before and after the current item, since either may be the swallower.
  ;; The first element, if it meets pred, is swallowed at the end.
  (let loop ([restlist regions] [acc '()])
    ;; If we are at the last element of the list, then we're done,
    ;; since we operate on the second element of the list (which is null).
    ;; Now that we're done, we check if the first element satisfies pred,
    ;; and if it does, we merge it into the second element, unless there
    ;; is only one element remaining in the list.
    (cond [(null? (cdr restlist)) (merge-adjacent-regions (let ([first-pass (reverse (cons (car restlist) acc))])
							    (if (and (pred (car first-pass)) (not (null? (cdr first-pass))))
								(cons (merge-into (car first-pass) (cadr first-pass)) (cddr first-pass))
								first-pass)))]
	  ;; If our second element satisfies pred, we have to merge it into one of it's neighbors.
	  [(pred (cadr restlist))
	   ;; If the second element is the last element in the list, we have to merge into the first element.
	   (cond [(null? (cddr restlist))
		  (loop (list (merge-into (cadr restlist) (car restlist))) acc)]
		 ;; If the first element has type =, merge the second element into the third element, and recurse.
		 [(eq? '= (cdar restlist))
		  (loop (list* (car restlist) (merge-into (cadr restlist) (caddr restlist)) (cdddr restlist)) acc)]
		 ;; Otherwise, merge the second element into the first element, and recurse.
		 [#t (loop (list* (merge-into (cadr restlist) (car restlist)) (cddr restlist))
			   acc)])]
	  ;; If our second element doesn't satisfy pred, add the first element to the accumulator, and recurse on
	  ;; the list from the second element onwards (cdr restlist)
	  [#t (loop (cdr restlist) (cons (car restlist) acc))])))
