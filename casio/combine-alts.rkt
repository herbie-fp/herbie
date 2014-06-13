#lang racket
(require casio/alternative)
(require casio/programs)
(require casio/matcher)
(require casio/points)
(require casio/common)
(require casio/redgreen)

(provide plausible-alts combine-alts)
;; This value is entirely arbitrary and should probably be changed,
;; before it destroys something.
(define *branch-cost* 5)
(define *min-region-size* 5)

;; A lexically scoped predicate that is true on the top level,
;; but if you're in code that was called because you were trying
;; to improve one of the branches of a conditional, then it is
;; a predicate returning true to points that will go to your part
;; of the branch, and false to points that won't. This predicate
;; takes the form of a racket expression which must be evaluated to be called.
(define *point-pred* (make-parameter (const #t)))

;; Basically matrix flipping, but for lists. So, if you pass it '((1 2 3) (4 5 6) (7 8 9)),
;; it returns '((1 4 7) (2 5 8) (3 6 9)).
(define (flip-lists list-list)
  (apply map list list-list))

;; This constant determines how aggressive our filtration is.
;; Higher values mean we will filter more aggresively, and might
;; accidentally filter out a good option.
(define *plausibility-min-region-size* 3)

;; Works like ormap, except instead of returning true if
;; any invocation of pred returns true, it returns true only
;; if at least reg-size invocations return true in a row.
(define (region-ormap pred reg-size . lsts)
  (let loop ([rest-arg-lists (flip-lists lsts)] [true-count 0])
    (cond [(= true-count reg-size) #t]
	  [(null? rest-arg-lists) #f]
	  [(apply pred (car rest-arg-lists))
	   (loop (cdr rest-arg-lists) (add1 true-count))]
	  [#t (loop (cdr rest-arg-lists) 0)])))

(define (first-pass-filter . alts)
  (let loop ([cur-alt (car alts)] [rest-alts (cdr alts)] [acc '()])
    (let ([rest-alts* (filter (λ (altn) (or (< (alt-cost altn) (alt-cost cur-alt))
					    (region-ormap < *plausibility-min-region-size*
							  (alt-errors altn) (alt-errors cur-alt))))
			      rest-alts)])
      (if (null? rest-alts*)
	  (begin (debug "Made it through the first filter: " acc #:from 'regime-changes #:depth 3)
		 (cons cur-alt acc))
	  (loop (car rest-alts*) (cdr rest-alts*) (cons cur-alt acc))))))

;; Determines which alternatives out of a list of alternatives
;; are plausible for use in regime combinations.
(define (plausible-alts alts)
  (debug "Looking for plausible alts out of " alts #:from 'regime-changes #:depth 3)
  ;; Returns a list of error-cost-points, which are the cost
  ;; of the program consed on to an error point.
  (define (make-cost-error-points altn)
    (map (curry cons (alt-cost altn)) (alt-errors altn)))

  ;; alts -> [error]
  (define (best-errors . alts)
    (apply (curry map min)
	   (map alt-errors alts)))
  ;; alts -> [cost-error-point]
  ;; For a list of alts, returns a list of, at each point,
  ;; an error-cost-point representing the best error our
  ;; alts have at that point, and the best cost of an alt
  ;; that has that error.
  (define (best-cost-errors . alts)
    (map (λ (ceps)
	   (let* ([min-err (apply min (map cdr ceps))]
		  [best-cost (apply min (map car (filter (λ (cep) (= (cdr cep) min-err))
							 ceps)))])
	     (cons best-cost min-err)))
	 (flip-lists (map make-cost-error-points alts))))
  ;; Determines whether this error-cost-point is better
  ;; than the other error-cost-point.
  (define (better? cep1 cep2)
    ;; If we have less error than the least error at this point,
    ;; we're better.
    (or (< (cdr cep1) (cdr cep2))
	(and (= (cdr cep1) (cdr cep2))
	     (<= (car cep1) (car cep2)))))
  ;; Determines whether an alt is equivilent in errors and cost.
  (define (same? alt1 alt2) (and (= (alt-cost alt1) (alt-cost alt2))
				 (andmap = (alt-errors alt1) (alt-errors alt2))))
  ;; Filter alts based on this predicate: If it's the better than all the other alts at some point,
  ;; keep it, otherwise discard it. Then, remove duplicate alts, where alts are considered the same
  ;; if they have the same error performance and cost.
  (if (>= 1 (length alts))
      alts
      (remove-duplicates (filter (lambda (altn) (region-ormap better?
							      *plausibility-min-region-size*
							      (make-cost-error-points altn)
							      (apply best-cost-errors (remove altn alts))))
				 (apply first-pass-filter alts))
			 same?)))

(define (point-with-dim index point val)
  (map (λ (pval pindex) (if (= pindex index) val pval))
       point
       (range (length point))))

;; Given two points, the first of which is pred, and the second is not,
;; finds the point where pred becomes false, by calling split to binary
;; search the space until (split a b) returns a, b, or #f.
(define (binary-search split pred p1 p2)
  ;; Get the midpoint using our given split function
  (let ([midpoint (split p1 p2)])
    ;; If the split function returned false, we're done.
    (cond [(not midpoint) p1]
	  ;; If our midpoint is one of our points, we're done.
	  [(or (= p1 midpoint) (= p2 midpoint)) midpoint]
	  ;; If our predicate is still true of our midpoint, search the
	  ;; space between our midpoint and p2.
	  [(pred midpoint) (binary-search split pred midpoint p2)]
	  ;; Otherwise, search the space between our midpoint and p1.
	  [#t (binary-search split pred p1 midpoint)])))

;; Given two floating point numbers, the first of which is pred,
;; and the second is not, find where pred becomes false (within epsilon).
(define (binary-search-floats pred p1 p2 epsilon)
  (define (close-enough a b) (> epsilon (abs (- a b))))
  (binary-search (lambda (a b) (if (close-enough a b) #f
				   (/ (+ a b) 2)))
		 pred p1 p2))

;; Implemented here for example.
(define binary-search-ints (curry binary-search (compose floor (compose (curryr / 2) +))))

(define (combine-alts #:pre-combo-func [recurse-func identity] alts)
  (let* ([options (build-list (length (program-variables (alt-program (car alts))))
			      (curryr option-on-var alts))]
	 [best-option (argmin (compose errors-score option-errors) options)]
	 [splitpoints (option-splitpoints best-option)])
    (debug "Using option: " best-option #:from 'regime-changes #:depth 2)
    (if (= (length splitpoints) 1) #f
	(let* ([improved-alts (recurse-on-alts recurse-func alts splitpoints)]
	       [prog-body* (prog-combination splitpoints improved-alts)])
	  (alt `(λ ,(program-variables (alt-program (car alts)))
		  ,prog-body*)
	       (stitch-errors splitpoints (*points*) (map alt-errors improved-alts))
	       (calc-cost (used-alts improved-alts splitpoints))
	       (make-regime-change alts improved-alts splitpoints prog-body*)
	       #f 0)))))

(define (make-regime-change orig-alts improved-alts splitpoints final-prog-body)
  (let ([new-rule (rule 'regimes 'a final-prog-body '())])
    (change new-rule '() (list* '(a . ()) `(splitpoints . ,splitpoints)
				(map (λ (orig impr)
				       `(alt ,orig ,impr))
				     orig-alts
				     improved-alts)))))

(define (used-alts alts splitpoints)
  (let ([used-alt-indices (remove-duplicates (map sp-cidx splitpoints))])
    (map (curry list-ref alts) used-alt-indices)))

(define (calc-cost alts)
  (+ *branch-cost*
     (apply max (map alt-cost alts))))

(struct option (splitpoints errors) #:transparent
	#:methods gen:custom-write
        [(define (write-proc opt port mode)
           (display "#<option " port)
           (write (option-splitpoints opt) port)
           (display ">" port))])

;; Takes a list of items, and returns a list of lists of items, where
;; the items are grouped by the value produced when key-func is evaluated
;; on them.
(define (multipartition items key-func)
  (let loop ([rest-items items] [acc '()])
    (if (null? rest-items) (reverse (map (compose reverse cdr) acc))
	(let* ([key (key-func (car rest-items))]
	       [lookup (assoc key acc)])
	  (loop (cdr rest-items)
		(if lookup
		    (cons (cons (car lookup) (cons (car rest-items) (cdr lookup)))
			  (remove lookup acc))
		    (cons (cons key (list (car rest-items)))
			  acc)))))))

;; This function takes in a list of entries, where each entry is a list containing
;; a point, the exact value at that point, and the errors for any number of alts
;; at that point, and returns a list of entries, each containing the value of a point on
;; a single dimension, and the alts errors.
(define (sum-errors-on-points point-lst var-idx)
  (let ([equivilences (multipartition point-lst (compose (curryr list-ref var-idx) car))])
    (map (λ (equivilent-lst)
	   (list (list-ref (car (car equivilent-lst)) var-idx)
		 (apply (curry map (λ errs (apply + errs)))
			(map cddr equivilent-lst))))
	 equivilences)))

(define (option-on-var var-idx alts)
  (let* ([point-lst (flip-lists (list* (*points*) (*exacts*) (map (compose (curry map ulps->bits) alt-errors) alts)))]
	 [point-lst* (sum-errors-on-points point-lst var-idx)]
	 [points-exacts-errs (flip-lists point-lst*)]
	 [points* (car points-exacts-errs)]
	 [alt-errs* (flip-lists (cadr points-exacts-errs))]
	 [scaled-min-region-size (/ (* (length points*) *min-region-size*) (length (*points*)))]
	 [split-indices (err-lsts->split-indices alt-errs* #:min-region-size scaled-min-region-size)]
	 [split-points (sindices->spoints points* var-idx alts split-indices)])
    (option split-points (pick-errors split-points (*points*) (map alt-errors alts)))))

;; When doing the binary search, this is the value we set to all the point
;; dimensions that we are not testing.
(define *default-test-value* 0)

;; The fraction of the space between two points that we are okay
;; with being off the perfect splitpoint.
(define *epsilon-fraction* (/ 1 200))

(define (error-at prog point exact)
  (car (errors prog
(list point) (list exact))))

;; Accepts a list of sindices in one indexed form and returns the
;; proper splitpoints in float form.
(define (sindices->spoints points var-idx alts sindices)
  (define (sidx->spoint sidx next-sidx)
    (let* ([alt1 (list-ref alts (si-cidx sidx))]
	   [alt2 (list-ref alts (si-cidx next-sidx))]
	   [p1 (list-ref points (sub1 (si-pidx sidx)))]
	   [p2 (list-ref points (si-pidx sidx))]
	   [pred (λ (p)
		   (let* ([p* (point-with-dim var-idx (make-list (length (program-variables (alt-program (car alts))))
								 *default-test-value*)
					      p)]
			  [exact (car (make-exacts (alt-program alt1) (list p*)))]
			  [e1 (error-at (alt-program alt1) p* exact)]
			  [e2 (error-at (alt-program alt2) p* exact)])
		     (< e1 e2)))])
      (sp (si-cidx sidx) var-idx (binary-search-floats pred p1 p2 (* (- p1 p2) *epsilon-fraction*)))))
  (append (map sidx->spoint
	       (take sindices (sub1 (length sindices)))
	       (drop sindices 1))
	  (list (let ([last-sidx (list-ref sindices (sub1 (length sindices)))])
		  (sp (si-cidx last-sidx)
		      var-idx
		      +inf.0)))))

(define (pick-errors splitpoints points err-lsts)
  (let loop ([rest-splits splitpoints] [rest-points points]
	     [rest-errs (flip-lists err-lsts)] [acc '()])
    (cond [(null? rest-points) (reverse acc)]
	  [(< (list-ref (car rest-points) (sp-vidx (car rest-splits)))
	      (sp-point (car rest-splits)))
	   (loop rest-splits (cdr rest-points)
		 (cdr rest-errs) (cons (list-ref (car rest-errs) (sp-cidx (car rest-splits)))
				       acc))]
	  [#t (loop (cdr rest-splits) rest-points rest-errs acc)])))

;; Assuming that the err-lsts are the errors for only the points in which that alt has the region,
;; and they are still in ascending order, this stitches them back together into a single errors list.
(define (stitch-errors splitpoints points err-lsts)
  (let loop ([rest-splits splitpoints] [rest-points points]
	     [rest-errs err-lsts] [acc '()])
    (cond [(null? rest-points) (reverse acc)]
	  [(< (list-ref (car rest-points) (sp-vidx (car rest-splits)))
	      (sp-point (car rest-splits)))
	   (let* ([cidx (sp-cidx (car rest-splits))]
		  [entry (list-ref rest-errs cidx)])
	     (loop rest-splits (cdr rest-points)
		   (with-entry cidx rest-errs (cdr entry))
		   (cons (car entry) acc)))]
	  [#t (loop (cdr rest-splits) rest-points rest-errs acc)])))

(define (with-entry idx lst item)
  (if (= idx 0)
      (cons item (cdr lst))
      (cons (car lst) (with-entry (sub1 idx) (cdr lst) item))))

;; Partitions a list of points and exacts into num-alts contexts,
;; along the given splitoints.
(define (partition-points splitpoints points exacts num-alts)
  (let loop ([rest-splits splitpoints] [rest-points points]
	     [rest-exacts exacts] [accs (make-list num-alts (alt-context '() '()))])
    (cond [(null? rest-points)
	   (map (λ (context) (alt-context (reverse (alt-context-points context))
					  (reverse (alt-context-exacts context))))
		accs)]
	  [(<= (list-ref (car rest-points) (sp-vidx (car rest-splits)))
	       (sp-point (car rest-splits)))
	   (loop rest-splits (cdr rest-points) (cdr rest-exacts)
		 (let* ([entry-idx (sp-cidx (car rest-splits))]
		        [old-entry (list-ref accs entry-idx)])
		   (with-entry entry-idx accs (alt-context (cons (car rest-points)
								 (alt-context-points old-entry))
							   (cons (car rest-exacts)
								 (alt-context-exacts old-entry))))))]
	  [#t (loop (cdr rest-splits) rest-points rest-exacts accs)])))

(define (prog-combination splitpoints alts)
  (let ([rsplits (reverse splitpoints)])
    (let loop ([rest-splits (cdr rsplits)]
	       [acc (program-body (alt-program (list-ref alts (sp-cidx (car rsplits)))))])
      (if (null? rest-splits) acc
	  (loop (cdr rest-splits)
		(let ([splitpoint (car rest-splits)])
		  `(if (< ,(list-ref (program-variables (alt-program (car alts))) (sp-vidx splitpoint))
			  ,(sp-point splitpoint))
		       ,(program-body (alt-program (list-ref alts (sp-cidx splitpoint))))
		       ,acc)))))))

(struct alt-context (points exacts))

;; Recurse on the alts in altns, assuming they're the same alts list
;; on which split-indices (more precisely, that the indices match up),
;; by invoking recurse function with the points and exacts properly
;; dynamically scoped for each alt.
(define (recurse-on-alts recurse-function altns splitpoints)
  (define (recurse-on-points altns contexts)
    (map (λ (altn context)
	   (if (= (length (*points*)) (length (alt-context-points context)))
	       (error "Regime contains entire input space!")
	       (parameterize ([*points* (alt-context-points context)]
			      [*exacts* (alt-context-exacts context)])
		 (if (= 0 (length (*points*))) altn ;; Not every alternative is relevant to this combination, but we don't filter the lists
		     ;; because we refer to the alts by index a lot.
		     (recurse-function (make-alt (alt-program altn)))))))
	 altns
	 contexts))
  (recurse-on-points altns (partition-points splitpoints (*points*) (*exacts*) (length altns))))

(define (ulps->bits e)
  (if (ordinary-float? e)
      (/ (log e) (log 2))
      64))

;; Takes a list of numbers, and returns the partial sum of those numbers.
;; For example, if your list is [1 4 6 3 8], then this returns [1 5 11 14 22].
(define (partial-sum lst)
  (let loop ([rest-lst (cdr lst)] [psum-acc (list (car lst))])
    (if (null? rest-lst)
	(reverse psum-acc)
	(loop (cdr rest-lst)
	      (cons (+ (car psum-acc) (car rest-lst))
		    psum-acc)))))

;; Struct represeting a splitpoint
;; cidx = Candidate index: the index of the candidate program that should be used to the left of this splitpoint
;; vidx = Variable index: The index of the variable that this splitpoint should split on.
;; point = Split Point: The point at which we should split.
(struct sp (cidx vidx point) #:prefab)

;; Struct representing a splitindex
;; cidx = Candidate index: the index candidate program that should be used to the left of this splitindex
;; pidx = Point index: The index of the point to the left of which we should split.
(struct si (cidx pidx) #:prefab)

;; Struct representing a candidate set of splitpoints that we are considering.
;; cost = The total error in the region to the left of our rightmost splitpoint
;; splitpoints = The splitpoints we are considering in this candidate.
(struct cse (cost splitpoints) #:transparent)

;; Given error-lsts, returns a list of sp objects representing where the optimal splitpoints are.
;; Takes two optional parameters: max-splits, the maximum number of splitpoints to return, and
;; min-weight, the minimum total error in a region (?).
(define (err-lsts->split-indices #:max-splits [max-splits 5] #:min-region-size [min-region-size *min-region-size*]
				 #:min-weight [min-weight 5] err-lsts)
  ;; We have num-candidates candidates, each of whom has error lists of length num-points.
  ;; We keep track of the partial sums of the error lists so that we can easily find the cost of regions.
  (let ([num-candidates (length err-lsts)]
	[num-points (length (car err-lsts))]
	[psums (map (compose list->vector partial-sum) err-lsts)])
    ;; Our intermediary data is a vector of cse's where each cse represents the optimal splitindices after
    ;; however many passes if we only consider indices to the left of that cse's index. Given one of these
    ;; lists, this function tries to add another splitindices to each cse.
    (define (add-splitpoint idx-offset sp-prev)
      ;; If there's not enough room to add another splitpoint, just pass the sp-prev along.
      (if (< (length sp-prev) min-region-size) sp-prev 
	  ;; Loop over each item in sp-prev, keeping track of it's index.
	  (map (λ (point-idx point-entry)
		 ;; We build a huge list of all the potential splitpoint combinations we could make,
		 ;; and then get the one with the minimum cost.
		 (argmin cse-cost
			 ;; We add the possibility of not adding a splitpoint to our list so that if
			 ;; the splitpoints that are already there are optimal, we'll keep them.
			 ;; We subtract the min-weight from this option so that we will only create
			 ;; new regions if they are more than min-weight better than not creating that
			 ;; region.
			 (cons (cse (- (cse-cost point-entry) min-weight) (cse-splitpoints point-entry))
			       ;; We are building a list of considering every possible previous splitpoint,
			       ;; and every possible additional candidate. We also keep track of all the
			       ;; indices.
			       (apply append (map (λ (prev-split-idx prev-entry)
						    (map (λ (cand-idx cand-psums)
							   ;; new-cost is the cost of the new region we're adding
							   ;; to the end of the existing region possibility.
							   (let ([new-cost (- (vector-ref cand-psums point-idx)
									      (vector-ref cand-psums prev-split-idx))])
							     ;; Our total cost is the old cost plus the cost of our
							     ;; new region,
							     (cse (+ (cse-cost prev-entry)
								     new-cost)
								  ;; And our new splitpoints are the old ones, with
								  ;; our new one added on.
								  (cons (si cand-idx (add1 point-idx))
									(cse-splitpoints prev-entry)))))
							 (range num-candidates)
							 psums))
						  (range idx-offset point-idx)
						  (take sp-prev (- point-idx idx-offset)))))))
	       (range (+ idx-offset min-region-size) num-points)
	       (drop sp-prev min-region-size))))
  (let* ([sp-initial
	  ;; We get the initial set of cse's by, at every point-index,
	  ;; accumulating the candidates that are the best we can do
	  ;; by using only one candidate to the left of that point.
	  (map (λ (point-idx)
		 (if (< point-idx min-region-size) #f
		     (argmin cse-cost
			     ;; Consider all the candidates we could put in this region
			     (map (λ (cand-idx cand-psums)
				    (let ([cost (vector-ref cand-psums point-idx)])
				      (cse cost
					   (list (si cand-idx (add1 point-idx))))))
				  (range num-candidates)
				  psums))))
	       (range min-region-size num-points))]
	 ;; We get the final splitpoints consideration by piping the initial through
	 ;; add-splipoints as many times as we want splitpoints.
	 ;; Each call to add splitpoints will be operating on a slightly smaller list, so
	 ;; we pass each one an index offset, calculated by pass-num * min-region-size.
	 [sp-final (pipe sp-initial (build-list (sub1 max-splits)
						(compose (curry curry add-splitpoint) (curry * min-region-size) add1)))]) 
      ;; Extract the splitpoints from our data structure, and reverse it.
      (reverse (cse-splitpoints
		(list-ref
		 sp-final
		 (sub1 (length sp-final))))))))
