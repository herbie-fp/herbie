#lang racket
(require casio/alternative)
(require casio/programs)
(require casio/matcher)
(require casio/points)
(require casio/common)

(provide infer-regimes (struct-out sp))

(define (infer-regimes #:pre-combo-func [recurse-func identity] alts)
  (debug "Combining-alts: " alts #:from 'regime-changes #:depth 2)
  (let* ([options (build-list (length (program-variables (alt-program (car alts))))
			      (curryr option-on-var alts))]
	 [best-option (argmin (compose errors-score option-errors) options)]
	 [splitpoints (option-splitpoints best-option)])
    (debug "Using option: " best-option #:from 'regime-changes #:depth 2)
    (if (= (length splitpoints) 1) #f
	(let* ([combining-alts (used-alts alts splitpoints)]
	       ;; the splitpoints contain a reference to the alts in the form of an index. Since we filter the
	       ;; alts based on which ones actually got used, we also want to update these indices to reflect
	       ;; the new alts list.
	       [splitpoints* (coerce-indices splitpoints)]
	       [alts* (recurse-on-alts recurse-func combining-alts splitpoints*)]
	       [prog-body* (prog-combination splitpoints* alts*)])
          (make-regime-alt
           `(λ ,(program-variables (alt-program (car alts))) ,prog-body*)
           (used-alts alts splitpoints)
           splitpoints*)))))

;; Takes a list of splitpoints, `splitpoints`, whose indices originally referred to some list of alts `alts`,
;; and changes their indices so that they make sense on a list of alts given by `(used-alts alts splitpoints)`.
(define (coerce-indices splitpoints)
  (let* ([used-indices (remove-duplicates (map sp-cidx splitpoints))]
	 [mappings (map cons used-indices (range (length used-indices)))])
    (map (λ (splitpoint)
	   (sp (cdr (assoc (sp-cidx splitpoint) mappings))
	       (sp-vidx splitpoint)
	       (sp-point splitpoint)))
	 splitpoints)))

(define (used-alts alts splitpoints)
  (let ([used-alt-indices (remove-duplicates (map sp-cidx splitpoints))])
    (map (curry list-ref alts) used-alt-indices)))

(struct option (splitpoints errors) #:transparent
	#:methods gen:custom-write
        [(define (write-proc opt port mode)
           (display "#<option " port)
           (write (option-splitpoints opt) port)
           (display ">" port))])

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
  (let* ([p&e (sort (map cons (*points*) (*exacts*)) <
                    #:key (compose (curryr list-ref var-idx) car))])
    (parameterize ([*points* (map car p&e)] [*exacts* (map cdr p&e)])
      (let* ([point-lst (flip-lists (list* (*points*) (*exacts*) (map (compose (curry map ulps->bits) alt-errors) alts)))]
             [point-lst* (sum-errors-on-points point-lst var-idx)]
             [points-exacts-errs (flip-lists point-lst*)]
             [points* (car points-exacts-errs)]
             [alt-errs* (flip-lists (cadr points-exacts-errs))]
             [split-indices (err-lsts->split-indices alt-errs*)]
             [split-points (sindices->spoints points* var-idx alts split-indices)])
        (option split-points (pick-errors split-points (*points*) (map alt-errors alts)))))))

(define (error-at prog point exact)
  (car (errors prog
(list point) (list exact))))

;; Accepts a list of sindices in one indexed form and returns the
;; proper splitpoints in float form.
;; Assumption: soundness. More specifically, that the two alts are equivilent across the reals.
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
			  [exact ((eval-prog (alt-program alt1) mode:bf) p*)]
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

(define (point-with-dim index point val)
  (map (λ (pval pindex) (if (= pindex index) val pval))
       point
       (range (length point))))

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
	       altn #;(error "Regime contains entire input space!")
	       (parameterize ([*points* (alt-context-points context)]
			      [*exacts* (alt-context-exacts context)])
		 (if (= 0 (length (*points*))) altn ;; Not every alternative is relevant to this combination, but we don't filter the lists
		     ;; because we refer to the alts by index a lot.
		     (let* ([orig (make-alt (alt-program altn))]
			    [result (recurse-function orig)])
		       (when (> (errors-score (alt-errors result)) (errors-score (alt-errors orig)))
			 (debug "Improved Alt " result " is worse than it's original, " altn #:from 'regime-changes))
		       result)))))
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
;; Takes an optional parameter: max-splits, the maximum number of splitpoints to return
(define (err-lsts->split-indices #:max-splits [max-splits 5] err-lsts)
  ;; We have num-candidates candidates, each of whom has error lists of length num-points.
  ;; We keep track of the partial sums of the error lists so that we can easily find the cost of regions.
  (define num-candidates (length err-lsts))
  (define num-points (length (car err-lsts)))
  (define min-weight num-points)

  (define psums (map (compose list->vector partial-sum) err-lsts))

  ;; Our intermediary data is a list of cse's,
  ;; where each cse represents the optimal splitindices after however many passes
  ;; if we only consider indices to the left of that cse's index.
  ;; Given one of these lists, this function tries to add another splitindices to each cse.
  (define (add-splitpoint sp-prev)
    ;; If there's not enough room to add another splitpoint, just pass the sp-prev along.
    (for/list ([point-idx (in-naturals)] [point-entry sp-prev])
      ;; We take the CSE corresponding to the best choice of previous split point.
      ;; The default, not making a new split-point, gets a bonus of min-weight
      (let ([acost (- (cse-cost point-entry) min-weight)] [aest point-entry])
        (for ([prev-split-idx (in-naturals)] [prev-entry (take sp-prev point-idx)])
          ;; For each previous split point, we need the best candidate to fill the new regime
          (let ([best #f] [bcost #f])
            (for ([cidx (in-naturals)] [psum psums])
              (let ([cost (- (vector-ref psum point-idx)
                             (vector-ref psum prev-split-idx))])
                (when (or (not best) (< cost bcost))
                  (set! bcost cost)
                  (set! best cidx))))
            (when (< (+ (cse-cost prev-entry) bcost) acost)
              (set! acost (+ (cse-cost prev-entry) bcost))
              (set! aest (cse acost (cons (si best (+ point-idx 1))
                                          (cse-splitpoints prev-entry)))))))
        aest)))

  ;; We get the initial set of cse's by, at every point-index,
  ;; accumulating the candidates that are the best we can do
  ;; by using only one candidate to the left of that point.
  (define initial
    (for/list ([point-idx (in-range num-points)])
      (argmin cse-cost
              ;; Consider all the candidates we could put in this region
              (map (λ (cand-idx cand-psums)
                      (let ([cost (vector-ref cand-psums point-idx)])
                        (cse cost
                             (list (si cand-idx (add1 point-idx))))))
                         (range num-candidates)
                         psums))))
    
  ;; We get the final splitpoints by applying add-splitpoints as many times as we want
  (define final
    (for/accumulate (prev initial) ([i (range 1 max-splits)])
      (add-splitpoint prev)))

  ;; Extract the splitpoints from our data structure, and reverse it.
  (reverse (cse-splitpoints (last final))))
