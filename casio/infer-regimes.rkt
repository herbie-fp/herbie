#lang racket
(require casio/alternative)
(require casio/programs)
(require casio/matcher)
(require casio/points)
(require casio/common)

(provide infer-splitpoints (struct-out sp))

(define (infer-splitpoints alts)
  (debug "Finding splitpoints for:" alts #:from 'regime-changes #:depth 2)
  (let* ([options (build-list (length (program-variables (alt-program (car alts))))
			      (curryr option-on-var alts))]
	 [best-option (argmin (compose errors-score option-errors) options)]
	 [splitpoints (option-splitpoints best-option)]
	 [altns (used-alts splitpoints alts)]
	 [splitpoints* (coerce-indices splitpoints)])
    (debug "Found splitpoints:" splitpoints* ", with alts" altns)
    (list splitpoints* altns)))

(struct option (splitpoints errors) #:transparent
	#:methods gen:custom-write
        [(define (write-proc opt port mode)
           (display "#<option " port)
           (write (option-splitpoints opt) port)
           (display ">" port))])

(define (used-alts splitpoints all-alts)
  (let ([used-indices (remove-duplicates (map sp-cidx splitpoints))])
    (map (curry list-ref all-alts) used-indices)))

;; Takes a list of splitpoints, `splitpoints`, whose indices originally referred to some list of alts `alts`,
;; and changes their indices so that they are consecutive starting from zero, but all indicies that
;; previously matched still match.
(define (coerce-indices splitpoints)
  (let* ([used-indices (remove-duplicates (map sp-cidx splitpoints))]
	 [mappings (map cons used-indices (range (length used-indices)))])
    (map (λ (splitpoint)
	   (sp (cdr (assoc (sp-cidx splitpoint) mappings))
	       (sp-vidx splitpoint)
	       (sp-point splitpoint)))
	 splitpoints)))

(define (option-on-var var-idx alts)
  (match-let ([`(,pts ,exs) (sorted-context-list (*pcontext*) var-idx)])
    (let* ([err-lsts (parameterize ([*pcontext* (mk-pcontext pts exs)])
		       (map alt-errors alts))]
	   [bit-err-lsts (map (curry map ulps->bits) err-lsts)]
	   [split-indices (err-lsts->split-indices bit-err-lsts)]
	   [split-points (sindices->spoints pts var-idx alts split-indices)])
      (option split-points (pick-errors split-points pts err-lsts)))))

(define (sorted-context-list context vidx)
  (let ([p&e (sort (for/list ([(pt ex) (in-pcontext context)]) (cons pt ex))
		   < #:key (compose (curryr list-ref vidx) car))])
    (list (map car p&e) (map cdr p&e))))

(define (error-at prog point exact)
  (car (errors prog (mk-pcontext (vector point) (vector exact)))))

;; Accepts a list of sindices in one indexed form and returns the
;; proper splitpoints in float form.
(define (sindices->spoints points var-idx alts sindices)
  (define (sidx->spoint sidx next-sidx)
    (let* ([alt1 (list-ref alts (si-cidx sidx))]
	   [alt2 (list-ref alts (si-cidx next-sidx))]
	   [p1 (list-ref (list-ref points (sub1 (si-pidx sidx))) var-idx)]
	   [p2 (list-ref (list-ref points (si-pidx sidx)) var-idx)]
	   [pred (λ (p)
		   (let* ([p* (point-with-dim var-idx (make-list (length (program-variables (alt-program (car alts))))
								 *default-test-value*)
					      p)]
			  [exact ((eval-prog (*start-prog*) mode:bf) p*)]
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

(define (pick-errors splitpoints pts err-lsts)
  (reverse
   (first-value
    (for/fold ([acc '()] [rest-splits splitpoints])
	([pt (in-list pts)]
	 [errs (flip-lists err-lsts)])
      (if (<= (list-ref pt (sp-vidx (car rest-splits)))
	      (sp-point (car rest-splits)))
	  (values (cons (list-ref errs (sp-cidx (car rest-splits)))
			acc)
		  rest-splits)
	  (values acc (cdr rest-splits)))))))

(define (with-entry idx lst item)
  (if (= idx 0)
      (cons item (cdr lst))
      (cons (car lst) (with-entry (sub1 idx) (cdr lst) item))))

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
