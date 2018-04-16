#lang racket

(require "../common.rkt")
(require "../config.rkt")
(require "../alternative.rkt")
(require "../programs.rkt")
(require "../points.rkt")
(require "../float.rkt")
(require "../syntax/syntax.rkt")
(require "matcher.rkt")
(require "localize.rkt")

(module+ test
  (require rackunit))

(provide infer-splitpoints (struct-out sp) splitpoints->point-preds)

(define (infer-splitpoints alts [axis #f])
  (match alts
   [(list alt)
    (list (list (sp 0 0 +inf.0)) (list alt))]
   [_
    (debug "Finding splitpoints for:" alts #:from 'regime-changes #:depth 2)
    (define options
      (map (curry option-on-expr alts)
           (if axis (list axis) (exprs-to-branch-on alts))))
    (define options*
      (for/list ([option options] #:unless (check-duplicates (map sp-point (option-splitpoints option))))
        option))
    (define best-option (argmin (compose errors-score option-errors) options*))
    (define splitpoints (option-splitpoints best-option))
    (define altns (used-alts splitpoints alts))
    (define splitpoints* (coerce-indices splitpoints))
    (debug #:from 'regimes "Found splitpoints:" splitpoints* ", with alts" altns)
    (list splitpoints* altns)]))

(struct option (splitpoints errors) #:transparent
	#:methods gen:custom-write
        [(define (write-proc opt port mode)
           (display "#<option " port)
           (write (option-splitpoints opt) port)
           (display ">" port))])

;; TODO: Figure out alts
(define (exprs-to-branch-on alts)
  (define critexprs (all-critical-subexpressions (*start-prog*)))
  (remove-duplicates critexprs))

;; Requires that expr is a λ expression
(define (critical-subexpression? expr subexpr)
  (define crit-vars (free-variables subexpr))
  (define replaced-expr (replace-expression expr subexpr 1))
  (define non-crit-vars (free-variables replaced-expr))
  (set-disjoint? crit-vars non-crit-vars))

;; Requires that prog is a λ expression
(define (all-critical-subexpressions prog)
  (define (subexprs-in-expr expr)
    (cons expr (if (list? expr) (append-map subexprs-in-expr (cdr expr)) null)))
  (define prog-body (location-get (list 2) prog))
  (for/list ([expr (remove-duplicates (subexprs-in-expr prog-body))]
             #:when (and (not (null? (free-variables expr)))
                         (critical-subexpression? prog-body expr)))
    expr))

(define basic-point-search (curry binary-search (λ (p1 p2)
						  (if (for/and ([val1 p1] [val2 p2])
							(> *epsilon-fraction* (abs (- val1 val2))))
						      p1
						      (for/list ([val1 p1] [val2 p2])
							(/ (+ val1 val2) 2))))))

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
	       (sp-bexpr splitpoint)
	       (sp-point splitpoint)))
	 splitpoints)))

(define (option-on-expr alts expr)
  (define vars (program-variables (*start-prog*)))
  (match-define (list pts exs) (sort-context-on-expr (*pcontext*) expr vars))
  (define err-lsts
    (parameterize ([*pcontext* (mk-pcontext pts exs)]) (map alt-errors alts)))
  (define bit-err-lsts (map (curry map ulps->bits) err-lsts))
  (define merged-err-lsts (map (curry merge-err-lsts pts) bit-err-lsts))
  (define split-indices (err-lsts->split-indices merged-err-lsts))
  (define split-points (sindices->spoints pts expr alts split-indices))
  (option split-points (pick-errors split-points pts err-lsts vars)))

;; Accepts a list of sindices in one indexed form and returns the
;; proper splitpoints in float form.
(define (sindices->spoints points expr alts sindices)
  (for ([alt alts])
    (assert
     (set-empty? (set-intersect (free-variables expr)
                                (free-variables (replace-expression (alt-program alt) expr 0))))
     #:extra-info (cons expr alt)))

  (define eval-expr
    (eval-prog `(λ ,(program-variables (alt-program (car alts))) ,expr) 'fl))

  (define (sidx->spoint sidx next-sidx)
    (let* ([alt1 (list-ref alts (si-cidx sidx))]
	   [alt2 (list-ref alts (si-cidx next-sidx))]
	   [p1 (eval-expr (list-ref points (si-pidx sidx)))]
	   [p2 (eval-expr (list-ref points (sub1 (si-pidx sidx))))]
	   [eps (* (abs (ulp-difference p2 p1)) *epsilon-fraction*)]
	   [pred (λ (v)
		   (let* ([start-prog* (replace-expression (*start-prog*) expr v)]
			  [prog1* (replace-expression (alt-program alt1) expr v)]
			  [prog2* (replace-expression (alt-program alt2) expr v)]
			  [context
			   (parameterize ([*num-points* (*binary-search-test-points*)])
			     (prepare-points start-prog* 'TRUE))])
		     (< (errors-score (errors prog1* context))
			(errors-score (errors prog2* context)))))])
      (debug #:from 'regimes "searching between" p1 "and" p2 "on" expr)
      (define (close-enough a b)
        (> eps (abs (ulp-difference a b))))
      (sp (si-cidx sidx) expr (binary-search-floats pred p2 p1 close-enough))))

  (append
   (if (flag-set? 'reduce 'binary-search)
       (map sidx->spoint
	    (take sindices (sub1 (length sindices)))
	    (drop sindices 1))
       (for/list ([sindex (take sindices (sub1 (length sindices)))])
	 (sp (si-cidx sindex) expr (eval-expr (list-ref points (si-pidx sindex))))))
   (list (let ([last-sidx (list-ref sindices (sub1 (length sindices)))])
	   (sp (si-cidx last-sidx)
	       expr
	       +inf.0)))))

(define (merge-err-lsts pts errs)
  (let loop ([pt (car pts)] [pts (cdr pts)] [err (car errs)] [errs (cdr errs)])
    (if (null? pts)
        (list err)
        (if (equal? pt (car pts))
            (loop pt (cdr pts) (+ err (car errs)) (cdr errs))
            (cons err (loop (car pts) (cdr pts) (car errs) (cdr errs)))))))

(define (point-with-dim index point val)
  (map (λ (pval pindex) (if (= pindex index) val pval))
       point
       (range (length point))))

(define (pick-errors splitpoints pts err-lsts variables)
  (reverse
   (first-value
    (for/fold ([acc '()] [rest-splits splitpoints])
	([pt (in-list pts)]
	 [errs (flip-lists err-lsts)])
      (let* ([expr-prog `(λ ,variables ,(sp-bexpr (car rest-splits)))]
	     [float-val ((eval-prog expr-prog 'fl) pt)]
	     [pt-val (if (ordinary-float? float-val) float-val
			 ((eval-prog expr-prog 'bf) pt))])
	(if (or (<= pt-val (sp-point (car rest-splits)))
		(and (null? (cdr rest-splits)) (nan? pt-val)))
	    (if (nan? pt-val) (error "wat")
		(values (cons (list-ref errs (sp-cidx (car rest-splits)))
			      acc)
			rest-splits))
	    (values acc (cdr rest-splits))))))))

(define (with-entry idx lst item)
  (if (= idx 0)
      (cons item (cdr lst))
      (cons (car lst) (with-entry (sub1 idx) (cdr lst) item))))

;; Takes a vector of numbers, and returns the partial sum of those numbers.
;; For example, if your vector is #(1 4 6 3 8), then this returns #(1 5 11 14 22).
(define (partial-sum vec)
  (first-value
   (for/fold ([res (make-vector (vector-length vec))]
	      [cur-psum 0])
       ([(el idx) (in-indexed (in-vector vec))])
     (let ([new-psum (+ cur-psum el)])
       (vector-set! res idx new-psum)
       (values res new-psum)))))

;; Struct represeting a splitpoint
;; cidx = Candidate index: the index of the candidate program that should be used to the left of this splitpoint
;; bexpr = Branch Expression: The expression that this splitpoint should split on
;; point = Split Point: The point at which we should split.
(struct sp (cidx bexpr point) #:prefab)

;; Struct representing a splitindex
;; cidx = Candidate index: the index candidate program that should be used to the left of this splitindex
;; pidx = Point index: The index of the point to the left of which we should split.
(struct si (cidx pidx) #:prefab)

;; Struct representing a candidate set of splitpoints that we are considering.
;; cost = The total error in the region to the left of our rightmost splitpoint
;; splitpoints = The splitpoints we are considering in this candidate.
(struct cse (cost splitpoints) #:transparent)

;; Given error-lsts, returns a list of sp objects representing where the optimal splitpoints are.
(define (err-lsts->split-indices err-lsts)
  ;; We have num-candidates candidates, each of whom has error lists of length num-points.
  ;; We keep track of the partial sums of the error lists so that we can easily find the cost of regions.
  (define num-candidates (length err-lsts))
  (define num-points (length (car err-lsts)))
  (define min-weight num-points)

  (define psums (map (compose partial-sum list->vector) err-lsts))

  ;; Our intermediary data is a list of cse's,
  ;; where each cse represents the optimal splitindices after however many passes
  ;; if we only consider indices to the left of that cse's index.
  ;; Given one of these lists, this function tries to add another splitindices to each cse.
  (define (add-splitpoint sp-prev)
    ;; If there's not enough room to add another splitpoint, just pass the sp-prev along.
    (for/list ([point-idx (in-naturals)] [point-entry (in-list sp-prev)])
      ;; We take the CSE corresponding to the best choice of previous split point.
      ;; The default, not making a new split-point, gets a bonus of min-weight
      (let ([acost (- (cse-cost point-entry) min-weight)] [aest point-entry])
        (for ([prev-split-idx (in-naturals)] [prev-entry (in-list (take sp-prev point-idx))])
          ;; For each previous split point, we need the best candidate to fill the new regime
          (let ([best #f] [bcost #f])
            (for ([cidx (in-naturals)] [psum (in-list psums)])
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
    (let loop ([prev initial])
      (let ([next (add-splitpoint prev)])
        (if (equal? prev next)
            next
            (loop next)))))

  ;; Extract the splitpoints from our data structure, and reverse it.
  (reverse (cse-splitpoints (last final))))

(define (splitpoints->point-preds splitpoints num-alts)
  (let* ([expr (sp-bexpr (car splitpoints))]
	 [variables (program-variables (*start-prog*))]
	 [intervals (map cons (cons #f (drop-right splitpoints 1))
			 splitpoints)])
    (for/list ([i (in-range num-alts)])
      (let ([p-intervals (filter (λ (interval) (= i (sp-cidx (cdr interval)))) intervals)])
	(debug #:from 'splitpoints "intervals are: " p-intervals)
	(λ (p)
	  (let ([expr-val ((eval-prog `(λ ,variables ,expr) 'fl) p)])
	    (for/or ([point-interval p-intervals])
	      (let ([lower-bound (if (car point-interval) (sp-point (car point-interval)) #f)]
		    [upper-bound (sp-point (cdr point-interval))])
                (or (and (nan? expr-val) (= i (- num-alts 1)))
                    (and (or (not lower-bound) (lower-bound . < . expr-val))
                         (expr-val . <= . upper-bound)))))))))))

(module+ test
  (parameterize ([*start-prog* '(λ (x y) (/ x y))])
    (define sps
      (list (sp 0 '(/ y x) -inf.0)
            (sp 2 '(/ y x) 0.0)
            (sp 1 '(/ y x) +inf.0)))
    (match-define (list p0? p1? p2?) (splitpoints->point-preds sps 3))

    (check-true (p0? '(0 -1)))
    (check-true (p2? '(-1 1)))
    (check-true (p1? '(+1 1)))))
