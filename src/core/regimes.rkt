#lang racket

(require "../common.rkt" "../alternative.rkt" "../programs.rkt")
(require "../points.rkt" "../float.rkt") ; For binary search

(module+ test
  (require rackunit))

(provide infer-splitpoints (struct-out sp) splitpoints->point-preds combine-alts)

(struct option (split-indices pts expr errors) #:transparent
	#:methods gen:custom-write
        [(define (write-proc opt port mode)
           (display "#<option " port)
           (write (option-split-indices opt) port)
           (display ">" port))])

;; `infer-splitpoints` and `combine-alts` are split so the mainloop
;; can insert a timeline break between them.

(define (infer-splitpoints alts)
  (debug "Finding splitpoints for:" alts #:from 'regime #:depth 2)
  (define branch-exprs (exprs-to-branch-on alts))
  (debug "Trying" (length branch-exprs) "branch expressions:" branch-exprs
         #:from 'regime-changes #:depth 3)
  (define options (map (curry option-on-expr alts) branch-exprs))
  (define best (argmin (compose errors-score option-errors) options))
  (debug "Found split indices:" best #:from 'regime #:depth 3)
  best)

(define (combine-alts best-option alts)
  (match-define (option splitindices pts expr _) best-option)
  (match splitindices
   [(list (si cidx _)) (list-ref alts cidx)]
   [_
    (define splitpoints (sindices->spoints pts expr alts splitindices))
    (debug #:from 'regimes "Found splitpoints:" splitpoints ", with alts" alts)

    (define expr*
      (for/fold
          ([expr (program-body (alt-program (list-ref alts (sp-cidx (last splitpoints)))))])
          ([splitpoint (cdr (reverse splitpoints))])
        `(if (<= ,(sp-bexpr splitpoint) ,(sp-point splitpoint))
             ,(program-body (alt-program (list-ref alts (sp-cidx splitpoint))))
             ,expr)))

    ;; We don't want unused alts in our history!
    (define-values (alts* splitpoints*) (remove-unused-alts alts splitpoints))
    (alt `(λ ,(program-variables (*start-prog*)) ,expr*)
         (list 'regimes splitpoints*) alts*)]))

(define (remove-unused-alts alts splitpoints)
  (for/fold ([alts* '()] [splitpoints* '()]) ([splitpoint splitpoints])
    (define alt (list-ref alts (sp-cidx splitpoint)))
    ;; It's important to snoc the alt in order for the indices not to change
    (define alts** (remove-duplicates (append alts* (list alt))))
    (define splitpoint* (struct-copy sp splitpoint [cidx (index-of alts** alt)]))
    (define splitpoints** (append splitpoints* (list splitpoint*)))
    (values alts** splitpoints**)))

(define (exprs-to-branch-on alts)
  (if (flag-set? 'reduce 'branch-expressions)
      (let ([alt-critexprs (for/list ([alt alts])
              (all-critical-subexpressions (alt-program alt)))]
            [critexprs (all-critical-subexpressions (*start-prog*))])
           (remove-duplicates (foldr append '() (cons critexprs alt-critexprs))))
      (program-variables (*start-prog*))))

;; Requires that expr is a λ expression
(define (critical-subexpression? expr subexpr)
  (define crit-vars (free-variables subexpr))
  (define replaced-expr (replace-expression expr subexpr 1))
  (define non-crit-vars (free-variables replaced-expr))
  (set-disjoint? crit-vars non-crit-vars))

;; Requires that prog is a λ expression
(define (all-critical-subexpressions prog)
  (define (subexprs-in-expr expr)
    (cons expr (if (list? expr) (append-map subexprs-in-expr (cdr expr)) '())))
  (define prog-body (location-get (list 2) prog))
  (for/list ([expr (remove-duplicates (subexprs-in-expr prog-body))]
             #:when (and (not (null? (free-variables expr)))
                         (critical-subexpression? prog-body expr)))
    expr))

(define (option-on-expr alts expr)
  (define vars (program-variables (*start-prog*)))
  (define pcontext* (sort-context-on-expr (*pcontext*) expr vars))
  (define pts (for/list ([(pt ex) (in-pcontext pcontext*)]) pt))
  (define splitvals (map (eval-prog `(λ ,vars ,expr) 'fl) pts))
  (define can-split? (append (list #f) (for/list ([val (cdr splitvals)] [prev splitvals]) (< prev val))))
  (define err-lsts
    (for/list ([alt alts]) (errors (alt-program alt) pcontext*)))
  (define bit-err-lsts (map (curry map ulps->bits) err-lsts))
  (define merged-err-lsts (map (curry merge-err-lsts pts) bit-err-lsts))
  (define split-indices (err-lsts->split-indices merged-err-lsts can-split?))
  (for ([pidx (map si-pidx (drop-right split-indices 1))])
    (assert (> pidx 0))
    (assert (list-ref can-split? pidx)))
  (option split-indices pts expr (pick-errors split-indices pts err-lsts)))

(define (pick-errors split-indices pts err-lsts)
  (for/list ([i (in-naturals)] [pt pts] [errs (flip-lists err-lsts)])
    (for/first ([si split-indices] #:when (< i (si-pidx si)))
      (list-ref errs (si-cidx si)))))

(module+ test
  (parameterize ([*start-prog* '(λ (x) 1)]
                 [*pcontext* (mk-pcontext '((0.5) (4.0)) '(1.0 1.0))])
    (define alts (map (λ (body) (make-alt `(λ (x) ,body))) (list '(fmin x 1) '(fmax x 1))))

    ;; This is a basic sanity test
    (check (λ (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts 'x)
           '(1 0))

    ;; This test ensures we handle equal points correctly. All points
    ;; are equal along the `1` axis, so we should only get one
    ;; splitpoint (the second, since it is better at the further point).
    (check (λ (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts '1)
           '(0))

    (check (λ (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts '(if (== x 0.5) 1 +nan.0))
           '(0))))

;; (pred p1) and (not (pred p2))
(define (binary-search-floats pred p1 p2)
  (let ([midpoint (midpoint-float p1 p2)])
    (cond [(< (bit-difference p1 p2) 48) midpoint]
	  [(pred midpoint) (binary-search-floats pred midpoint p2)]
	  [else (binary-search-floats pred p1 midpoint)])))

(define (extract-subexpression program expr)
  (define var (gensym 'branch))
  (define body* (replace-expression (program-body program) expr var))
  (define vars* (set-subtract (program-variables program) (free-variables expr)))
  (assert (subset? (free-variables body*) (cons var vars*))
          #:extra-info (λ () (format "Can't cleanly extract ~a from ~a" expr program)))
  `(λ (,var ,@vars*) ,body*))

;; Accepts a list of sindices in one indexed form and returns the
;; proper splitpoints in float form. A crucial constraint is that the
;; float form always come from the range [f(idx1), f(idx2)). If the
;; float form of a split is f(idx2), or entirely outside that range,
;; problems may arise.
(define (sindices->spoints points expr alts sindices)
  (define eval-expr
    (eval-prog `(λ ,(program-variables (alt-program (car alts))) ,expr) 'fl))

  (define progs (map (compose (curryr extract-subexpression expr) alt-program) alts))
  (define start-prog (extract-subexpression (*start-prog*) expr))

  (define (find-split prog1 prog2 v1 v2)
    (define (pred v)
      (define ctx
        (parameterize ([*num-points* (*binary-search-test-points*)])
          (prepare-points start-prog `(== ,(caadr start-prog) ,v))))
      (< (errors-score (errors prog1 ctx)) (errors-score (errors prog2 ctx))))
    (debug #:from 'regimes "Searching between" v1 "and" v2 "on" expr)
    (binary-search-floats pred v1 v2))

  (define (sidx->spoint sidx next-sidx)
    (define prog1 (list-ref progs (si-cidx sidx)))
    (define prog2 (list-ref progs (si-cidx next-sidx)))

    (define p1 (eval-expr (list-ref points (sub1 (si-pidx sidx)))))
    (define p2 (eval-expr (list-ref points (si-pidx sidx))))

    (sp (si-cidx sidx) expr (find-split prog1 prog2 p1 p2)))

  (append
   (if (flag-set? 'reduce 'binary-search)
       (map sidx->spoint
	    (take sindices (sub1 (length sindices)))
	    (drop sindices 1))
       (for/list ([sindex (take sindices (sub1 (length sindices)))])
	 (sp (si-cidx sindex) expr (eval-expr (list-ref points (- (si-pidx sindex) 1))))))
   (list (let ([last-sidx (list-ref sindices (sub1 (length sindices)))])
	   (sp (si-cidx last-sidx)
	       expr
	       +nan.0)))))

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

(define (point->alt splitpoints)
  (assert (all-equal? (map sp-bexpr splitpoints)))
  (assert (nan? (sp-point (last splitpoints))))
  (define expr `(λ ,(program-variables (*start-prog*)) ,(sp-bexpr (car splitpoints))))
  (define prog (eval-prog expr 'fl))

  (λ (pt)
    (define val (prog pt))
    (for/first ([right splitpoints]
                #:when (or (nan? (sp-point right)) (<= val (sp-point right))))
      ;; Note that the last splitpoint has an sp-point of +nan.0, so we always find one
      (sp-cidx right))))

;; Takes a vector of numbers, and returns the partial sum of those numbers.
;; For example, if your vector is #(1 4 6 3 8), then this returns #(1 5 11 14 22).
(define (partial-sum vec)
  (define res (make-vector (vector-length vec)))
  (for/fold ([cur-psum 0]) ([(el idx) (in-indexed (in-vector vec))])
    (let ([new-psum (+ cur-psum el)])
      (vector-set! res idx new-psum)
      new-psum))
  res)

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
;; indices = The si's we are considering in this candidate.
(struct cse (cost indices) #:transparent)

;; Given error-lsts, returns a list of sp objects representing where the optimal splitpoints are.
(define (err-lsts->split-indices err-lsts can-split-lst)
  ;; We have num-candidates candidates, each of whom has error lists of length num-points.
  ;; We keep track of the partial sums of the error lists so that we can easily find the cost of regions.
  (define num-candidates (length err-lsts))
  (define num-points (length (car err-lsts)))
  (define min-weight num-points)

  (define psums (map (compose partial-sum list->vector) err-lsts))
  (define can-split? (curry vector-ref (list->vector can-split-lst)))

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
        (for ([prev-split-idx (in-range 0 point-idx)] [prev-entry (in-list sp-prev)]
              #:when (can-split? (si-pidx (car (cse-indices prev-entry)))))
          ;; For each previous split point, we need the best candidate to fill the new regime
          (let ([best #f] [bcost #f])
            (for ([cidx (in-naturals)] [psum (in-list psums)])
              (let ([cost (- (vector-ref psum point-idx)
                             (vector-ref psum prev-split-idx))])
                (when (or (not best) (< cost bcost))
                  (set! bcost cost)
                  (set! best cidx))))
            (when (and (< (+ (cse-cost prev-entry) bcost) acost))
              (set! acost (+ (cse-cost prev-entry) bcost))
              (set! aest (cse acost (cons (si best (+ point-idx 1))
                                          (cse-indices prev-entry)))))))
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
                        (cse cost (list (si cand-idx (+ point-idx 1))))))
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
  (reverse (cse-indices (last final))))

(define (splitpoints->point-preds splitpoints num-alts)
  (define which-alt (point->alt splitpoints))
  (for/list ([i (in-range num-alts)])
    (λ (pt) (equal? (which-alt pt) i))))

(module+ test
  (parameterize ([*start-prog* '(λ (x y) (/ x y))])
    (define sps
      (list (sp 0 '(/ y x) -inf.0)
            (sp 2 '(/ y x) 0.0)
            (sp 0 '(/ y x) +inf.0)
            (sp 1 '(/ y x) +nan.0)))
    (match-define (list p0? p1? p2?) (splitpoints->point-preds sps 3))

    (check-true (p0? '(0 -1)))
    (check-true (p2? '(-1 1)))
    (check-true (p0? '(+1 1)))
    (check-true (p1? '(0 0)))))
