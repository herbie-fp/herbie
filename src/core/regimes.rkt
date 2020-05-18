#lang racket

(require "../common.rkt" "../alternative.rkt" "../programs.rkt" "../timeline.rkt")
(require "../syntax/types.rkt" "../interface.rkt")
(require "../points.rkt" "../float.rkt") ; For binary search

(module+ test
  (require rackunit))

(provide infer-splitpoints (struct-out sp) splitpoints->point-preds combine-alts)

(struct option (split-indices alts pts expr errors) #:transparent
	#:methods gen:custom-write
        [(define (write-proc opt port mode)
           (display "#<option " port)
           (write (option-split-indices opt) port)
           (display ">" port))])

;; TODO: splitpoint lists sp ends with +nan.0. These is suspect, for multi-precision, but I think is sound.

;; Struct representing a splitpoint
;; cidx = Candidate index: the index of the candidate program that should be used to the left of this splitpoint
;; bexpr = Branch Expression: The expression that this splitpoint should split on
;; point = Split Point: The point at which we should split.
(struct sp (cidx bexpr point) #:prefab)

;; Struct representing a splitindex
;; cidx = Candidate index: the index candidate program that should be used to the left of this splitindex
;; pidx = Point index: The index of the point to the left of which we should split.
(struct si (cidx pidx) #:prefab)

;; `infer-splitpoints` and `combine-alts` are split so the mainloop
;; can insert a timeline break between them.

(define (infer-splitpoints alts repr)
  (debug "Finding splitpoints for:" alts #:from 'regime #:depth 2)
  (define branch-exprs
    (if (flag-set? 'reduce 'branch-expressions)
        (exprs-to-branch-on alts)
        (program-variables (alt-program (first alts)))))
  (debug "Trying" (length branch-exprs) "branch expressions:" branch-exprs
         #:from 'regime-changes #:depth 3)
  (define options
    ;; We can only combine alts for which the branch expression is
    ;; critical, to enable binary search.
    (reap [sow]
      (for ([bexpr branch-exprs])
        (define unsound-option (option-on-expr alts bexpr repr))
        (sow unsound-option)
        (define sound-alts (filter (λ (alt) (critical-subexpression? (program-body (alt-program alt)) bexpr)) alts))
        (when (and (> (length sound-alts) 1)
                   (for/or ([si (option-split-indices unsound-option)])
                     (not (set-member? sound-alts (list-ref alts (si-cidx si))))))
          (sow (option-on-expr sound-alts bexpr repr))))))
  (define best (argmin (compose errors-score option-errors) options))
  (debug "Found split indices:" best #:from 'regime #:depth 3)
  best)

(define (exprs-to-branch-on alts)
  (define alt-critexprs (map (compose all-critical-subexpressions alt-program) alts))
  (define start-critexprs (all-critical-subexpressions (*start-prog*)))
  ;; We can only binary search if the branch expression is critical
  ;; for all of the alts and also for the start prgoram.
  (filter
   (λ (e) (equal? (representation-type (get-representation* (type-of e (*var-reprs*)))) 'real))
   (set-intersect start-critexprs (apply set-union alt-critexprs))))
  
;; Requires that expr is not a λ expression
(define (critical-subexpression? expr subexpr)
  (define crit-vars (free-variables subexpr))
  (define replaced-expr (replace-expression expr subexpr 1))
  (define non-crit-vars (free-variables replaced-expr))
  (set-disjoint? crit-vars non-crit-vars))

;; Requires that prog is a λ expression
(define (all-critical-subexpressions prog)
  (define (subexprs-in-expr expr)
    (cons expr (if (list? expr) (append-map subexprs-in-expr (cdr expr)) '())))
  (define prog-body (program-body prog))
  ;; We append program-variables here in case of (λ (x y) 0) or
  ;; similar, where the variables do not appear in the body but are
  ;; still worth splitting on
  (for/list ([expr (remove-duplicates (append (program-variables prog)
                                              (subexprs-in-expr prog-body)))]
             #:when (and (not (null? (free-variables expr)))
                         (critical-subexpression? prog-body expr)))
    expr))

(define (combine-alts best-option repr)
  (match-define (option splitindices alts pts expr _) best-option)
  (match splitindices
   [(list (si cidx _)) (list-ref alts cidx)]
   [_
    (define splitpoints (sindices->spoints pts expr alts splitindices repr))
    (debug #:from 'regimes "Found splitpoints:" splitpoints ", with alts" alts)

    (define expr*
      (for/fold
          ([expr (program-body (alt-program (list-ref alts (sp-cidx (last splitpoints)))))])
          ([splitpoint (cdr (reverse splitpoints))])
        `(if ,(mk-<= repr (sp-bexpr splitpoint) (sp-point splitpoint))
             ,(program-body (alt-program (list-ref alts (sp-cidx splitpoint))))
             ,expr)))

    ;; We don't want unused alts in our history!
    (define-values (alts* splitpoints*) (remove-unused-alts alts splitpoints))
    (alt `(λ ,(program-variables (alt-program (first alts))) ,expr*)
         (list 'regimes splitpoints*) alts*)]))

(define (remove-unused-alts alts splitpoints)
  (for/fold ([alts* '()] [splitpoints* '()]) ([splitpoint splitpoints])
    (define alt (list-ref alts (sp-cidx splitpoint)))
    ;; It's important to snoc the alt in order for the indices not to change
    (define alts** (remove-duplicates (append alts* (list alt))))
    (define splitpoint* (struct-copy sp splitpoint [cidx (index-of alts** alt)]))
    (define splitpoints** (append splitpoints* (list splitpoint*)))
    (values alts** splitpoints**)))

(define (sort-context-on-expr context expr variables repr)
  (define fn (eval-prog `(λ ,variables ,expr) 'fl repr))
  (let ([p&e (sort (for/list ([(pt ex) (in-pcontext context)]) (cons pt ex))
		   (λ (x1 x2) (</total x1 x2 repr))
                   #:key (λ (pts) (apply fn (car pts))))])
    (mk-pcontext (map car p&e) (map cdr p&e))))

(define (option-on-expr alts expr repr)
  (debug #:from 'regimes #:depth 4 "Trying to branch on" expr "from" alts)
  (define vars (program-variables (alt-program (first alts))))
  (define pcontext* (sort-context-on-expr (*pcontext*) expr vars repr))
  (define pts (for/list ([(pt ex) (in-pcontext pcontext*)]) pt))
  (define fn (eval-prog `(λ ,vars ,expr) 'fl repr))
  (define splitvals (for/list ([pt pts]) (apply fn pt)))
  (define can-split? (append (list #f)
                             (for/list ([val (cdr splitvals)] [prev splitvals])
                               (<-all-precisions prev val repr))))
  (define err-lsts
    (for/list ([alt alts]) (errors (alt-program alt) pcontext* repr)))
  (define bit-err-lsts (map (curry map ulps->bits) err-lsts))
  (define split-indices (err-lsts->split-indices bit-err-lsts can-split?))
  (for ([pidx (map si-pidx (drop-right split-indices 1))])
    (assert (> pidx 0))
    (assert (list-ref can-split? pidx)))
  (assert (= (si-pidx (last split-indices)) (length pts)))
  (option split-indices alts pts expr (pick-errors split-indices pts err-lsts)))

(define/contract (pick-errors split-indices pts err-lsts)
  (-> (listof si?) (listof (listof value?)) (listof (listof value?))
      (listof nonnegative-integer?))
  (for/list ([i (in-naturals)] [pt pts] [errs (flip-lists err-lsts)])
    (for/first ([si split-indices] #:when (< i (si-pidx si)))
      (list-ref errs (si-cidx si)))))

(module+ test
  (parameterize ([*start-prog* '(λ (x) 1)]
                 [*pcontext* (mk-pcontext '((0.5) (4.0)) '(1.0 1.0))]
                 [*var-reprs* (list (cons 'x (get-representation 'binary64)))]
                 [*output-repr* (get-representation 'binary64)])
    (define alts (map (λ (body) (make-alt `(λ (x) ,body))) (list '(fmin x 1) '(fmax x 1))))
    (define repr (get-representation 'binary64))

    ;; This is a basic sanity test
    (check (λ (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts 'x repr)
           '(1 0))

    ;; This test ensures we handle equal points correctly. All points
    ;; are equal along the `1` axis, so we should only get one
    ;; splitpoint (the second, since it is better at the further point).
    (check (λ (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts '1 repr)
           '(0))

    (check (λ (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts '(if (== x 0.5) 1 +nan.0) repr)
           '(0))))

;; (pred p1) and (not (pred p2))
(define (binary-search-floats pred p1 p2 repr)
  (let ([midpoint (midpoint p1 p2 repr)])
    (cond [(< (bit-difference p1 p2 repr) 48) midpoint]
	  [(pred midpoint) (binary-search-floats pred midpoint p2 repr)]
	  [else (binary-search-floats pred p1 midpoint repr)])))

(define (extract-subexpression program var expr)
  (define body* (replace-expression (program-body program) expr var))
  (define vars* (set-subtract (program-variables program) (free-variables expr)))
  (if (subset? (free-variables body*) (cons var vars*))
      `(λ (,var ,@vars*) ,body*)
      #f))

;; Accepts a list of sindices in one indexed form and returns the
;; proper splitpoints in float form. A crucial constraint is that the
;; float form always come from the range [f(idx1), f(idx2)). If the
;; float form of a split is f(idx2), or entirely outside that range,
;; problems may arise.
(define (sindices->spoints points expr alts sindices repr)
  (define eval-expr
    (eval-prog `(λ ,(program-variables (alt-program (car alts))) ,expr) 'fl repr))

  (define var (gensym 'branch))
  (define progs (map (compose (curryr extract-subexpression var expr) alt-program) alts))
  (define start-prog (extract-subexpression (*start-prog*) var expr))

  (define (find-split prog1 prog2 v1 v2)
    (define iters 0)
    (define (pred v)
      (set! iters (+ 1 iters))
      (parameterize ([*num-points* (*binary-search-test-points*)]
                     [*timeline-disabled* true]
                     [*var-reprs* (dict-set (*var-reprs*) var repr)])
        (define ctx
          (prepare-points start-prog `(== ,(caadr start-prog) ,v) repr))
        (< (errors-score (errors prog1 ctx repr))
           (errors-score (errors prog2 ctx repr)))))
    (define pt (binary-search-floats pred v1 v2 repr))
    (timeline-push! 'bstep v1 v2 iters pt)
    pt)

  (define (sidx->spoint sidx next-sidx)
    (define prog1 (list-ref progs (si-cidx sidx)))
    (define prog2 (list-ref progs (si-cidx next-sidx)))

    (define p1 (apply eval-expr (list-ref points (sub1 (si-pidx sidx)))))
    (define p2 (apply eval-expr (list-ref points (si-pidx sidx))))

    (sp (si-cidx sidx) expr (find-split prog1 prog2 p1 p2)))

  (define final-sp (sp (si-cidx (last sindices)) expr +nan.0))

  (append
   (if (and (flag-set? 'reduce 'binary-search)
            ;; Binary search is only valid if we correctly extracted the branch expression
            (andmap identity (cons start-prog progs)))
       (begin
         (debug #:from 'binary-search "Improving bounds with binary search for" expr "and" alts)
         (for/list ([si1 sindices] [si2 (cdr sindices)])
           (sidx->spoint si1 si2)))
       (begin
         (debug #:from 'binary-search "Only using regimes for bounds on" expr "and" alts)
         (for/list ([sindex (take sindices (sub1 (length sindices)))])
	   (sp (si-cidx sindex) expr (apply eval-expr (list-ref points (- (si-pidx sindex) 1)))))))
   (list final-sp)))

(define (point-with-dim index point val)
  (map (λ (pval pindex) (if (= pindex index) val pval))
       point
       (range (length point))))

;; Takes a vector of numbers, and returns the partial sum of those numbers.
;; For example, if your vector is #(1 4 6 3 8), then this returns #(1 5 11 14 22).
(define (partial-sum vec)
  (define res (make-vector (vector-length vec)))
  (for/fold ([cur-psum 0]) ([(el idx) (in-indexed (in-vector vec))])
    (let ([new-psum (+ cur-psum el)])
      (vector-set! res idx new-psum)
      new-psum))
  res)

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

(define (splitpoints->point-preds splitpoints alts repr)
  (assert (= (set-count (list->set (map sp-bexpr splitpoints))) 1))
  (assert (equal? (sp-point (last splitpoints)) +nan.0))

  (define vars (program-variables (alt-program (first alts))))
  (define expr `(λ ,vars ,(sp-bexpr (car splitpoints))))
  (define prog (eval-prog expr 'fl repr))

  (for/list ([i (in-naturals)] [alt alts]) ;; alts necessary to terminate loop
    (λ (pt)
      (define val (apply prog pt))
      (for/first ([right splitpoints]
                  #:when (or (equal? (sp-point right) +nan.0)
                             (<=/total val (sp-point right) repr)))
        ;; Note that the last splitpoint has an sp-point of +nan.0, so we always find one
        (equal? (sp-cidx right) i)))))

(module+ test
  (parameterize ([*start-prog* '(λ (x y) (/ x y))]
                 [*var-reprs* (map (curryr cons (get-representation 'binary64)) '(x y))]
                 [*output-repr* (get-representation 'binary64)])
    (define sps
      (list (sp 0 '(/ y x) -inf.0)
            (sp 2 '(/ y x) 0.0)
            (sp 0 '(/ y x) +inf.0)
            (sp 1 '(/ y x) +nan.0)))
    (match-define (list p0? p1? p2?)
                  (splitpoints->point-preds
                    sps
                    (map make-alt (build-list 3 (const '(λ (x y) (/ x y)))))
                    (get-representation 'binary64)))

    (check-pred p0? '(0 -1))
    (check-pred p2? '(-1 1))
    (check-pred p0? '(+1 1))
    (check-pred p1? '(0 0))))
