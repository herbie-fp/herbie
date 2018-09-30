#lang typed/racket

(require (except-in "../common.rkt" assert) "../programs.rkt" "../type-check.rkt")
(require "../points.rkt" "../float.rkt") ; For binary search

(define-type Type (U 'real 'complex' boolean))

(require/typed
 "../alternative.rkt"
 [#:struct alt ([program : Any] [event : Any] [prevs : (Listof alt)])])
(require/typed
 "../float.rkt"
 [ulps->bits (-> Integer Real)]
 [bit-difference (-> Flonum Flonum Flonum)]
 [midpoint-float (-> Flonum Flonum Flonum)])
(require/typed
 "../debug.rkt"
 [debug (->* () (#:from Symbol #:tag Symbol #:depth Fixnum) #:rest Any Void)])
(require/typed
 "../config.rkt"
 [*binary-search-test-points* (Parameter Fixnum)]
 [*num-points* (Parameter Fixnum)])
(require/typed
 "../common.rkt"
 [flag-set? (-> Symbol Symbol Boolean)]
 [*start-prog* (Parameter Any)]
 [set-disjoint? (All (a) (-> (Listof a) (Listof a) Boolean))]
 [all-equal? (-> (Listof Any) Boolean)]
 [index-of (-> (Listof alt) alt (U Fixnum False))]
 [flip-lists (All (a) (-> (Listof (Listof a)) (Listof (Listof a))))])
(require/typed
 "../programs.rkt"
 [free-variables (-> Any (Listof Symbol))]
 [program-body (-> Any Any)]
 [program-variables (-> Any (Listof Symbol))]
 [replace-expression (-> Any Any Any Any)]
 [eval-prog (-> Any (U 'fl 'bf 'ival 'nonffi) (-> (Listof Value) Value))])
(require/typed
 "../points.rkt"
 [#:opaque pcontext pcontext?]
 [sort-context-on-expr (-> pcontext Any (Listof Symbol) pcontext)]
 [*pcontext* (Parameter pcontext)]
 [in-pcontext (-> pcontext (Sequenceof (Listof Value) Value))]
 [prepare-points (-> Any Any pcontext)]
 [errors (-> Any pcontext (Listof Integer))]
 [errors-score (-> (Listof Integer) Real)])
(require/typed
 "../type-check.rkt"
 [type-of (-> Any (Listof (Pair Symbol Type)) Type)])

(require/typed
 racket/set
 [subset? (All (v) (-> (Listof v) (Listof v) Boolean))]
 [set-member? (All (v) (-> (Listof v) v Boolean))]
 [set-union (->* ((Listof Any)) () #:rest (Listof Any) (Listof Any))]
 [set-intersect (->* ((Listof Any)) () #:rest (Listof Any) (Listof Any))]
 [set-subtract (All (v) (-> (Listof v) (Listof v) (Listof v)))])

(module+ test
  (require/typed
   "../alternative.rkt"
   [make-alt (-> Any alt)])
  (require/typed
   "../points.rkt"
   [mk-pcontext (-> (Listof (Listof Value)) (Listof Value) pcontext)])
  (require typed/rackunit))



(define-type Value Flonum #;(U Flonum Complex Boolean))

(provide infer-splitpoints (struct-out sp) splitpoints->point-preds combine-alts)

(struct option
  ([split-indices : (Listof si)]
   [alts : (Listof alt)]
   [pts : (Listof (Listof Value))]
   [expr : Any]
   [errors : (Listof Integer)])
  #:transparent)

;; Struct represeting a splitpoint
;; cidx = Candidate index: the index of the candidate program that should be used to the left of this splitpoint
;; bexpr = Branch Expression: The expression that this splitpoint should split on
;; point = Split Point: The point at which we should split.
(struct sp ([cidx : Integer] [bexpr : Any] [point : Value]) #:transparent)

;; Struct representing a splitindex
;; cidx = Candidate index: the index candidate program that should be used to the left of this splitindex
;; pidx = Point index: The index of the point to the left of which we should split.
(struct si ([cidx : Integer] [pidx : Integer]) #:transparent)

;; `infer-splitpoints` and `combine-alts` are split so the mainloop
;; can insert a timeline break between them.

(define-syntax-rule (reap t [sows ...] body ...)
  (let* ([sows (let ([store : (Listof t) '()])
                 (cons
                  (λ () store)
                  (λ ([elt : t]) (set! store (cons elt store)))))] ...)
    (let ([sows (cdr sows)] ...)
      body ...)
    (values (reverse ((car sows))) ...)))

(: infer-splitpoints (-> (Listof alt) option))
(define (infer-splitpoints alts)
  (debug "Finding splitpoints for:" alts #:from 'regime #:depth 2)
  (define branch-exprs
    (if (flag-set? 'reduce 'branch-expressions)
        (exprs-to-branch-on alts)
        (program-variables (*start-prog*))))
  (debug "Trying" (length branch-exprs) "branch expressions:" branch-exprs
         #:from 'regime-changes #:depth 3)
  (define options : (Listof option)
    ;; We can only combine alts for which the branch expression is
    ;; critical, to enable binary search.
    (reap option [sow]
      (for ([bexpr branch-exprs])
        (define unsound-option (option-on-expr alts bexpr))
        (sow unsound-option)
        (define sound-alts (filter (λ ([a : alt]) (critical-subexpression? (program-body (alt-program a)) bexpr)) alts))
        (when (and (> (length sound-alts) 1)
                   (for/or : Boolean ([si (in-list (option-split-indices unsound-option))])
                     (not (set-member? sound-alts (list-ref alts (si-cidx si))))))
          (sow (option-on-expr sound-alts bexpr))))))
  (define best (argmin (compose errors-score option-errors) options))
  (debug "Found split indices:" best #:from 'regime #:depth 3)
  best)

(: exprs-to-branch-on (-> (Listof alt) (Listof Any)))
(define (exprs-to-branch-on alts)
  (define alt-critexprs (map (compose all-critical-subexpressions alt-program) alts))
  (define start-critexprs (all-critical-subexpressions (*start-prog*)))
  ;; We can only binary search if the branch expression is critical
  ;; for all of the alts and also for the start prgoram.
  (define ces (set-intersect start-critexprs (apply set-union (car alt-critexprs) (cdr alt-critexprs))))
  (filter
   (λ (e) (equal? (type-of e (for/list : (Listof (Pair Symbol 'real)) ([v (program-variables (*start-prog*))]) (cons v 'real))) 'real))
   ces))
  
;; Requires that expr is not a λ expression
(define (critical-subexpression? expr subexpr)
  (define crit-vars (free-variables subexpr))
  (define replaced-expr (replace-expression expr subexpr 1))
  (define non-crit-vars (free-variables replaced-expr))
  (set-disjoint? crit-vars non-crit-vars))

(: subexprs-in-expr (-> Any (Listof Any)))
(define (subexprs-in-expr expr)
  (cons expr (if (list? expr) (append-map subexprs-in-expr (cdr expr)) '())))

;; Requires that prog is a λ expression
(: all-critical-subexpressions (-> Any (Listof Any)))
(define (all-critical-subexpressions prog)
  (define prog-body (program-body prog))
  (for/list ([expr (remove-duplicates (subexprs-in-expr prog-body))]
             #:when (and (not (null? (free-variables expr)))
                         (critical-subexpression? prog-body expr)))
    expr))

(: combine-alts (-> option alt))
(define (combine-alts best-option)
  (match-define (option splitindices alts pts expr _) best-option)
  (match splitindices
   [(list (si cidx _)) (list-ref alts cidx)]
   [_
    (define splitpoints (sindices->spoints pts expr alts splitindices))
    (debug #:from 'regimes "Found splitpoints:" splitpoints ", with alts" alts)

    (define expr*
      (for/fold
          ([expr : Any (program-body (alt-program (list-ref alts (sp-cidx (last splitpoints)))))])
          ([splitpoint (cdr (reverse splitpoints))])
        `(if (<= ,(sp-bexpr splitpoint) ,(sp-point splitpoint))
             ,(program-body (alt-program (list-ref alts (sp-cidx splitpoint))))
             ,expr)))

    ;; We don't want unused alts in our history!
    (define-values (alts* splitpoints*) (remove-unused-alts alts splitpoints))
    (alt `(λ ,(program-variables (*start-prog*)) ,expr*)
         (list 'regimes splitpoints*) alts*)]))

(: remove-unused-alts (-> (Listof alt) (Listof sp) (Values (Listof alt) (Listof sp))))
(define (remove-unused-alts alts splitpoints)
  (for/fold ([alts* : (Listof alt) '()] [splitpoints* : (Listof sp) '()]) ([splitpoint splitpoints])
    (define alt (list-ref alts (sp-cidx splitpoint)))
    ;; It's important to snoc the alt in order for the indices not to change
    (define alts** (remove-duplicates (append alts* (list alt))))
    (define splitpoint* (struct-copy sp splitpoint [cidx (assert (index-of alts** alt))]))
    (define splitpoints** (append splitpoints* (list splitpoint*)))
    (values alts** splitpoints**)))

(: ulps->bits* (-> Integer Flonum))
(define (ulps->bits* x) (real->double-flonum (ulps->bits x)))

(: option-on-expr (-> (Listof alt) Any option))
(define (option-on-expr alts expr)
  (debug #:from 'regimes #:depth 4 "Trying to branch on" expr "from" alts)
  (define vars (program-variables (*start-prog*)))
  (define pcontext* (sort-context-on-expr (*pcontext*) expr vars))
  (define pts (for/list : (Listof (Listof Value)) ([(pt ex) (in-pcontext pcontext*)]) pt))
  (define splitvals (map (eval-prog `(λ ,vars ,expr) 'fl) pts))
  (define can-split? (append (list #f) (for/list : (Listof Boolean) ([val (cdr splitvals)] [prev splitvals])
                                                 (< prev val))))
  (define err-lsts
    (for/list : (Listof (Listof Integer)) ([alt alts]) (errors (alt-program alt) pcontext*)))
  (define bit-err-lsts (map (λ ([x : (Listof Integer)]) (map ulps->bits* x)) err-lsts))
  (define split-indices (err-lsts->split-indices bit-err-lsts can-split?))
  (define pidxs : (Listof Integer) (map (λ ([x : si]) (si-pidx x)) (drop-right split-indices 1)))
  (for ([pidx pidxs])
    (assert (> pidx 0))
    (assert (list-ref can-split? pidx)))
  (assert (= (si-pidx (last split-indices)) (length pts)))
  (option split-indices alts pts expr (pick-errors split-indices pts err-lsts)))

(: pick-errors (-> (Listof si) (Listof (Listof Value)) (Listof (Listof Integer)) (Listof Integer)))
(define (pick-errors split-indices pts err-lsts)
  (for/list ([i (in-naturals)] [pt pts] [errs (flip-lists err-lsts)])
    (let loop : Integer ([sis split-indices])
         (if (< i (si-pidx (car sis)))
             (list-ref errs (si-cidx (car sis)))
             (loop (cdr sis))))))

(module+ test
  (parameterize ([*start-prog* '(λ (x) 1)]
                 [*pcontext* (mk-pcontext '((0.5) (4.0)) '(1.0 1.0))])
    (define alts (map (λ ([body : Any]) (make-alt `(λ (x) ,body))) (list '(fmin x 1) '(fmax x 1))))

    ;; This is a basic sanity test
    (check (λ ([x : option] [y : (Listof Integer)]) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts 'x)
           '(1 0))

    ;; This test ensures we handle equal points correctly. All points
    ;; are equal along the `1` axis, so we should only get one
    ;; splitpoint (the second, since it is better at the further point).
    (check (λ ([x : option] [y : (Listof Integer)]) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts '1)
           '(0))

    (check (λ ([x : option] [y : (Listof Integer)]) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts '(if (== x 0.5) 1 +nan.0))
           '(0))))

;; (pred p1) and (not (pred p2))
(: binary-search-floats (-> (-> Flonum Boolean) Flonum Flonum Flonum))
(define (binary-search-floats pred p1 p2)
  (let ([midpoint (midpoint-float p1 p2)])
    (cond [(< (bit-difference p1 p2) 48) midpoint]
	  [(pred midpoint) (binary-search-floats pred midpoint p2)]
	  [else (binary-search-floats pred p1 midpoint)])))

(: extract-subexpression (-> Any Any Any))
(define (extract-subexpression program expr)
  (define var (gensym 'branch))
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
(: sindices->spoints (-> (Listof (Listof Value)) Any (Listof alt) (Listof si) (Listof sp)))
(define (sindices->spoints points expr alts sindices)
  (define eval-expr
    (eval-prog `(λ ,(program-variables (alt-program (car alts))) ,expr) 'fl))

  (define progs (map (λ ([x : alt]) (extract-subexpression (alt-program x) expr)) alts))
  (define start-prog (extract-subexpression (*start-prog*) expr))

  (: find-split (-> Any Any Flonum Flonum Flonum))
  (define (find-split prog1 prog2 v1 v2)
    (define (pred v)
      (define ctx
        (parameterize ([*num-points* (*binary-search-test-points*)])
          (prepare-points start-prog `(== ,(first (program-variables start-prog)) ,v))))
      (< (errors-score (errors prog1 ctx)) (errors-score (errors prog2 ctx))))
    (binary-search-floats pred v1 v2))

  (: sidx->spoint (-> si si sp))
  (define (sidx->spoint sidx next-sidx)
    (define prog1 (list-ref progs (si-cidx sidx)))
    (define prog2 (list-ref progs (si-cidx next-sidx)))

    (define p1 (eval-expr (list-ref points (sub1 (si-pidx sidx)))))
    (define p2 (eval-expr (list-ref points (si-pidx sidx))))

    (sp (si-cidx sidx) expr (find-split prog1 prog2 p1 p2)))

  (define final-sp (sp (si-cidx (last sindices)) expr +nan.0))

  (append
   (if (and (flag-set? 'reduce 'binary-search)
            ;; Binary search is only valid if we correctly extracted the branch expression
            (andmap (inst identity Any) (cons start-prog progs)))
       (begin
         (debug #:from 'binary-search "Improving bounds with binary search for" expr "and" alts)
         (for/list : (Listof sp) ([si1 sindices] [si2 (cdr sindices)])
           (sidx->spoint si1 si2)))
       (begin
         (debug #:from 'binary-search "Only using regimes for bounds on" expr "and" alts)
         (for/list : (Listof sp) ([sindex (take sindices (sub1 (length sindices)))])
	   (sp (si-cidx sindex) expr (eval-expr (list-ref points (- (si-pidx sindex) 1)))))))
   (list final-sp)))

;; Takes a vector of numbers, and returns the partial sum of those numbers.
;; For example, if your vector is #(1 4 6 3 8), then this returns #(1 5 11 14 22).
(: partial-sum (-> (Vectorof Flonum) (Vectorof Flonum)))
(define (partial-sum vec)
  (define res : (Vectorof Flonum) (make-vector (vector-length vec) 0.0))
  (for/fold ([cur-psum : Flonum 0.0]) ([(el idx) (in-indexed (in-vector vec))])
    (let ([new-psum (+ cur-psum el)])
      (vector-set! res idx new-psum)
      new-psum))
  res)

;; Struct representing a candidate set of splitpoints that we are considering.
;; cost = The total error in the region to the left of our rightmost splitpoint
;; indices = The si's we are considering in this candidate.
(struct cse ([cost : Flonum] [indices : (Listof si)]) #:transparent)

;; Given error-lsts, returns a list of si objects representing where the optimal splitpoints are.
(: err-lsts->split-indices (-> (Listof (Listof Flonum)) (Listof Boolean) (Listof si)))
(define (err-lsts->split-indices err-lsts can-split-lst)
  ;; We have num-candidates candidates, each of whom has error lists of length num-points.
  ;; We keep track of the partial sums of the error lists so that we can easily find the cost of regions.
  (define num-candidates (length err-lsts))
  (define num-points (length (car err-lsts)))
  (define min-weight num-points)

  (define psums (map (λ ([x : (Listof Flonum)]) (partial-sum (list->vector x))) err-lsts))
  (define can-split? (curry vector-ref (list->vector can-split-lst)))

  ;; Our intermediary data is a list of cse's,
  ;; where each cse represents the optimal splitindices after however many passes
  ;; if we only consider indices to the left of that cse's index.
  ;; Given one of these lists, this function tries to add another splitindices to each cse.
  (: add-splitpoint (-> (Listof cse) (Listof cse)))
  (define (add-splitpoint sp-prev)
    ;; If there's not enough room to add another splitpoint, just pass the sp-prev along.
    (for/list : (Listof cse) ([point-idx (in-naturals)] [point-entry (in-list sp-prev)])
      ;; We take the CSE corresponding to the best choice of previous split point.
      ;; The default, not making a new split-point, gets a bonus of min-weight
      (let ([acost (- (cse-cost point-entry) min-weight)] [aest point-entry])
        (for ([prev-split-idx (in-range 0 point-idx)] [prev-entry (in-list sp-prev)]
              #:when (can-split? (si-pidx (car (cse-indices prev-entry)))))
          ;; For each previous split point, we need the best candidate to fill the new regime
          (let ([best : (U False Integer) #f] [bcost : (U False Flonum) #f])
            (for ([cidx (in-naturals)] [psum (in-list psums)])
              (let ([cost (- (vector-ref psum point-idx)
                             (vector-ref psum prev-split-idx))]
                    [best* best]
                    [bcost* bcost])
                (if (not best*)
                    (begin (set! bcost cost) (set! best cidx))
                    (begin
                      (assert bcost*)
                      (when (< cost bcost*)
                        (set! bcost cost)
                        (set! best cidx))))))
            (when (and (< (+ (cse-cost prev-entry) (assert bcost)) acost))
              (set! acost (+ (cse-cost prev-entry) (assert bcost)))
              (set! aest (cse acost (cons (si (assert best) (+ point-idx 1))
                                          (cse-indices prev-entry)))))))
        aest)))

  ;; We get the initial set of cse's by, at every point-index,
  ;; accumulating the candidates that are the best we can do
  ;; by using only one candidate to the left of that point.
  (define initial
    (for/list : (Listof cse) ([point-idx (in-range num-points)])
      (argmin cse-cost
              ;; Consider all the candidates we could put in this region
              (map (λ ([cand-idx : Integer] [cand-psums : (Vectorof Flonum)])
                      (let ([cost (vector-ref cand-psums point-idx)])
                        (cse cost (list (si cand-idx (+ point-idx 1))))))
                   (range num-candidates)
                   psums))))

  ;; We get the final splitpoints by applying add-splitpoints as many times as we want
  (define final
    (let loop : (Listof cse) ([prev initial])
      (let ([next (add-splitpoint prev)])
        (if (equal? prev next)
            next
            (loop next)))))

  ;; Extract the splitpoints from our data structure, and reverse it.
  (reverse (cse-indices (last final))))

(: splitpoints->point-preds (-> (Listof sp) (Listof alt) (Listof (-> (Listof Value) Boolean))))
(define (splitpoints->point-preds splitpoints alts)
  (assert (all-equal? (map sp-bexpr splitpoints)))
  (assert (nan? (sp-point (last splitpoints))))

  (define vars (program-variables (alt-program (first alts))))
  (define expr `(λ ,vars ,(sp-bexpr (car splitpoints))))
  (define prog (eval-prog expr 'fl))

  (for/list ([i (in-naturals)] [alt alts]) ;; alts necessary to terminate loop
    (λ ([pt : (Listof Value)])
      (define val : Value (prog pt))
      (equal?
       (let loop : Integer ([sps splitpoints])
         (if (or (nan? (sp-point (car sps))) (<= val (sp-point (car sps))))
             (sp-cidx (car sps))
             (loop (cdr sps))))
       i))))

(module+ test
  (parameterize ([*start-prog* '(λ (x y) (/ x y))])
    (define sps
      (list (sp 0 '(/ y x) -inf.0)
            (sp 2 '(/ y x) 0.0)
            (sp 0 '(/ y x) +inf.0)
            (sp 1 '(/ y x) +nan.0)))
    (match-define (list p0? p1? p2?)
                  (splitpoints->point-preds sps (map make-alt (build-list 3 (const '(λ (x y) (/ x y)))))))

    (check-true (p0? '(0.0 -1.0)))
    (check-true (p2? '(-1.0 1.0)))
    (check-true (p0? '(+1.0 1.0)))
    (check-true (p1? '(0.0 0.0)))))
