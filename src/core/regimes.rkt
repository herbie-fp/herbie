#lang racket

(require math/bigfloat)
(require "../common.rkt" "../alternative.rkt" "../programs.rkt" "../timeline.rkt"
         "../syntax/types.rkt" "../errors.rkt" "../points.rkt")
(require "../float.rkt" "../pretty-print.rkt" "../ground-truth.rkt") ; For binary search

(provide infer-splitpoints (struct-out sp) splitpoints->point-preds combine-alts
         pareto-regimes)

(module+ test
  (require rackunit "../load-plugin.rkt")
  (load-herbie-builtins))

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

(define (infer-splitpoints alts ctx)
  (timeline-event! 'regimes)
  (timeline-push! 'inputs (map (compose ~a program-body alt-program) alts))
  (define branch-exprs
    (if (flag-set? 'reduce 'branch-expressions)
        (exprs-to-branch-on alts ctx)
        (program-variables (alt-program (first alts)))))
  (define err-lsts (batch-errors (map alt-program alts) (*pcontext*) ctx))
  (define options
    ;; We can only combine alts for which the branch expression is
    ;; critical, to enable binary search.
    (reap [sow]
      (for ([bexpr branch-exprs])
        (define unsound-option (option-on-expr alts err-lsts bexpr ctx))
        (sow unsound-option)
        (define sound-alts (filter (λ (alt) (critical-subexpression? (program-body (alt-program alt)) bexpr)) alts))
        (when (and (> (length sound-alts) 1)
                   (for/or ([si (option-split-indices unsound-option)])
                     (not (set-member? sound-alts (list-ref alts (si-cidx si))))))
          (sow (option-on-expr sound-alts err-lsts bexpr ctx))))))
  (define best (argmin (compose errors-score option-errors) options))
  (timeline-push! 'count (length alts) (length (option-split-indices best)))
  best)

(define (exprs-to-branch-on alts ctx)
  (define alt-critexprs (map (compose all-critical-subexpressions alt-program) alts))
  (define start-critexprs (all-critical-subexpressions (*start-prog*)))
  ;; We can only binary search if the branch expression is critical
  ;; for all of the alts and also for the start prgoram.
  (filter
   (λ (e) (equal? (type-of e ctx) 'real))
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

(define (combine-alts best-option ctx)
  (match-define (option splitindices alts pts expr _) best-option)
  (match splitindices
   [(list (si cidx _)) (list-ref alts cidx)]
   [_
    (timeline-event! 'bsearch)
    (define splitpoints (sindices->spoints pts expr alts splitindices ctx))

    (define expr*
      (for/fold
          ([expr (program-body (alt-program (list-ref alts (sp-cidx (last splitpoints)))))])
          ([splitpoint (cdr (reverse splitpoints))])
        (define repr (repr-of (sp-bexpr splitpoint) ctx))
        (define <=-operator (get-parametric-operator '<= repr repr))
        `(if (,<=-operator ,(sp-bexpr splitpoint) ,(repr->real (sp-point splitpoint) repr))
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

(define (option-on-expr alts err-lsts expr ctx)
  (define repr (repr-of expr ctx))
  (define timeline-stop! (timeline-start! 'branch (~a expr)))

  (define vars (program-variables (alt-program (first alts))))
  (define pts (for/list ([(pt ex) (in-pcontext (*pcontext*))]) pt))
  (define fn (eval-prog `(λ ,vars ,expr) 'fl ctx))
  (define splitvals (for/list ([pt pts]) (apply fn pt)))
  (define big-table ; val and errors for each alt, per point
    (for/list ([(pt ex) (in-pcontext (*pcontext*))] [err-lst err-lsts])
      (list* pt (apply fn pt) err-lst)))
  (match-define (list pts* splitvals* err-lsts* ...)
                (flip-lists (sort big-table (curryr </total repr) #:key second)))

  (define bit-err-lsts* (map (curry map ulps->bits) err-lsts*))

  (define can-split? (append (list #f)
                             (for/list ([val (cdr splitvals*)] [prev splitvals*])
                               (</total prev val repr))))
  (define split-indices (err-lsts->split-indices bit-err-lsts* can-split?))
  (define out (option split-indices alts pts* expr (pick-errors split-indices pts* err-lsts* repr)))
  (timeline-stop! (errors-score (option-errors out)))
  out)

(define/contract (pick-errors split-indices pts err-lsts repr)
  (->i ([sis (listof si?)] 
        [vss (r) (listof (listof (representation-repr? r)))]
        [errss (listof (listof real?))]
        [r representation?])
       [idxs (listof nonnegative-integer?)])
  (for/list ([i (in-naturals)] [pt pts] [errs (flip-lists err-lsts)])
    (for/first ([si split-indices] #:when (< i (si-pidx si)))
      (list-ref errs (si-cidx si)))))

(module+ test
  (define repr (get-representation 'binary64))
  (parameterize ([*start-prog* '(λ (x) 1)]
                 [*pcontext* (mk-pcontext '((0.5) (4.0)) '(1.0 1.0))]
                 [*context* (make-debug-context '(x))])
    (define alts (map (λ (body) (make-alt `(λ (x) ,body))) (list '(fmin.f64 x 1) '(fmax.f64 x 1))))
    (define err-lsts `((,(expt 2 53) 1) (1 ,(expt 2 53))))

    ;; This is a basic sanity test
    (check (λ (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts err-lsts 'x (*context*))
           '(1 0))

    ;; This test ensures we handle equal points correctly. All points
    ;; are equal along the `1` axis, so we should only get one
    ;; splitpoint (the second, since it is better at the further point).
    (check (λ (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts err-lsts '1 (*context*))
           '(0))

    (check (λ (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts err-lsts '(if (==.f64 x 0.5) 1 +nan.0) (*context*))
           '(1 0))))

;; (pred p1) and (not (pred p2))
(define (binary-search-floats pred p1 p2 repr)
  (let ([midpoint (midpoint p1 p2 repr)])
    (cond
     ; Avoid sampling on [+max, nan] at all costs
     ; (only works for floats, problematic since p1 and p2 are repr values)
     ; (this is handled by catching all sampling errors, so we can comment this out)
     ; [(nan? midpoint) p1]
     [(<= (ulp-difference p1 p2 repr) (expt 2 48))
      ((representation-bf->repr repr)
       (bigfloat-interval-shortest
        ((representation-repr->bf repr) p1)
        ((representation-repr->bf repr) p2)))]
     [else
      ; cmp usually equals 0 if sampling failed
      ; if so, give up and return the current midpoint
      (define cmp (pred midpoint))
      (cond
       [(negative? cmp) (binary-search-floats pred midpoint p2 repr)]
       [(positive? cmp) (binary-search-floats pred p1 midpoint repr)]
       [else ((representation-bf->repr repr)
              (bigfloat-interval-shortest
               ((representation-repr->bf repr) p1)
               ((representation-repr->bf repr) p2)))])])))

(define (extract-subexpression program var expr)
  (define body* (replace-expression (program-body program) expr var))
  (define vars* (set-subtract (program-variables program) (free-variables expr)))
  (if (subset? (free-variables body*) (cons var vars*))
      `(λ (,var ,@vars*) ,body*)
      #f))

(define (prepend-argument f val pcontext repr #:length length)
  (define-values (newpts newexs newlen)
    (for/fold ([newpts '()] [newexs '()] [newlen 0])
        ([(pt _) (in-pcontext pcontext)]
         #:break (>= newlen length))
      (define pt* (cons val pt))
      (define ex* (apply f pt*))
      (if (nan? ex*)
          (values (cons pt* newpts) (cons ex* newexs) (+ 1 newlen))
          (values newpts newexs newlen))))
  (when (< newlen length)
    (raise-herbie-error "Cannot sample enough valid points."
                        #:url "faq.html#sample-valid-points"))
  (mk-pcontext newpts newexs))

;; Accepts a list of sindices in one indexed form and returns the
;; proper splitpoints in float form. A crucial constraint is that the
;; float form always come from the range [f(idx1), f(idx2)). If the
;; float form of a split is f(idx2), or entirely outside that range,
;; problems may arise.
(define (sindices->spoints points expr alts sindices ctx)
  (define repr (repr-of expr ctx))

  (define eval-expr
    (eval-prog `(λ ,(program-variables (alt-program (car alts))) ,expr) 'fl ctx))

  (define var (gensym 'branch))
  (define ctx* (context-extend (*context*) var repr))
  (define progs (map (compose (curryr extract-subexpression var expr) alt-program) alts))
  (define start-prog (extract-subexpression (*start-prog*) var expr))
  (define start-fn (parameterize ([*context* ctx*]) (eval-prog-real start-prog ctx)))

  (define (find-split prog1 prog2 v1 v2)
    (define iters 0)

    (define best-guess #f)
    (define current-guess v1)
    (define sampling-fail? #f)

    (define (pred v)
      (set! iters (+ 1 iters))
      (set! best-guess current-guess)
      (set! current-guess v)
      (with-handlers ([exn:fail:user:herbie?
                       (λ (e) (set! sampling-fail? #t) 0)]) ; couldn't sample points
        (define pctx
          (prepend-argument start-fn v (*pcontext*) repr #:length (*binary-search-test-points*)))
        (parameterize ([*context* ctx*])
          (define acc1 (errors-score (errors prog1 pctx ctx*)))
          (define acc2 (errors-score (errors prog2 pctx ctx*)))
          (- acc1 acc2))))
    (define pt (binary-search-floats pred v1 v2 repr))
    (when sampling-fail?
      (set! pt best-guess))
    pt)

  ; a little more rigorous than it sounds:
  ; finds the shortest number `x` near `p1` such that
  ; `x1` is in `[p1, p2]` and is no larger than
  ;  - if `p1` is negative, `p1 / 2`
  ;  - if `p1` is positive, `p1 * 2`
  (define (left-point p1 p2)
    (let ([left ((representation-repr->bf repr) p1)]
          [right ((representation-repr->bf repr) p2)])
      ((representation-bf->repr repr)
        (if (bfnegative? left)
            (bigfloat-interval-shortest left (bfmin (bf/ left 2.bf) right))
            (bigfloat-interval-shortest left (bfmin (bf* left 2.bf) right))))))

  (define use-binary
    (and (flag-set? 'reduce 'binary-search)
         ;; Binary search is only valid if we correctly extracted the branch expression
         (andmap identity (cons start-prog progs))))
  
  (append
   (for/list ([si1 sindices] [si2 (cdr sindices)])
     (define timeline-stop! (timeline-start! 'times (~a expr)))
     (define prog1 (list-ref progs (si-cidx si1)))
     (define prog2 (list-ref progs (si-cidx si2)))

     (define p1 (apply eval-expr (list-ref points (sub1 (si-pidx si1)))))
     (define p2 (apply eval-expr (list-ref points (si-pidx si1))))

     (define split-at
       (if use-binary
           (with-handlers ([exn:fail:user:herbie:sampling? (const p1)])
             (find-split prog1 prog2 p1 p2))
           (left-point p1 p2)))
     (timeline-stop!)

     (timeline-push! 'method (if use-binary "binary-search" "left-value"))
     (timeline-push! 'bstep (value->json p1 repr) (value->json p2 repr) (value->json split-at repr))
     (sp (si-cidx si1) expr split-at))
   (list (sp (si-cidx (last sindices)) expr +nan.0))))

;; Struct representing a candidate set of splitpoints that we are considering.
;; cost = The total error in the region to the left of our rightmost splitpoint
;; indices = The si's we are considering in this candidate.
(struct cse (cost indices) #:transparent)

;; Given error-lsts, returns a list of sp objects representing where the optimal splitpoints are.
(define (valid-splitindices? can-split? split-indices)
  (and
   (for/and ([pidx (map si-pidx (drop-right split-indices 1))])
     (and (> pidx 0)) (list-ref can-split? pidx))
   (= (si-pidx (last split-indices)) (length can-split?))))

(define/contract (err-lsts->split-indices err-lsts can-split-lst)
  (->i ([e (listof list)] [cs (listof boolean?)]) [result (cs) (curry valid-splitindices? cs)])
  ;; We have num-candidates candidates, each of whom has error lists of length num-points.
  ;; We keep track of the partial sums of the error lists so that we can easily find the cost of regions.
  (define num-candidates (length err-lsts))
  (define num-points (length (car err-lsts)))
  (define min-weight num-points)

  (define psums (map (compose partial-sums list->vector) err-lsts))
  (define can-split? (curry vector-ref (list->vector can-split-lst)))

  ;; Our intermediary data is a list of cse's,
  ;; where each cse represents the optimal splitindices after however many passes
  ;; if we only consider indices to the left of that cse's index.
  ;; Given one of these lists, this function tries to add another splitindices to each cse.
  (define (add-splitpoint sp-prev)
    ;; If there's not enough room to add another splitpoint, just pass the sp-prev along.
    (for/vector #:length num-points ([point-idx (in-naturals)] [point-entry (in-vector sp-prev)])
      ;; We take the CSE corresponding to the best choice of previous split point.
      ;; The default, not making a new split-point, gets a bonus of min-weight
      (let ([acost (- (cse-cost point-entry) min-weight)] [aest point-entry])
        (for ([prev-split-idx (in-range 0 point-idx)] [prev-entry (in-vector sp-prev)]
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
    (for/vector #:length num-points ([point-idx (in-range num-points)])
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
  (reverse (cse-indices (vector-ref final (- num-points 1)))))

(define (valid-splitpoints? splitpoints)
  (and (= (set-count (list->set (map sp-bexpr splitpoints))) 1)
       (nan? (sp-point (last splitpoints)))))

(define/contract (splitpoints->point-preds splitpoints alts ctx)
  (-> valid-splitpoints? (listof alt?) representation? (listof procedure?))

  (define bexpr (sp-bexpr (car splitpoints)))
  (define ctx* (struct-copy context ctx [repr (repr-of bexpr ctx)]))
  (define prog (eval-prog `(λ ,(context-vars ctx*) ,bexpr) 'fl ctx*))

  (for/list ([i (in-naturals)] [alt alts]) ;; alts necessary to terminate loop
    (λ (pt)
      (define val (apply prog pt))
      (for/first ([right splitpoints]
                  #:when (or (equal? (sp-point right) +nan.0)
                             (<=/total val (sp-point right) (context-repr ctx*))))
        ;; Note that the last splitpoint has an sp-point of +nan.0, so we always find one
        (equal? (sp-cidx right) i)))))

(define (pareto-regimes sorted ctx)
  (let loop ([alts sorted] [idx 0])
    (cond
     [(null? alts) '()]
     [(= (length alts) 1) (list (car alts))]
     [else
      (define opt (infer-splitpoints alts ctx))
      (define branched-alt (combine-alts opt ctx))
      (define high (si-cidx (argmax (λ (x) (si-cidx x)) (option-split-indices opt))))
      (cons branched-alt (loop (take alts high) (+ idx (- (length alts) high))))])))

(module+ test
  (parameterize ([*start-prog* '(λ (x y) (/.f64 x y))]
                 [*context* (make-debug-context '(x y))])
    (define sps
      (list (sp 0 '(/.f64 y x) -inf.0)
            (sp 2 '(/.f64 y x) 0.0)
            (sp 0 '(/.f64 y x) +inf.0)
            (sp 1 '(/.f64 y x) +nan.0)))
    (match-define (list p0? p1? p2?)
                  (splitpoints->point-preds
                    sps
                    (map make-alt (build-list 3 (const '(λ (x y) (/ x y)))))
                    repr))

    (check-pred p0? '(0.0 -1.0))
    (check-pred p2? '(-1.0 1.0))
    (check-pred p0? '(+1.0 1.0))
    (check-pred p1? '(0.0 0.0))))
