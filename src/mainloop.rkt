#lang racket

(require "common.rkt" "errors.rkt" "alternative.rkt" "timeline.rkt"
         "syntax/types.rkt" "syntax/syntax.rkt" "syntax/rules.rkt"
         "conversions.rkt" "patch.rkt" "points.rkt" "programs.rkt"
         "ground-truth.rkt" "preprocess.rkt" "symmetry.rkt"
         "core/alt-table.rkt" "core/localize.rkt" "core/simplify.rkt"
         "core/regimes.rkt" "core/bsearch.rkt" "soundiness.rkt" "core/egg-herbie.rkt")

(provide (all-defined-out))

;; I'm going to use some global state here to make the shell more
;; friendly to interact with without having to store your own global
;; state in the repl as you would normally do with debugging. This is
;; probably a bad idea, and I might change it back later. When
;; extending, make sure this never gets too complicated to fit in your
;; head at once, because then global state is going to mess you up.

(struct shellstate (table next-alts locs lowlocs patched) #:mutable)

(define (empty-shellstate)
  (shellstate #f #f #f #f #f))

(define-resetter ^shell-state^
  (λ () (empty-shellstate))
  (λ () (empty-shellstate)))

(define (^locs^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-locs! (^shell-state^) newval))
  (shellstate-locs (^shell-state^)))
(define (^lowlocs^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-lowlocs! (^shell-state^) newval))
  (shellstate-lowlocs (^shell-state^)))
(define (^table^ [newval 'none])
  (when (not (equal? newval 'none))  (set-shellstate-table! (^shell-state^) newval))
  (shellstate-table (^shell-state^)))
(define (^next-alts^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-next-alts! (^shell-state^) newval))
  (shellstate-next-alts (^shell-state^)))
(define (^patched^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-patched! (^shell-state^) newval))
  (shellstate-patched (^shell-state^)))

;; Iteration 0 alts (original alt in every repr, constant alts, etc.)
(define (starting-alts altn ctx)
  (filter alt-expr
    (for/list ([(k v) (in-hash (*conversions*))]
              #:unless (equal? k (context-repr ctx))
              #:when (set-member? v (context-repr ctx)))
      (define rewrite (get-rewrite-operator k))
      (define body* (apply-repr-change-expr (list rewrite (alt-expr altn)) ctx))
      (make-alt body*))))

;; Information
(define (list-alts)
  (printf "Key: [.] = done, [>] = chosen\n")
  (let ([ndone-alts (atab-not-done-alts (^table^))])
    (for ([alt (atab-active-alts (^table^))]
	  [n (in-naturals)])
      (printf "~a ~a ~a\n"
       (cond [(set-member? (^next-alts^) alt) ">"]
             [(set-member? ndone-alts alt) " "]
             [else "."])
       (~r #:min-width 4 n)
       (alt-expr alt))))
  (printf "Error: ~a bits\n" (errors-score (atab-min-errors (^table^)))))

;; Begin iteration
(define (choose-alt! n)
  (unless (< n (length (atab-active-alts (^table^))))
    (raise-user-error 'choose-alt! "Couldn't select the ~ath alt of ~a (not enough alts)"
                      n (length (atab-active-alts (^table^)))))
  (define-values (picked table*)
    (atab-pick-alt (^table^) #:picking-func (curryr list-ref n) #:only-fresh #f))
  (^next-alts^ (list picked))
  (^table^ table*)
  (void))

(define (score-alt alt)
  (errors-score (errors (alt-expr alt) (*pcontext*) (*context*))))

; Pareto mode alt picking
(define (choose-mult-alts from)
  (define repr (context-repr (*context*)))
  (define altns (filter (compose list? alt-expr) from))
  (cond
   [(< (length altns) (*pareto-pick-limit*)) altns] ; take max
   [else
    (define best (argmin score-alt altns))
    (define altns* (sort (filter-not (curry alt-equal? best) altns) < #:key (curryr alt-cost repr)))
    (define simplest (car altns*))
    (define altns** (cdr altns*))
    (define div-size (round (/ (length altns**) (- (*pareto-pick-limit*) 1))))
    (append
      (list best simplest)
      (for/list ([i (in-range 1 (- (*pareto-pick-limit*) 1))])
        (list-ref altns** (- (* i div-size) 1))))]))

(define (choose-alts)
  (define fresh-alts (atab-not-done-alts (^table^)))
  (define select (if (*pareto-mode*) choose-mult-alts (compose list (curry argmin score-alt))))
  (define alts (select fresh-alts))
  (define repr (context-repr (*context*)))
  (for ([alt (atab-active-alts (^table^))])
    (timeline-push! 'alts
                    (~a (alt-expr alt))
                    (cond
                     [(set-member? alts alt) "next"]
                     [(set-member? fresh-alts alt) "fresh"]
                     [else "done"])
                    (score-alt alt)
                    (~a (representation-name repr))))
  alts)

(define (choose-best-alt!)
  (define-values (picked table*)
    (atab-pick-alt (^table^) #:picking-func (curry argmin score-alt) #:only-fresh #t))
  (^next-alts^ (list picked))
  (^table^ table*)
  (void))

;; Invoke the subsystems individually
(define (localize!)
  (unless (^next-alts^)
    (raise-user-error 'localize! "No alt chosen. Run (choose-best-alt!) or (choose-alt! n) to choose one"))
  (timeline-event! 'localize)

  (define loc-errss
    (batch-localize-error (map alt-expr (^next-alts^)) (*context*)))
  (define repr (context-repr (*context*)))

  ; high-error locations
  (^locs^
    (for/list ([loc-errs (in-list loc-errss)]
               #:when true
               [(err expr) (in-dict loc-errs)]
               [i (in-range (*localize-expressions-limit*))])
      (timeline-push! 'locations (~a expr) (errors-score err)
                      (not (patch-table-has-expr? expr)) (format "~a" (representation-name repr)))
      expr))

  ; low-error locations (Pherbie-only with multi-precision)
  (^lowlocs^
    (if (and (*pareto-mode*) (not (hash-empty? (*conversions*))))
        (for/list ([loc-errs (in-list loc-errss)]
                   #:when true
                   [(err expr) (in-dict (reverse loc-errs))]
                   [i (in-range (*localize-expressions-limit*))])
          (timeline-push! 'locations (~a expr) (errors-score err) #f (~a (representation-name repr)))
          expr) 
        '()))

  (void))

;; Returns the locations of `subexpr` within `expr`
(define (get-locations expr subexpr)
  (let loop ([expr expr] [loc '()])
    (match expr
      [(== subexpr)
       (list (reverse loc))]
      [(list op args ...)
       (apply
        append
        (for/list ([arg (in-list args)] [i (in-naturals 1)])
          (loop arg (cons i loc))))]
      [_
       (list)])))

;; Converts a patch to full alt with valid history
(define (reconstruct! alts)
  ;; extracts the base expression of a patch
  (define (get-starting-expr altn)
    (match (alt-event altn)
     ['(patch) (alt-expr altn)]
     [_
      (if (null? (alt-prevs altn))
          #f
          (get-starting-expr (first (alt-prevs altn))))]))

  ;; takes a patch and converts it to a full alt
  (define (reconstruct-alt altn loc0 orig)
    (let loop ([altn altn])
      (match-define (alt _ event prevs) altn)
      (cond
       [(equal? event '(patch)) orig]
       [else
        (define event*
          ;; The 2 at the start of locs is left over from when we
          ;; differentiated between "programs" with a λ term and
          ;; "expressions" without
          (match event
           [(list 'taylor '() name var)
            (list 'taylor loc0 name var)]
           [(list 'rr '() input proof soundiness)
            (list 'rr loc0 input proof soundiness)]
           [(list 'simplify '() input proof soundiness)
            (list 'simplify loc0 input proof soundiness)]))
        (define expr* (location-do loc0 (alt-expr orig) (const (alt-expr altn))))
        (alt expr* event* (list (loop (first prevs))))])))
  
  (^patched^
   (reap [sow]
     (for ([altn (in-list alts)])
       (define expr0 (get-starting-expr altn))
       (if expr0     ; if expr0 is #f, altn is a full alt (probably iter 0 simplify)
           (for* ([alt0 (in-list (^next-alts^))]
                 [loc (in-list (get-locations (alt-expr alt0) expr0))])
             (sow (reconstruct-alt altn loc alt0)))
           (sow altn)))))

  (void))

;; Finish iteration
(define (finalize-iter!)
  (unless (^patched^)
    (raise-user-error 'finalize-iter! "No candidates ready for pruning!"))

  (timeline-event! 'eval)
  (define new-alts (^patched^))
  (define orig-fresh-alts (atab-not-done-alts (^table^)))
  (define orig-done-alts (set-subtract (atab-active-alts (^table^)) (atab-not-done-alts (^table^))))
  (define-values (errss costs) (atab-eval-altns (^table^) new-alts (*context*)))
  (timeline-event! 'prune)
  (^table^ (atab-add-altns (^table^) new-alts errss costs))
  (define final-fresh-alts (atab-not-done-alts (^table^)))
  (define final-done-alts (set-subtract (atab-active-alts (^table^)) (atab-not-done-alts (^table^))))

  (timeline-push! 'count
                  (+ (length new-alts) (length orig-fresh-alts) (length orig-done-alts))
                  (+ (length final-fresh-alts) (length final-done-alts)))

  (define data
    (hash 'new (list (length new-alts)
                     (length (set-intersect new-alts final-fresh-alts)))
          'fresh (list (length orig-fresh-alts)
                       (length (set-intersect orig-fresh-alts final-fresh-alts)))
          'done (list (- (length orig-done-alts) (length (or (^next-alts^) empty)))
                      (- (length (set-intersect orig-done-alts final-done-alts))
                         (length (set-intersect final-done-alts (or (^next-alts^) empty)))))
          'picked (list (length (or (^next-alts^) empty))
                        (length (set-intersect final-done-alts (or (^next-alts^) empty))))))
  (timeline-push! 'kept data)

  (define repr (context-repr (*context*)))
  (timeline-push! 'min-error (errors-score (atab-min-errors (^table^))) (format "~a" (representation-name repr)))
  (rollback-iter!)
  (void))

(define (inject-candidate! expr)
  (define new-alts (list (make-alt expr)))
  (define-values (errss costs) (atab-eval-altns (^table^) new-alts (*context*)))
  (^table^ (atab-add-altns (^table^) new-alts errss costs))
  (void))

(define (finish-iter!)
  (unless (^next-alts^) (choose-best-alt!))
  (unless (^locs^) (localize!))
  (reconstruct! (patch-table-run (^locs^) (^lowlocs^)))
  (finalize-iter!)
  (void))

(define (rollback-iter!)
  (^locs^ #f)
  (^lowlocs^ #f)
  (^next-alts^ #f)
  (^patched^ #f)
  (void))

(define (rollback-improve!)
  (rollback-iter!)
  (reset!)
  (^table^ #f)
  (void))

;; Run a complete iteration
(define (run-iter!)
  (when (^next-alts^)
    (raise-user-error 'run-iter! "An iteration is already in progress\n~a"
                      "Run (finish-iter!) to finish it, or (rollback-iter!) to abandon it.\n"))

  (^next-alts^ (choose-alts))
  (^table^
    (for/fold ([table (^table^)]) ([picked (choose-alts)] [i (in-naturals 1)])
      (define (picking-func x)
        (for/first ([v x] #:when (alt-equal? v picked)) v))
      (define-values (_ table*)
        (atab-pick-alt table #:picking-func picking-func #:only-fresh #t))
      table*))
  (localize!)
  (reconstruct! (patch-table-run (^locs^) (^lowlocs^)))
  (finalize-iter!))
  
(define (setup-context! vars specification precondition repr)
  (*context* (context vars repr (map (const repr) vars)))
  (when (empty? (*needed-reprs*)) ; if empty, probably debugging
    (*needed-reprs* (list repr (get-representation 'bool))))

  (match-define (cons domain pts+exs)
                  (sample-points precondition 
                    (list specification) 
                    (list (*context*))))
  (cons domain (apply mk-pcontext pts+exs)))

(define (initialize-alt-table! prog pcontext ctx)
  (define alt (make-alt prog))
  (*start-prog* prog)
  (define table (make-alt-table (*pcontext*) alt ctx))

  ; Add starting alt in every precision
  (^table^
   (if (*pareto-mode*)
       (let ([new-alts (starting-alts alt ctx)])
         (define-values (errss costs) (atab-eval-altns table new-alts ctx))
         (atab-add-altns table new-alts errss costs))
       table))

  (when (flag-set? 'setup 'simplify)
      (reconstruct! (patch-table-run-simplify (atab-active-alts (^table^))))
      (finalize-iter!)
      (^next-alts^ #f)))

;; This is only here for interactive use; normal runs use run-improve!
(define (run-improve vars prog iters
                     #:precondition [precondition #f]
                     #:preprocess [preprocess empty]
                     #:precision [precision 'binary64]
                     #:specification [specification #f])
  (rollback-improve!)
  (define repr (get-representation precision))

  (define original-points (setup-context! vars (or specification prog) precondition repr))
  (run-improve! iters prog specification preprocess original-points repr))

(define (make-preprocess-pcontext prog pcontext iters
                                  #:specification [specification #f]
                                  #:preprocess [preprocess '()])
  (timeline-event! 'preprocess)

  ;; 1. Test identities

  (define variables (context-vars (*context*)))
  ;; TODO: What is specification, and why would it ever be `#f`?
  (define expression specification)
  (define egraph (make-egraph))
  ;; TODO: There are probably mathematical terms for describing what kinds of identities these are, what are they?
  (define swaps
    (for/list ([pair (in-combinations variables 2)])
      (match-define (list a b) pair)
      (replace-vars (list (cons a b) (cons b a)) expression)))
  (define abs
    (for/list ([variable (in-list variables)])
      ;; TODO: Replace with premade abs operation from preprocessing.rkt?
      ;; Actually probably just the first part, the one that gets the impl for the repr
      ;; TODO: Do we need to do negate instead?
      (replace-vars (list (cons variable `(fabs.f64 ,variable))) expression)))

  (define query
    (make-egg-query (cons expression (append swaps abs)) (*simplify-rules*)))
  (define node-ids
    (for/list ([expression (in-list (egraph-query-exprs query))])
      (egraph-add-expr egraph expression)))
  (egraph-run-rules
   egraph
   (egraph-query-node-limit query)
   (egraph-query-rules query)
   node-ids
   (egraph-query-const-folding? query)
   #:limit (egraph-query-iter-limit query))

  ;; TODO: Check for unsoundness, abort all preprocessing if so

  ;; 2. Compress (connected components)
  ;; Consider a graph (separate from the e-graph) where each node is a variable, and there exists an edge between two variables whenever swapping them is equivalent to the original expression.
  ;; See https://pavpanchekha.com/blog/symmetric-expressions.html

  ;; TODO: The complexity on this thing is awful, filter-map, member, member
  (define adjacency-list
    (for/hash ([variable (in-list variables)])
      (values
       variable
       (filter-map
        (lambda (variable*)
          (let ([expression*
                 (replace-vars
                  (list (cons variable variable*) (cons variable* variable))
                  expression)])
            (and
             (not (equal? variable variable*))
             ;; TODO: This is inefficient, we could just keep track of the e-node ids and this would be union-find
             (egraph-is-equal egraph expression expression*)
             variable*)))
        variables))))
  ;; TODO: Is this good style, or should you still use lambda when you capture local variables?
  (define (neighbors variable) (hash-ref adjacency-list variable))
  (define (depth-first-search start)
    (reverse
     (let search ([variable start]
                  [visited '()])
       ;; TODO: Use hash-set instead
       (if (member variable visited)
           visited
           (foldl search (cons variable visited) (neighbors variable))))))
  (define connected-components
    (for/fold ([components '()]
               [visited '()]
               ;; TODO: How to avoid this reverse
               #:result (reverse components))
              ([variable (in-list variables)])
      (if (member variable visited)
          (values components visited)
          (let ([component (depth-first-search variable)])
            (values
             (cons component components)
             (append visited component))))))

  ;; 3. Output instructions

  (define preprocess-abs
    (for/list ([variable (in-list variables)]
               [expression* abs]
               #:when (egraph-is-equal egraph expression expression*))
      (list 'abs variable)))
  (define preprocess-sort
    (for/list ([component connected-components]
               #:when (> (length component) 1))
      (cons 'sort component)))
  (timeline-push! 'symmetry (map ~a preprocess-abs))
  (timeline-push! 'symmetry (map ~a preprocess-sort))
  (*herbie-preprocess* (append preprocess preprocess-abs preprocess-sort))

  (define (preprocess->operator preprocess)
    (match preprocess
      [('sort component ...)
       ;; TODO: Going a little overboard on the name
       ;; Relies on sublist being a sorted sublist of list
       (define overlay
         (let loop ([list variables]
                    ;; Compilers has a good name for what this is I think
                    [sublist component])
           (cond
             [(empty? list)
              empty]
             [(and (not (empty? sublist)) (equal? (first list) (first sublist)))
              (cons #t (loop (rest list) (rest sublist)))]
             [else
              (cons #f (loop (rest list) sublist))])))
       (lambda (points)
         (define sorted
           (sort
            (filter-map (lambda (point take?) (and take? point)) overlay points)
            (curryr </total repr)))
         (for/fold ([result empty]
                    [points* sorted)
                    #:result result)
                   ([take? overlay]
                    [point points])
           (if take?
               (values (cons (first points*) result) (rest points*))
               (values (cons point result) points*))))]
      [('abs variable)
       (define index (index-of variables variable))
       ;; TODO: Make abs an actually correct operation
       (lambda (points) (list-update points index abs))]))
  (define reduce-range
    ;; TODO: Okay prolly a bit too cute
    (foldl (compose compose preprocess->operator) (*herbie-preprocess*)))
  (pcontext
   (map reduce-range (pcontext-points pcontext))
   (pcontext-exacts pcontext)))

(define (run-improve! prog pcontext iters
                      #:specification [specification #f]
                      #:preprocess [preprocess '()])
  (define processed-pcontext
    (make-preprocess-pcontext prog pcontext iters
                              #:specification specification
                              #:preprocess preprocess))

  (match-define (cons best rest) (mutate! prog iters processed-pcontext))

  (*herbie-preprocess* (remove-unnecessary-preprocessing best pcontext (*herbie-preprocess*)))
  (cons best rest))

(define (preprocessing-<=? alt pcontext preprocessing-one preprocessing-two ctx)
  (define vars (context-vars ctx))
  (define pcontext1 (preprocess-pcontext pcontext preprocessing-one ctx))
  (define pcontext2 (preprocess-pcontext pcontext preprocessing-two ctx))
  (<= (errors-score (errors (alt-expr alt) pcontext1 (*context*)))
      (errors-score (errors (alt-expr alt) pcontext2 (*context*)))))

(define (drop-at ls index)
  (define-values (front back) (split-at ls index))
  (append front (rest back)))

; until fixed point, iterate through preprocessing attempting to drop preprocessing with no effect on error
(define (remove-unnecessary-preprocessing alt pcontext preprocessing #:removed [removed empty])
  (define-values (result newly-removed)
    (let loop ([preprocessing preprocessing] [i 0] [removed removed])
      (cond
        [(>= i (length preprocessing))
         (values preprocessing removed)]
        [(preprocessing-<=? alt pcontext (drop-at preprocessing i) preprocessing (*context*))
         (loop (drop-at preprocessing i) i (cons (list-ref preprocessing i) removed))]
        [else
         (loop preprocessing (+ i 1) removed)])))
  (cond
    [(< (length result) (length preprocessing))
     (remove-unnecessary-preprocessing alt pcontext result #:removed newly-removed)]
    [else
     (timeline-push! 'remove-preprocessing (map ~a newly-removed))
     result]))

(define (mutate! prog iters pcontext)
  (*pcontext* pcontext)
  (initialize-alt-table! prog (*pcontext*) (*context*))
  (for ([iter (in-range iters)] #:break (atab-completed? (^table^)))
    (run-iter!))
  (extract!))

(define (extract!)
  (define ctx (*context*))
  (define repr (context-repr ctx))
  (define all-alts (atab-all-alts (^table^)))
  (*all-alts* (atab-active-alts (^table^)))

  (define ndone-alts (atab-not-done-alts (^table^)))
  (for ([alt (atab-active-alts (^table^))])
    (timeline-push! 'alts (~a (alt-expr alt))
                    (if (set-member? ndone-alts alt) "fresh" "done")
                    (score-alt alt) (~a (representation-name repr))))
  (define joined-alts
    (cond
     [(and (flag-set? 'reduce 'regimes) (> (length all-alts) 1)
           (equal? (representation-type repr) 'real)
           (not (null? (context-vars ctx))))
      (cond
       [(*pareto-mode*)
        (map (curryr combine-alts ctx) (pareto-regimes (sort all-alts < #:key (curryr alt-cost repr)) ctx))]
       [else
        (define option (infer-splitpoints all-alts ctx))
        (list (combine-alts option ctx))])]
     [else
      (list (argmin score-alt all-alts))]))

  (define cleaned-alts
    (cond
      [(flag-set? 'generate 'simplify)
       (timeline-event! 'simplify)

       (define input-progs (map alt-expr joined-alts))
       (define egg-query (make-egg-query input-progs (*fp-safe-simplify-rules*) #:const-folding? #f))
       (define simplified (simplify-batch egg-query))

       (for/list ([altn joined-alts] [progs simplified])
         (alt (last progs) 'final-simplify (list altn)))]
      [else
       joined-alts]))
        
  (define alts-deduplicated
    (remove-duplicates cleaned-alts alt-equal?))

  (timeline-push! 'stop (if (atab-completed? (^table^)) "done" "fuel") 1)
  ;; find the best, sort the rest by cost
  (define errss (map (λ (x) (errors (alt-expr x) (*pcontext*) (*context*))) alts-deduplicated))
  (define-values (best end-score rest)
    (for/fold ([best #f] [score #f] [rest #f])
              ([altn (in-list alts-deduplicated)] [errs (in-list errss)])
      (let ([new-score (errors-score errs)])
        (cond
         [(not best) (values altn new-score '())]
         [(< new-score score) (values altn new-score (cons best rest))] ; kick out current best
         [else (values best score (cons altn rest))]))))

  (match-define (cons best-annotated rest-annotated)
    (cond
     [(flag-set? 'generate 'proofs)
      (timeline-event! 'soundness)
      (add-soundiness (cons best rest) (*pcontext*) (*context*))]
     [else
      (cons best rest)]))

  (timeline-event! 'end)
  
  (cons best-annotated (sort rest-annotated > #:key (curryr alt-cost repr))))
