#lang racket

(require "common.rkt" "errors.rkt" "timeline.rkt"
         "syntax/rules.rkt" "syntax/types.rkt"
         "alternative.rkt" "conversions.rkt"
         "patch.rkt" "points.rkt" "programs.rkt"
         "ground-truth.rkt" "preprocess.rkt" "symmetry.rkt"
         "core/alt-table.rkt" "core/localize.rkt" "core/simplify.rkt"
         "core/regimes.rkt" "core/bsearch.rkt" "soundiness.rkt")

(provide (all-defined-out))

;; I'm going to use some global state here to make the shell more
;; friendly to interact with without having to store your own global
;; state in the repl as you would normally do with debugging. This is
;; probably a bad idea, and I might change it back later. When
;; extending, make sure this never gets too complicated to fit in your
;; head at once, because then global state is going to mess you up.

(struct shellstate (table next-alt locs lowlocs patched) #:mutable)

(define ^shell-state^ (make-parameter (shellstate #f #f #f #f #f)))

(define (^locs^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-locs! (^shell-state^) newval))
  (shellstate-locs (^shell-state^)))
(define (^lowlocs^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-lowlocs! (^shell-state^) newval))
  (shellstate-lowlocs (^shell-state^)))
(define (^table^ [newval 'none])
  (when (not (equal? newval 'none))  (set-shellstate-table! (^shell-state^) newval))
  (shellstate-table (^shell-state^)))
(define (^next-alt^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-next-alt! (^shell-state^) newval))
  (shellstate-next-alt (^shell-state^)))
(define (^patched^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-patched! (^shell-state^) newval))
  (shellstate-patched (^shell-state^)))

;; Iteration 0 alts (original alt in every repr, constant alts, etc.)
(define (starting-alts altn ctx)
  (define prog (alt-program altn))
  (filter (λ (altn) (program-body (alt-program altn)))
    (for/list ([(k v) (in-hash (*conversions*))]
              #:unless (equal? k (context-repr ctx))
              #:when (set-member? v (context-repr ctx)))
      (define rewrite (get-rewrite-operator k))
      (define prog* `(λ ,(program-variables prog) (,rewrite ,(program-body prog))))
      (alt (apply-repr-change prog* ctx) 'start '()))))

;; Information
(define (list-alts)
  (printf "Key: [.] = done, [>] = chosen\n")
  (let ([ndone-alts (atab-not-done-alts (^table^))])
    (for ([alt (atab-active-alts (^table^))]
	  [n (in-naturals)])
      (printf "~a ~a ~a\n"
       (cond [(equal? alt (^next-alt^)) ">"]
             [(set-member? ndone-alts alt) " "]
             [else "."])
       (~r #:min-width 4 n)
       (program-body (alt-program alt)))))
  (printf "Error: ~a bits\n" (errors-score (atab-min-errors (^table^)))))

;; Begin iteration
(define (choose-alt! n)
  (unless (< n (length (atab-active-alts (^table^))))
    (raise-user-error 'choose-alt! "Couldn't select the ~ath alt of ~a (not enough alts)"
                      n (length (atab-active-alts (^table^)))))
  (define-values (picked table*)
    (atab-pick-alt (^table^) #:picking-func (curryr list-ref n) #:only-fresh #f))
  (^next-alt^ picked)
  (^table^ table*)
  (void))

(define (score-alt alt)
  (errors-score (errors (alt-program alt) (*pcontext*) (*context*))))

; Pareto mode alt picking
(define (choose-mult-alts from)
  (define repr (context-repr (*context*)))
  (define altns (filter (compose list? program-body alt-program) from))
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
  (for ([alt (atab-active-alts (^table^))])
    (timeline-push! 'alts
                    (~a (program-body (alt-program alt)))
                    (cond
                     [(set-member? alts alt) "next"]
                     [(set-member? fresh-alts alt) "fresh"]
                     [else "done"])
                    (score-alt alt)))
  alts)

(define (choose-best-alt!)
  (define-values (picked table*)
    (atab-pick-alt (^table^) #:picking-func (curry argmin score-alt) #:only-fresh #t))
  (^next-alt^ picked)
  (^table^ table*)
  (void))

;; Invoke the subsystems individually
(define (localize!)
  (unless (^next-alt^)
    (raise-user-error 'localize! "No alt chosen. Run (choose-best-alt!) or (choose-alt! n) to choose one"))
  (timeline-event! 'localize)

  (define orig-prog (alt-program (^next-alt^)))
  (define vars (program-variables orig-prog))
  (define loc-errs (localize-error (alt-program (^next-alt^)) (*context*)))

  ; high-error locations
  (^locs^
    (for/list ([(err expr) (in-dict loc-errs)] [i (in-range (*localize-expressions-limit*))])
      (timeline-push! 'locations (~a expr) (errors-score err)
                      (not (patch-table-has-expr? expr)))
      (cons vars expr)))

  ; low-error locations
  (^lowlocs^
    (if (*pareto-mode*)
        (for/list ([(err expr) (in-dict (reverse loc-errs))] [i (in-range (*localize-expressions-limit*))])
          (timeline-push! 'locations (~a expr) (errors-score err) #f)
          (cons vars expr)) 
        '()))

  (void))

;; Converts a patch to full alt with valid history
(define (reconstruct! alts)
  ;; extracts the base expression of a patch
  (define (get-starting-expr altn)
    (match altn
     [(alt prog '(patch) _) (program-body prog)]
     [(alt _ _ (list)) #f]
     [(alt _ _ (list prev)) (get-starting-expr prev)]))

  ;; takes a patch and converts it to a full alt
  (define (reconstruct-alt altn loc0 orig)
    (let loop ([altn altn])
      (match-define (alt prog event prev) altn)
      (cond
       [(equal? event '(patch)) orig]
       [else
        (define event*
          (match event
           [(list 'taylor name var loc)
            (list 'taylor name var (append loc0 (cdr loc)))]
           [(list 'change cng)
            (match-define (change rule loc binds) cng)
            (list 'change (change rule (append loc0 (cdr loc)) binds))]
            [`(simplify ,loc ,input ,proof ,soundiness)
             (list 'simplify (append loc0 (cdr loc)) input proof soundiness)]))
        (define prog* (location-do loc0 (alt-program orig) (λ (_) (program-body prog))))
        (alt prog* event* (list (loop (first prev))))])))
  
  (^patched^
    (for/fold ([patched '()] #:result (reverse patched))
              ([altn (in-list alts)])
      (define expr0 (get-starting-expr altn))
      (if expr0     ; if expr0 is #f, altn is a full alt (probably iter 0 simplify)
          (let ([locs (get-locations (alt-program (^next-alt^)) expr0)])
            (append (map (λ (l) (reconstruct-alt altn l (^next-alt^))) locs) patched))
          (cons altn patched))))
      
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
          'done (list (- (length orig-done-alts) (if (^next-alt^) 1 0))
                      (- (length (set-intersect orig-done-alts final-done-alts))
                         (if (set-member? final-done-alts (^next-alt^)) 1 0)))
          'picked (list (if (^next-alt^) 1 0)
                        (if (and (^next-alt^) (set-member? final-done-alts (^next-alt^))) 1 0))))
  (timeline-push! 'kept data)

  (timeline-push! 'min-error (errors-score (atab-min-errors (^table^))))
  (rollback-iter!)
  (void))

(define (inject-candidate! prog)
  (define new-alts (list (make-alt prog)))
  (define-values (errss costs) (atab-eval-altns (^table^) new-alts (*context*)))
  (^table^ (atab-add-altns (^table^) new-alts errss costs))
  (void))

(define (finish-iter!)
  (when (not (^next-alt^))
    (choose-best-alt!))
  (when (not (^locs^))
    (localize!))
  (reconstruct! (patch-table-run (^locs^) (^lowlocs^)))
  (finalize-iter!)
  (void))

(define (rollback-iter!)
  (^locs^ #f)
  (^lowlocs^ #f)
  (^next-alt^ #f)
  (^patched^ #f)
  (void))

(define (rollback-improve!)
  (rollback-iter!)
  (reset!)
  (^table^ #f)
  (void))

;; Run a complete iteration
(define (run-iter!)
  (when (^next-alt^)
    (raise-user-error 'run-iter! "An iteration is already in progress\n~a"
                      "Run (finish-iter!) to finish it, or (rollback-iter!) to abandon it.\n"))
  (^patched^
    (for/fold ([full '()]) ([picked (choose-alts)] [i (in-naturals 1)])
      (define (picking-func x)
        (for/first ([v x] #:when (alt-equal? v picked)) v))
      (define-values (_ table*)
        (atab-pick-alt (^table^) #:picking-func picking-func #:only-fresh #t))
      (^next-alt^ picked)
      (^table^ table*)
      (localize!)
      (reconstruct! (patch-table-run (^locs^) (^lowlocs^)))
      (append full (^patched^))))
  (finalize-iter!))
  
(define (setup-context! specification precondition repr)
  (define vars (program-variables specification))
  (*context* (context vars repr (map (const repr) vars)))
  (when (empty? (*needed-reprs*)) ; if empty, probably debugging
    (*needed-reprs* (list repr (get-representation 'bool))))

  (apply mk-pcontext (sample-points precondition (list specification) (*context*))))

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
      (^next-alt^ #f)))

;; This is only here for interactive use; normal runs use run-improve!
(define (run-improve prog iters
                     #:precondition [precondition #f]
                     #:preprocess [preprocess empty]
                     #:precision [precision 'binary64]
                     #:specification [specification #f])
  (rollback-improve!)
  (define repr (get-representation precision))

  (define original-points (setup-context! (or specification prog) precondition repr))
  (run-improve! iters prog specification preprocess original-points repr))


(define (run-improve! prog pcontext iters
                      #:specification [specification #f]
                      #:preprocess [preprocess '()])
  (define vars (program-variables specification))
  (timeline-event! 'preprocess)

  ;; If the specification is given, it is used for sampling points
  (define sortable (connected-components specification))

  (define new-preprocess
    (for/list ([sortable-variables (in-list sortable)]
               #:when (> (length sortable-variables) 1))
      (cons 'sort sortable-variables)))
  (timeline-push! 'symmetry (map ~a new-preprocess))
  (*herbie-preprocess* (append preprocess new-preprocess))

  (define processed-pcontext (preprocess-pcontext pcontext (*herbie-preprocess*) (*context*)))

  (match-define (cons best rest) (mutate! prog iters processed-pcontext))

  (*herbie-preprocess* (remove-unnecessary-preprocessing best pcontext (*herbie-preprocess*)))
  (cons best rest))

(define (preprocessing-<=? alt pcontext preprocessing-one preprocessing-two ctx)
  (define vars (program-variables (alt-program alt)))
  (define pcontext1 (preprocess-pcontext pcontext preprocessing-one ctx))
  (define pcontext2 (preprocess-pcontext pcontext preprocessing-two ctx))
  (<= (errors-score (errors (alt-program alt) pcontext1 (*context*)))
      (errors-score (errors (alt-program alt) pcontext2 (*context*)))))

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
    (run-iter!)
    (print-warnings))

  (extract!))

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

(define (extract!)
  (define ctx (*context*))
  (define repr (context-repr ctx))
  (define all-alts (atab-all-alts (^table^)))
  (*all-alts* (atab-active-alts (^table^)))

  (define ndone-alts (atab-not-done-alts (^table^)))
  (for ([alt (atab-active-alts (^table^))])
    (timeline-push! 'alts (~a (program-body (alt-program alt)))
                    (if (set-member? ndone-alts alt) "fresh" "done")
                    (score-alt alt)))
  (define joined-alts
    (cond
     [(and (flag-set? 'reduce 'regimes) (> (length all-alts) 1)
           (equal? (representation-type repr) 'real)
           (not (null? (program-variables (alt-program (car all-alts))))))
      (cond
       [(*pareto-mode*)
        (pareto-regimes (sort all-alts < #:key (curryr alt-cost repr)) ctx)]
       [else
        (define option (infer-splitpoints all-alts ctx))
        (list (combine-alts option ctx))])]
     [else
      (list (argmin score-alt all-alts))]))
  (timeline-event! 'simplify)
  (define progss*
    (simplify-batch
     ctx
      (simplify-input 
        (map (compose program-body alt-program) joined-alts) empty
        (*fp-safe-simplify-rules*) #t)))

  ;; TODO: we don't have fp-safe for egglog yet
  (define cleaned-alts
    (if (flag-set? 'generate 'egglog)
        joined-alts
      (remove-duplicates
        (for/list ([altn joined-alts] [progs progss*])
          (alt `(λ ,(program-variables (alt-program altn)) ,(last progs))
                'final-simplify (list altn)))
        alt-equal?)))
  (define alts-deduplicated
    (remove-duplicates cleaned-alts alt-equal?))
  

  
  (timeline-push! 'stop (if (atab-completed? (^table^)) "done" "fuel") 1)
  ;; find the best, sort the rest by cost
  (define errss (map (λ (x) (errors (alt-program x) (*pcontext*) (*context*))) alts-deduplicated))
  (define-values (best end-score rest)
    (for/fold ([best #f] [score #f] [rest #f])
              ([altn (in-list alts-deduplicated)] [errs (in-list errss)])
      (let ([new-score (errors-score errs)])
        (cond
         [(not best) (values altn new-score '())]
         [(< new-score score) (values altn new-score (cons best rest))] ; kick out current best
         [else (values best score (cons altn rest))]))))

  (timeline-event! 'soundness)

  (define best-annotated
    (add-soundiness best (*pcontext*) (*context*)))

  (timeline-event! 'end)
  
  (cons best-annotated (sort rest > #:key (curryr alt-cost repr))))
