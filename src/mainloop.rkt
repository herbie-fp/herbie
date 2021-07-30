#lang racket

(require "syntax/rules.rkt" "syntax/sugar.rkt" "syntax/types.rkt"
         "core/alt-table.rkt" "core/localize.rkt" "core/regimes.rkt"
         "alternative.rkt" "common.rkt" "conversions.rkt" "errors.rkt"
         "interface.rkt" "patch.rkt" "points.rkt" "preprocess.rkt"
         "programs.rkt" "sampling.rkt" "timeline.rkt" "symmetry.rkt")

(require "core/taylor.rkt" "core/simplify.rkt")

(provide (all-defined-out))

;; I'm going to use some global state here to make the shell more
;; friendly to interact with without having to store your own global
;; state in the repl as you would normally do with debugging. This is
;; probably a bad idea, and I might change it back later. When
;; extending, make sure this never gets too complicated to fit in your
;; head at once, because then global state is going to mess you up.

(struct shellstate
  (table next-alt locs lowlocs duplocs patched)
  #:mutable)

(define ^shell-state^ (make-parameter (shellstate #f #f #f #f #f #f)))

(define (^locs^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-locs! (^shell-state^) newval))
  (shellstate-locs (^shell-state^)))
(define (^lowlocs^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-lowlocs! (^shell-state^) newval))
  (shellstate-lowlocs (^shell-state^)))
(define (^duplocs^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-duplocs! (^shell-state^) newval))
  (shellstate-duplocs (^shell-state^)))
(define (^table^ [newval 'none])
  (when (not (equal? newval 'none))  (set-shellstate-table! (^shell-state^) newval))
  (shellstate-table (^shell-state^)))
(define (^next-alt^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-next-alt! (^shell-state^) newval))
  (shellstate-next-alt (^shell-state^)))
(define (^patched^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-patched! (^shell-state^) newval))
  (shellstate-patched (^shell-state^)))

(define *sampler* (make-parameter #f))

;; Iteration 0 alts (original alt in every repr, constant alts, etc.)
(define (starting-alts altn)
  (define prec (representation-name (*output-repr*)))
  (define prog (alt-program altn))
  (filter (λ (altn) (program-body (alt-program altn)))
    (for/list ([(k v) (in-hash (*conversions*))]
              #:unless (equal? k prec)
              #:when (set-member? v prec))
      (define rewrite (get-rewrite-operator k))
      (define prog* `(λ ,(program-variables prog) (,rewrite ,(program-body prog))))
      (alt (apply-repr-change prog*) 'start '()))))

;; Setting up
(define (setup-prog! prog
                     #:precondition [precondition #f]
                     #:preprocess [preprocess empty]
                     #:precision [precision 'binary64]
                     #:specification [specification #f])
  (*output-repr* (get-representation precision))
  (when (empty? (*needed-reprs*)) ; if empty, probably debugging
    (*needed-reprs* (list (*output-repr*) (get-representation 'bool))))
  (*var-reprs* (map (curryr cons (*output-repr*)) (program-variables prog)))
  (*start-prog* prog)
  (rollback-improve!)
  (define precondition-prog
    (or precondition (list 'λ (program-variables prog) 'TRUE)))

  (debug #:from 'progress #:depth 3 "[1/2] Preparing points")
  ;; If the specification is given, it is used for sampling points
  (timeline-event! 'analyze)
  (parameterize ([*timeline-disabled* true])
    (define symmetry-groups (map symmetry-group
                                 (filter (lambda (group) (> (length group) 1)) (connected-components (or specification prog)))))
    ;; make variables strings for the json
    (timeline-push! 'symmetry (map (compose ~a preprocess->sexp) symmetry-groups))
    (define preprocess-structs (append preprocess symmetry-groups))
    (*herbie-preprocess* preprocess-structs))
  (*sampler* (make-sampler (*output-repr*) precondition-prog (list (or specification prog)) (*herbie-preprocess*)))
  
  (timeline-event! 'sample)
  (define contexts (prepare-points (or specification prog) precondition-prog (*output-repr*) (*sampler*) (*herbie-preprocess*)))
  (*pcontext* (car contexts))
  (*pcontext-unprocessed* (cdr contexts))
  (debug #:from 'progress #:depth 3 "[2/2] Setting up program.")
  (define alt (make-alt prog))
  (^table^ (make-alt-table (*pcontext*) alt (*output-repr*)))

  ; Add starting alt in every precision
  (when (*pareto-mode*)
    (define alts (starting-alts alt))
    (^table^ (atab-add-altns (^table^) alts (*output-repr*))))
  alt)

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
  (errors-score (errors (alt-program alt) (*pcontext*) (*output-repr*))))

; Pareto mode alt picking
(define (choose-mult-alts from)
  (define altns (filter (compose list? program-body alt-program) from))
  (cond
   [(< (length altns) (*pareto-pick-limit*)) altns] ; take max
   [else
    (define best (argmin score-alt altns))
    (define altns* (sort (filter-not (curry alt-equal? best) altns) < #:key alt-cost))
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
  (debug #:from 'pick #:depth 4 "Picked " picked)
  (void))

;; Invoke the subsystems individually
(define (localize!)
  (unless (^next-alt^)
    (raise-user-error 'localize! "No alt chosen. Run (choose-best-alt!) or (choose-alt! n) to choose one"))
  (timeline-event! 'localize)

  (define vars (program-variables (alt-program (^next-alt^))))
  (define locs-errs (localize-error (alt-program (^next-alt^)) (*output-repr*)))

  ; duplicate locations (already seen)
  (^duplocs^ '())

  ; high-error locations
  (let loop ([locs locs-errs] [count 0])
    (cond
     [(or (null? locs) (= count (*localize-expressions-limit*)))
      (void)]
     [else
      (match-define (cons err loc) (car locs))
      (define expr (location-get loc (alt-program (^next-alt^))))
      (cond
       [(and (*use-improve-cache*)          ; if in cache, put into duplocs
             (patch-table-has-expr? expr))
        (timeline-push! 'locations (~a expr) (errors-score err) #f)
        (^duplocs^ (cons (cons loc expr) (^duplocs^)))
        (loop (cdr locs) (if (*localize-limit-for-new*) count (+ count 1)))]
       [else                                ; else, add to list
        (timeline-push! 'locations (~a expr) (errors-score err) #t)
        (patch-table-add! (^next-alt^) loc #f)
        (loop (cdr locs) (+ count 1))])]))

  ; low-error locations
  (when (*pareto-mode*) ; Pareto mode uses low-error locations
    (for ([(err loc) (in-dict (take-up-to (reverse locs-errs) (*localize-expressions-limit*)))])
      (let ([expr (location-get loc (alt-program (^next-alt^)))])
        (timeline-push! 'locations (~a expr) (errors-score err) #t)
        (patch-table-add! (^next-alt^) loc #t))))

  (void))

; Returns 
(define (gen-cached-alts)
  ;; cached alts have outdated location data
  (define (repair-cached-alt altn loc)
    (let loop ([altn altn])
      (match altn
      [(alt _ _ (list))
        altn]
      [(alt prog (list 'patch _) (list prev))
        (alt prog (list 'patch loc) (list (^next-alt^)))]
      [(alt prog event (list prev))
        (alt prog event (list (loop prev)))])))
    
  (for/fold ([cached '()]) ([(loc expr) (in-dict (^duplocs^))])
    (append cached (repair-cached-alt (patch-table-get expr) loc))))

;; Converts subexpressions back to full alternatives
(define (reconstruct! alts)
  ;; takes a subexpression alt and converts it to a full alt
  (define (reconstruct-alt altn orig)
    (define-values (altn* loc-prefix)
      (let loop ([altn altn])
        (match-define (alt prog event prev) altn)
        (cond
         [(null? prev) (values altn '())]
         [(equal? (car event) 'patch) (values (car prev) (cadr event))]
         [else
          (define-values (altn* loc-prefix) (loop (car prev)))
          (cond
           [(null? loc-prefix) (values altn '())]
           [else
            (define evt*
              (match event
               [(list 'taylor name var loc)
                (list 'taylor name var (append loc-prefix (cdr loc)))]
               [(list 'change cng)
                (match-define (change rule loc binds) cng)
                (list 'change (change rule (append loc-prefix (cdr loc)) binds))]
               [(list 'simplify loc)
                (list 'simplify (append loc-prefix (cdr loc)))]))
            (define prog* (location-do loc-prefix (alt-program orig) (λ (_) (program-body prog))))
            (values (alt prog* evt* (list altn*)) loc-prefix)])])))
    altn*)
  
  (^patched^ (map (curryr reconstruct-alt (^next-alt^)) alts)))

;; Finish iteration
(define (finalize-iter!)
  (unless (^patched^)
    (raise-user-error 'finalize-iter! "No candidates ready for pruning!"))

  (timeline-event! 'prune)
  (define new-alts (^patched^))
  (define orig-fresh-alts (atab-not-done-alts (^table^)))
  (define orig-done-alts (set-subtract (atab-active-alts (^table^)) (atab-not-done-alts (^table^))))
  (^table^ (atab-add-altns (^table^) new-alts (*output-repr*)))
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
  (^table^ (atab-add-altns (^table^) (list (make-alt prog)) (*output-repr*)))
  (void))

(define (finish-iter!)
  (when (not (^next-alt^))
    (debug #:from 'progress #:depth 3 "picking best candidate")
    (choose-best-alt!))
  (when (not (^locs^))
    (debug #:from 'progress #:depth 3 "localizing error")
    (localize!))
  (when (patch-table-runnable?)
    (define patched (patch-table-run))
    (define cached (gen-cached-alts))
    (reconstruct! (append patched cached)))
  (debug #:from 'progress #:depth 3 "adding candidates to table")
  (finalize-iter!)
  (void))

(define (rollback-iter!)
  (^locs^ #f)
  (^lowlocs^ #f)
  (^duplocs^ #f)
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
  (debug #:from 'progress #:depth 3 "Picking candidate(s)")
  (^patched^
    (for/fold ([full '()]) ([picked (choose-alts)] [i (in-naturals 1)])
      (define (picking-func x)
        (for/first ([v x] #:when (alt-equal? v picked)) v))
      (debug #:from 'pick #:depth 4 (format "Picked [~a] " i) picked)
      (define-values (_ table*)
        (atab-pick-alt (^table^) #:picking-func picking-func #:only-fresh #t))
      (^next-alt^ picked)
      (^table^ table*)
      (debug #:from 'progress #:depth 3 "localizing error")
      (localize!)
      (cond
       [(patch-table-runnable?)
        (define patched (patch-table-run))
        (define cached (gen-cached-alts))
        (reconstruct! (append patched cached))
        (append full (^patched^))]
       [else
        full])))
  (debug #:from 'progress #:depth 3 "adding candidates to table")
  (finalize-iter!))
  

(define (run-improve prog iters
                     #:precondition [precondition #f]
                     #:preprocess [preprocess empty]
                     #:precision [precision 'binary64]
                     #:specification [specification #f])
  (debug #:from 'progress #:depth 1 "[Phase 1 of 3] Setting up.")
  (setup-prog! prog
               #:specification specification
               #:precondition precondition
               #:preprocess preprocess
               #:precision precision)
  (debug #:from 'progress #:depth 1 "[Phase 2 of 3] Improving.")
  (when (flag-set? 'setup 'simplify)
      (reconstruct! (patch-table-run-simplify (atab-active-alts (^table^))))
      (finalize-iter!)
      (^next-alt^ #f))
  (for ([iter (in-range iters)] #:break (atab-completed? (^table^)))
    (debug #:from 'progress #:depth 2 "iteration" (+ 1 iter) "/" iters)
    (run-iter!)
    (print-warnings))
  (debug #:from 'progress #:depth 1 "[Phase 3 of 3] Extracting.")
  (extract!))

(define (extract!)
  (define repr (*output-repr*))
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
           (equal? (type-name (representation-type repr)) 'real)
           (not (null? (program-variables (alt-program (car all-alts))))))
      (cond
       [(*pareto-mode*)
        (pareto-regimes (sort all-alts < #:key alt-cost) repr (*sampler*))]
       [else
        (define option (infer-splitpoints all-alts repr))
        (list (combine-alts option repr (*sampler*)))])]
     [else
      (list (argmin score-alt all-alts))]))
  (timeline-event! 'simplify)
  (define progss*
    (simplify-batch
      (map (compose program-body alt-program) joined-alts)
      #:rules (*fp-safe-simplify-rules*) #:precompute #t))
  (define cleaned-alts
    (remove-duplicates
      (for/list ([altn joined-alts] [progs progss*])
        (alt `(λ ,(program-variables (alt-program altn)) ,(last progs))
              'final-simplify (list altn)))
      alt-equal?))
  (timeline-event! 'end)

  (define best (argmin score-alt cleaned-alts))
  (*herbie-preprocess* (remove-unecessary-preprocessing best (*herbie-preprocess*)))
  (define rest (filter-not (curry alt-equal? best) cleaned-alts))
  (cons best (sort rest > #:key alt-cost)))
