#lang racket

(require "rules.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "alt-table.rkt"
         "bsearch.rkt"
         "egg-herbie.rkt"
         "localize.rkt"
         "regimes.rkt"
         "simplify.rkt"
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
         "explain.rkt"
         "patch.rkt"
         "../syntax/platform.rkt"
         "points.rkt"
         "preprocess.rkt"
         "programs.rkt"
         "../utils/timeline.rkt"
         "sampling.rkt"
         "soundiness.rkt")
(provide (all-defined-out))

;; I'm going to use some global state here to make the shell more
;; friendly to interact with without having to store your own global
;; state in the repl as you would normally do with debugging. This is
;; probably a bad idea, and I might change it back later. When
;; extending, make sure this never gets too complicated to fit in your
;; head at once, because then global state is going to mess you up.

(struct shellstate (table next-alts locs patched) #:mutable)
(define-resetter ^shell-state^ (λ () (shellstate #f #f #f #f)) (λ () (shellstate #f #f #f #f)))

(define (^locs^ [newval 'none])
  (when (not (equal? newval 'none))
    (set-shellstate-locs! (^shell-state^) newval))
  (shellstate-locs (^shell-state^)))
(define (^table^ [newval 'none])
  (when (not (equal? newval 'none))
    (set-shellstate-table! (^shell-state^) newval))
  (shellstate-table (^shell-state^)))
(define (^next-alts^ [newval 'none])
  (when (not (equal? newval 'none))
    (set-shellstate-next-alts! (^shell-state^) newval))
  (shellstate-next-alts (^shell-state^)))
(define (^patched^ [newval 'none])
  (when (not (equal? newval 'none))
    (set-shellstate-patched! (^shell-state^) newval))
  (shellstate-patched (^shell-state^)))

;; These high-level functions give the high-level workflow of Herbie:
;; - First, set up a context by sampling input points
;; - Then, do some initial steps: preprocessing, explain, and initialize the alt table
;; - Then, in a loop, choose some alts, localize, run the patch table, and finalize
;; - Then do regimes, final simplify, add soundiness, and remove preprocessing

(define (setup-context! vars specification precondition repr)
  (*context* (context vars repr (map (const repr) vars)))
  (define sample (sample-points precondition (list specification) (list (*context*))))
  (match-define (cons domain pts+exs) sample)
  (cons domain (apply mk-pcontext pts+exs)))

(define (run-improve! initial specification context pcontext)
  (timeline-event! 'preprocess)
  (define-values (simplified preprocessing) (find-preprocessing initial specification context))
  (timeline-push! 'symmetry (map ~a preprocessing))
  (define pcontext* (preprocess-pcontext context pcontext preprocessing))
  (match-define (and alternatives (cons (alt best _ _ _) _))
    (mutate! simplified context pcontext* (*num-iterations*)))
  (timeline-event! 'preprocess)
  (define final-alts
    (for/list ([altern alternatives])
      (alt-add-preprocessing
       altern
       (remove-unnecessary-preprocessing best context pcontext (alt-preprocessing altern)))))
  (values final-alts (remove-unnecessary-preprocessing best context pcontext preprocessing)))

(define (mutate! simplified context pcontext iterations)
  (*pcontext* pcontext)
  (explain! simplified context pcontext)
  (initialize-alt-table! simplified context pcontext)
  (for ([iteration (in-range iterations)] #:break (atab-completed? (^table^)))
    (run-iter!))
  (extract!))

(define (run-iter!)
  (when (^next-alts^)
    (raise-user-error 'run-iter!
                      "An iteration is already in progress\n~a"
                      "Run (finish-iter!) to finish it, or (rollback-iter!) to abandon it.\n"))

  (choose-alts!)
  (localize!)
  (reconstruct! (patch-table-run (^locs^)))
  (finalize-iter!))

(define (extract!)
  (timeline-push-alts! '())

  (define all-alts (atab-all-alts (^table^)))
  (define joined-alts (make-regime! all-alts))
  (define cleaned-alts (final-simplify! joined-alts))
  (define annotated-alts (add-soundness! cleaned-alts))

  (timeline-push! 'stop (if (atab-completed? (^table^)) "done" "fuel") 1)
  (sort-alts annotated-alts))

;; The next few functions are for interactive use in a REPL, usually for debugging
;; In Emacs, you can install racket-mode and then use C-c C-k to start that REPL

(define (list-alts)
  (printf "Key: [.] = done, [>] = chosen\n")
  (let ([ndone-alts (atab-not-done-alts (^table^))])
    (for ([alt (atab-active-alts (^table^))] [n (in-naturals)])
      (printf "~a ~a ~a\n"
              (cond
                [(set-member? (^next-alts^) alt) ">"]
                [(set-member? ndone-alts alt) " "]
                [else "."])
              (~r #:min-width 4 n)
              (alt-expr alt))))
  (printf "Error: ~a bits\n" (errors-score (atab-min-errors (^table^)))))

(define (choose-alt! n)
  (unless (< n (length (atab-active-alts (^table^))))
    (raise-user-error 'choose-alt!
                      "Couldn't select the ~ath alt of ~a (not enough alts)"
                      n
                      (length (atab-active-alts (^table^)))))
  (define picked (list-ref (atab-active-alts (^table^)) n))
  (^next-alts^ (list picked))
  (^table^ (atab-set-picked (^table^) (^next-alts^)))
  (void))

(define (inject-candidate! expr)
  (define new-alts (list (make-alt expr)))
  (define-values (errss costs) (atab-eval-altns (^table^) new-alts (*context*)))
  (^table^ (atab-add-altns (^table^) new-alts errss costs))
  (void))

(define (rollback-improve!)
  (rollback-iter!)
  (reset!)
  (^table^ #f)
  (void))

;; The rest of the file is various helper / glue functions used by
;; Herbie. These often wrap other Herbie components, but add logging
;; and timeline data.

(define (score-alt alt)
  (errors-score (errors (alt-expr alt) (*pcontext*) (*context*))))

; Pareto mode alt picking
(define (choose-mult-alts altns)
  (define repr (context-repr (*context*)))
  (cond
    [(< (length altns) (*pareto-pick-limit*)) altns] ; take max
    [else
     (define best (argmin score-alt altns))
     (define altns* (sort (filter-not (curry alt-equal? best) altns) < #:key (curryr alt-cost repr)))
     (define simplest (car altns*))
     (define altns** (cdr altns*))
     (define div-size (round (/ (length altns**) (- (*pareto-pick-limit*) 1))))
     (append (list best simplest)
             (for/list ([i (in-range 1 (- (*pareto-pick-limit*) 1))])
               (list-ref altns** (- (* i div-size) 1))))]))

(define (timeline-push-alts! picked-alts)
  (define fresh-alts (atab-not-done-alts (^table^)))
  (define repr (context-repr (*context*)))
  (for ([alt (atab-active-alts (^table^))])
    (timeline-push! 'alts
                    (~a (alt-expr alt))
                    (cond
                      [(set-member? picked-alts alt) "next"]
                      [(set-member? fresh-alts alt) "fresh"]
                      [else "done"])
                    (score-alt alt)
                    (~a (representation-name repr)))))

(define (choose-alts!)
  (define fresh-alts (atab-not-done-alts (^table^)))
  (define alts (choose-mult-alts fresh-alts))
  (unless (*pareto-mode*)
    (set! alts (take alts 1)))
  (timeline-push-alts! alts)
  (^next-alts^ alts)
  (^table^ (atab-set-picked (^table^) alts))
  (void))

;; Invoke the subsystems individually
(define (localize!)
  (unless (^next-alts^)
    (raise-user-error 'localize!
                      "No alt chosen. Run (choose-alts!) or (choose-alt! n) to choose one"))

  (timeline-event! 'simplify)
  (define exprs (map alt-expr (^next-alts^)))
  (define localized-exprs empty)
  (define repr (context-repr (*context*)))

  (when (flag-set? 'localize 'costs)
    (define loc-costss (batch-localize-costs exprs (*context*)))
    (define cost-localized
      (for/list ([loc-costs (in-list loc-costss)]
                 #:when true
                 [(cost-diff expr) (in-dict loc-costs)]
                 [_ (in-range (*localize-expressions-limit*))])
        (timeline-push! 'locations
                        (~a expr)
                        "cost-diff"
                        cost-diff
                        (not (patch-table-has-expr? expr))
                        (~a (representation-name repr)))
        expr))
    (set! localized-exprs (remove-duplicates (append localized-exprs cost-localized))))

  (timeline-event! 'localize)
  (when (flag-set? 'localize 'errors)
    (define loc-errss (batch-localize-errors exprs (*context*)))
    ;;Timeline will push duplicates
    (define error-localized
      (for/list ([loc-errs (in-list loc-errss)]
                 #:when true
                 [(err expr) (in-dict loc-errs)]
                 [_ (in-range (*localize-expressions-limit*))])
        (timeline-push! 'locations
                        (~a expr)
                        "accuracy"
                        (errors-score err)
                        (not (patch-table-has-expr? expr))
                        (~a (representation-name repr)))
        expr))
    (set! localized-exprs (remove-duplicates (append localized-exprs error-localized))))

  (^locs^ localized-exprs)
  (void))

;; Returns the locations of `subexpr` within `expr`
(define (get-locations expr subexpr)
  (reap [sow]
        (let loop ([expr expr] [loc '()])
          (match expr
            [(== subexpr) (sow (reverse loc))]
            [(list _ args ...)
             (for ([arg (in-list args)] [i (in-naturals 1)])
               (loop arg (cons i loc)))]
            [_ (void)]))))

;; Converts a patch to full alt with valid history
(define (reconstruct! alts)
  ;; extracts the base expression of a patch
  (define (get-starting-expr altn)
    (match* ((alt-event altn) (alt-prevs altn))
      [((list 'patch expr _) _) expr]
      [(_ (list prev)) (get-starting-expr prev)]
      [(_ _) (error 'get-starting-spec "unexpected: ~a" altn)]))

  ;; takes a patch and converts it to a full alt
  (define (reconstruct-alt altn loc0 orig)
    (let loop ([altn altn])
      (match-define (alt _ event prevs _) altn)
      (match event
        [(list 'patch _ _) orig]
        [_
         (define event*
           (match event
             [(list 'taylor name var) (list 'taylor loc0 name var)]
             [(list 'rr input proof soundiness) (list 'rr loc0 input proof soundiness)]
             [(list 'simplify input proof soundiness) (list 'simplify loc0 input proof soundiness)]))
         (define expr* (location-do loc0 (alt-expr orig) (const (alt-expr altn))))
         (alt expr* event* (list (loop (first prevs))) (alt-preprocessing orig))])))

  (^patched^ (reap [sow]
                   (for ([altn (in-list alts)]) ;; does not have preproc
                     (define start-expr (get-starting-expr altn))
                     (if start-expr
                         (for ([full-altn (in-list (^next-alts^))])
                           (define expr (alt-expr full-altn))
                           (for ([loc (in-list (get-locations expr start-expr))])
                             (sow (reconstruct-alt altn loc full-altn))))
                         ; altn is a full alt (probably iter 0 simplify)
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
    (hash 'new
          (list (length new-alts) (length (set-intersect new-alts final-fresh-alts)))
          'fresh
          (list (length orig-fresh-alts) (length (set-intersect orig-fresh-alts final-fresh-alts)))
          'done
          (list (- (length orig-done-alts) (length (or (^next-alts^) empty)))
                (- (length (set-intersect orig-done-alts final-done-alts))
                   (length (set-intersect final-done-alts (or (^next-alts^) empty)))))
          'picked
          (list (length (or (^next-alts^) empty))
                (length (set-intersect final-done-alts (or (^next-alts^) empty))))))
  (timeline-push! 'kept data)

  (define repr (context-repr (*context*)))
  (timeline-push! 'min-error
                  (errors-score (atab-min-errors (^table^)))
                  (format "~a" (representation-name repr)))
  (rollback-iter!)
  (void))

(define (finish-iter!)
  (unless (^next-alts^)
    (choose-alts!))
  (unless (^locs^)
    (localize!))
  (reconstruct! (patch-table-run (^locs^)))
  (finalize-iter!)
  (void))

(define (rollback-iter!)
  (^locs^ #f)
  (^next-alts^ #f)
  (^patched^ #f)
  (void))

(define (initialize-alt-table! alternatives context pcontext)
  (match-define (cons initial simplified) alternatives)
  (*start-prog* (alt-expr initial))
  (define table (make-alt-table pcontext initial context))
  (timeline-event! 'eval)
  (define-values (errss costs) (atab-eval-altns table simplified context))
  (timeline-event! 'prune)
  (^table^ (atab-add-altns table simplified errss costs)))

(define (explain! simplified context pcontext)
  (timeline-event! 'explain)
  (define expr (alt-expr (car simplified)))

  (define-values (fperrors
                  explanations-table
                  confusion-matrix
                  maybe-confusion-matrix
                  total-confusion-matrix
                  freqs)
    (explain expr (*context*) (*pcontext*)))

  (for ([fperror (in-list fperrors)])
    (match-define (list expr truth opreds oex upreds uex) fperror)
    (timeline-push! 'fperrors expr truth opreds oex upreds uex))

  (for ([explanation (in-list explanations-table)])
    (match-define (list op expr expl val maybe-count flow-list) explanation)
    (timeline-push! 'explanations op expr expl val maybe-count flow-list))

  (timeline-push! 'confusion confusion-matrix)

  (timeline-push! 'maybe-confusion maybe-confusion-matrix)

  (timeline-push! 'total-confusion total-confusion-matrix)
  (for ([(key val) (in-dict freqs)])
    (timeline-push! 'freqs key val)))

(define (make-regime! alts)
  (define ctx (*context*))
  (define repr (context-repr ctx))

  (cond
    [(and (flag-set? 'reduce 'regimes)
          (> (length alts) 1)
          (equal? (representation-type repr) 'real)
          (not (null? (context-vars ctx))))
     (define opts (pareto-regimes (sort alts < #:key (curryr alt-cost repr)) ctx))
     (for/list ([opt (in-list opts)])
       (combine-alts opt ctx))]
    [else
     (list (argmin score-alt alts))]))

(define (final-simplify! alts)
  (cond
    [(flag-set? 'generate 'simplify)
     (timeline-event! 'simplify)
     
     (define exprs (map alt-expr alts))
     (define reprs (map (lambda (expr) (repr-of expr (*context*))) exprs))

     ; egg schedule (only mathematical rewrites)
     (define rules (real-rules (*rules*)))
     (define lifting-rules (platform-lifting-rules))
     (define schedule
       `((,lifting-rules . ((iteration . 1) (scheduler . simple)))
         (,rules . ((node . ,(*node-limit*)) (const-fold? . #f)))))

     ; egg runner
     (define runner (make-egg-runner exprs reprs schedule))

     ; run egg
     (define simplified
       (map last
            (simplify-batch
              runner
              (typed-egg-extractor
                (if (*egraph-platform-cost*)
                    platform-egg-cost-proc
                    default-egg-cost-proc)))))

     ; de-duplication
     (remove-duplicates
        (for/list ([altn (in-list alts)] [prog (in-list simplified)])
          (if (equal? (alt-expr altn) prog)
              altn
              (alt prog 'final-simplify (list altn) (alt-preprocessing altn))))
        alt-equal?)]
    [else alts]))

(define (add-soundness! alts)
  (cond
    [(flag-set? 'generate 'proofs)
     (timeline-event! 'soundness)
     (add-soundiness alts (*pcontext*) (*context*))]
    [else alts]))

(define (sort-alts alts)
  (define repr (context-repr (*context*)))
  ;; find the best, sort the rest by cost
  (define errss (batch-errors (map alt-expr alts) (*pcontext*) (*context*)))
  (define best (car (argmin (compose errors-score cdr) (map cons alts (flip-lists errss)))))
  (cons best (sort (set-remove alts best) > #:key (curryr alt-cost repr))))