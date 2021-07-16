#lang racket

(require "common.rkt" "programs.rkt" "points.rkt" "alternative.rkt" "errors.rkt"
         "timeline.rkt" "syntax/rules.rkt" "syntax/types.rkt" "conversions.rkt"
         "core/localize.rkt" "core/taylor.rkt" "core/alt-table.rkt"
         "core/simplify.rkt" "core/matcher.rkt" "core/regimes.rkt" "interface.rkt"
         "syntax/sugar.rkt" "preprocess.rkt" "symmetry.rkt" "sampling.rkt")

(provide (all-defined-out))

;; I'm going to use some global state here to make the shell more
;; friendly to interact with without having to store your own global
;; state in the repl as you would normally do with debugging. This is
;; probably a bad idea, and I might change it back later. When
;; extending, make sure this never gets too complicated to fit in your
;; head at once, because then global state is going to mess you up.

(struct shellstate
  (table next-alt locs lowlocs gened-series gened-rewrites gened-simplify simplified)
  #:mutable)

(define ^shell-state^ (make-parameter (shellstate #f #f #f #f #f #f #f #f)))

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

;; Keep track of state for (finish-iter!)
(define (^gened-series^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-gened-series! (^shell-state^) newval))
  (shellstate-gened-series (^shell-state^)))
(define (^gened-rewrites^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-gened-rewrites! (^shell-state^) newval))
  (shellstate-gened-rewrites (^shell-state^)))
(define (^gened-simplify^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-gened-simplify! (^shell-state^) newval))
  (shellstate-gened-simplify (^shell-state^)))

(define *sampler* (make-parameter #f))
(define *pareto-pick-limit* (make-parameter 5))

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
  (define-values (locs lowlocs)
    (localize-error (alt-program (^next-alt^)) (*output-repr*)))

  (for ([(err loc) (in-dict locs)])
    (timeline-push! 'locations
                    (~a (location-get loc (alt-program (^next-alt^))))
                    (errors-score err)))
  (^locs^ (map cdr locs))

  (when (*pareto-mode*) ; Pareto mode uses low-error locations
    (for ([(err loc) (in-dict lowlocs)])
      (timeline-push! 'locations
                      (~a (location-get loc (alt-program (^next-alt^))))
                      (errors-score err))))
  (^lowlocs^ (map cdr (if (*pareto-mode*) lowlocs (list))))
  (void))

(define transforms-to-try
  (let ([invert-x (λ (x) `(/ 1 ,x))] [exp-x (λ (x) `(exp ,x))] [log-x (λ (x) `(log ,x))]
	[ninvert-x (λ (x) `(/ 1 (neg ,x)))])
    `((0 ,identity ,identity)
      (inf ,invert-x ,invert-x)
      (-inf ,ninvert-x ,ninvert-x)
      #;(exp ,exp-x ,log-x)
      #;(log ,log-x ,exp-x))))

; taylor uses older format, resugaring and desugaring needed
; not all taylor transforms are valid in a given repr, return false on failure
(define (taylor-expr expr repr var f finv)
  (define expr* (resugar-program expr repr #:full #f))
  (with-handlers ([exn:fail? (const #f)]) ; in case taylor fails
    (define genexpr (approximate expr* var #:transform (cons f finv)))
    (λ (x) (desugar-program (genexpr) repr (*var-reprs*) #:full #f))))

(define (exact-min x y)
  (if (<= x y) x y))

(define (much-< x y)
  (< x (/ y 2)))

;; Taylor is problematic since it doesn't know what reprs are
;; There are two types of errors that occur due to this inconsistency
;;  - reduce:
;;      the internal simplifier will try to desugar an subexpression
;;      with an operator/precision mismatch
;;  - external:
;;      Taylor is successful in generating an expression but
;;      external desugaring fails because of an unsupported/mismatched
;;      operator
(define (taylor-fail-desugaring expr)
  (λ _
    (debug #:from 'progress #:depth 5 "Series expansion (desugaring failure)")
    (debug #:from 'progress #:depth 5 "Problematic expression: " expr)
    #f))

(define (taylor-alt altn loc)
  (define expr (location-get loc (alt-program altn)))
  (define repr (repr-of expr (*output-repr*) (*var-reprs*)))
  (define vars (free-variables expr))
  (reap [sow]
    (for* ([var vars] [transform-type transforms-to-try])
      (match-define (list name f finv) transform-type)
      (define genexpr (taylor-expr expr repr var f finv))
      (cond
       [genexpr  ; taylor successful
        #;(define pts (for/list ([(p e) (in-pcontext (*pcontext*))]) p))
        (let loop ([last (for/list ([(p e) (in-pcontext (*pcontext*))]) +inf.0)] [i 0])
          (define expr*
            (with-handlers ([exn:fail? (taylor-fail-desugaring expr)]) ; failed on desugaring
              (location-do loc (alt-program altn) genexpr)))
          (when expr*
            (define errs (errors expr* (*pcontext*) (*output-repr*)))
            (define altn* (alt expr* `(taylor ,name ,loc) (list altn)))
            (when (ormap much-< errs last)
              #;(eprintf "Better on ~a\n" (ormap (λ (pt x y) (and (much-< x y) (list pt x y))) pts errs last))
              (sow altn*)
              (when (< i 3)
                (loop (map exact-min errs last) (+ i 1))))))]
       [else  ; taylor failed
        (debug #:from 'progress #:depth 5 "Series expansion (internal failure)")
        (debug #:from 'progress #:depth 5 "Problematic expression: " expr)
        (sow altn)]))))

(define (gen-series!)
  (unless (^locs^)
    (raise-user-error 'gen-series! "No locations selected. Run (localize!) or modify (^locs^)"))
  (^gened-series^ '())
  (when (flag-set? 'generate 'taylor)
    (timeline-event! 'series)

    (define series-expansions
      (apply
       append
       (for/list ([location (^locs^)] [n (in-naturals 1)])
         (debug #:from 'progress #:depth 4 "[" n "/" (length (^locs^)) "] generating series at" location)
         (define tnow (current-inexact-milliseconds))
         (begin0
             (taylor-alt (^next-alt^) location)
           (timeline-push! 'times
                           (~a (location-get location (alt-program (^next-alt^))))
                           (- (current-inexact-milliseconds) tnow))))))
    
    (timeline-push! 'count (length (^locs^)) (length series-expansions))

    (^gened-series^ series-expansions)
    (define table* (atab-add-altns (^table^) (^gened-series^) (*output-repr*)))
    (timeline-push! 'min-error (errors-score (atab-min-errors table*))))
  (void))

(define (gen-rewrites!)
  (unless (^locs^)
    (raise-user-error 'gen-rewrites! "No locations selected. Run (localize!) or modify (^locs^)"))
  (^gened-rewrites^ '())

  (timeline-event! 'rewrite)
  (define rewrite (if (flag-set? 'generate 'rr) rewrite-expression-head rewrite-expression))
  (timeline-push! 'method (~a (object-name rewrite)))
  (define altn (alt-add-event (^next-alt^) '(start rm)))

  (define changelists
    (apply append
	   (for/list ([location (^locs^)] [n (in-naturals 1)])
	     (debug #:from 'progress #:depth 4 "[" n "/" (length (^locs^)) "] rewriting at" location)
             (define tnow (current-inexact-milliseconds))
             (define expr (location-get location (alt-program altn)))
             (begin0 (rewrite expr (*output-repr*) #:rules (*rules*) #:root location)
               (timeline-push! 'times (~a expr) (- (current-inexact-milliseconds) tnow))))))

  (define reprchange-rules
    (if (*pareto-mode*)
        (filter (λ (r) (expr-contains? (rule-output r) rewrite-repr-op?)) (*rules*))
        (list)))

  ; Empty in normal mode
  (define changelists-low-locs
    (apply append
      (for/list ([location (^lowlocs^)] [n (in-naturals 1)])
        (debug #:from 'progress #:depth 4 "[" n "/" (length (^lowlocs^)) "] rewriting at" location)
              (define tnow (current-inexact-milliseconds))
              (define expr (location-get location (alt-program altn)))
              (begin0 (rewrite expr (*output-repr*) #:rules reprchange-rules #:root location)
                (timeline-push! 'times (~a expr) (- (current-inexact-milliseconds) tnow))))))

  (define changelists* (append changelists changelists-low-locs))
  (define rules-used
    (append-map (curry map change-rule) changelists*))
  (define rule-counts
    (for ([rgroup (group-by identity rules-used)])
      (timeline-push! 'rules (~a (rule-name (first rgroup))) (length rgroup))))

  (define (repr-rewrite-alt altn)
    (alt (apply-repr-change (alt-program altn)) (alt-event altn) (alt-prevs altn)))

  (define rewritten
    (filter (λ (altn) (program-body (alt-program altn)))
      (map repr-rewrite-alt
        (for/list ([cl changelists*])
          (for/fold ([altn altn]) ([cng cl])
            (alt (change-apply cng (alt-program altn)) (list 'change cng) (list altn)))))))

  (define rewritten*
    (if (and (*pareto-mode*) (> (length rewritten) 1000))
        (take rewritten 1000)
        rewritten))
        
  (timeline-push! 'count (length (^locs^)) (length rewritten*))

  (^gened-rewrites^ rewritten*)
  (define table* (atab-add-altns (^table^) (^gened-rewrites^) (*output-repr*)))
  (timeline-push! 'min-error (errors-score (atab-min-errors table*)))
  (void))

(define (simplify!)
  (unless (or (^gened-series^) (^gened-rewrites^))
    (raise-user-error 'simplify! "No candidates generated. Run (gen-series!) or (gen-rewrites!)"))
  (^gened-simplify^ '())

  (when (flag-set? 'generate 'simplify)
    (timeline-event! 'simplify)
    (define children (append (or (^gened-series^) empty) (or (^gened-rewrites^) empty)))

    (define locs-list
      (for/list ([child (in-list children)] [n (in-naturals 1)])
        ;; We want to avoid simplifying if possible, so we only
        ;; simplify things produced by function calls in the rule
        ;; pattern. This means no simplification if the rule output as
        ;; a whole is not a function call pattern, and no simplifying
        ;; subexpressions that don't correspond to function call
        ;; patterns.
        (match (alt-event child)
          [(list 'taylor _ loc) (list loc)]
          [(list 'change cng)
           (match-define (change rule loc _) cng)
           (define pattern (rule-output rule))
           (define expr (location-get loc (alt-program child)))
           (cond
            [(not (list? pattern)) '()]
            [else
             (for/list ([pos (in-naturals 1)]
                        [arg-pattern (cdr pattern)] #:when (list? arg-pattern))
               (append (change-location cng) (list pos)))])]
          [_ (list '(2))])))

    (define to-simplify
      (for/list ([child (in-list children)] [locs locs-list]
                 #:when true [loc locs])
        (location-get loc (alt-program child))))

    (define simplification-options
      (simplify-batch to-simplify #:rules (*simplify-rules*) #:precompute true))

    (define simplify-hash
      (make-immutable-hash (map cons to-simplify simplification-options)))

    (define simplified
      (apply append
             (for/list ([child (in-list children)] [locs locs-list])
               (make-simplification-combinations child locs simplify-hash))))

    (timeline-push! 'count (length locs-list) (length simplified))

    (^gened-simplify^ simplified))
  (void))


;; Finish iteration
(define (finalize-iter!)
  (unless (^gened-simplify^)
    (raise-user-error 'finalize-iter! "No candidates simplified. Run (simplify!)"))

  (timeline-event! 'prune)
  (define new-alts (^gened-simplify^))
  (define orig-fresh-alts (atab-not-done-alts (^table^)))
  (define orig-done-alts (set-subtract (atab-active-alts (^table^)) (atab-not-done-alts (^table^))))
  (^table^ (atab-add-altns (^table^) (^gened-simplify^) (*output-repr*)))
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
  (when (not (^gened-series^))
    (debug #:from 'progress #:depth 3 "generating series expansions")
    (gen-series!))
  (when (not (^gened-rewrites^))
    (debug #:from 'progress #:depth 3 "generating rewritten candidates")
    (gen-rewrites!))
  (when (not (^gened-simplify^))
    (debug #:from 'progress #:depth 3 "simplifying candidates")
    (simplify!))
  (debug #:from 'progress #:depth 3 "adding candidates to table")
  (finalize-iter!)
  (void))

(define (rollback-iter!)
  (^locs^ #f)
  (^next-alt^ #f)
  (^gened-rewrites^ #f)
  (^gened-series^ #f)
  (^gened-simplify^ #f)
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
  (define-values (rewritten series)
    (for/fold ([rewritten '()] [series '()])
              ([picked (choose-alts)] [i (in-naturals 1)])
      (debug #:from 'pick #:depth 4 (format "Picked [~a] " i) picked)
      (define (picking-func x)
        (for/first ([v x] #:when (alt-equal? v picked)) v))
      (define-values (_ table*)
        (atab-pick-alt (^table^) #:picking-func picking-func #:only-fresh #t))
      (^next-alt^ picked)
      (^table^ table*)

      (debug #:from 'progress #:depth 3 "localizing error")
      (localize!)
      (debug #:from 'progress #:depth 3 "generating rewritten candidates")
      (gen-rewrites!)
      (debug #:from 'progress #:depth 3 "generating series expansions")
      (gen-series!)
      (values (append (^gened-rewrites^) rewritten)
              (append (^gened-series^) series))))
  (^gened-rewrites^ rewritten)
  (^gened-series^ series)
  (debug #:from 'progress #:depth 3 "simplifying candidates")
  (simplify!)
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
    (^gened-rewrites^ (atab-active-alts (^table^)))
    (simplify!)
    (finalize-iter!))
  (for ([iter (in-range iters)] #:break (atab-completed? (^table^)))
    (debug #:from 'progress #:depth 2 "iteration" (+ 1 iter) "/" iters)
    (run-iter!))
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
