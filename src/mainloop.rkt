#lang racket

(require "common.rkt" "programs.rkt" "points.rkt" "alternative.rkt" "errors.rkt"
         "timeline.rkt" "core/localize.rkt" "core/taylor.rkt" "core/alt-table.rkt"
         "core/simplify.rkt" "core/matcher.rkt" "core/regimes.rkt" "interface.rkt"
         "type-check.rkt") ;; For taylor not running on complex exprs

(provide (all-defined-out))

;; I'm going to use some global state here to make the shell more
;; friendly to interact with without having to store your own global
;; state in the repl as you would normally do with debugging. This is
;; probably a bad idea, and I might change it back later. When
;; extending, make sure this never gets too complicated to fit in your
;; head at once, because then global state is going to mess you up.

(struct shellstate
  (table next-alt locs children gened-series gened-rewrites simplified precondition precision)
  #:mutable)

(define ^shell-state^ (make-parameter (shellstate #f #f #f #f #f #f #f 'TRUE #f)))

(define (^locs^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-locs! (^shell-state^) newval))
  (shellstate-locs (^shell-state^)))
(define (^table^ [newval 'none])
  (when (not (equal? newval 'none))  (set-shellstate-table! (^shell-state^) newval))
  (shellstate-table (^shell-state^)))
(define (^next-alt^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-next-alt! (^shell-state^) newval))
  (shellstate-next-alt (^shell-state^)))
(define (^children^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-children! (^shell-state^) newval))
  (shellstate-children (^shell-state^)))
(define (^precondition^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-precondition! (^shell-state^) newval))
  (shellstate-precondition (^shell-state^)))
(define (^precision^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-precision! (^shell-state^) newval))
  (shellstate-precision (^shell-state^)))

;; Keep track of state for (finish-iter!)
(define (^gened-series^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-gened-series! (^shell-state^) newval))
  (shellstate-gened-series (^shell-state^)))
(define (^gened-rewrites^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-gened-rewrites! (^shell-state^) newval))
  (shellstate-gened-rewrites (^shell-state^)))
(define (^simplified^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-simplified! (^shell-state^) newval))
  (shellstate-simplified (^shell-state^)))

(define (check-unused-variables vars precondition expr)
  ;; Fun story: you might want variables in the precondition that
  ;; don't appear in the `expr`, because that can allow you to do
  ;; non-uniform sampling. For example, if you have the precondition
  ;; `(< x y)`, where `y` is otherwise unused, then `x` is sampled
  ;; non-uniformly (biased toward small values).
  (define used (set-union (free-variables expr) (free-variables precondition)))
  (unless (set=? vars used)
    (define unused (set-subtract vars used))
    (warn 'unused-variable
          "unused ~a ~a" (if (equal? (set-count unused) 1) "variable" "variables")
          (string-join (map ~a unused) ", "))))

;; Setting up
(define (setup-prog! prog
                     #:precondition [precondition 'TRUE]
                     #:precision [precision 'binary64]
                     #:specification [specification #f])
  (*output-prec* precision)
  ;; TODO(interface): when the syntax checker is udpated, set *var-precs* too
  (*start-prog* prog)
  (rollback-improve!)
  (check-unused-variables (program-variables prog) precondition (program-body prog))

  (debug #:from 'progress #:depth 3 "[1/2] Preparing points")
  (timeline-event! 'sample)
  ;; If the specification is given, it is used for sampling points
  (define context (prepare-points (or specification prog) precondition precision))
  (^precondition^ precondition)
  (^precision^ precision)
  (*pcontext* context)
  (debug #:from 'progress #:depth 3 "[2/2] Setting up program.")
  (define alt (make-alt prog))
  (^table^ (make-alt-table context alt precision))
  alt)

;; Information
(define (list-alts)
  (printf "Key: [.] = done; [>] = chosen\n")
  (let ([ndone-alts (atab-not-done-alts (^table^))])
    (for ([alt (atab-all-alts (^table^))]
	  [n (in-naturals)])
      (printf "~a ~a ~a\n"
       (cond [(equal? alt (^next-alt^)) ">"]
             [(set-member? ndone-alts alt) " "]
             [else "."])
       n
       alt))))

;; Begin iteration
(define (choose-alt! n)
  (if (>= n (length (atab-all-alts (^table^))))
      (printf "We don't have that many alts!\n")
      (let-values ([(picked table*) (atab-pick-alt (^table^) #:picking-func (curryr list-ref n)
						   #:only-fresh #f)])
	(^next-alt^ picked)
	(^table^ table*)
	(void))))

(define (best-alt alts prec)
  (define repr (get-representation prec))
  (argmin (λ (alt) (errors-score (errors (alt-program alt) (*pcontext*) repr)))
		   alts))

(define (choose-best-alt!)
  (let-values ([(picked table*) (atab-pick-alt (^table^)
                                  #:picking-func (curryr best-alt (*output-prec*))
                                  #:only-fresh #t)])
    (^next-alt^ picked)
    (^table^ table*)
    (debug #:from 'pick #:depth 4 "Picked " picked)
    (void)))

;; Invoke the subsystems individually
(define (localize!)
  (timeline-event! 'localize)
  (define locs (localize-error (alt-program (^next-alt^)) (*output-prec*)))
  (for/list ([(err loc) (in-dict locs)])
    (timeline-push! 'locations
                    (location-get loc (alt-program (^next-alt^)))
                    (errors-score err)))
  (^locs^ (map cdr locs))
  (void))

(define transforms-to-try
  (let ([invert-x (λ (x) `(/ 1 ,x))] [exp-x (λ (x) `(exp ,x))] [log-x (λ (x) `(log ,x))]
	[ninvert-x (λ (x) `(/ 1 (- ,x)))])
    `((0 ,identity ,identity)
      (inf ,invert-x ,invert-x)
      (-inf ,ninvert-x ,ninvert-x)
      #;(exp ,exp-x ,log-x)
      #;(log ,log-x ,exp-x))))

(define (taylor-alt altn loc)
  (define expr (location-get loc (alt-program altn)))
  (define vars (free-variables expr))
  (if (or (null? vars) ;; `approximate` cannot be called with a null vars list
          (not (equal? (type-of expr (for/hash ([var vars]) (values var 'real))) 'real)))
      (list altn)
      (for/list ([transform-type transforms-to-try])
        (match-define (list name f finv) transform-type)
        (define transformer (map (const (cons f finv)) vars))
        (alt
         (location-do loc (alt-program altn) (λ (expr) (approximate expr vars #:transform transformer)))
         `(taylor ,name ,loc)
         (list altn)))))

(define (gen-series!)
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
                           (location-get location (alt-program (^next-alt^)))
                           (- (current-inexact-milliseconds) tnow))))))
    
    (timeline-log! 'inputs (length (^locs^)))
    (timeline-log! 'outputs (length series-expansions))

    (^children^ (append (^children^) series-expansions)))
  (^gened-series^ #t)
  (void))

(define (gen-rewrites!)
  (timeline-event! 'rewrite)
  (define rewrite (if (flag-set? 'generate 'rr) rewrite-expression-head rewrite-expression))
  (timeline-log! 'method (object-name rewrite))
  (define altn (alt-add-event (^next-alt^) '(start rm)))

  (define changelists
    (apply append
	   (for/list ([location (^locs^)] [n (in-naturals 1)])
	     (debug #:from 'progress #:depth 4 "[" n "/" (length (^locs^)) "] rewriting at" location)
             (define tnow (current-inexact-milliseconds))
             (define expr (location-get location (alt-program altn)))
             (begin0 (rewrite expr #:root location)
               (timeline-push! 'times expr (- (current-inexact-milliseconds) tnow))))))

  (define rules-used
    (append-map (curry map change-rule) changelists))
  (define rule-counts
    (sort
     (hash->list
      (for/hash ([rgroup (group-by identity rules-used)])
        (values (rule-name (first rgroup)) (length rgroup))))
     > #:key cdr))

  (define rewritten
    (for/list ([cl changelists])
      (for/fold ([altn altn]) ([cng cl])
        (alt (change-apply cng (alt-program altn)) (list 'change cng) (list altn)))))

  (timeline-log! 'inputs (length (^locs^)))
  (timeline-log! 'rules rule-counts)
  (timeline-log! 'outputs (length rewritten))

  (^children^ (append (^children^) rewritten))
  (^gened-rewrites^ #t)
  (void))

(define (num-nodes expr)
  (if (not (list? expr)) 1
      (add1 (apply + (map num-nodes (cdr expr))))))

(define (simplify!)
  (when (flag-set? 'generate 'simplify)
    (timeline-event! 'simplify)

    (define locs-list
      (for/list ([child (^children^)] [n (in-naturals 1)])
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
      (for/list ([child (^children^)] [locs locs-list]
                 #:when true [loc locs])
        (location-get loc (alt-program child))))

    (define simplifications
      (simplify-batch to-simplify #:rules (*simplify-rules*)))

    (define simplify-hash
      (make-immutable-hash (map cons to-simplify simplifications)))

    (define simplified
      (for/list ([child (^children^)] [locs locs-list])
        (for/fold ([child child]) ([loc locs])
          (define child* (location-do loc (alt-program child) (λ (expr) (hash-ref simplify-hash expr))))
          (if (not (equal? (alt-program child) child*))
              (alt child* (list 'simplify loc) (list child))
              child))))

    (timeline-log! 'inputs (length locs-list))
    (timeline-log! 'outputs (length simplified))

    (^children^ simplified))
  (^simplified^ #t)
  (void))


;; Finish iteration
(define (finalize-iter!)
  (timeline-event! 'prune)
  (^table^ (atab-add-altns (^table^) (^children^) (*output-prec*)))
  (timeline-log! 'kept-alts (length (atab-not-done-alts (^table^))))
  (timeline-log! 'done-alts (- (length (atab-all-alts (^table^))) (length (atab-not-done-alts (^table^)))))
  (timeline-log! 'min-error (errors-score (atab-min-errors (^table^))))
  (rollback-iter!)
  (void))

(define (inject-candidate! prog)
  (^table^ (atab-add-altns (^table^) (list (make-alt prog)) (*output-prec*)))
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
  (when (not (^simplified^))
    (debug #:from 'progress #:depth 3 "simplifying candidates")
    (simplify!))
  (debug #:from 'progress #:depth 3 "adding candidates to table")
  (finalize-iter!)
  (void))

(define (rollback-iter!)
  (^children^ '())
  (^locs^ #f)
  (^next-alt^ #f)
  (^gened-rewrites^ #f)
  (^gened-series^ #f)
  (^simplified^ #f)
  (void))

(define (rollback-improve!)
  (rollback-iter!)
  (reset!)
  (^table^ #f)
  (void))

;; Run a complete iteration
(define (run-iter!)
  (if (^next-alt^)
      (begin (printf "An iteration is already in progress!\n")
	     (printf "Finish it up manually, or by running (finish-iter!)\n")
	     (printf "Or, you can just run (rollback-iter!) to roll it back and start it over.\n"))
      (begin (debug #:from 'progress #:depth 3 "picking best candidate")
	     (choose-best-alt!)
	     (debug #:from 'progress #:depth 3 "localizing error")
	     (localize!)
	     (debug #:from 'progress #:depth 3 "generating rewritten candidates")
	     (gen-rewrites!)
	     (debug #:from 'progress #:depth 3 "generating series expansions")
	     (gen-series!)
	     (debug #:from 'progress #:depth 3 "simplifying candidates")
	     (simplify!)
	     (debug #:from 'progress #:depth 3 "adding candidates to table")
	     (finalize-iter!)))
  (void))

(define (run-improve prog iters
                     #:precondition [precondition 'TRUE]
                     #:precision [precision 'binary64]
                     #:specification [specification #f])
  (debug #:from 'progress #:depth 1 "[Phase 1 of 3] Setting up.")
  (define repr (get-representation precision))
  (define alt
    (setup-prog! prog #:specification specification #:precondition precondition #:precision precision))
  (cond
   [(and (flag-set? 'setup 'early-exit)
         (< (errors-score (errors (alt-program alt) (*pcontext*) repr)) 0.1))
    (debug #:from 'progress #:depth 1 "Initial program already accurate, stopping.")
    alt]
   [else
    (debug #:from 'progress #:depth 1 "[Phase 2 of 3] Improving.")
    (when (flag-set? 'setup 'simplify)
      (^children^ (atab-all-alts (^table^)))
      (simplify!)
      (finalize-iter!))
    (for ([iter (in-range iters)] #:break (atab-completed? (^table^)))
      (debug #:from 'progress #:depth 2 "iteration" (+ 1 iter) "/" iters)
      (run-iter!))
    (debug #:from 'progress #:depth 1 "[Phase 3 of 3] Extracting.")
    (get-final-combination precision)]))

(define (get-final-combination precision)
  (define all-alts (atab-all-alts (^table^)))
  (*all-alts* all-alts)
  (define joined-alt
    (cond
     [(and (flag-set? 'reduce 'regimes) (> (length all-alts) 1))
      (timeline-event! 'regimes)
      (define option (infer-splitpoints all-alts precision))
      (timeline-event! 'bsearch)
      (combine-alts option precision)]
     [else
      (best-alt all-alts precision)]))
  (timeline-event! 'simplify)
  (define cleaned-alt
    (alt `(λ ,(program-variables (alt-program joined-alt))
            ,(simplify-expr (program-body (alt-program joined-alt)) #:rules (*fp-safe-simplify-rules*)))
         'final-simplify (list joined-alt)))
  (timeline-event! 'end)
  cleaned-alt)

;; Other tools
(define (resample! precision)
  (let ([context (prepare-points (*start-prog*) (^precondition^) precision)])
    (*pcontext* context)
    (^table^ (atab-new-context (^table^) context)))
  (void))
