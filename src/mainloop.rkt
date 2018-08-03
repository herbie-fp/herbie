#lang racket

(require "common.rkt" "programs.rkt" "points.rkt" "alternative.rkt")
(require "core/localize.rkt" "core/taylor.rkt" "core/alt-table.rkt" "core/simplify.rkt"
         "core/matcher.rkt" "core/regimes.rkt")
(require "type-check.rkt") ;; For taylor not running on complex exprs

(provide (all-defined-out))

; For debugging
(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

;; I'm going to use some global state here to make the shell more
;; friendly to interact with without having to store your own global
;; state in the repl as you would normally do with debugging. This is
;; probably a bad idea, and I might change it back later. When
;; extending, make sure this never gets too complicated to fit in your
;; head at once, because then global state is going to mess you up.

(struct shellstate
  (table next-alt locs children gened-series gened-rewrites simplified precondition timeline)
  #:mutable)

(define ^shell-state^ (make-parameter (shellstate #f #f #f #f #f #f #f 'TRUE '())))

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
(define (^timeline^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-timeline! (^shell-state^) newval))
  (map unbox (reverse (shellstate-timeline (^shell-state^)))))

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

(define (timeline-event! type)
  (let ([b (box (list (cons 'type type) (cons 'time (current-inexact-milliseconds))))])
    (set-shellstate-timeline! (^shell-state^) (cons b (shellstate-timeline (^shell-state^))))
    (λ (key value) (set-box! b (cons (cons key value) (unbox b))))))

;; Setting up
(define (setup-prog! prog #:precondition [precondition 'TRUE])
  (*start-prog* prog)
  (rollback-improve!)
  (timeline-event! 'start) ; This has no associated data, so we don't name it
  (debug #:from 'progress #:depth 3 "[1/2] Preparing points")
  (let* ([context (prepare-points prog precondition)]
         [altn (make-alt prog)])
    (^precondition^ precondition)
    (*pcontext* context)
    (*analyze-context* context)
    (debug #:from 'progress #:depth 3 "[2/2] Setting up program.")
    (define log! (timeline-event! 'setup))
    (^table^ (make-alt-table context altn))
    (assert (equal? (atab-all-alts (^table^)) (list altn)))
    (void)))

;; Information
(define (list-alts)
  (printf "Here are the current alts in the table\n")
  (printf "Key:\n")
  (printf "x = already expanded\n")
  (printf "+ = currently chosen\n")
  (printf "* = left to expand\n")
  (printf)
  (let ([ndone-alts (atab-not-done-alts (^table^))])
    (for ([alt (atab-all-alts (^table^))]
	  [n (in-naturals)])
      (printf "~a ~a ~a\n"
       (cond [(equal? alt (^next-alt^)) "+"]
             [(set-member? ndone-alts alt) "*"]
             [else "x"])
       n
       alt)))
  (void))

;; Begin iteration
(define (choose-alt! n)
  (if (>= n (length (atab-all-alts (^table^))))
      (printf "We don't have that many alts!\n")
      (let-values ([(picked table*) (atab-pick-alt (^table^) #:picking-func (curryr list-ref n)
						   #:only-fresh #f)])
	(^next-alt^ picked)
	(^table^ table*)
	(void))))

(define (best-alt alts)
  (argmin alt-cost
	  (argmins (λ (alt) (errors-score (errors (alt-program alt) (*pcontext*))))
		   alts)))

(define (choose-best-alt!)
  (let-values ([(picked table*) (atab-pick-alt (^table^) #:picking-func best-alt
					       #:only-fresh #t)])
    (^next-alt^ picked)
    (^table^ table*)
    (debug #:from 'pick #:depth 4 "Picked " picked)
    (void)))

;; Invoke the subsystems individually
(define (localize!)
  (define log! (timeline-event! 'localize))
  (^locs^ (localize-error (alt-program (^next-alt^))))
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
    (define log! (timeline-event! 'series))
    (define series-expansions
      (apply
       append
       (for/list ([location (^locs^)] [n (in-naturals 1)])
         (debug #:from 'progress #:depth 4 "[" n "/" (length (^locs^)) "] generating series at" location)
         (taylor-alt (^next-alt^) location))))
    (^children^ (append (^children^) series-expansions)))
  (^gened-series^ #t)
  (void))

(define (gen-rewrites!)
  (define alt-rewrite (if (flag-set? 'generate 'rr) alt-rewrite-rm alt-rewrite-expression))
  (define log! (timeline-event! 'rewrite))
  (define rewritten
    (apply append
	   (for/list ([location (^locs^)] [n (in-naturals 1)])
	     (debug #:from 'progress #:depth 4 "[" n "/" (length (^locs^)) "] rewriting at" location)
	     (alt-rewrite (alt-add-event (^next-alt^) '(start rm)) #:root location))))
  (^children^
   (append (^children^) rewritten))
  (^gened-rewrites^ #t)
  (void))

(define (num-nodes expr)
  (if (not (list? expr)) 1
      (add1 (apply + (map num-nodes (cdr expr))))))

(define (simplify!)
  (when (flag-set? 'generate 'simplify)
    (define log! (timeline-event! 'simplify))
    (define simplified
      (for/list ([child (^children^)] [n (in-naturals 1)])
        (debug #:from 'progress #:depth 4 "[" n "/" (length (^children^)) "] simplifiying candidate" child)
        ;; We want to avoid simplifying if possible, so we only
        ;; simplify things produced by function calls in the rule
        ;; pattern. This means no simplification if the rule output as
        ;; a whole is not a function call pattern, and no simplifying
        ;; subexpressions that don't correspond to function call
        ;; patterns.
        (define locs
          (match (alt-event child)
            [(list 'taylor _ loc) (list loc)]
            [(list 'change cng)
             (match-define (change rule loc _) cng)
             (define pattern (rule-input rule))
             (define expr (location-get loc (alt-program child)))
             (cond
              [(not (list? pattern)) '()]
              [(not (list? expr)) '()]
              [else
               (for/list ([pos (in-naturals 1)] [arg (cdr expr)] [arg-pattern (cdr pattern)]
                          #:when (list? arg-pattern) #:when (list? arg))
                 (append (change-location cng) (list pos)))])]
            [_ (list '(2))]))

        (for/fold ([child child]) ([loc locs])
          (define child* (location-do loc (alt-program child) (λ (expr) (simplify-expr expr #:rules (*simplify-rules*)))))
          (debug #:from 'simplify "Simplified" loc "to" child*)
          (if (> (num-nodes (program-body (alt-program child))) (num-nodes (program-body child*)))
              (alt child* (list 'simplify loc) (list child))
              child))))
    (^children^ simplified))
  (^simplified^ #t)
  (void))


;; Finish iteration
(define (finalize-iter!)
  (define log! (timeline-event! 'prune))
  (^table^ (atab-add-altns (^table^) (^children^)))
  (rollback-iter!)
  (void))

(define (inject-candidate! prog)
  (^table^ (atab-add-altns (^table^) (list (make-alt prog))))
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
  (^table^ #f)
  (^timeline^ '())
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

(define (run-improve prog iters #:precondition [precondition 'TRUE])
  (debug #:from 'progress #:depth 1 "[Phase 1 of 3] Setting up.")
  (setup-prog! prog #:precondition precondition)
  (if (and (flag-set? 'setup 'early-exit) (< (errors-score (errors (*start-prog*) (*pcontext*))) 0.1))
      (begin
	(debug #:from 'progress #:depth 1 "Initial program already accurate, stopping.")
	(make-alt (*start-prog*)))
      (begin
	(debug #:from 'progress #:depth 1 "[Phase 2 of 3] Improving.")
        (^table^
         (atab-add-altns (^table^)
                         (if (flag-set? 'setup 'simplify)
                             (for/list ([altn (atab-all-alts (^table^))])
                               (alt `(λ ,(program-variables (alt-program altn))
                                       ,(simplify-expr (program-body (alt-program altn)) #:rules (*simplify-rules*)))
                                    'initial-simplify (list altn)))
                             (list))))
        (for ([iter (in-range iters)] #:break (atab-completed? (^table^)))
          (debug #:from 'progress #:depth 2 "iteration" (+ 1 iter) "/" iters)
          (run-iter!))
        (debug #:from 'progress #:depth 1 "[Phase 3 of 3] Extracting.")
        (get-final-combination))))

(define (get-final-combination)
  (define all-alts (atab-all-alts (^table^)))
  (define joined-alt
    (cond
     [(and (flag-set? 'reduce 'regimes) (> (length all-alts) 1))
      (timeline-event! 'regimes)
      (define option (infer-splitpoints all-alts))
      (timeline-event! 'binary-search)
      (combine-alts option all-alts)]
     [else
      (best-alt all-alts)]))
  (define cleaned-alt
    (alt `(λ ,(program-variables (alt-program joined-alt))
            ,(simplify-expr (program-body (alt-program joined-alt)) #:rules (*fp-safe-simplify-rules*)))
         'final-simplify (list joined-alt)))
  (timeline-event! 'end)
  cleaned-alt)

;; Other tools
(define (resample!)
  (let ([context (prepare-points (*start-prog*) (^precondition^))])
    (*pcontext* context)
    (^table^ (atab-new-context (^table^) context)))
  (void))
