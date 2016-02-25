#lang racket

(require "../common.rkt")
(require "../main.rkt")
(require "../programs.rkt")
(require "../points.rkt")
(require "../localize-error.rkt")
(require "../taylor.rkt")
(require "../alt-table.rkt")
(require "../alternative.rkt")
(require "../simplify/simplify.rkt")
(require "../test.rkt")
(require "../plot.rkt")
(require "../matcher.rkt")

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
  (table next-alt locs children gened-series gened-rewrites simplified samplers timeline)
  #:mutable)

(define ^shell-state^ (make-parameter (shellstate #f #f #f '() #f #f #f #f '())))

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
(define (^samplers^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-samplers! (^shell-state^) newval))
  (shellstate-samplers (^shell-state^)))
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

(define *setup-fuel* (make-parameter 3))

(define (timeline-event! type)
  (let ([b (box (list (cons 'type type) (cons 'time (current-inexact-milliseconds))))])
    (set-shellstate-timeline! (^shell-state^) (cons b (shellstate-timeline (^shell-state^))))
    (λ (key value) (set-box! b (cons (cons key value) (unbox b))))))

;; Setting up
(define (setup-prog! prog #:samplers [samplers #f])
  (*start-prog* prog)
  (rollback-improve!)
  (timeline-event! 'start) ; This has no associated data, so we don't name it
  (debug #:from 'progress #:depth 3 "[1/2] Preparing points")
  (let* ([samplers (or samplers (map (curryr cons sample-default)
				     (program-variables prog)))]
	 [context (prepare-points prog samplers)])
    (^samplers^ samplers)
    (*pcontext* context)
    (*analyze-context* context)
    (debug #:from 'progress #:depth 3 "[2/2] Setting up program.")
    (define log! (timeline-event! 'setup))
    (^table^ (setup-prog prog (*setup-fuel*)))
    (void)))

;; Information
(define (list-alts)
  (println "Here are the current alts in the table")
  (println "Key:")
  (println "x = already expanded")
  (println "+ = currently chosen")
  (println "* = left to expand")
  (println)
  (let ([ndone-alts (atab-not-done-alts (^table^))])
    (for ([alt (atab-all-alts (^table^))]
	  [n (in-naturals)])
      (println (cond [(equal? alt (^next-alt^)) "+"]
		     [(member alt ndone-alts) "*"]
		     [#t "x"])
	       " " n " " alt)))
  (void))

;; Begin iteration
(define (choose-alt! n)
  (if (>= n (length (atab-all-alts (^table^))))
      (println "We don't have that many alts!")
      (let-values ([(picked table*) (atab-pick-alt (^table^) #:picking-func (curryr list-ref n)
						   #:only-fresh #f)])
	(^next-alt^ picked)
	(^table^ table*)
	(void))))

(define (choose-best-alt!)
  (let-values ([(picked table*) (atab-pick-alt (^table^) #:picking-func best-alt
					       #:only-fresh #t)])
    (^next-alt^ picked)
    (^table^ table*)
    (void)))

;; Invoke the subsystems individually
(define (localize!)
  (define log! (timeline-event! 'localize))
  (^locs^ (localize-error (alt-program (^next-alt^))))
  (void))

(define (gen-series!)
  (when ((flag 'generate 'taylor) #t #f)
    (define log! (timeline-event! 'series))
    (define series-expansions
      (apply
       append
       (for/list ([location (^locs^)]
                  [n (sequence-tail (in-naturals) 1)])
         (debug #:from 'progress #:depth 4 "[" n "/" (length (^locs^)) "] generating series at" location)
         (taylor-alt (^next-alt^) location))))
    (^children^ (append (^children^) series-expansions)))
  (^gened-series^ #t)
  (void))

(define (gen-rewrites!)
  (define alt-rewrite ((flag 'generate 'rr) alt-rewrite-rm alt-rewrite-expression))
  (define log! (timeline-event! 'rewrite))
  (define rewritten
    (apply append
	   (for/list ([location (^locs^)]
		      [n (sequence-tail (in-naturals) 1)])
	     (debug #:from 'progress #:depth 4 "[" n "/" (length (^locs^)) "] rewriting at" location)
	     (alt-rewrite (alt-add-event (^next-alt^) '(start rm)) #:root location))))
  (^children^
   (append (^children^) rewritten))
  (^gened-rewrites^ #t)
  (void))

(define (simplify!)
  (when ((flag 'generate 'simplify) #t #f)
    (define log! (timeline-event! 'simplify))
    (define simplified
      (for/list ([child (^children^)]
                 [n (sequence-tail (in-naturals) 1)])
        (debug #:from 'progress #:depth 4 "[" n "/" (length (^children^)) "] simplifiying candidate" child)
        (with-handlers ([exn:fail? (λ (e) (println "Failed while simplifying candidate" child) (raise e))])
          (apply alt-apply child (simplify child)))))
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
      (begin (println "An iteration is already in progress!")
	     (println "Finish it up manually, or by running (finish-iter!)")
	     (println "Or, you can just run (rollback-iter!) to roll it back and start it over."))
      (begin (debug #:from 'progress #:depth 3 "picking best candidate")
	     (choose-best-alt!)
	     (debug #:from 'progress #:depth 3 "localizing error")
	     (localize!)
	     (debug #:from 'progress #:depth 3 "generating series expansions")
	     (gen-series!)
	     (debug #:from 'progress #:depth 3 "generating rewritten candidates")
	     (gen-rewrites!)
	     (debug #:from 'progress #:depth 3 "simplifying candidates")
	     (simplify!)
	     (debug #:from 'progress #:depth 3 "adding candidates to table")
	     (finalize-iter!)))
  (void))

(define (run-improve prog iters #:samplers [samplers #f] #:get-context [get-context? #f])
  (debug #:from 'progress #:depth 1 "[Phase 1 of 3] Setting up.")
  (setup-prog! prog)
  (if (> 0.1 (errors-score (errors (*start-prog*) (*pcontext*))))
      (let ([init-alt (make-alt (*start-prog*))])
	(debug #:from 'progress #:depth 1 "Initial program already accurate, stopping.")
	(if get-context?
	    (list init-alt (*pcontext*))
	    init-alt))
      (begin
	(debug #:from 'progress #:depth 1 "[Phase 2 of 3] Improving.")
	(for ([iter (sequence-map add1 (in-range iters))]
	      #:break (atab-completed? (^table^)))
	  (debug #:from 'progress #:depth 2 "iteration" iter "/" iters)
	  (run-iter!))
	(finalize-table!)
	(debug #:from 'progress #:depth 1 "[Phase 3 of 3] Extracting.")
	(if get-context?
	    (list (get-final-combination) (*pcontext*))
	    (get-final-combination)))))

(define (visualize alt #:marks [marks '()] #:axis [axis 0])
  (define pts (for/list ([(pt ex) (in-pcontext (*pcontext*))]) pt))
  (define errs (alt-errors alt))

  (define renderers
    (list* (error-avg errs pts #:axis axis) (error-points errs pts #:axis axis)
           (for/list ([x-val marks]) (error-mark x-val))))

  (apply herbie-plot renderers))

;; Finishing Herbie
(define (finalize-table!)
  (^table^ (post-process (^table^)))
  (void))

(define (get-final-combination)
  (begin0
      (if ((flag 'reduce 'regimes) #t #f)
          (let ([log! (timeline-event! 'regimes)])
            (remove-pows (match-let ([`(,tables ,splitpoints) (split-table (^table^))])
                           (if (= (length tables) 1)
                               (extract-alt (car tables))
                               (combine-alts splitpoints (map extract-alt tables))))))
          (extract-alt (^table^)))
    (timeline-event! 'end))) ; No data here

;; Other tools
(define (resample!)
  (let ([context (prepare-points (*start-prog*) (^samplers^))])
    (*pcontext* context)
    (^table^ (atab-new-context (^table^) context)))
  (void))
