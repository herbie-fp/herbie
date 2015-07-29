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
(require "../loop-rewrites.rkt")
(require "../test.rkt")
(require "../prettyify.rkt")
(require "../loop-errors.rkt")
(require "../plot.rkt")
(require "util.rkt")

(provide (all-defined-out)
	 (all-from-out "../plot.rkt")
	 )

;; I'm going to use some global state here to make the shell more
;; friendly to interact with without having to store your own global
;; state in the repl as you would normally do with debugging. This is
;; probably a bad idea, and I might change it back later. When
;; extending, make sure this never gets too complicated to fit in your
;; head at once, because then global state is going to mess you up.

(struct shellstate
  (table next-alt locs children gened-series gened-rewrites simplified)
  #:mutable)

(define ^shell-state^ (make-parameter (shellstate #f #f #f '() #f #f #f)))

(define (^table^ [newval 'none])
  (when (not (equal? newval 'none))  (set-shellstate-table! (^shell-state^) newval))
  (shellstate-table (^shell-state^)))
(define (^next-alt^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-next-alt! (^shell-state^) newval))
  (shellstate-next-alt (^shell-state^)))
(define (^children^ [newval 'none])
  (when (not (equal? newval 'none)) (set-shellstate-children! (^shell-state^) newval))
  (shellstate-children (^shell-state^)))

;; Setting up
(define (setup-prog! prog #:samplers [samplers #f])
  (*start-prog* prog)
  (rollback-improve!)
  (debug #:from 'progress #:depth 3 "[1/2] Preparing points")
  (let* ([samplers (or samplers (map (curryr cons (sample-list (curryr make-list 10)
							       sample-double
							       (curryr make-list .25)))
				     (program-variables prog)))]
	 [alt (make-alt (unfold-lets prog))])
    (*pcontext* (prepare-points prog samplers))
    (debug #:from 'progress #:depth 3 "[2/2] Setting up program.")
    (^table^ (make-alt-table (*pcontext*) alt))
    (void)))

(define (setup-test! test)
  (setup-prog! (test-program test) #:samplers (test-samplers test)))

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

;; Generate children
(define (generate-kahand!)
  (^children^ (append (^children^)
                      (rewrite-loops (^next-alt^)
                                     add-error-term
                                     'added-error-term)))
  (void))

;; Finish iteration
(define (finalize-iter!)
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
  (debug #:from 'progress #:depth 3 "adding candidates to table")
  (finalize-iter!)
  (void))

(define (rollback-iter!)
  (^children^ '())
  (^next-alt^ #f)
  (void))

(define (rollback-improve!)
  (rollback-iter!)
  (^table^ #f)
  (void))

;; Run a complete iteration
(define (run-iter!)
  (if (^next-alt^)
      (begin (println "An iteration is already in progress!")
	     (println "Finish it up manually, or by running (finish-iter!)")
	     (println "Or, you can just run (rollback-iter!) to roll it back and start it over."))
      (begin (debug #:from 'progress #:depth 3 "picking best candidate")
	     (choose-best-alt!)
             (generate-kahand!)
	     (debug #:from 'progress #:depth 3 "adding candidates to table")
	     (finalize-iter!)))
  (void))

(define (run-improve prog iters #:samplers [samplers #f] #:get-context [get-context? #f])
  (debug #:from 'progress #:depth 1 "[Phase 1 of 3] Setting up.")
  (setup-prog! prog #:samplers samplers)
  (debug #:from 'progress #:depth 1 "[Phase 2 of 3] Improving.")
  (for ([iter (sequence-map add1 (in-range iters))]
	#:break (atab-completed? (^table^)))
    (debug #:from 'progress #:depth 2 "iteration" iter "/" iters)
    (run-iter!))
  (debug #:from 'progress #:depth 1 "[Phase 3 of 3] Extracting.")
  (if get-context?
      (begin0 (list (get-final-combination) (*pcontext*))
	(rollback-improve!))
      (begin0 (get-final-combination)
	(rollback-improve!))))

(define (run-improve-test test iters)
  (run-improve (test-program test) iters #:samplers (test-samplers test)))

;; Finishing Herbie
(define (get-final-combination)
  (remove-pows
   (factor-common-subexprs
    (extract-alt (^table^)))))
