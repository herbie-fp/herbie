#lang racket

(require herbie/common)
(require herbie/main)
(require herbie/programs)
(require herbie/points)
(require herbie/localize-error)
(require herbie/taylor)
(require herbie/alt-table)
(require herbie/alternative)
(require herbie/simplify/simplify)
(require interface/visualize)
(require interface/util)

(provide (all-defined-out)
	 (all-from-out interface/visualize))

;; I'm going to use some global state here to make the shell more
;; friendly to interact with without having to store your own global
;; state in the repl as you would normally do with debugging. This is
;; probably a bad idea, and I might change it back later. When
;; extending, make sure this never gets too complicated to fit in your
;; head at once, because then global state is going to mess you up.
(define ^locs^ (make-parameter '()))
(define ^table^ (make-parameter #f))
(define ^next-alt^ (make-parameter #f))
(define ^children^ (make-parameter '()))

(define *setup-fuel* (make-parameter 3))

(define (setup-prog! prog #:samplers [samplers #f])
  (*start-prog* prog)
  (let* ([samplers (or samplers (map (curryr cons sample-default)
				     (program-variables prog)))]
	 [context (prepare-points prog samplers)])
    (*pcontext* context)
    (*analyze-context* ((flag 'localize 'cache) context #f))
    (^table^ (setup-prog prog (*setup-fuel*)))))

(define (list-alts)
  (println "Here are the current alts in the table (an x indicates an alt has already been expanded):")
  (let ([ndone-alts (atab-not-done-alts (^table^))])
    (for ([alt (atab-all-alts (^table^))]
	  [n (in-naturals)])
      (println (if (member alt ndone-alts) "*" "x")
	       " " n " " alt))))

(define (choose-alt! n)
  (if (>= n (length (atab-all-alts (^table^))))
      (println "We don't have that many alts!")
      (let-values ([(picked table*) (atab-pick-alt (^table^) #:picking-func (curryr list-ref n))])
	(^next-alt^ picked)
	(^table^ table*)
	(void))))

(define (localize!)
  (^locs^ (localize-error (alt-program (^next-alt^)))))

(define (gen-series!)
  (let ([prog (alt-program (^next-alt^))])
    (^children^ (map (compose make-alt
			      (curryr location-do prog
				      (curryr approximate-0
					      (program-variables prog))))
		     (^locs^)))))

(define (simplify!)
  (^children^ (map (λ (altn)
		     (apply alt-apply altn (simplify altn)))
		   (^children^))))

(define (finalize-iter!)
  (^table^ (atab-add-altns (^table^) (^children^)))
  (^children^ '()))

(define (finalize-table!)
  (^table^ (post-process (^table^))))

(define (get-final-combination)
  (match-let ([`(,tables ,splitpoints) (split-table (^table^))])
    (if (= (length tables) 1)
	(extract-alt (car tables))
	(combine-alts splitpoints (map extract-alt tables)))))
