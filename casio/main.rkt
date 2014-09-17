#lang racket

(require casio/common)
(require casio/points)
(require casio/alternative)
(require casio/localize-error)
(require casio/simplify/simplify)
(require casio/infer-regimes)
(require casio/locations)
(require casio/programs)
(require casio/periodicity)
(require casio/taylor)
(require casio/alt-table)
(require casio/matcher)

(provide setup improve)

; For debugging
(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

(define (improve-prog prog fuel)
  (setup prog (map (curryr cons sample-float) (program-variables prog))
	 (curryr improve fuel)))

(define (setup prog samplers cont)
  (let*-values ([(pts exs) (prepare-points prog samplers)])
    (parameterize ([*points* pts] [*exacts* exs] [*analyze-points* ((flag 'localize 'cache) pts #f)] [*start-prog* prog])
      (cont (make-alt prog)))))

(define (improve alt fuel)
  (let ([alt-table (setup-alt alt fuel)])
    (improve-loop alt-table fuel)))

;; Implementation

(define (setup-alt altn fuel)
  (let ([maybe-period ((flag 'setup 'periodicity)
		       (curry optimize-periodicity
			      (λ (altn)
				 ;; We call improve-loop directly because we don't want simplify or periodicity running on our
				 ;; subexpressions.
                                 (improve-loop
                                  (make-alt-table (*points*) (make-alt (alt-program altn)))
                                  fuel)))
                       identity)]
	[maybe-simplify ((flag 'setup 'simplify) simplify-alt identity)])
    (make-alt-table (*points*) (maybe-period (maybe-simplify altn)))))

(define (improve-loop table fuel)
  (cond [(<= fuel 0)
	 (debug "Ran out of fuel, reducing... " #:from 'main #:depth 2)
	 (reduce-alts table fuel)]
	[(atab-completed? table)
	 (debug "Ran out of unexpanded alts in alt table, reducing... " #:from 'main #:depth 2)
	 (reduce-alts table fuel)]
	[#t
	 (improve-loop
	  (let-values ([(picked table*) (atab-pick-alt table #:picking-func (curry argmin (compose errors-score alt-errors)))])
	    (atab-add-altns table* (append-map generate-alts (list picked))))
	  (- fuel 1))]))

(define (generate-alts altn)
  (append-map (curry generate-alts-at altn) (localize-error (alt-program altn))))

(define (generate-alts-at altn loc)
  (let ([rewrite
         ((flag 'generate 'rm) alt-rewrite-rm alt-rewrite-expression)]
        [cleanup
         ((flag 'generate 'simplify) simplify-alt identity)])
    (map cleanup (rewrite (alt-add-event altn '(start rm)) #:root loc))))

(define (simplify-alt altn)
  (apply alt-apply altn (simplify altn)))

(define (reduce-alts table fuel)
  (let ([combine
         ((flag 'reduce 'regimes) regimes-alts (const #f))]
        [maybe-zach ((flag 'reduce 'zach) zach-alt (const '()))]
        [maybe-taylor ((flag 'reduce 'taylor) taylor-alt (λ (x y) x))])
    (let* ([all-alts (atab-all-alts table)]
           [locss (map (compose localize-error alt-program) all-alts)]
           [alts*
            (apply append
                   (for/list ([alt all-alts] [locs locss])
                     (append
                      (append-map (curry maybe-zach alt) locs)
                      (map (curry maybe-taylor alt) locs))))]
           [table* (atab-add-altns table alts*)]
           [all-alts (atab-all-alts table*)])
      (let ([combo (combine all-alts fuel)])
	(if combo
	    (best-alt (cons combo all-alts))
	    (best-alt all-alts))))))

(define (taylor-alt altn loc)
  (let ([new-prog
         (location-do loc (alt-program altn)
                      (λ (expr) (approximate expr (free-variables expr))))])
    (alt-event new-prog `(taylor ,loc) (list altn))))

(define (zach-alt altn loc)
  (let ([sibling (location-sibling loc)])
    (if (and sibling
             (= (length (location-get (location-parent loc)
                                      (alt-program altn))) 3))
        (generate-alts-at (alt-add-event altn '(start zaching)) sibling)
        '())))

(define (regimes-alts alts fuel)
  (let* ([recurse-func ((flag 'regimes 'recurse)
                        (λ (altn) (improve-loop (make-alt-table (*points*) altn) (quotient fuel 2)))
                        identity)]
         [alts* (map (curryr alt-add-event '(start regimes)) alts)])
    (if (< (length alts*) 2)
        #f
        (infer-regimes alts* #:pre-combo-func recurse-func))))

(define (best-alt alts)
  (when (null? alts)
    (error "Trying to find the best of no alts!"))
  (argmin alt-history-length (argmins alt-cost (argmins (compose errors-score alt-errors) alts))))
