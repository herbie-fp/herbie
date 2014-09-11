#lang racket

(require casio/points)
(require casio/alternative)
(require casio/common)
(require casio/matcher)
(require casio/programs)
(require casio/main)

(provide (all-defined-out))

(define (print-improve prog max-iters)
  (let-values ([(pts exs) (prepare-points prog sample-float)])
    (parameterize ([*points* pts] [*exacts* exs])
      (let* ([start (make-alt prog)]
             [end (improve-alt start max-iters)])
        (println "Started at: " start)
        (println "Ended at: " end)
        (println "Improvement by an average of "
                 (- (errors-score (alt-errors start)) (errors-score (alt-errors end)))
                 " bits of precision")
        (void)))))

(define (setup prog)
  (define-values (points exacts) (prepare-points prog))
  (*points* points)
  (*exacts* exacts)
  (void))

(define (repl-print x)
  (begin (println x) (void)))

(define (prog-improvement prog1 prog2)
  (let-values ([(points exacts) (prepare-points prog1)])
    (- (errors-score (errors prog1 points exacts)) (errors-score (errors prog2 points exacts)))))

(define (annotated-alts-compare alt1 alt2)
  (annotated-errors-compare (alt-errors alt1) (alt-errors alt2)))

(define (annotated-errors-compare err1 err2)
  (repl-print (let loop ([region #f] [rest-diff (map (λ (e1 e2)
						      (cond [(> e1 e2) '>]
							    [(< e1 e2) '<]
							    [#t '=]))
						    err1 err2)]
			 [rest-points (*points*)] [acc '()])
		(cond [(null? rest-diff) (reverse acc)]
		      [(eq? region (car rest-diff))
		       (loop region (cdr rest-diff)
			     (cdr rest-points) (cons (car rest-diff) acc))]
		      [#t (loop (car rest-diff) (cdr rest-diff)
				(cdr rest-points) (cons (cons (car rest-points) (car rest-diff))
							acc))]))))

(define (compare-alts . altns)
  (repl-print (let loop ([cur-region-idx -1] [rest-lsts (map alt-errors altns)]
			 [rest-points (*points*)] [acc '()])
		(if (null? rest-points)
		    (reverse acc)
		    (let ([best-idx
			   (let ([errs (map car rest-lsts)])
			     (argmin (λ (idx) (list-ref errs idx))
				    (range (length errs))))])
		      (if (= best-idx cur-region-idx)
			  (loop cur-region-idx (map cdr rest-lsts)
				(cdr rest-points)
				(cons best-idx acc))
			  (loop best-idx (map cdr rest-lsts)
				(cdr rest-points)
				(cons (list best-idx (list-ref altns best-idx)
					    (car rest-points))
				      acc))))))))

(define (print-alt-info altn)
  (if (not (alt-prev altn))
      (println "Started with: " (alt-program altn))
      (begin (print-alt-info (alt-prev altn))
             (let ([chng (alt-change altn)])
               (println "Applied rule " (change-rule chng)
                        " at " (change-location chng)
                        " [ " (location-get (change-location chng)
                                            (alt-program (alt-prev altn)))
                        " ], and got:" (alt-program altn))
               (void)))))

(define (incremental-changes-apply changes expr)
  (let loop ([rest-chngs changes] [cur-expr expr])
    (if (null? rest-chngs)
	cur-expr
	(begin (println cur-expr)
	       (println (car rest-chngs))
	       (loop (cdr rest-chngs) (change-apply (car rest-chngs) cur-expr))))))
