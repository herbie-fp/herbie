#lang racket

(require casio/points)
(require casio/alternative)
(require casio/common)
(require casio/matcher)
(require casio/programs)
(require casio/main)

(provide (all-defined-out))

(define (print-improve prog max-iters)
  (let-values ([(pts exs) (prepare-points prog)])
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
  (repl-print (let loop ([region 1] [rest-diff (map - err1 err2)]
			 [rest-points (*points*)] [acc '()])
		(cond [(null? rest-diff) (reverse acc)]
		      [(<= (* (car rest-diff) region) 0)
		       (loop (car rest-diff) (cdr rest-diff)
			     (cdr rest-points) (cons (list (car rest-diff) (car rest-points)) acc))]
		      [#t (loop region (cdr rest-diff) (cdr rest-points) (cons (car rest-diff) acc))]))))

(define (print-alt-info altn)
  (if (not (alt-prev altn))
      (println "Started with: " (alt-program altn))
      (begin (print-alt-info (alt-prev altn))
             (let ([chng (alt-change altn)])
               (println "After considering " (change*-hardness chng)
                        " other options, applied rule " (change-rule chng)
                        " at " (change-location chng)
                        " [ " (location-get (change-location chng) (alt-program (alt-prev altn))) " ]"
                        ", and got:")
               (println (alt-program altn))
               (void)))))

(define (incremental-changes-apply changes expr)
  (let loop ([rest-chngs changes] [cur-expr expr])
    (if (null? rest-chngs)
	cur-expr
	(begin (println cur-expr)
	       (println (car rest-chngs))
	       (loop (cdr rest-chngs) (change-apply (car rest-chngs) cur-expr))))))
