#lang racket

(require casio/points)
(require casio/alternative)
(require casio/common)
(require casio/matcher)
(require casio/programs)

(provide (all-defined-out))

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
  (repl-print (let loop ([region #f] [rest-diff (map - err1 err2)]
			 [rest-points (*points*)] [acc '()])
		(cond [(null? rest-diff) (reverse acc)]
		      [(>= (* (car rest-diff) region) 0)
		       (loop (car rest-diff) (cdr rest-diff)
			     (cdr rest-points) (cons (cons (car rest-diff) (car rest-points)) acc))]
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
