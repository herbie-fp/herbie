#lang racket

(require casio/common)
(require casio/points)
(require casio/programs)
(require casio/alternative)
(require casio/analyze-subexpressions)
(require casio/main)

(define (shell prog)
  (debug-reset)
  (define-values (points exacts) (prepare-points prog))
  (parameterize ([*points* points] [*exacts* exacts])
    (let ([orig (make-alt prog)])
      (let toploop ([alts (list orig)])

	(println "Alternatives: ")
	(let printloop ([alts alts] [idx 0])
	  (unless (null? alts)
	      (println "$" idx ": " (car alts))
	      (printloop (cdr alts) (+ idx 1))))

	(display "focus> ")
	(let* ([altn (list-ref alts (read))]
	       [locs (remove-duplicates (list '(cdr cdr car) (analyze-local-error altn)))])

	  (println "Locations: ") 
	  (let printloop ([locs locs] [idx 0])
	    (unless (null? locs)
	      (println "@" idx ": " (location-get (car locs) (alt-program altn)))
	      (printloop (cdr locs) (+ idx 1))))

	  (display "rewrite> ")
	  (let* ([cmd (read)]
		 [rr (if (eq? (car cmd) 'tree) alt-rewrite-tree alt-rewrite-expression)]
		 [loc (list-ref locs (cadr cmd))])
	    (toploop (rr altn #:root loc))))))))

	  
	
