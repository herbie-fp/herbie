#lang racket

(require casio/common)
(require casio/points)
(require casio/programs)
(require casio/alternative)
(require casio/brute-force)
(require casio/redgreen)
(require casio/analyze-subexpressions)
(require casio/simplify)

(define (rewrite-local-error altn loc)
  (alt-rewrite-expression altn #:root loc #:destruct #t))

(define (rewrite-brute-force altn)
  (alt-rewrite-tree altn))

(define (try-analyze altn)
  (let ([locs (map car (analyze-local-error altn))])
    (append
     (apply append
	    (for/list ([loc locs])
	      (rewrite-local-error altn loc)))
     (apply append
	    (for/list ([loc locs])
	      (let-values ([(parent other) (location-parent loc)])
		(if (and
		     parent
		     (= (length (location-get parent (alt-program altn))) 3))
		    (alt-rewrite-expression altn #:root other)
		    '())))))))

(define (try-simplify altn)
  (let ([simpl-altn (simplify altn)])
    (if (much-better? simpl-altn altn)
	simpl-altn
	altn)))

(define (improve prog max-iters)
  (debug-reset)
  (define-values (points exacts) (prepare-points prog))
  (parameterize ([*points* points] [*exacts* exacts])
    ; Save original program
    (let ([orig (make-alt prog)])
      (let loop ([alts (list orig)] [olds (list)] [trace (list)]
		 [iter max-iters])
	; Invariant: (no-duplicates? alts)
	; Invariant: (no-duplicates? olds)
	(cond
	 [(= iter 0)
	  (let ([sorted (sort (reverse (append alts olds trace))
			      much-better?)])
	    (debug "Done:" (car sorted) #:from 'improve)
	    (values (car sorted) orig))]
	 [(and (null? alts) (not (null? olds)))
	  ; We've exhausted all "intelligent" things to do
	  (debug "Resorting to brute force"
		 #:from 'improve)
	  (let* ([old (car olds)]
		 [old* (cdr olds)]
		 [alts* (rewrite-brute-force old)]
		 [greens
		  (map remove-red (filter (curryr much-better? old) alts*))])
	    (cond
	     [(null? greens)
	      (debug "Produced" (length alts*) "alternatives, none green"
		     #:from 'improve #:tag 'info)
	      (loop alts* old* (cons old trace) (- iter 1))]
	     [else
	      (debug "Discovered" (length greens) "green changes"
		     #:from 'improve #:tag 'info)
	      (loop greens (list) (append olds alts alts* trace)
		    (- iter 1))]))]
	 [(and (null? alts) (null? olds))
	  (error "(improve) cannot proceed: no olds or alts")]
	 [else
	  (debug "Step:" (car alts) #:from 'improve)
	  (let* ([altn (car alts)]
		 [alts* (cdr alts)]
		 [next (map try-simplify (try-analyze altn))]
		 [greens
		  (map remove-red (filter (curryr much-better? altn) next))])
	    (cond
	     [(null? greens)
	      (let ([next-alts (append alts* next)]
		    [next-olds (cons altn olds)])
		(debug "Produced" (length next) "alternatives, none green"
		       #:from 'improve #:tag 'info)
		(loop next-alts next-olds trace (- iter 1)))]
	     [else 
	      (debug "Discovered" (length greens) "green changes"
		     #:from 'improve #:tag 'info)
	      (loop (sort greens alternative<?) (list)
		    (append alts olds next) (- iter 1))]))])))))

;; For usage at the REPL, we define a few helper functions.
;;
;; PROGRAM-A and PROGRAM-B are two example programs to test.
;; (explore prog iters) returns a list of alternatives found
;; (improve prog iters) prints the found alternatives
(define (print-improve prog max-iters)
  (let-values ([(end start) (improve prog max-iters)])
    (println "Started at: " start)
    (println "Ended at:   " end)
    (let ([diff (errors-difference (alt-errors start) (alt-errors end))])
      (println "Improvement by an average of "
	       (/ (apply + (filter ordinary-float? diff))
		  (log 2) (length diff))
	       " bits of precision"))
    (void)))

(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

;(define (plot-alternatives prog iterations)
;  "Return a spectrum plot of the alternatives found."
;  (let* ([alts (explore prog iterations)]
;         [logs (map (lambda (x) (- (/ (log (alternative-error x)) (log 10)))) alts)]
;         [rands (for/list ([i (range (length logs))]) (random))])
;    (display "Found program with score ")
;    (display (alternative-score (car alts)))
;    (newline)
;    (pretty-print (alternative-program (car alts)))
;    (parameterize ([plot-width 800] [plot-height 100]
;                   [plot-x-label #f] [plot-y-label #f])
;      (plot (points (map vector logs rands))))))

(provide improve)
