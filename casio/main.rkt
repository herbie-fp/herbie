#lang racket

(require casio/common)
(require casio/points)
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
    (apply append
	   (for/list ([loc locs])
	     (rewrite-local-error altn loc)))))

(define (try-simplify altn)
  (let ([simpl-altn (simplify altn)])
    (if (alternative<? simpl-altn altn)
	simpl-altn
	altn)))

(define (improve prog max-iters)
  (debug-reset)
  (define-values (points exacts) (prepare-points prog))
  (parameterize ([*points* points] [*exacts* exacts])
    ; Save original program
    (let ([orig (make-alt prog)])
      (let loop ([alts (list orig)] [olds (list)] [iter max-iters])
	; Invariant: (sorted? alts alternative<?)
	; Invariant: (no-duplicates? alts)
	; Invariant: (sorted? olds alternative<?)
	; Invariant: (no-duplicates? olds)
	(cond
	 [(= iter 0)
	  (values (car alts) orig)]
	 [(and (null? alts) (not (null? olds)))
	  ; We've exhausted all "intelligent" things to do
	  (let* ([old (car olds)]
		 [old* (cdr olds)])
	    (debug "No alternatives left; resorting to brute force"
		   #:from 'improve)
	    (loop (sort (rewrite-brute-force old) alternative<?)
		  old* (- iter 1)))]
	 [(and (null? alts) (null? olds))
	  (error "(improve) cannot proceed: no olds or alts")]
	 [else
	  (let* ([altn (car alts)]
		 [alts* (cdr alts)]
		 [next (map try-simplify (try-analyze altn))]
		 [greens (filter green? next)])
	    (debug "Trying to improve" altn #:from 'improve)
	    (cond
	     [(null? greens)
	      (let ([next-alts (sort (append alts* next) alternative<?)]
		    [next-olds (sort (cons altn olds) alternative<?)])
		(debug "Produced" (length next) "alternatives; none green"
		       #:from 'improve #:tag 'info)
		(loop next-alts next-olds (- iter 1)))]
	     [else 
	      (debug "Found" (length greens) "green changes:" greens
		     #:from 'improve #:tag 'info)
	      (loop (sort greens alternative<?) (list) (- iter 1))]))])))))

;; For usage at the REPL, we define a few helper functions.
;;
;; PROGRAM-A and PROGRAM-B are two example programs to test.
;; (explore prog iters) returns a list of alternatives found
;; (improve prog iters) prints the found alternatives
(define (print-improve prog max-iters)
  (let-values ([(end start) (improve prog max-iters)])
    (println "Started at: " start)
    (println "Ended at:   " end)
    (println "Improvement by an average of "
	     (/ (apply + (filter ordinary-float?
				 (errors-difference (alt-errors start) (alt-errors end))))
		       (log 2))
	     " bits of precision")))

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
