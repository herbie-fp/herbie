#lang racket

(require casio/common)
(require casio/points)
(require casio/alternative)
(require casio/brute-force)
(require casio/redgreen)
(require casio/analyze-subexpressions)

(define *strategies* (list improve-by-analysis brute-force-search))

(define (improve prog max-iters)
  (debug-reset)
  (define-values (points exacts) (prepare-points prog))
  (improve-on-points prog max-iters points exacts))

(define (improve-on-points prog max-iters points exacts)
  (parameterize ([*points* points] [*exacts* exacts])
    (let ([orig (make-alt prog)])
      (let loop ([strats *strategies*] [alt0 orig])
	(if (null? strats)
	    (values alt0 orig)
	    (let ([alt1 ((car strats) alt0 max-iters)])
	      (debug (car strats) "->" alt1 #:from 'improve)
	      (if (and (green? alt1) (not (eq? alt1 alt0)))
		  (loop *strategies* (remove-red alt1))
		  (loop (cdr strats) alt1))))))))

;; For usage at the REPL, we define a few helper functions.
;;
;; PROGRAM-A and PROGRAM-B are two example programs to test.
;; (explore prog iters) returns a list of alternatives found
;; (improve prog iters) prints the found alternatives
(define (print-improve prog max-iters)
  (let-values ([(end start) (improve prog max-iters)])
    (println start)
    (println end)
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

(provide improve *strategies*)
