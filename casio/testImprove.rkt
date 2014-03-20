#lang racket

(require casio/main)
(require casio/common)
(require racket/engine)

(provide test-improve)

(define (average-trials f num-trials #:divisor-proc [divisor #f])
  (let loop ([remaining-trials num-trials] [acc '()])
    (show-progress num-trials remaining-trials)
    (if (> remaining-trials 0)
	(loop (- remaining-trials 1) (cons (f) acc))
	(/ (apply + acc) (if divisor
			     (divisor)
			     num-trials)))))

(define (test-improve prog max-iters num-trials)
  (let* ([num-timeouts 0]
	 [result (average-trials (lambda () (timeout (lambda () (let-values ([(end start) (improve prog max-iters)])
					      	     	          (improvement start end)))
						     (* 1500 (expt 1.2 max-iters))
						     0
						     #:timeout-proc (λ ()
								       (newline)
								       (println "TIMEOUT!")
								       (set! num-timeouts
									     (+ 1 num-timeouts)))))
				 num-trials
				 (lambda () (- num-trials num-timeouts)))])
    (newline)
    (display "The average improvement was ")
    (display result)
    (println ".")
    (display "There were ")
    (display num-timeouts)
    (println " timeouts.")))

(define (timeout f time-limit default-value #:timeout-proc proc)
  (let* ([value #f]
	 [result (engine-run time-limit
			     (engine (lambda (_)
				       (set! value (f)))))])
    (if result
	value
	(begin (proc) default-value))))

(define (show-progress total left)
  (newline)
  (display (* 100 (/ (- total left) total)))
  (display "%")
  (when (*debug*) (newline)))
