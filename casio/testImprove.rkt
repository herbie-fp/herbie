#lang racket

(require casio/main)
(require casio/common)
(require racket/engine)

(provide test-improve)

(define (average-trials f num-trials)
  (let loop ([remaining-trials num-trials] [acc '()])
    (if (> remaining-trials 0)
	(loop (- remaining-trials 1) (cons (f) acc))
	(/ (apply + acc) num-trials))))

(define (test-improve prog max-iters num-trials)
  (average-trials (lambda () (let-values ([(end start) (improve prog max-iters)])
			       (timeout (lambda () (improvement start end))
					(* 800 (expt 1.5 max-iters))
					0)))
		  num-trials))

(define (timeout f time-limit default-value)
  (let* ([value #f]
	 [result (engine-run time-limit
			     (engine (lambda (_)
				       (set! value (f)))))])
    (if result
	value
	default-value)))
