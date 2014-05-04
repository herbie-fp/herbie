#lang racket

(require casio/main)
(require casio/alternative)
(require casio/common)
(require casio/rules)

(provide print-alt-info)

(define (print-alt-info altn)
  (if (not (alt-prev altn))
      (println "Started with: " (alt-program altn))
      (begin (print-alt-info (alt-prev altn))
	     (let ([chng (alt-change altn)])
	       (println "After considering " (change*-hardness chng)
			" other options, applied rule " (change-rule chng)
			" at " (change-location chng)
			", and got " (alt-program altn))))))
		      
