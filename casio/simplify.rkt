#lang racket

(require casio/rules)
(require casio/alternative)
(require casio/programs)

(provide simplify)

(define (simplify altn)
  (let* ([change (alt-change altn)]
	 [location (change-location change)]
	 [rule (change-rule change)])
    (define (simplify-at-slocations slocations prog)
      (if (null? slocations)
	  prog
	  (simplify-at-slocations (cdr slocations)
				  (location-do (append (location (car slocations)))
					       prog
					       simplify-expression))))
    (simplify-at-slocations (rule-slocations rule) (alt-prog altn))))

(define (simplify-expression expr)
  expr) ;TODO
