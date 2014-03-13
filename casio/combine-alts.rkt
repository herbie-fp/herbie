#lang racket
(require casio/alternative)
(require casio/programs)
(require casio/rules)

(provide (all-defined-out))

(define (combine-alts alt0 alt1 dividor division-var)
  (let* ([split-initial-prog `(lambda ,(program-variables (alt-program alt0))
			       (if (< ,division-var ,dividor)
				   (program-body (alt-initial alt0))
				   (program-body (alt-initial alt1))))]
	 [split-initial-alt (make-alt split-initial-prog)])
    (build-alt split-initial-alt
	       (append (map (lambda (chng)
			      (change (change-rule chng)
				      (append '(cdr cdr car cdr cdr car) (change-location chng))
				      (change-bindings chng)))
			    (alt-changes alt0))
		       (map (lambda (chng)
			      (change (change-rule chng)
				      (append '(cdr cdr car cdr cdr cdr car) (change-location chng))
				      (change-bindings chng)))
			    (alt-changes alt1))))))

(define (build-alt initial changes)
  (if (null? changes)
      initial
      (build-alt (alt-apply initial (car changes)) (cdr changes))))

(define (alt-initial altn)
  (if (alt-prev altn)
      (alt-initial (alt-prev altn))
      altn))

(define (alt-changes altn)
  (let loop ([cur-alt altn] [acc '()])
    (if (alt-prev cur-alt)
	(loop (alt-prev cur-alt) (cons (alt-change cur-alt) acc))
	acc)))
    
