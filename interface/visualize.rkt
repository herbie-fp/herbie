#lang racket

(require herbie/common)
(require herbie/points)
(require herbie/programs)

(require plot)
(require math/flonum)

(provide (all-defined-out))

(define y-min 0)
(define y-max 64)
(define x-min (- (log2 +max.0)))
(define x-max (log2 +max.0))

(define (viz-error type #:prog [prog (*start-prog*)] #:key-expr [sort-on #f])
  (let* ([eval-func (if sort-on
			(eval-prog `(λ ,(program-variables prog)
				      ,sort-on)
				   mode:fl)
			car)]
	 [sorted-pts (sort (for/list ([(pt ex) (in-pcontext (*pcontext*))]
				      [err (errors prog (*pcontext*))])
			     (list (two-sided-log2 (eval-func pt)) (log2 err)))
			   <
			   #:key car)]
	 [graph-func (match type
		       ['scatter viz-points-scatter]
		       ['line viz-points-group]
		       [_ (error "We don't know that graph type!")])])
    (graph-func sorted-pts)))

(define (viz-points-scatter pts)
  (plot (points pts #:y-min y-min #:y-max y-max
		#:x-min x-min #:x-max x-max)))

;; These assume sorted points
(define (viz-points-group pts [group-size 10])
  (let* ([bticks (for/list ([n (in-range
				(inexact->exact
				 (ceiling (/ (- x-max x-min)
					     group-size))))])
		   (+ (* group-size (add1 n)) x-min))])
    (plot (lines (bucket-points pts bticks))
	  #:y-min y-min #:y-max y-max
	  #:x-min x-min #:x-max x-max)))

(define (bucket-points pts ticks)
  (let loop ([rest-ticks ticks]
	     [rest-points pts]
	     [acc '()])
    (if (null? rest-ticks) (reverse acc)
	(let-values ([(bucketed-points leftover-points)
		      (splitf-at rest-points
				 (λ (p)
				   (> (car rest-ticks)
				      (car p))))])
	  (loop (cdr rest-ticks) leftover-points
		(if (null? bucketed-points)
		    acc
		    (cons (list (car rest-ticks)
				(/ (foldl + 0 (map cadr bucketed-points))
				   (length bucketed-points)))
			  acc)))))))

(define (two-sided-log2 x)
  (cond [(positive? x)
	 (log2 x)]
	[(negative? x)
	 (- (log2 (- x)))]
	[#t 0]))
