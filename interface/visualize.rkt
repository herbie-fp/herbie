#lang racket

(require herbie/common)
(require herbie/points)
(require herbie/programs)
(require interface/util)

(require plot)
(require math/flonum)

(provide (all-defined-out))

(define y-min (make-parameter 0))
(define y-max (make-parameter 64))
(define x-min (make-parameter (- (log2 +max.0))))
(define x-max (make-parameter (log2 +max.0)))

(define (viz-error type #:prog [prog (*start-prog*)] #:key-expr [sort-on #f]
		   #:mark-x [mark-x #f] #:mark-y [mark-y #f]
		   #:out-file [filename #f])
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
		       [_ (error "We don't know that graph type!")])]
	 [lines (append (list (graph-func sorted-pts))
			(if mark-x (list (inverse (const mark-x))) '())
			(if mark-y (list (inverse (const mark-y))) '()))])
    (plot lines #:y-min (y-min) #:y-max (y-max)
	  #:x-min (x-min) #:x-max (x-max)
	  #:out-file filename)))

(define (viz-points-scatter pts)
  (points pts))

;; These assume sorted points
(define (viz-points-group pts [group-size 10])
  (let* ([bticks (for/list ([n (in-range
				(inexact->exact
				 (ceiling (/ (- (x-max) (x-min))
					     group-size))))])
		   (+ (* group-size (add1 n)) (x-min)))])
    (lines (bucket-points pts bticks))))

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
