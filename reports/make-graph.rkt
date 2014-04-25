#lang racket

(require casio/alternative)
(require casio/points)
(require casio/common)
(require reports/html-tools)
(require reports/tools-common)
(require reports/svg-tools)
(require json)

(provide (all-defined-out))

;; Makes a graph of the error-performance of a run
;; with starting alt 'start' and ending alt 'end',
;; and writes it to a file at filename. dir should
;; be a string.
(define (make-graph start end points exacts dir include-css)
  ;; Returns the full local path of the canonical location for an included file with the given extension.
  (define (include-path index extension)
    (string-append dir (include-name index extension)))

  (define (include-name index extension)
    (string-append (number->string index) "include." extension))

  (define point-filter (compose reasonable-error? cdr))

  ;; Copy the css files to our graph directory 
  (for/list ([css include-css] [i (build-list (length include-css) identity)])
    (copyr-file css (include-path i "css")))

  ;; Generate the html for our graph page
  (let ([page-path (string-append dir "graph.html")])
    (parameterize ([*points* points]) ;; We need this for ordering
      (let ([ascending-points (ascending-order 0 (*points*))])
	(define (errors->error-line errors)
	  (ys->lines (ascending-order 0 errors)))
	(define alt->error-line (compose errors->error-line alt-errors))
	(define ys->lines (compose (curry filter point-filter) (curry map cons (map car ascending-points))))
	(let ([pre-errors (alt->error-line start)]
	      [post-errors (alt->error-line end)])
	  (write-file page-path
		      (html (newline)
			    (head (newline)
				  ;; Include all our given css
				  (for/list ([i (build-list (length include-css) identity)])
				    (link #:args `((rel . "stylesheet") (type . "text/css") (href . ,(include-name i "css")) (media . "screen")))
				    (newline))
				  (newline)
				  (body (newline)
					(text (make-graph-svg (list pre-errors post-errors
								    (errors->error-line (errors-difference (alt-errors start)
													   (alt-errors end))))
							      (list "yellow" "blue" "green")
							      (list "pre-errors" "post-errors" "improvement")
							      0 0 800 800))
					(newline)
					(text (make-graph-svg (map ys->lines (list exacts (fn-points (alt-program start) ascending-points)
									       (fn-points (alt-program end) ascending-points)))
							      (list "green" "yellow" "blue")
							      (list "exacts" "pre-behavior" "post-behavior")
							      0 900 800 800))
					(newline))
				  ))))))))

;; Copies a file, replacing the file at destination if it exists.
(define (copyr-file src dest)
  (when (file-exists? dest) (delete-file dest))
  (copy-file src dest))

(define (make-log-scale min-domain max-domain min-range max-range)
  (let-values ([(log exp) (make-log-scale* min-domain max-domain min-range max-range)])
    log))

;; (define (make-log-scale* min-domain max-domain min-range max-range)
;;   (define (safe-log x) (if (= x 0) 0 (log-base x)))
;;   (cond [(= 0 min-range)
;; 	 ;; Special case: When the mininum range is zero, use this trick to get it all to work.
;; 	 (let-values ([(log* exp*) (make-log-scale* min-domain max-domain (+ min-range 1) (+ max-range 1))])
;; 	   (values (lambda (x) (- (log* x) 1))
;; 		   (lambda (y) (exp* (+ y 1)))))]
;; 	[(= 0 max-range)
;; 	 ;; Special case: When the maximum range is zero, use this trick to get it all to work.
;; 	 (let-values ([(log* exp*) (make-log-scale* min-domain max-domain (- min-range 1) (- max-range 1))])
;; 	   (values (lambda (x) (+ (log* x) 1))
;; 		   (lambda (y) (exp* (- y 1)))))]
;; 	[#t (let* ([a (/ (- min-range max-range)
;; 			 (log-base (/ min-domain max-domain)))]
;; 		   [b (/ (expt *base* (/ min-range a)) min-domain)])
;; 	      (values (lambda (x) (* a (safe-log (* b x))))
;; 		      (lambda (y) (/ (expt *base* (/ y a)) b))))]))

(define (linear-scale* min-domain max-domain min-range max-range)
  (let* ([a (/ (- max-range min-range)
	       (- max-domain min-domain))]
	 [b (- min-range (* a min-domain))])
    (values (lambda (x) (+ (* a x) b))
	    (lambda (y) (/ (- y b) a)))))


(define *base* 2)
(define log-base (compose (curry (flip-args /) (log *base*)) log))

(define (make-log-scale* min-domain max-domain min-range max-range)
  (if (< min-domain 1)
      (let ([translate (curry + (- 1 min-domain))])
	(let-values ([(lscale escale) (make-log-scale* (translate min-domain) (translate max-domain) min-range max-range)])
	  (values (compose lscale translate)
		  (compose escale translate))))
      (let-values  ([(out-linear-l in-linear-e) (linear-scale* (log-base min-domain) (log-base max-domain)
							       min-range max-range)])
	(values (compose out-linear-l log-base)
		(compose (curry expt *base*) in-linear-e)))))

(define (make-full-log-scale min-domain max-domain min-range max-range)
  (let-values ([(log exp) (make-full-log-scale* min-domain max-domain min-range max-range)])
    log))

;; Fails on extremely unbalance domains, like (-1000000,10)
(define (make-full-log-scale* min-domain max-domain min-range max-range)
  (cond [(< 0 min-domain) (make-log-scale* min-domain max-domain min-range max-range)]
	[(> 0 max-domain) (let-values ([(log exp) (make-log-scale* (- max-domain) (- min-domain) (- max-range) (- min-range))])
			    (values (lambda (x) (- (log (- x))))
				    (lambda (y) (- (exp (- y))))))]
	[#t (let* ([neg-scalar (/ (max 0 (log-base (- min-domain)))
				  (+ (log-base max-domain) (log-base (- min-domain))))]
		   [zero-position (+ min-range (* neg-scalar (- max-range min-range)))])
	      (let-values ([(pos-log pos-exp)
			    (make-log-scale* 1 (+ max-domain 1) zero-position max-range)]
			   [(neg-log neg-exp)
			    (make-log-scale* 1 (+ (- min-domain) 1) 0 (- zero-position min-range))])
		;; It's potentially possible for the domain on one side of zero to be
		;; so much bigger than the domain on the other side that the value of zero-position
		;; is rounded to max-range or min-range. In this case, we want to handle it gracefully.
		(cond [(= 0 (- zero-position max-range))
		       (values (compose neg-log add1)
			       (compose sub1 neg-exp))]
		      [(= 0 (- zero-position min-range))
		       (values (compose pos-log add1)
			       (compose sub1 pos-exp))]
		      [#t
		       (values (lambda (x) (if (< x 0)
					       (- zero-position (neg-log (- 1 x)))
					       (pos-log (+ x 1))))
			       (lambda (y) (if (< y zero-position)
					       (- 1 (neg-exp (- zero-position y)))
					       (- (pos-exp y) 1))))])))]))

(define (line->pathdata-string line)
  (define (print-point p)
    (write (car p))
    (display ",")
    (write (cdr p)))
  (write-string (display "M")
		(print-point (car line))
		(for/list ([point (cdr line)])
		  (display "L")
		  (print-point point))))

(define (make-graph-svg lines colors names x-pos y-pos width height)
  (define *num-ticks* 8)
  (define *tick-length* 20)
  (define *label-verticle-distance* 30)
  (define *text-height* 8)
  (define *text-width* 80)
  (define *margin-%* 15)
  (define *key-verticle-spacing* 10)
  (define *key-horizontal-spacing* 10)
  (define *key-circle-radius* 8)
  (let ([all-points (apply append lines)]
	[margin (* width (/ *margin-%* 100))])
    (let ([xs (map car all-points)]
	  [ys (map cdr all-points)])
      (let ([max-x (apply max xs)]
	    [min-x (apply min xs)]
	    [max-y (apply max ys)]
	    [min-y (apply min ys)])
	(let-values ([(x-log x-exp) (make-full-log-scale* min-x max-x margin (- width margin))]
		     [(y-log y-exp) (make-full-log-scale* min-y max-y margin (- height margin))])
	  (let ([x-ticks (build-list (add1 *num-ticks*) (lambda (n) (+ margin (* n (/ (- width (* 2 margin)) *num-ticks*)))))]
		[y-ticks (build-list (add1 *num-ticks*) (lambda (n) (- height (+ margin (* n (/ (- height (* 2 margin)) *num-ticks*))))))]
		[y-log* (lambda (y) (- height (y-log y)))]
		[y-exp* (lambda (x) (y-exp (- height x)))])
	    (let ([lines* (map (lambda (line) (map (lambda (p) (cons (x-log (car p)) (y-log* (cdr p))))
						   line))
			       lines)]
		  ;; The y-coordinate of the x-axis, and the x-coordinate of the y-axis respectively.
		  [x-axis-y (y-log* (max 0 min-y))]
		  [y-axis-x (x-log (max 0 min-x))])
	      ;; Write the outer svg tag
	      (write-string (svg #:args `((width . ,(number->string width)) (height . ,(number->string height))
					  (x . ,x-pos) (y . ,y-pos))
				 (newline)
				 ;; Draw the x-axis
				 (line #:args `((x1 . ,margin) (y1 . ,x-axis-y)
						(x2 . ,(- width margin)) (y2 . ,x-axis-y)
						(stroke . "black")))
				 (newline)
				 ;; Draw the x-ticks
				 (for/list ([x x-ticks])
				   (line #:args `((x1 . ,(exact->inexact x)) (y1 . ,x-axis-y)
						  (x2 . ,(exact->inexact x)) (y2 . ,(+ x-axis-y *tick-length*))
						  (stroke . "black")))
				   (newline)
				   (draw-text `(,(exact->inexact (- x 10)) . ,(+ x-axis-y *label-verticle-distance*))
					      40
					     (display (~r (x-exp x) #:notation 'exponential #:precision 2)))
				   (newline))
				 ;; Draw the y-axis
				 (line #:args `((x1 . ,y-axis-x) (y1 . ,(- height margin))
						(x2 . ,y-axis-x) (y2 . ,margin)
						(stroke . "black")))
				 (newline)
				 ;; Draw the y-ticks
				 (for/list ([y y-ticks])
				   (line #:args `((x1 . ,y-axis-x) (y1 . ,(exact->inexact y))
						  (x2 . ,(- y-axis-x *tick-length*)) (y2 . ,(exact->inexact y))
						  (stroke . "black")))
				   (newline)
				   (text-tag #:args `((x . ,(- y-axis-x *text-width*))
						      (y . ,(- height (- (exact->inexact y) *text-height*)))
						      (fill . "black"))
					     (display (~r (y-exp y) #:notation 'exponential #:precision 4)))
				   (newline))
				 ;; Draw the key
				 (for/list ([name names] [color colors] [index (build-list (length names) identity)])
				   (let ([verticle-mod (* index (+ (arithmetic-shift *key-circle-radius* 1)
								     *key-verticle-spacing*))])
				     (circle #:args `((cx . ,(arithmetic-shift margin -1)) (cy . ,(+ verticle-mod
												     (arithmetic-shift margin -1)))
						      (r . ,*key-circle-radius*) (stroke . "black")
						      (stroke-width . 3) (fill . ,color)))
				     (newline)
				     (xml-comment (text color))
				     (newline)
				     (text-tag #:args `((x . ,(+ (arithmetic-shift margin -1) (+ *key-circle-radius* *key-horizontal-spacing*)))
							(y . ,(+ (arithmetic-shift margin -1)
								 verticle-mod))
							(fill . "black"))
					     (text name)))
				   (newline))
				 ;; Draw the data.
				 (for/list ([line lines*] [color colors])
				   (path #:args `((d . ,(line->pathdata-string line)) (stroke . ,color)
						  (fill . "none")))
				   (newline))
				 (newline))))))))))
