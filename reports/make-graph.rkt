#lang racket

(require casio/alternative)
(require casio/points)
(require casio/common)
(require reports/html-tools)
(require reports/svg-tools)

(provide (all-defined-out))

(struct graph-line (points color name) #:transparent)

(define good-point? (compose reasonable-error? cdr))

;; Returns the full local path of the canonical location for an included file with the given extension.
(define (include-path dir index extension)
  (string-append dir (include-name index extension)))

(define (include-name index extension)
  (string-append (number->string index) "include." extension))

(define (reorder-ys xs ys)
  (parameterize ([*points* (map list xs)])
    (ascending-order 0 ys)))

(define (ys->points xs ys)
  (filter good-point? (map cons xs (reorder-ys xs ys))))

(define (alt->error-points xs altn)
  (ys->points xs (alt-errors altn)))

(define (alt->behave-points xs altn)
  (ys->points xs (fn-points (alt-program altn) (map list xs))))

(define (alt->error-gline xs altn color name)
  (graph-line (alt->error-points xs altn) color name))

(define (alt->behave-gline xs altn color name)
  (graph-line (alt->behave-points xs altn) color name))

(define (get-improvement-line xs start end color name)
  (graph-line (ys->points xs (map (curry expt 2) (errors-difference (alt-errors start) (alt-errors end))))
	      color name))

(define (get-exacts-line xs exacts color name)
  (graph-line (ys->points xs exacts) color name))

;; Makes a graph of the error-performance of a run
;; with starting alt 'start' and ending alt 'end',
;; and writes it to a file at filename. dir should
;; be a string.
(define (make-graph start end points exacts dir include-css)

  ;; Copy the css files to our graph directory 
  (for/list ([css include-css] [i (build-list (length include-css) identity)])
    (copyr-file css (include-path dir i "css")))

  ;; Generate the html for our graph page
  (let ([page-path (string-append dir "graph.html")]
	[xs (map car points)])
    (let ([pre-errors (alt->error-gline xs start "blue" "pre-errors")]
	  [post-errors (alt->error-gline xs end "green" "post-errors")]
	  [improvement-line (get-improvement-line xs start end "yellow" "improvement")]
	  [exacts-line (get-exacts-line xs exacts "green" "exacts")]
	  [pre-behavior (alt->behave-gline xs start "yellow" "pre-behavior")]
	  [post-behavior (alt->behave-gline xs end "blue" "post-behavior")])
      (write-file page-path
		  (html (newline)
			(head (newline)
			      ;; Include all our given css
			      (for/list ([i (build-list (length include-css) identity)])
				(link #:args `((rel . "stylesheet") (type . "text/css") (href . ,(include-name i "css")) (media . "screen")))
				(newline))
			      (newline)
			      (body (newline)
				    (text (make-graph-svg (list pre-errors post-errors improvement-line)
							  0 0 800 800))
				    (newline)
				    (text (make-graph-svg (list exacts-line pre-behavior post-behavior)
							  0 900 800 800))
				    (newline))
			      ))))))

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

(define (line-points->pathdata-string line)
  (define (print-point p)
    (write (car p))
    (display ",")
    (write (cdr p)))
  (write-string (display "M")
		(print-point (car line))
		(for/list ([point (cdr line)])
		  (display "L")
		  (print-point point))))

(define (make-graph-svg lines x-pos y-pos width height)
  (define *num-ticks* 8)
  (define *tick-length* 20)
  (define *label-verticle-distance* 30)
  (define *text-height* 8)
  (define *text-width* 80)
  (define *margin-%* 15)
  (define *key-verticle-spacing* 10)
  (define *key-horizontal-spacing* 10)
  (define *key-circle-radius* 8)
  (let ([all-points (apply append (map graph-line-points lines))]
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
	    (let ([lines* (map (lambda (line) (graph-line (map (lambda (p) (cons (x-log (car p)) (y-log* (cdr p))))
							       (graph-line-points line))
							  (graph-line-color line)
							  (graph-line-name line)))
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
				 (for/list ([line lines] [index (build-list (length lines) identity)])
				   (let ([verticle-mod (* index (+ (arithmetic-shift *key-circle-radius* 1)
								     *key-verticle-spacing*))])
				     (circle #:args `((cx . ,(arithmetic-shift margin -1)) (cy . ,(+ verticle-mod
												     (arithmetic-shift margin -1)))
						      (r . ,*key-circle-radius*) (stroke . "black")
						      (stroke-width . 3) (fill . ,(graph-line-color line))))
				     (newline)
				     (text-tag #:args `((x . ,(+ (arithmetic-shift margin -1) (+ *key-circle-radius* *key-horizontal-spacing*)))
							(y . ,(+ (arithmetic-shift margin -1)
								 verticle-mod))
							(fill . "black"))
					     (text (graph-line-name line))))
				   (newline))
				 ;; Draw the data.
				 (for/list ([line lines*])
				   (path #:args `((d . ,(line-points->pathdata-string (graph-line-points line)))
						  (stroke . ,(graph-line-color line))
						  (fill . "none")))
				   (newline))
				 (newline))))))))))
