#lang racket

(require casio/alternative)
(require casio/points)
(require casio/common)
(require reports/html-tools)
(require reports/svg-tools)

(provide (all-defined-out))

(struct graph-line (points color name width) #:transparent)

(define *default-width* 2)

(define good-point? (compose reasonable-error? cdr))

;; Returns the full local path of the canonical location for an included file with the given extension.
(define (include-path dir index extension)
  (string-append dir (include-name index extension)))

;; Returns the filename for an included file with the given extension
(define (include-name index extension)
  (string-append (number->string index) "include." extension))

;; Given a list of xs and a list of ys, returns the ys reordered so that,
;; if each input y cooresponds to it's matching x, the xs that the reordered ys
;; coorespond to are sorted in ascending order.
(define (reorder-ys xs ys)
  (parameterize ([*points* (map list xs)])
    (ascending-order 0 ys)))

;; Given a list of xs and ys where each y cooresponds to the x with the matching
;; index, but the xs and ys are sorted in no particular order other than that,
;; and some of the ys may be inf or nan, returns a list of points, where each point
;; is a cons cell with (eq? (car p) x), (eq? (cdr p) y), with only reasonable-error? ys,
;; and sorted so that the xs of the points are in ascending order.
(define (ys->points xs ys)
  (filter good-point? (map cons (reorder-ys xs xs) (reorder-ys xs ys))))

;; Given an alternative, and the xs that the alternative's errors were
;; evaluated on, returns a list of points (see point definition above)
;; representing the alt's errors.
(define (alt->error-points xs altn)
  (ys->points xs (alt-errors altn)))

;; Given an alternative and a list of xs, returns a list of points
;; representing that functions behavior on those xs.
(define (alt->behave-points xs altn)
  (ys->points xs (fn-points (alt-program altn) (map list xs))))

;; Given an alternative, a list of points that that alterative's
;; errors were evaluated on, a color, and a name, builds a graph-line
;; that represents that alternatives errors.
(define (alt->error-gline xs altn color name #:width [width *default-width*])
  (graph-line (alt->error-points xs altn) color name width))

;; Given an alternative, a list of points, a color, and a name, builds
;; a graph-line that represents that alternatives behavior on those points.
(define (alt->behave-gline xs altn color name #:width [width *default-width*])
  (graph-line (alt->behave-points xs altn) color name width))

;; Given a start, an end, a list of points on which both the start's errors
;; and the end's errors were evaluated, a color and a name, builds a graph-line
;; object that represents the error improvement between the start and end.
(define (get-improvement-line xs start end color name #:width [width *default-width*])
  (graph-line (ys->points xs (map (curry expt 2) (errors-difference (alt-errors start) (alt-errors end))))
	      color name width))

;; Builds a graph line for exacts, given the exacts, the points,
;; a color and a name.
(define (get-exacts-line xs exacts color name #:width [width *default-width*])
  (graph-line (ys->points xs exacts) color name width))

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
    (let ([pre-errors (alt->error-gline xs start "yellow" "pre-errors" #:width 5)]
	  [post-errors (alt->error-gline xs end "blue" "post-errors")]
	  [improvement-line (get-improvement-line xs start end "green" "improvement")]
	  [exacts-line (get-exacts-line xs exacts "green" "exacts" #:width 8)]
	  [pre-behavior (alt->behave-gline xs start "yellow" "pre-behavior" #:width 5)]
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
							  0 0 800 800 #:relog-ys #t))
				    (newline)
				    (text (make-graph-svg (list exacts-line pre-behavior post-behavior)
							  0 900 800 800))
				    (newline))
			      ))))))

;; Copies a file, replacing the file at destination if it exists.
(define (copyr-file src dest)
  (when (file-exists? dest) (delete-file dest))
  (copy-file src dest))

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
  (cond [(not (= min-domain max-domain))
	 (let-values  ([(out-linear-l in-linear-e) (linear-scale* (log-base min-domain) (log-base max-domain)
								  min-range max-range)])
	   (values (compose out-linear-l log-base)
		   (compose (curry expt *base*) in-linear-e)))]
	[(< min-domain 1)
	 (make-log-scale* min-domain 1 min-range max-range)]
	[(> min-domain 1)
	 (make-log-scale* 1 min-domain min-range max-range)]
	[#t (values (const min-range) (const 1))]))

(define (make-full-log-scale* min-domain max-neg-domain min-pos-domain max-domain min-range max-range)
  (let* ([neg-scalar (/ (max 0 (log-base (- min-domain)))
			(+ (log-base max-domain) (log-base (- min-domain))))]
	 [zero-position (+ min-range (* neg-scalar (- max-range min-range)))])
    (let-values ([(pos-log pos-exp)
		  (make-log-scale* min-pos-domain max-domain zero-position max-range)]
		 [(neg-log neg-exp)
		  (make-log-scale* (- max-neg-domain) (- min-domain) 0 (- zero-position min-range))])
      (values (lambda (x) (cond [(= x 0) zero-position]
				[(< x 0)
				 (- zero-position (neg-log (- x)))]
				[#t (pos-log x)]))
	      (lambda (y) (cond [(= y zero-position) 0]
				[(< y zero-position) (neg-exp (- zero-position y))]
				[#t (pos-exp y)]))))))

(define (data-scale* data min-range max-range)
  (let ([min-data (apply min data)]
	[max-data (apply max data)])
    (cond [(positive? min-data) (make-log-scale* min-data max-data min-range max-range)]
	  [(negative? max-data) (let-values ([(log-scale exp-scale)
					      (make-log-scale* (- max-data) (- min-data) min-range max-range)])
				  (values (lambda (x) (- (log-scale (- x))))
					  (lambda (y) (- (exp-scale (- y))))))]
	  [(= 0 min-data) (let ([smallest-nonzero (apply min (filter (compose not zero?) data))])
			    (let-values ([(log-scale exp-scale)
					  (make-log-scale* smallest-nonzero max-data min-range max-range)])
			    (values (lambda (x) (if (= x 0) min-range
						    (log-scale x)))
				    (lambda (x) (if (= x min-range) 0
						    (exp-scale x))))))]
	  [#t (let ([max-neg-data (apply max (filter negative? data))]
		    [min-pos-data (apply min (filter positive? data))])
		(make-full-log-scale* min-data max-neg-data min-pos-data max-data min-range max-range))])))

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

(define *num-ticks* 8)
(define *tick-length* 20)
(define *label-verticle-distance* 30)
(define *text-height* 8)
(define *text-width* 80)
(define *margin-%* 15)
(define *key-verticle-spacing* 10)
(define *key-horizontal-spacing* 10)
(define *key-circle-radius* 8)

(define (make-graph-svg lines x-pos y-pos width height #:relog-xs [relog-x #f] #:relog-ys [relog-y #f])
  (let ([all-points (apply append (map graph-line-points lines))]
	[margin (* width (/ *margin-%* 100))])
    (let ([xs (map car all-points)]
	  [ys (map cdr all-points)])
      (let-values ([(x-log x-exp) (data-scale* xs margin (- width margin))]
		   [(y-log y-exp) (data-scale* ys margin (- height margin))])
	  (let ([x-ticks (build-list (add1 *num-ticks*) (lambda (n) (+ margin (* n (/ (- width (* 2 margin)) *num-ticks*)))))]
		[y-ticks (build-list (add1 *num-ticks*) (lambda (n) (- height (+ margin (* n (/ (- height (* 2 margin)) *num-ticks*))))))]
		[y-log* (lambda (y) (- height (y-log y)))]
		[y-exp* (lambda (x) (y-exp (- height x)))])
	    (let ([lines* (map (lambda (line) (graph-line (map (lambda (p) (cons (x-log (car p)) (y-log* (cdr p))))
							       (graph-line-points line))
							  (graph-line-color line)
							  (graph-line-name line)
							  (graph-line-width line)))
			       lines)]
		  ;; The y-coordinate of the x-axis, and the x-coordinate of the y-axis respectively.
		  [x-axis-y (y-log* (max 0 (apply min ys)))]
		  [y-axis-x (x-log (max 0 (apply min xs)))])
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
					     (display (~r (if relog-x (log-base (x-exp x)) (x-exp x)) #:notation 'exponential #:precision 2)))
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
						      (y . ,(- (exact->inexact y) *text-height*))
						      (fill . "black"))
					     (display (if relog-y
							  (~r (let ([exp-value (y-exp* y)])
								(if (= exp-value 0) ;; Handle the special case
								    0
								    (log-base exp-value))))
							  (~r (y-exp* y) #:notation 'exponential #:precision 4))))
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
						  (stroke-width . ,(graph-line-width line))
						  (fill . "none")))
				   (newline))
				 (newline)))))))))
