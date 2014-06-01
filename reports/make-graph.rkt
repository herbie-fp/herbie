#lang racket

(require casio/alternative)
(require casio/points)
(require casio/common)
(require casio/rules)
(require casio/test)
(require reports/svg-tools)
(require reports/tools-common)
(require unstable/list)

(provide make-graph)

(define *line-width* 4)
(define *point-width* 4)
(define *point-opacity* .02)
(define *tick-length* 10)

(define err (current-error-port))

(define (make-graph test end-alt points start-errs end-errs target-errs dir)

  ;; Copy the css files to our graph directory 
  (copy-file "reports/graph.css" (build-path dir "graph.css") #t)

  ;; Generate the html for our graph page
  (write-file (build-path dir "graph.html")
    (printf "<!doctype html>\n")
    (printf "<html>\n")
    (printf "<head>")
    (printf "<meta charset='utf-8' />")
    (printf "<link rel='stylesheet' type='text/css' href='graph.css' />")
    (printf "</head>\n")

    (printf "<body>\n")
    (printf "<div id='graphs'>\n")
    (for ([idx (range (length (test-vars test)))])
      (let-values ([(x-scale x-unscale)
                    (data-log-scale* (map (curryr list-ref idx) points) 10.0 490.0)]
                   [(y-scale y-unscale) (linear-scale* 0 64 175.0 20.0)])
        (printf "<svg width='500' height='300'>\n")
        (draw-line idx points start-errs x-scale y-scale "red")
        (when target-errs
          (draw-line idx points target-errs x-scale y-scale "green"))
        (draw-line idx points end-errs x-scale y-scale "blue")
        (draw-axes x-scale x-unscale y-scale y-unscale)
        (draw-key (list-ref (test-vars test) idx)))
      (printf "</svg>\n"))
    (printf "</div>\n")
    (printf "<ol id='process-info'>\n")
    (output-history end-alt)
    (printf "</ol>\n")))

(define (output-history altn #:stop-at [stop-at #f])
  (cond
   [(not (alt-change altn))
    (printf "<li>Started with <code>~a</code></li>\n" (alt-program altn))]
   [(and stop-at (eq? stop-at altn))
    (void)]
   [(eq? (rule-name (change-rule (alt-change altn))) 'regimes)
    (let* ([vars (change-bindings (alt-change altn))]
           [lft1 (second (assoc 'lft vars))]
           [lft2 (third  (assoc 'lft vars))]
           [rgt1 (second (assoc 'rgt vars))]
           [rgt2 (third  (assoc 'rgt vars))]
           [cond (cdr (assoc 'cond vars))])
      (printf "<h2><code>if <span class='condition'>~a</span></code></h2>\n" cond)
      (printf "<ol>\n")
      (output-history lft1)
      (printf "<li class='regime-break'></li>\n")
      (output-history lft2 #:stop-at lft1)
      (printf "</ol>\n")

      (printf "<h2><code>if not <span class='condition'>~a</span></code></h2>\n" cond)
      (printf "<ol>\n")
      (output-history rgt1)
      (printf "<li class='regime-break'></li>\n")
      (output-history rgt2 #:stop-at rgt1)
      (printf "</ol>\n"))]
   [else
    (output-history (alt-prev altn) #:stop-at stop-at)
    (printf "<li>Considered <span class='count'>~a</span> options "
            (+ 1 (change*-hardness (alt-change altn))))
    (printf "and applied <span class='rule'>~a</span> "
            (rule-name (change-rule (alt-change altn))))
    (printf "to get <code>~a</code></li>\n" (alt-program altn))]))

;(define (make-graph-svg var idx points start-errs end-errs target-errs)
;  (let ([all-points (apply append (map graph-line-points lines))]
;	[margin (* width (/ *margin-%* 100))])
;    (let ([xs (map car all-points)]
;	  [ys (map cdr all-points)])
;      (let-values ([(x-scale x-unscale) (data-log-scale* xs margin (- width margin))]
;		   [(y-scale y-unscale) (linear-scale* 0 64 (- height margin) margin)])
;	(let ([lines*
;               (map (lambda (line) (graph-line
;                                    (map (lambda (p) (cons (x-scale (car p)) (y-scale (cdr p))))
;                                         (graph-line-points line))
;                                    (graph-line-color line)
;                                    (graph-line-name line)
;                                    (graph-line-width line)))
;                    lines)]
;	      ;; The y-coordinate of the x-axis, and the x-coordinate of the y-axis respectively.
;	      [x-axis-y (y-scale (max 0 (apply min ys)))]
;	      [y-axis-x (x-scale (max 0 (apply min xs)))])
;	  ;; Write the outer svg tag
;	  (write-string
;           (svg #:args `((width . ,(number->string width)) (height . ,(number->string height)))
;                (newline)
;                ;; Draw the data.
;                (graph-draw-lines lines*)
;                ;; Draw the x-axis
;                (graph-draw-x-axis margin (- width margin) x-axis-y)
;                ;; Draw the x-ticks
;                (graph-draw-x-ticks x-axis-y margin (- width (* 2 margin)) 16
;                                    (λ (x) (~r (x-unscale x) #:notation 'exponential #:precision 2)))
;                ;; Draw the y-axis
;                (graph-draw-y-axis y-axis-x (- height margin) margin)
;                ;; Draw the y-ticks
;                (graph-draw-y-ticks y-axis-x margin (- height (* 2 margin)) 8
;                                    (λ (y) (~r (y-unscale y) #:notation 'positional #:precision 0)))
;                ;; Draw the key
;                (graph-draw-key margin (lines->color-names lines)))))))))

(define (points->pathdata line)
  (write-string
   (let loop ([pts line] [restart #t])
     (cond
      [(null? pts)
       (void)]
      [(nan? (cdar pts))
       (loop (cdr pts) #t)]
      [else
       (printf (if restart "M~a,~a" "L~a,~a")
               (caar pts) (cdar pts))
       (loop (cdr pts) #f)]))))

(define (ulps->bits x)
  (cond
   [(nan? x) +nan.0]
   [(infinite? x) 64]
   [else (/ (log x) (log 2))]))

(define (draw-line idx pts exs x-scale y-scale color)
  #;(for ([pt pts] [ex exs])
    (printf "<circle cx='~a' cy='~a' r='~a' fill='~a' opacity='~a'/>\n"
            (x-scale (list-ref pt idx)) (y-scale ex)
            *point-width* color *point-opacity*))
  (printf "<path d='~a' stroke='~a' stroke-width='~a' fill='none' />\n"
          (points->pathdata
           (for/list ([gp (group-by (curryr list-ref (+ 1 idx))
                                    (map cons exs pts))])
             (let ([x-value (list-ref (car gp) (+ 1 idx))]
                   [y-value (median (map car gp))])
               (cons (x-scale x-value)
                     (y-scale (ulps->bits y-value))))))
          color *line-width*))

(define (draw-axes x-scale x-unscale y-scale y-unscale)
  (graph-draw-x-axis 10 490 175)
  (graph-draw-y-axis (max 10 (min 790 (x-scale 0))) 175 20)

  (graph-draw-x-ticks 175 10.0 480.0 16
    (λ (x) (~r (x-unscale x) #:notation 'exponential #:precision 0)))

  (graph-draw-y-ticks (max 10 (min 490 (x-scale 0))) 175.0 20.0 8
    (λ (y) (~r (y-unscale y) #:precision 0))))

(define (draw-key name)
  (printf "<g class='legend'>\n")
  (printf "<text x='10' y='15' fill='black' class='title'>~a</text>"
          name)

  (printf "<circle cx='50' cy='10' r='5' fill='red'/>")
  (printf "<text x='57' y='13' fill='black'>Input</text>")

  (printf "<circle cx='100' cy='10' r='5' fill='blue'/>")
  (printf "<text x='107' y='13' fill='black'>Output</text>")

  (printf "<circle cx='155' cy='10' r='5' fill='green'/>")
  (printf "<text x='162' y='13' fill='black'>Target</text>") 

  (printf "</g>"))

(define (median l)
  (let ([len (length l)] [sl (sort l <)])
    (if (odd? len)
        (list-ref sl (/ (- len 1) 2))
        (/ (+ (list-ref sl (/ len 2)) (list-ref sl (+ (/ len 2) 1))) 2))))

(struct graph-line (points color name width) #:transparent)

(define good-point? (compose reasonable-error? cdr))

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
  (map cons (reorder-ys xs xs) (reorder-ys xs ys)))

(define (ys->points* xs ys)
  (filter good-point? (ys->points xs ys)))

;; Given an alternative, and the xs that the alternative's errors were
;; evaluated on, returns a list of points (see point definition above)
;; representing the alt's errors.
(define (alt->error-points xs altn)
  (ys->points* xs (alt-errors altn)))

;; Given an alternative and a list of xs, returns a list of points
;; representing that functions behavior on those xs.
(define (alt->behave-points xs altn)
  (ys->points* xs (fn-points (alt-program altn) (map list xs))))

;; Given an alternative, a list of points that that alterative's
;; errors were evaluated on, a color, and a name, builds a graph-line
;; that represents that alternatives errors.
(define (alt->error-gline xs altn color name #:width [width *line-width*])
  (graph-line (alt->error-points xs altn) color name width))

;; Splits a list into a lists of lists, where each list cooresponds to "tokens"
;; in the original list, containing only items that match pred, and seperated
;; by those items which do not match pred.
(define (tokenize-list pred lst)
  (let loop ([rest lst] [lists-acc '()] [cur-acc '()])
    (if (null? rest)
	(reverse (if (not (null? cur-acc))
		     (cons cur-acc lists-acc)
		     lists-acc))
	(let ([cur-el (car rest)]
	      [rest* (cdr rest)])
	  (if (pred cur-el)
	      (loop rest* lists-acc (cons cur-el cur-acc))
	      (loop rest* (if (null? cur-acc)
			      lists-acc
			      (cons (reverse cur-acc) lists-acc))
		    '()))))))

;; Returns a list of graph-lines of this alternatives error-performance.
(define (alt->error-lines xs altn color name #:width [width *line-width*])
  (ys->tokenized-lines xs (map (λ (e) (cond [(or (nan? e) (infinite? e)) e]
					    [(= 0 e) 0]
					    [(= 1 e) .5]
					    [#t (/ (log e) (log 2))]))
			       (alt-errors altn))
		       color name width))

(define (ys->tokenized-lines xs ys color name width)
  (map (λ (points) (graph-line points color name width))
       (tokenize-list good-point?
		      (ys->points xs ys))))

;; Given an alternative, a list of points, a color, and a name, builds
;; a graph-line that represents that alternatives behavior on those points.
(define (alt->behave-lines xs altn color name #:width [width *line-width*])
  (ys->tokenized-lines xs (fn-points (alt-program altn) (map list xs)) color name width))

;; Given a start, an end, a list of points on which both the start's errors
;; and the end's errors were evaluated, a color and a name, builds a graph-line
;; object that represents the error improvement between the start and end.
(define (get-improvement-lines xs start end color name #:width [width *line-width*])
  (map (λ (points) (graph-line points color name width))
       (tokenize-list good-point?
		      (ys->points xs (map ulps->bits
                                          (errors-difference (alt-errors start)
                                                             (alt-errors end)))))))

;; Takes a list of bits improvement that may or may not include infs, and replaces all infs
;; with cooresponding real numbers of bits lost.

;; Makes a graph of the error-performance of a run
;; with starting alt 'start' and ending alt 'end',
;; and writes it to a file at filename. dir should
;; be a string, not a path-object.
;; Creates a linear scale so that min-domain maps to min-range and max-domain maps to
;; max-range. There are no restrictions on min-domain, max-domain, min-range, or max-range,
;; except that min-domain not equal max-domain.
;; This function returns two values: The scale mapping from the domain to the range,
;; and a scale mapping the other way.
(define (linear-scale* min-domain max-domain min-range max-range)
  (let* ([a (/ (- max-range min-range)
	       (- max-domain min-domain))]
	 [b (- min-range (* a min-domain))])
    (values (lambda (x) (+ (* a x) b))
	    (lambda (y) (/ (- y b) a)))))


(define *base* 2)
(define log-base (compose (curry (flip-args /) (log *base*)) log))

;; Make a log scale that maps min-domain to min-range, and max-domain to max-range.
;; Both min-domain and max-domain are required to be positive, and behaviour could
;; be undesired if min-domain and max-domain are equal, although those cases are handled.
;; There are no restrictions on min-range or max-range.
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

;; min-range should be less than max-range
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
				[(< y zero-position) (- (neg-exp (- zero-position y)))]
				[#t (pos-exp y)]))))))

(define (data-lin-scale* data min-range max-range)
  (let ([min-data (apply min data)]
	[max-data (apply max data)])
    (linear-scale* min-data max-data min-range max-range)))

(define (data-log-scale* data min-range max-range)
  (if (> min-range max-range)
      (let-values ([(flog fexp) (data-log-scale* data max-range min-range)])
	(values (λ (x) (- (+ min-range max-range) (flog x)))
		(λ (y) (fexp (- (+ min-range max-range) y)))))
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
	      [(= 0 max-data) (let ([largest-non-zero (apply max (filter (compose not zero?) data))])
				(let-values ([(log-scale exp-scale)
					      (make-log-scale* (- largest-non-zero) (- min-data) max-range min-range)])
				  (values (lambda (x) (if (= x 0) max-range
							  (log-scale (- x))))
					  (lambda (y) (if (= y max-range) 0
							  (- (exp-scale y)))))))]
	      [#t (let ([max-neg-data (apply max (filter negative? data))]
			[min-pos-data (apply min (filter positive? data))])
		    (make-full-log-scale* min-data max-neg-data min-pos-data max-data min-range max-range))]))))

(define *label-verticle-distance* 20)
(define *text-height* 8)
(define *text-width* 25)
(define *margin-%* 15)
(define *x-label-rotation* 40)

;; Returns count evenly distributed numbers from min to max,
;; where the first number is min and the last is max.
(define (make-ticks count min max)
  (build-list (add1 count) (λ (n) (+ min (* n (/ (- max min) count))))))

(define (graph-draw-lines lines)
  (for/list ([line lines])
    (path #:args `((d . ,(points->pathdata (graph-line-points line)))
		   (stroke . ,(graph-line-color line))
		   (stroke-width . ,(graph-line-width line))
		   (fill . "none")))
    (newline)))

(define (graph-draw-x-ticks y-pos min-x max-x num-ticks x-pos->label)
  (let ([x-ticks (make-ticks num-ticks min-x max-x)])
    (for/list ([x x-ticks])
      (line #:args `((x1 . ,x) (y1 . ,y-pos) (x2 . ,x) (y2 . ,(+ y-pos *tick-length*)) (stroke . "black")))
      (newline)
      (draw-text (cons x (+ y-pos *label-verticle-distance*))
		 *x-label-rotation* (text (x-pos->label x)))
      (newline))))

(define (graph-draw-y-ticks x-pos min-y max-y num-ticks y-pos->label)
  (let ([y-ticks (make-ticks num-ticks min-y max-y)])
    (for/list ([y (cdr y-ticks)])
      (line #:args `((x1 . ,x-pos) (y1 . ,y) (x2 . ,(- x-pos *tick-length*)) (y2 . ,y) (stroke . "black")))
      (newline)
      (draw-text (cons (- x-pos *text-width*) (+ y (/ *text-height* 2)))
		 0 (text (y-pos->label y)))
      (newline))))

;; Draws axis. See graph-draw-key for assumptions.
(define (graph-draw-y-axis x-pos y1 y2)
  (graph-draw-axis-line x-pos x-pos y1 y2))

(define (graph-draw-x-axis x1 x2 y-pos)
  (graph-draw-axis-line x1 x2 y-pos y-pos))

(define (graph-draw-axis-line x1 x2 y1 y2)
  (line #:args `((x1 . ,x1) (y1 . ,y1) (x2 . ,x2) (y2 . ,y2) (stroke . "black")))
  (newline))

;; Takes a list of lines, of which some can have the same names and colors, and returns color-name pairs
;; (the input for graph-draw-key) cooresponding to the lines, removing duplicate color-name pairs.
(define (lines->color-names lines)
  (remove-duplicates (map (λ (line) (cons (graph-line-color line) (graph-line-name line)))
			  lines)))

(define *key-verticle-spacing* 10)
(define *key-horizontal-spacing* 10)
(define *key-circle-radius* 8)
(define *key-outline-width* 3)

;; Draws a graph-key. This function assumes that it is called within an svg
;; tag, which is within either write-file or write-string. If this is not within
;; an svg tag, you'll get the inner tags without context, so it might not work
;; like you expect. If this is not within a write-file or write-string, the
;; tags will be printed to standard out.
;; The function accepts as an argument cons cells, of which the first item is
;; the string representation of the color, and the second is the name associate
;; with that color.
;; The formatting of the graph key is controlled by some globally defined format
;; variables, see above.
(define (graph-draw-key margin color-names)
  (for/list ([color-name color-names] [index (build-list (length color-names) identity)])
    (let ([verticle-mod (* index (+ (arithmetic-shift *key-circle-radius* 1)
				    *key-verticle-spacing*))]
	  [half-margin (arithmetic-shift margin -1)])
      (circle #:args `((cx . ,half-margin ) (cy . ,(+ half-margin verticle-mod))
		       (r . ,*key-circle-radius*) (stroke . "black")
		       (stroke-width . *key-outline-width*) (fill . ,(car color-name))))
      (newline)
      (text-tag #:args `((x . ,(+ half-margin *key-circle-radius* *key-horizontal-spacing*))
			 (y . ,(+ half-margin verticle-mod)) (fill . "black"))
		(text (cdr color-name)))
      (newline))))
