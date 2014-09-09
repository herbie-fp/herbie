#lang racket

(require racket/pretty)
(require casio/common)
(require casio/points)
(require casio/matcher)
(require casio/alternative)
(require casio/test)
(require casio/combine-alts)
(require casio/programs)

(provide make-graph make-traceback make-timeout)

(define *line-width* 3)
(define *point-width* 2)
(define *point-opacity* .2)

(define *tick-length* 10)
(define *label-shift* 10)
(define *label-rotation* 40)
(define *label-height* 8)
(define *label-width* 15)

(define (make-graph test end-alt points start-errs end-errs target-errs bits dir profile?)
  (printf "<!doctype html>\n")
  (printf "<html>\n")
  (printf "<head>")
  (printf "<meta charset='utf-8' />")
  (printf "<title>Results for ~a</title>" (test-name test))
  (printf "<link rel='stylesheet' type='text/css' href='../graph.css' />")
  (printf "</head>\n")
  (printf "<body>\n")

  (printf "<dl id='about'>\n")
  (printf "<dt>Test:</dt><dd>~a</dd>" (test-name test))
  (printf "<dt>Logs:</dt>")
  (printf "<dd><a href='debug.txt'>Debug output</a>")
  (when profile?
    (printf ", <a href='profile.txt'>Profiling report</a>"))
  (printf "</dd>\n")
  (printf "<dt>Bits:</dt><dd>~a bits</dd>\n" bits)
  (printf "</dl>\n")

  (printf "<div id='graphs'>\n")
  (for ([idx (range (length (test-vars test)))])
    (let-values ([(x-scale x-unscale)
                  (data-log-scale* (map (curryr list-ref idx) points) 10.0 490.0)]
                 [(y-scale y-unscale) (linear-scale* 0 64 175.0 20.0)])
      (printf "<svg width='500' height='300'>\n")
      (set-up-line)
      (draw-line idx points start-errs x-scale y-scale "red")
      (when target-errs
        (draw-line idx points target-errs x-scale y-scale "green"))
      (draw-line idx points end-errs x-scale y-scale "blue")
      (draw-axes x-scale x-unscale y-scale y-unscale)
      (draw-key (list-ref (test-vars test) idx) target-errs))
    (printf "</svg>\n"))
  (printf "</div>\n")

  (printf "<ol id='process-info'>\n")
  (output-history end-alt)
  (printf "</ol>\n")

  (printf "</body>\n")
  (printf "</html>\n"))

(define (make-traceback test err bits profile?)
  (printf "<!doctype html>\n")
  (printf "<html>\n")
  (printf "<head>\n")
  (printf "<meta charset='utf-8' />\n")
  (printf "<title>Exception for ~a</title>" (test-name test))
  (printf "<link rel='stylesheet' type='text/css' href='../graph.css' />")
  (printf "</head>")
  (printf "<body>\n")

  (printf "<dl id='about'>\n")
  (printf "<dt>Test:</dt><dd>~a</dd>" (test-name test))
  (printf "<dt>Logs:</dt>")
  (printf "<dd><a href='debug.txt'>Debug output</a>")
  (when profile?
    (printf ", <a href='profile.txt'>Profiling report</a>"))
  (printf "</dd>\n")
  (printf "<dt>Bits:</dt><dd>~a bits</dd>\n" bits)
  (printf "</dl>\n")

  (printf "<h2 id='error-message'>~a</h2>\n" (html-escape-unsafe (exn-message err)))
  (printf "<ol id='traceback'>\n")
  (for ([tb (continuation-mark-set->context (exn-continuation-marks err))])
    (printf "<li><code>~a</code> in <code>~a</code></li>\n"
            (html-escape-unsafe (~a (car tb))) (srcloc->string (cdr tb))))
  (printf "</ol>\n")

  (printf "</body>\n")
  (printf "</html>\n"))

(define (make-timeout test bits profile?)
  (printf "<!doctype html>\n")
  (printf "<html>\n")
  (printf "<head>\n")
  (printf "<meta charset='utf-8' />\n")
  (printf "<title>Timeout for ~a</title>" (test-name test))
  (printf "<link rel='stylesheet' type='text/css' href='../graph.css' />")
  (printf "</head>")
  (printf "<body>\n")

  (printf "<dl id='about'>\n")
  (printf "<dt>Test:</dt><dd>~a</dd>" (test-name test))
  (printf "<dt>Logs:</dt>")
  (printf "<dd><a href='debug.txt'>Debug output</a>")
  (when profile?
    (printf ", <a href='profile.txt'>Profiling report</a>"))
  (printf "</dd>\n")
  (printf "<dt>Bits:</dt><dd>~a bits</dd>\n" bits)
  (printf "</dl>\n")

  (printf "<h2>Test timed out</h2>\n")

  (printf "</body>\n")
  (printf "</html>\n"))

(struct interval (alt-idx start-point end-point vidx))

(define (output-history altn)
  (match altn
    [(alt-event _ 'start _)
     (printf "<li>Started with <code><pre>~a</pre></code></li>\n"
             (pretty-format (alt-program altn) 65))]

    [(alt-event prog `(start ,strategy) `(,prev))
     (output-history prev)
     (printf "<li class='event'>Using strategy <code>~a</code></li>\n" strategy)]

    [(alt-event _ `(regimes ,splitpoints) prevs)
     (let* ([start-sps (cons (sp -1 -1 -inf.0) (take splitpoints (sub1 (length splitpoints))))]
            [vars (program-variables (alt-program altn))]
            [intervals
             (for/list ([start-sp start-sps] [end-sp splitpoints])
               (interval (sp-cidx end-sp) (sp-point start-sp) (sp-point end-sp) (sp-vidx end-sp)))]
            [interval->string
             (λ (ival)
                (format "~a < ~a < ~a" (interval-start-point ival)
                        (list-ref vars (interval-vidx ival)) (interval-end-point ival)))])
       (for/list ([entry prevs] [entry-idx (range (length prevs))])
         (let* ([entry-ivals
                 (filter (λ (intrvl) (= (interval-alt-idx intrvl) entry-idx)) intervals)]
                [condition
                 (string-join (map interval->string entry-ivals) " or ")])
           (printf "<h2><code>if <span class='condition'>~a</span></code></h2>\n" condition)
           (printf "<ol>\n")
           (output-history entry)
           (printf "</ol>\n"))))]

    [(alt-event prog 'periodicity `(,base ,subs ...))
     (output-history base)
     (for ([sub subs])
       (printf "<hr/><li class='event'>Optimizing periodic subexpression</li>\n")
       (output-history sub))
     (printf "<hr/><li class='event'>Combined periodic subexpressions</li>\n")]

    [(alt-delta prog cng prev)
     (output-history prev)
     (printf "<li>Applied <span class='rule'>~a</span> "
             (rule-name (change-rule cng)))
     (printf "to get <code><pre>~a</pre></code></li>\n"
             (pretty-format prog 65))]))

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

(define (group-by f l)
  (let ([h (make-hash)])
    (for ([elt l])
      (let ([val (f elt)])
        (hash-set! h val (cons elt (hash-ref h val '())))))
    (hash-values h)))

(define (set-up-line)
  (printf "<defs>\n")
  (for ([color '(red green blue)])
    (printf "<marker id='pt-~a' viewBox='0 0 4 4' refX='2' ref='2'" color)
    (printf "markerWidth='4' markerHeight='4'>\n")
    (printf "<circle cx='2' cy='2' r='2' fill='~a' opacity='~a'/>\n"
            color *point-opacity*)
    (printf "</marker>\n"))
  (printf "</defs>\n"))

(define (draw-line idx pts exs x-scale y-scale color)
  (printf "<path d='~a' stroke='transparent' stroke-width='1' fill='none' marker-start='url(#pt-~a)' marker-mid='url(#pt-~a)' marker-end='url(#pt-~a)' />\n"
          (points->pathdata
           (sort
            (for/list ([gp (group-by (curryr list-ref (+ 1 idx))
                                     (map cons exs pts))])
              (let ([x-value (list-ref (car gp) (+ 1 idx))]
                    [y-value (median (map car gp))])
                (cons (x-scale x-value)
                      (y-scale (ulps->bits y-value)))))
            < #:key car))
          color color color))

(define (draw-axes x-scale x-unscale y-scale y-unscale)
  (let ([pos-0 (with-handlers ([(const #t) (λ (e) 10)])
                 (max 10 (min 790 (x-scale 0))))])
    (draw-x-axis 10 490 175)
    (draw-y-axis pos-0 175 20)

    (draw-x-ticks 175 10.0 480.0 16
      (λ (x) (~r (x-unscale x) #:notation 'exponential #:precision 0)))

    (draw-y-ticks pos-0 175.0 20.0 8
      (λ (y) (~r (y-unscale y) #:precision 0)))))

(define (draw-key name target?)
  (printf "<g class='legend'>\n")
  (printf "<text x='10' y='15' fill='black' class='title'>~a</text>" name)

  (printf "<circle cx='50' cy='10' r='5' fill='red'/>")
  (printf "<text x='57' y='13' fill='black'>Input</text>")

  (printf "<circle cx='100' cy='10' r='5' fill='blue'/>")
  (printf "<text x='107' y='13' fill='black'>Output</text>")

  (when target?
    (printf "<circle cx='155' cy='10' r='5' fill='green'/>")
    (printf "<text x='162' y='13' fill='black'>Target</text>"))

  (printf "</g>"))

(define (median l)
  (let ([len (length l)] [sl (sort l <)])
    (if (odd? len)
        (list-ref sl (/ (- len 1) 2))
        (/ (+ (list-ref sl (/ len 2)) (list-ref sl (- (/ len 2) 1)))
           2))))

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

;; Make a log scale that maps min-domain to min-range, and max-domain to max-range.
;; Both min-domain and max-domain are required to be positive, and behaviour could
;; be undesired if min-domain and max-domain are equal, although those cases are handled.
;; There are no restrictions on min-range or max-range.
(define (linear-scale* min-domain max-domain min-range max-range)
  (let* ([a (/ (- max-range min-range)
	       (- max-domain min-domain))]
	 [b (- min-range (* a min-domain))])
    (values (lambda (x) (+ (* a x) b))
	    (lambda (y) (/ (- y b) a)))))

(define (make-log-scale* min-domain max-domain min-range max-range)
  (cond
   [(not (= min-domain max-domain))
    (let-values  ([(out-linear-l in-linear-e)
                   (linear-scale* (log min-domain) (log max-domain)
                                  min-range max-range)])
      (values (compose out-linear-l log)
              (compose exp in-linear-e)))]
   [(< min-domain 1)
    (make-log-scale* min-domain 1 min-range max-range)]
   [(> min-domain 1)
    (make-log-scale* 1 min-domain min-range max-range)]
   [#t (values (const min-range) (const 1))]))

;; min-range should be less than max-range
(define (make-full-log-scale* min-domain max-neg-domain min-pos-domain max-domain min-range max-range)
  (let* ([neg-scalar (/ (max 0 (log (- min-domain)))
			(+ (log max-domain) (log (- min-domain))))]
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

;; Returns count evenly distributed numbers from min to max,
;; where the first number is min and the last is max.
(define (make-ticks count min max)
  (build-list (+ 1 count) (λ (n) (+ min (* n (/ (- max min) count))))))

(define (draw-x-ticks y-pos min-x max-x num-ticks x-pos->label)
  (for/list ([x (make-ticks num-ticks min-x max-x)])
    (printf "<line x1='~a' y1='~a' x2='~a' y2='~a' class='tick' />\n"
            x y-pos x (+ y-pos *tick-length*))
    (printf "<text x='~a' y='~a' transform='rotate(~a,~a,~a)' class='x-label'>~a</text>\n"
            x (+ y-pos *tick-length* *label-shift*)
            *label-rotation* x (+ y-pos *tick-length* *label-shift*)
            (x-pos->label x))))

(define (draw-y-ticks x-pos min-y max-y num-ticks y-pos->label)
  (for/list ([y (cdr (make-ticks num-ticks min-y max-y))])
    (printf "<line x1='~a' y1='~a' x2='~a' y2='~a' class='tick' />\n"
            x-pos y (- x-pos *tick-length*) y)
    (let ([x (- x-pos *label-width* *tick-length* *label-shift*)]
          [y (+ y (/ *label-height* 2))])
      (printf "<text x='~a' y='~a' class='y-label'>~a</text>\n"
              x y (y-pos->label y)))))

;; Draws axis. See graph-draw-key for assumptions.
(define (draw-y-axis x-pos y1 y2)
  (printf "<line x1='~a' y1='~a' x2='~a' y2='~a' class='axis' />\n"
          x-pos y1 x-pos y2))

(define (draw-x-axis x1 x2 y-pos)
  (printf "<line x1='~a' y1='~a' x2='~a' y2='~a' class='axis' />\n"
          x1 y-pos x2 y-pos))

(define (html-escape-unsafe err)
  (string-replace (string-replace (string-replace err "&" "&amp;") "<" "&lt;") ">" "&gt;"))

(define (srcloc->string sl)
  (if sl
      (string-append
       (path->string (srcloc-source sl))
       ":"
       (number->string (srcloc-line sl))
       ":"
       (number->string (srcloc-column sl)))
      "???"))
