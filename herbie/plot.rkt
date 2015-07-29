#lang racket

(require math/flonum)
(require plot)
(require unstable/sequence)
(require "common.rkt")
(require "points.rkt")
(require "programs.rkt")

(provide loop-errors-renderers herbie-plot loop-plot
         error-points error-avg
         *red-theme* *blue-theme* *green-theme*
         intercept best-fit-slope
         interleave)

(struct color-theme (scatter line fit))
(define *red-theme* (color-theme "pink" "red" "darkred"))
(define *blue-theme* (color-theme "lightblue" "blue" "navy"))
(define *green-theme* (color-theme "lightgreen" "green" "darkgreen"))

(define double-transform
  (invertible-function
   (compose flonum->ordinal fl)
   (compose ordinal->flonum round)))

(define double-axis
  (make-axis-transform double-transform))

(define double-ticks
  (ticks
   (ticks-layout (ticks-scale (linear-ticks #:number 10) double-transform))
   (λ (lft rgt pticks)
     (for/list ([ptick pticks])
       (~r (pre-tick-value ptick) #:notation 'exponential #:precision 0)))))

(define (error-points errs pts #:axis [axis 0] #:color [color *blue-theme*] #:alpha [alpha 0.02])
  (define x
    (if (number? axis)
        (curryr list-ref axis)
        (eval-prog axis mode:fl)))
  (points
    (for/list ([pt pts] [err errs])
      (vector (x pt) (+ (ulps->bits err) (random) -1/2)))
    #:sym 'fullcircle #:color (color-theme-line color) #:alpha alpha #:size 4))

(define (with-herbie-plot #:title [title #f] thunk)
  (parameterize ([plot-width 400] [plot-height 200]
                 [plot-x-transform double-axis]
                 [plot-x-ticks double-ticks]
                 [plot-x-tick-label-anchor 'top-right]
                 [plot-x-tick-label-angle 45]
                 [plot-x-label #f]
                 [plot-x-far-axis? #f]
                 [plot-y-far-axis? #f]
                 [plot-y-axis? #f]
                 [plot-font-size 10]
                 [plot-y-ticks (linear-ticks #:number 9 #:base 32 #:divisors '(2 4 8))]
                 [plot-y-label title])
    (thunk)))

(define (herbie-plot #:port [port #f] #:kind [kind 'auto] #:title [title #f] . renderers)
  (define thunk
    (if port
        (lambda () (plot-file (cons (y-axis) renderers) port kind #:y-min 0 #:y-max 64))
        (lambda () (plot (cons (y-axis) renderers) #:y-min 0 #:y-max 64))))
  (with-herbie-plot #:title title thunk))

(define (errors-by x errs pts)
  (sort (map (λ (pt err) (cons (x pt) err)) pts errs) < #:key car))

(define (vector-binary-search v x cmp)
  (define (search l r)
    (define mid (ceiling (/ (+ l r) 2)))
    (define c (cmp x (vector-ref v mid)))
    (cond
     [(= r l) l]
     [(= r (+ l 1))
      (if (= c 0) r l)]
     [(< c 0)
      (search l mid)]
     [(> c 0)
      (search mid r)]
     [(= c 0)
      mid]))
  (search 0 (vector-length v)))

(define (histogram-function errors-by-out #:bin-size [bin-size 32])
  (define xs (for/vector ([(x err) (in-pairs errors-by-out)]) x))
  (define errs (for/vector ([(x err) (in-pairs errors-by-out)]) err))

  (λ (x)
    (define idx (vector-binary-search xs x -))
    (list->vector
     (map ulps->bits
          (sort
           (cond
            [(<= (vector-length errs) bin-size) (vector->list errs)]
            [(< idx (/ bin-size 2))
             (for/list ([i (in-range 0 bin-size)]) (vector-ref errs i))]
            [(> idx (- (vector-length errs) (/ bin-size 2)))
             (for/list ([i (in-range (- (vector-length errs) bin-size) (vector-length errs))])
               (vector-ref errs i))]
            [else
             (define idx-min (round (- idx (/ bin-size 2))))
             (for/list ([i (in-range idx-min (+ idx-min bin-size))]) (vector-ref errs i))])
           <)))))

(define (error-avg errs pts #:axis [axis 0] #:color [color *blue-theme*] #:bin-size [bin-size 128])
  (define get-coord
    (if (number? axis)
        (curryr list-ref axis)
        (eval-prog axis mode:fl)))
  (define eby (errors-by get-coord errs pts))
  (define histogram-f (histogram-function eby #:bin-size bin-size))
  (define (avg-fun x)
    (define h (histogram-f x))
    (/ (apply + (vector->list h)) (vector-length h)))
  (function avg-fun
            (car (first eby)) (car (last eby))
            #:width 2 #:color (color-theme-fit color)))

;; got this from the internet
(define (best-fit-slope pts)
  (/ (- (for/avg ([pt pts])
                 (* (car pt) (cadr pt)))
        (* (for/avg ([pt pts])
                    (car pt))
           (for/avg ([pt pts])
                    (cadr pt))))
     (- (for/avg ([pt pts])
                 (sqr (car pt)))
        (sqr (for/avg ([pt pts])
                      (car pt))))))

(define (intercept slope pts)
  (- (for/avg ([pt pts])
              (cadr pt))
     (* slope
        (for/avg ([pt pts])
                 (car pt)))))

(define (loop-errors-renderers err-lsts
                               #:color-theme [cs (color-theme "gray" "dimgray" "black")]
                               #:name [name #f])
  (let* ([err-lsts* (map (curry map sqr) err-lsts)]
	 [pts (for*/list ([err-lst err-lsts]
			  [(err idx) (in-indexed err-lst)])
		(list idx err))]
	 [slope (/ (for/sum ([err-lst err-lsts*])
		     (best-fit-slope
		      (for/list ([(err idx) (in-indexed err-lst)])
			(list idx err))))
		   (length err-lsts*))]
         [pts-sqrd (for*/list ([err-lst err-lsts*]
                               [(err idx) (in-indexed err-lst)])
                     (list idx err))]
	 [avgs (for/list ([y-pts (flip-lists* err-lsts)])
		 (/ (apply + y-pts) (length y-pts)))]
	 [maxs (map (curry apply max) (flip-lists* err-lsts))]
	 [mins (map (curry apply min) (flip-lists* err-lsts))])
    (list (lines-interval (for/list ([(min idx) (in-indexed mins)])
			    (list idx min))
			  (for/list ([(max idx) (in-indexed maxs)])
			    (list idx max))
			  #:color (color-theme-scatter cs)
			  #:line1-style 'transparent
			  #:line2-style 'transparent
                          #:alpha 0.4)
	  (lines (for/list ([(avg idx) (in-indexed avgs)])
		   (list idx avg))
		 #:color (color-theme-line cs)
		 #:label name)
          (let ([inter (intercept slope pts-sqrd)])
            (function (λ (x) (sqrt (+ (* slope x) inter)))
		      #:color (color-theme-fit cs)
		      #:width 3)))))

(define (with-loop-plot #:x-label [x-label #f] #:y-label [y-label #f] thunk)
  (parameterize ([plot-width 500] [plot-height 250]
                 [plot-x-tick-label-anchor 'top-right]
                 [plot-x-tick-label-angle 45]
                 [plot-x-far-axis? #f]
                 [plot-y-far-axis? #f]
                 [plot-font-size 8]
                 [plot-y-label y-label]
		 [plot-x-label x-label])
    (thunk)))

(define (loop-plot #:port [port #f] #:kind [kind 'auto] #:x-label [x-label #f] #:y-label [y-label #f] renderers)
  (define thunk
    (if port
        (lambda () (plot-file renderers port kind))
        (lambda () (plot renderers))))
  (with-loop-plot #:x-label x-label #:y-label y-label thunk))

(define (interleave . lsts)
  (apply append (flip-lists* lsts)))

;; (define (mk-dynplot fname #:x-label [x-label #f] #:y-label [y-label #f] renderers)
;;   (call-with-output-file fname #:exists 'replace
;;     (λ (out)
;;       (loop-plot #:port out #:kind 'svg #:x-label x-label #:y-label y-label renderers)))
;;   (call-with-input-file fname
;;     (λ (out)
;;       (xml->xexpr (document-element (read-xml out))))))
