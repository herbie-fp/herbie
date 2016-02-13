#lang racket

(require math/flonum)
(require plot/no-gui plot/pict)
(require "common.rkt")
(require "points.rkt")
(require "programs.rkt")
(require "alternative.rkt")

(provide error-points herbie-plot error-mark error-avg error-axes
	 *red-theme* *blue-theme* *green-theme* *yellow-theme*
	 plot-cand-error
	 )

(struct color-theme (scatter line fit))
(define *red-theme* (color-theme "pink" "red" "darkred"))
(define *blue-theme* (color-theme "lightblue" "blue" "navy"))
(define *green-theme* (color-theme "lightgreen" "green" "darkgreen"))
(define *yellow-theme* (color-theme "gold" "yellow" "olive"))

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

(define (error-axes pts #:axis [axis 0])
  (error-points (map (const 1) pts) pts #:axis axis #:alpha 0))

(define (with-herbie-plot #:title [title #f] thunk)
  (parameterize ([plot-width 400] [plot-height 200]
                 [plot-background-alpha 0]
                 [plot-x-transform double-axis]
                 [plot-x-ticks double-ticks]
                 [plot-x-tick-label-anchor 'top]
                 [plot-x-tick-label-angle 45]
                 [plot-x-label #f]
                 [plot-x-far-axis? #f]
                 [plot-y-far-axis? #f]
                 [plot-y-axis? #f]
                 [plot-font-size 8]
                 [plot-y-ticks (linear-ticks #:number 9 #:base 32 #:divisors '(2 4 8))]
                 [plot-y-label title])
    (thunk)))

(define (herbie-plot #:port [port #f] #:kind [kind 'auto] #:title [title #f] . renderers)
  (define thunk
    (if port
        (lambda () (plot-file (cons (y-axis) renderers) port kind #:y-min 0 #:y-max (*bit-width*)))
        (lambda () (plot (cons (y-axis) renderers) #:y-min 0 #:y-max (*bit-width*)))))
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

(define (error-avg errs pts #:axis [axis 0] #:vars [vars '()]
                   #:color [color *blue-theme*] #:bin-size [bin-size 128])
  (define get-coord
    (if (number? axis)
        (curryr list-ref axis)
        (eval-prog `(λ ,vars ,axis) mode:fl)))
  (define eby (errors-by get-coord errs pts))
  (define histogram-f (histogram-function eby #:bin-size bin-size))
  (define (avg-fun x)
    (define h (histogram-f x))
    (/ (apply + (vector->list h)) (vector-length h)))
  (function avg-fun
            (car (first eby)) (car (last eby))
            #:width 2 #:color (color-theme-fit color)))

(define (error-mark x-val)
  (inverse (const x-val) #:color "gray"))
