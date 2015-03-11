#lang racket

(require math/flonum)
(require plot)
(require "common.rkt")
(require "points.rkt")
(require "programs.rkt")

(define double-transform
  (invertible-function
   (compose flonum->ordinal fl)
   (compose ordinal->flonum round)))

(define double-axis
  (make-axis-transform double-transform))

(define double-ticks
  (ticks
   (ticks-layout (ticks-scale (linear-ticks #:number 16) double-transform))
   (λ (lft rgt pticks)
     (for/list ([ptick pticks])
       (~r (pre-tick-value ptick) #:notation 'exponential #:precision 0)))))

(define (herbie-plot #:title [title #f] . renderers)
  (parameterize ([plot-width 500] [plot-height 250]
                 [plot-x-transform double-axis]
                 [plot-x-ticks double-ticks]
                 [plot-x-tick-label-anchor 'top-right]
                 [plot-x-tick-label-angle 45]
                 [plot-x-label #f]
                 [plot-x-far-axis? #f]
                 [plot-y-far-axis? #f]
                 [plot-y-axis? #f]
                 [plot-font-size 8]
                 [plot-y-ticks (linear-ticks #:number 9 #:base 32 #:divisors '(2 4 8))]
                 [plot-y-label title])
    (plot (cons (y-axis) renderers)
          #:y-min 0 #:y-max 64)))

(define (error-points errs pcontext #:axis [axis 0] #:color [color 0] #:alpha [alpha 0.2])
  (points
    (for/list ([(pt ex) (in-pcontext (*pcontext*))] [err errs])
      (vector (list-ref pt axis) (ulps->bits err)))
    #:sym 'fullcircle #:color color #:alpha alpha #:size 4))
