#lang racket

(require math/flonum)
(require plot)
(require "common.rkt")
(require "points.rkt")

(provide error-points herbie-plot)

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

(define (error-points errs pts #:axis [axis 0] #:color [color 0] #:alpha [alpha 0.2])
  (points
    (for/list ([pt pts] [err errs])
      (vector (list-ref pt axis) (ulps->bits err)))
    #:sym 'fullcircle #:color color #:alpha alpha #:size 4))

(define (with-herbie-plot #:title [title #f] thunk)
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
    (thunk)))

(define (herbie-plot #:port [port #f] #:kind [kind 'auto] #:title [title #f] . renderers)
  (define thunk
    (if port
        (lambda () (plot-file (cons (y-axis) renderers) port kind #:y-min 0 #:y-max 64))
        (lambda () (plot (cons (y-axis) renderers) #:y-min 0 #:y-max 64))))
  (with-herbie-plot #:title title thunk))

