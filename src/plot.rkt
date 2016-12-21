#lang racket

(require math/flonum)
(require plot/no-gui)
(require "common.rkt")
(require "float.rkt")
(require "points.rkt")
(require "programs.rkt")
(require "alternative.rkt")

(provide error-points herbie-plot error-mark error-avg error-axes
	 *red-theme* *blue-theme* *green-theme* *yellow-theme*)

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

(define (power10-upto x)
  (if (= x 0)
      '()
      (reverse
       (let loop ([power (round (/ (log x) (log 10)))])
         (define value (expt 10 power))
         (if (= value 0) '() (cons value (loop (- power 1))))))))

(define double-ticks
  (ticks
   (λ (min max)
     ;; There are three cases:
     ;; 1) 0 is between min and max
     ;; 2) 0 is one of min and max
     ;; 3) min and max are on the same side of 0
     (define possible-ticks
       (cond
        [(< (* min max) 0) (append (map - (power10-upto (- min))) '(0) (power10-upto max))]
        [(= min 0) (cons 0 (power10-upto max))]
        [(= max 0) (append (map - (power10-upto (abs min))) '(0))]
        [(> min 0) (set-subtract (power10-upto max) (power10-upto min))]
        [(< max 0) (map - (set-subtract (power10-upto (abs min)) (power10-upto (abs max))))]))
     (define N (length possible-ticks))
     (cond
      [(< N 20)
       ;; If there aren't enough possible big ticks, we fall back to the standard method
       ((ticks-layout (ticks-scale (linear-ticks #:number 10 #:base 10 #:divisors '(1 2 5)) double-transform)))]
      [else
       (for/list ([n (range 0 20) ] [i (cdr (range 0 (- N 1) (/ N 20)))])
         (pre-tick (list-ref possible-ticks (inexact->exact (floor i))) (even? n)))]))
   (λ (lft rgt pticks)
     (for/list ([ptick pticks])
       (define val (pre-tick-value ptick))
       (if (or (= val 0) (< 0.01 (abs val) 100))
           (~a val)
           (string-replace (~r val #:notation 'exponential #:precision 0) "1e" "e"))))))

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
  (list
   (y-tick-lines)
   (error-points (map (const 1) pts) pts #:axis axis #:alpha 0)))

(define (with-herbie-plot #:title [title #f] thunk)
  (parameterize ([plot-width 800] [plot-height 300]
                 [plot-background-alpha 0]
                 [plot-x-transform double-axis]
                 [plot-x-ticks double-ticks]
                 [plot-x-tick-label-anchor 'top]
                 [plot-x-label #f]
                 [plot-x-far-axis? #t]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-far-axis? #t]
                 [plot-y-axis? #t]
                 [plot-font-size 10]
                 [plot-y-ticks (linear-ticks #:number 9 #:base 32 #:divisors '(2 4 8))]
                 [plot-y-label title])
    (thunk)))

(define (herbie-plot #:port [port #f] #:kind [kind 'auto] #:title [title #f] . renderers)
  (define thunk
    (if port
        (lambda () (plot-file (cons (y-axis) renderers) port kind #:y-min 0 #:y-max (*bit-width*)))
        (lambda () (plot-pict (cons (y-axis) renderers) #:y-min 0 #:y-max (*bit-width*)))))
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
  (define xs (for/vector ([(x err) (in-dict errors-by-out)]) x))
  (define errs (for/vector ([(x err) (in-dict errors-by-out)]) err))

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
  (inverse (const x-val) #:color "gray" #:width 3))
