#lang racket

(require math/bigfloat math/flonum json plot/no-gui racket/draw)
(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../syntax/types.rkt" "../syntax/syntax.rkt" "../syntax/read.rkt"
         "../alternative.rkt" "../core/regimes.rkt" "../sandbox.rkt")

(provide make-cost-accuracy-plot make-cost-accuracy-json make-full-cost-accuracy-plot
         real->ordinal regime-splitpoints choose-ticks regime-var)

;; Racket 8.1 compatability

(define (plot-file-compat renderer-tree output [kind 'auto]
                          #:x-min [x-min #f] #:x-max [x-max #f]
                          #:y-min [y-min #f] #:y-max [y-max #f]
                          #:width [width (plot-width)]
                          #:height [height (plot-height)]
                          #:title [title (plot-title)]
                          #:x-label [x-label (plot-x-label)]
                          #:y-label [y-label (plot-y-label)]
                      ;   #:aspect-ratio [aspect-ratio (plot-aspect-ratio)]    (added Racket 8.1)
                          #:legend-anchor [legend-anchor (plot-legend-anchor)])
  (define bm (make-bitmap width height))
  (define dc (send bm make-dc))
  (plot/dc renderer-tree dc 0 0 width height
           #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
           #:title title #:x-label x-label #:y-label y-label
        ;  #:aspect-ratio aspect-ratio                                  (added Racket 8.1)
           #:legend-anchor legend-anchor)
  (send bm save-file output kind))

(define plot-file    ; handle buggy `plot-file` in 8.1
  (if (string=? (version) "8.1")
      plot-file-compat
      (let ()
        (local-require (only-in plot/no-gui [plot-file plot-file*]))
        plot-file*)))

;;  Repr conversions

(define (ordinal->real x repr)
  (repr->real ((representation-ordinal->repr repr) x) repr))

(define (real->ordinal x repr) 
  ((representation-repr->ordinal repr) (real->repr x repr))) 

(define (repr-transform repr)
  (invertible-function 
    (curryr real->ordinal repr)
    (compose (curryr ordinal->real repr) round)))

(define (repr-axis repr)
  (make-axis-transform (repr-transform repr)))

(define (first-power10 min max repr)
  (define value
    (cond
     [(negative? max) 
      (- (expt 10 (ceiling (/ (log (- max)) (log 10)))))]
    [else
      (expt 10 (floor (/ (log max) (log 10))))]))
  (if (<= value min) #f value))

(define (clamp x lo hi)
  (min hi (max x lo)))

(define (choose-between min max number repr)
  ; Returns a given number of ticks, roughly evenly spaced, between min and max
  ; For any tick, n divisions below max, the tick is an ordinal corresponding to:
  ;  (a) a power of 10 between n and (n + ε) divisions below max where ε is some tolerance, or
  ;  (b) a value, n divisions below max
  (define sub-range (round (/ (- max min) (add1 number))))
  (define near (λ (x n) (and (<= x n) (<= (abs (/ (- x n) sub-range)) 0.2)))) ; <- tolerance
  (for/list ([itr (in-range 1 (add1 number))])
    (define power10 
      (first-power10 (ordinal->real (clamp (- max (* (add1 itr) sub-range)) min max) repr)
                     (ordinal->real (clamp (- max (* itr sub-range)) min max) repr)
                     repr))
    (if (and power10 (near (real->ordinal power10 repr) (- max (* itr sub-range))))
        (real->ordinal power10 repr)
        (- max (* itr sub-range)))))

(define (pick-spaced-ordinals necessary min max number repr)
  (define sub-range (/ (- max min) number)) ; size of a division on the ordinal range
  (define necessary*      ; filter out necessary points that are too close
    (let loop ([necessary necessary])
      (cond
       [(< (length necessary) 2) necessary]
       [(< (- (cadr necessary) (car necessary)) sub-range)
        (loop (cdr necessary))]
       [else (cons (car necessary) (loop (cdr necessary)))])))
  (define all
    (let loop ([necessary necessary*] [min* min] [start 0])
      (cond
       [(>= start number) '()]
       [(empty? necessary)
        (choose-between min* max (- number start) repr)]
       [else
        (define idx (for/first ([i (in-range number)] 
                                #:when (<= (- (first necessary) (+ min (* i sub-range))) sub-range))
                        i))
        (append
          (choose-between min* (first necessary) (- idx start) repr)
          (loop (cdr necessary) (first necessary) (add1 idx)))])))
  (sort (append all necessary*) <))

(define (choose-ticks min max repr)
  (define tick-count 13)
  (define necessary (map (curryr real->ordinal repr) 
                         (filter (λ (x) (<= min x max)) (list min -1.0 0.0 1.0 max))))
  (define major-ticks
    (map
      (curryr ordinal->real repr)
      (pick-spaced-ordinals necessary (real->ordinal min repr) (real->ordinal max repr)
                            tick-count repr)))
  (for/list ([tick major-ticks])
    (pre-tick tick #t)))

(define/contract (regime-info altn)
  (-> alt? (or/c (listof sp?) #f))
  (let loop ([altn altn])
    (match altn
      [(alt _ `(regimes ,splitpoints) prevs) splitpoints]
      [(alt _ _ (list)) #f]
      [(alt _ _ (list prev _ ...)) (loop prev)])))

(define (regime-splitpoints altn)
  (map sp-point (drop-right (regime-info altn) 1)))

(define/contract (regime-var altn)
  (-> alt? (or/c expr? #f))
  (define info (regime-info altn))
  (and info (sp-bexpr (car info))))

;;; Cost vs. Accuracy JSON (internal, single benchmark)
(define (make-cost-accuracy-json result out)
  (define repr (test-output-repr (test-result-test result)))
  (define bits (representation-total-bits repr))
  (define costs (test-success-end-costs result))
  (define errs (map errors-score (test-success-end-errors result)))

  (define cost0 (test-success-start-cost result))
  (define err0 (errors-score (test-success-start-error result)))

  (define xmax (argmax identity (cons cost0 costs)))
  (define xmin (argmax identity (cons cost0 costs)))

  (define json-obj `#hasheq(
    (first . ,(list cost0 err0))
    (best . ,(list xmax bits))
    (points . ,
      (for/list ([acost costs] [aerr errs]) (list acost aerr)))))
  (write-json json-obj out))

;;; Cost vs. Accuracy JSON (internal, entire suite)
(define (make-full-cost-accuracy-json y-max start pts out)
  (match-define (list (cons costs scores) ...) pts)
  (define x-max (argmax identity (cons (car start) costs)))
  
  (define json-obj `#hasheq(
    (first . ,(list (car start) (cdr start)))
    (best . ,(list x-max y-max))
    (points . ,
      (for/list ([acost costs] [aerr scores]) (list acost aerr)))))
  (write-json json-obj out))
  
;;; Cost vs. Accuracy (internal, single benchmark)
(define (make-cost-accuracy-plot result out)
  (define repr (test-output-repr (test-result-test result)))
  (define bits (representation-total-bits repr))
  (define costs (test-success-end-costs result))
  (define errs (map errors-score (test-success-end-errors result)))

  (define cost0 (test-success-start-cost result))
  (define err0 (errors-score (test-success-start-error result)))

  (define xmax (argmax identity (cons cost0 costs)))
  (define xmin (argmax identity (cons cost0 costs)))

  (parameterize ([plot-width 800] [plot-height 300]
                 [plot-background-alpha 0]
                 [plot-font-size 10]
                 [plot-x-tick-label-anchor 'top]
                 [plot-x-label "Cost"]
                 [plot-x-far-axis? #t]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-ticks (linear-ticks #:number 9 #:base 32 #:divisors '(2 4 8))]
                 [plot-y-far-axis? #t]
                 [plot-y-axis? #t]
                 [plot-y-label "Error (bits)"])
    (define pnts (points (map vector costs errs)
                         #:sym 'fullcircle
                         #:size 9
                         #:fill-color "red"))
    (define spnt (points (list (vector cost0 err0))
                         #:sym 'fullsquare
                         #:color "black"
                         #:size 15))
    (plot-file (list spnt pnts (y-tick-lines))
               out 'png
               #:x-min 0 #:x-max (+ xmax xmin)
               #:y-min 0 #:y-max bits)))

;;; Cost vs. Accuracy (internal, entire suite)
(define (make-full-cost-accuracy-plot y-max start pts out)
  (match-define (list (cons costs scores) ...) pts)
  (define x-max (argmax identity (cons (car start) costs)))
  (parameterize ([plot-width 800] [plot-height 300]
                 [plot-background-alpha 0]
                 [plot-font-size 10]
                 [plot-x-tick-label-anchor 'top]
                 [plot-x-label "Cost"]
                 [plot-x-far-axis? #t]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-ticks (linear-ticks #:number 9)]
                 [plot-y-far-axis? #t]
                 [plot-y-axis? #t]
                 [plot-y-label "Error (bits)"])
    (define spnt (points (list (vector (car start) (cdr start)))
                         #:sym 'fullsquare
                         #:color "black"
                         #:size 15))
    (define curve (lines (map vector (map car pts) (map cdr pts))
                         #:color "red"
                         #:width 4))
    (plot-file (list spnt curve (y-tick-lines))
               out 'png
               #:x-min 0 #:x-max x-max
               #:y-min 0 #:y-max y-max)))


