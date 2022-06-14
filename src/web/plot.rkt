#lang racket

(require math/bigfloat math/flonum plot/no-gui racket/draw)
(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../syntax/syntax.rkt" "../syntax/read.rkt"
         "../alternative.rkt" "../interface.rkt" "../core/regimes.rkt" 
         "../sandbox.rkt")

(provide make-axis-plot make-points-plot make-cost-accuracy-plot
         make-full-cost-accuracy-plot real->ordinal repr-ticks regime-splitpoints)

(struct color-theme (scatter line fit))
(define *red-theme* (color-theme "pink" "red" "darkred"))
(define *blue-theme* (color-theme "lightblue" "blue" "navy"))
(define *green-theme* (color-theme "lightgreen" "green" "darkgreen"))
(define *gray-theme* (color-theme "gray" "gray" "gray"))

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

(define (repr-ticks repr)
  (ticks
   (curryr choose-ticks repr)
   (λ (lft rgt pticks)
     (for/list ([ptick pticks])
       (define val (pre-tick-value ptick))
       (if (or (= val 0) (< 0.01 (abs val) 100))
           (~r (exact->inexact val) #:precision 4)
           (string-replace (~r val #:notation 'exponential #:precision 0) "1e" "e"))))))

(define (error-points errs pts repr #:axis [axis 0] #:color [color *blue-theme*] #:alpha [alpha 0.02])
  (define x
    (if (number? axis)
        (λ x (list-ref x axis))
        (eval-prog axis 'fl)))
  (points
    (for/list ([pt pts] [err errs])
      (vector 
        (repr->real (apply x pt) repr) ; TODO: real rather than flonum
        (ulps->bits err)))
    #:sym 'fullcircle #:color (color-theme-line color) #:alpha alpha #:size 4))

(define (best-alt-points point-alt-idxs var-idxs)
  (define point-idxs (remove-duplicates (map cadr point-alt-idxs)))
  (define points-list (for/list ([i point-idxs])
    (filter (λ (x) (= (cadr x) i)) point-alt-idxs)))
  (define non-empty-points-list (for/list ([point-list points-list])
                                  point-list))
  (for/list ([point-list non-empty-points-list] [color (range 2 121)])
    (points (map (λ (p) (list (list-ref (car p) (car var-idxs))
                              (list-ref (car p) (cadr var-idxs))))
                 point-list) #:color color #:sym 'fullcircle #:size 5)))

(define (herbie-ratio-point-colors test-points baseline-errors herbie-errors oracle-errors)
  (define points-with-colors (for/list ([point test-points] [base-err baseline-errors]
                                        [herbie-err herbie-errors]
                                        [oracle-err oracle-errors])
    (define span (- base-err oracle-err))
    (define herbie-percent (if (= span 0) 1 (/ (- base-err herbie-err) span)))
    (define color-num (max (round (* 240 herbie-percent)) 0))
    (list point color-num)))
  (define colors (remove-duplicates (map cadr points-with-colors)))
  (for/list ([c colors])
    (filter (λ (p) (eq? (cadr p) c)) points-with-colors)))

(define (herbie-ratio-point-renderers points-colors var-idxs)
  (for/list ([l points-colors])
    (define color-num (cadar l))
    (define point-color (list color-num color-num color-num))
    (define color-points (map (λ (l) (list (list-ref (car l) (car var-idxs))
                                           (list-ref (car l) (cadr var-idxs)))) l))
    (points color-points #:color point-color #:sym 'fullcircle #:size 5)))

(define (error-axes pts repr #:axis [axis 0])
  (list
   (y-tick-lines)
   (error-points (map (const 1) pts) pts repr #:axis axis #:alpha 0)))

(define (with-herbie-plot repr #:title [title #f] thunk)
  (parameterize ([plot-width 800] [plot-height 300]
                 [plot-background-alpha 0]
                 [plot-x-transform (repr-axis repr)]
                 [plot-x-ticks (repr-ticks repr)]
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

(define (herbie-plot repr #:port [port #f] #:kind [kind 'auto] #:title [title #f] . renderers)
  (define bit-width (representation-total-bits repr))
  (define thunk
    (if port
        (lambda () (plot-file (cons (y-axis) renderers) port kind #:y-min 0 #:y-max bit-width))
        (lambda () (plot-pict (cons (y-axis) renderers) #:y-min 0 #:y-max bit-width))))
  (with-herbie-plot repr #:title title thunk))

(define (with-alt-plot repr #:title [title #f] thunk)
  (parameterize ([plot-width 800] [plot-height 800]
                 [plot-background-alpha 1]
                 [plot-x-transform (repr-axis repr)]
                 [plot-x-ticks (repr-ticks repr)]
                 [plot-x-tick-label-anchor 'top]
                 [plot-x-label #f]
                 [plot-x-far-axis? #t]
                 [plot-x-far-ticks no-ticks]
                 [plot-y-transform (repr-axis repr)]
                 [plot-y-ticks (repr-ticks repr)]
                 [plot-y-tick-label-anchor 'left]
                 [plot-y-label #f]
                 [plot-y-far-axis? #t]
                 [plot-y-far-ticks no-ticks]
                 [plot-font-size 10]
                 [plot-y-label title])
    (thunk)))

(define (alt-plot repr #:port [port #f] #:kind [kind 'auto] #:title [title #f] . renderers)
  (define thunk
    (lambda () (plot-file renderers port kind)))
  (with-alt-plot repr #:title title thunk))

(define (errors-by x errs pts cmp) 
  (sort (map (λ (pt err) (cons (apply x pt) err)) pts errs) cmp #:key car))

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

(define (error-avg errs pts repr #:axis [axis 0] #:vars [vars '()]
                   #:color [color *blue-theme*] #:bin-size [bin-size 128])
  (define get-coord
    (if (number? axis)
        (λ x (list-ref x axis))
        (eval-prog `(λ ,vars ,axis) 'fl repr)))
        
  (define-values (lt neg) ;; representation-specific operators
    (values
     (operator-info (get-parametric-operator '< repr repr) 'fl)
     (operator-info (get-parametric-operator 'neg repr) 'fl)))
  (define max (λ (x y) (if (lt x y) y x))) 
  (define min (λ (x y) (if (lt x y) x y)))

  ; max and min finite values (works for ieee754 and posit)
  (define-values (maxbound minbound) 
    (let ([pinf (real->ordinal +inf.0 repr)]
          [minf (real->ordinal -inf.0 repr)])
      (cond
       [(= pinf minf) ; posit
        (values ((representation-ordinal->repr repr) (sub1 (expt 2 (representation-total-bits repr))))
                ((representation-ordinal->repr repr) (add1 minf)))]
       [else 
        (values ((representation-ordinal->repr repr) (sub1 pinf))
                ((representation-ordinal->repr repr) (add1 minf)))])))

  (define eby (errors-by get-coord errs pts lt))
  (define histogram-f (histogram-function eby #:bin-size bin-size))
  (define (avg-fun x)
    (define h (histogram-f x))
    (/ (apply + (vector->list h)) (vector-length h)))
  (define-values (lbound ubound) ; plot requires finite bounds
    (match* ((car (first eby)) (car (last eby)))
            [(x x) (values #f #f)]
            [(x y)
              (values (repr->real (max minbound x) repr) ; make sure the min, max are finite
                      (repr->real (min maxbound y) repr))]))
  (function avg-fun lbound ubound #:width 2 #:color (color-theme-fit color)))

(define (error-mark x-val)
  (inverse (const x-val) #:color "gray" #:width 3))

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

(define (make-axis-plot result out idx)
  (define var (list-ref (test-vars (test-result-test result)) idx))
  (define end-alt (car (test-success-end-alts result)))
  (define split-var? (equal? var (regime-var end-alt)))
  (define repr (test-output-repr (test-result-test result)))
  (define pts (test-success-newpoints result))
  (herbie-plot
   #:port out #:kind 'png
   repr
   (error-axes pts repr #:axis idx)
   (map error-mark (if split-var? (regime-splitpoints end-alt) '()))))

(define (make-points-plot result out idx letter)
  (define-values (theme accessor)
    (match letter
     ['r (values *red-theme*   test-success-start-error)]
     ['g (values *green-theme* test-success-target-error)]
     ['b (values *blue-theme*  (compose car test-success-end-errors))]
     [(? (conjoin string? (curryr string-prefix? "o")))
      (define num (+ (string->number (substring letter 1)) 1))
      (values *gray-theme*
              (λ (x) (list-ref (test-success-end-errors x) num)))]))

  (define repr (test-output-repr (test-result-test result)))
  (define pts (test-success-newpoints result))
  (define err (accessor result))

  (herbie-plot
   #:port out #:kind 'png
   repr
   (error-points err pts repr #:axis idx #:color theme)
   (error-avg err pts repr #:axis idx #:color theme)))

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


(define (make-alt-plots point-alt-idxs alt-idxs title out result)
  (define best-alt-point-renderers (best-alt-points point-alt-idxs alt-idxs))
  (define repr (test-output-repr (test-result-test result)))
  (alt-plot best-alt-point-renderers repr #:port out #:kind 'png #:title title))

(define (make-point-alt-idxs result)
  (define repr (test-output-repr (test-result-test result)))
  (define all-alts (test-success-all-alts result))
  (define all-alt-bodies (map (λ (alt) (eval-prog (alt-program alt) 'fl repr)) all-alts))
  (define newpoints (test-success-newpoints result))
  (define newexacts (test-success-newexacts result))
  (oracle-error-idx all-alt-bodies newpoints newexacts repr))

(define (make-contour-plot point-colors var-idxs title out result)
  (define point-renderers (herbie-ratio-point-renderers point-colors var-idxs))
  (define repr (test-output-repr (test-result-test result)))
  (alt-plot point-renderers repr #:port out #:kind 'png #:title title))

#;
(define (make-plots result rdir profile?)
  (define (open-file #:type [type #f] idx fun . args)
    (call-with-output-file (build-path rdir (format "plot-~a~a.png" idx (or type ""))) #:exists 'replace
      (apply curry fun args)))

  (define vars (program-variables (alt-program (test-success-start-alt result))))
  (when (>= (length vars) 2)
    (define point-alt-idxs (make-point-alt-idxs result))
    (define newpoints (test-success-newpoints result))
    (define baseline-errs (test-success-baseline-error result))
    (define herbie-errs (car (test-success-end-errors result)))
    (define oracle-errs (test-success-oracle-error result))
    (define point-colors (herbie-ratio-point-colors newpoints baseline-errs herbie-errs oracle-errs))
    (for* ([i (range (- (length vars) 1))] [j (range 1 (length vars))])
      (define alt-idxs (list i j))
      (define title (format "~a vs ~a" (list-ref vars j) (list-ref vars i)))
      (open-file (- (+ j (* i (- (length vars)))) 1) #:type 'best-alts
                 make-alt-plots point-alt-idxs alt-idxs title result)
      (open-file (- (+ j (* i (- (length vars)))) 1) #:type 'contours
                 make-contour-plot point-colors alt-idxs title result)))

  (for ([var (test-vars (test-result-test result))] [idx (in-naturals)])
    (when (> (length (remove-duplicates (map (curryr list-ref idx) (test-success-newpoints result)))) 1)
      ;; This is bad code
      (open-file idx make-axis-plot result idx)
      (open-file idx #:type 'r make-points-plot result idx 'r)
      (when (test-success-target-error result)
        (open-file idx #:type 'g make-points-plot result idx 'g))
      (open-file idx #:type 'b make-points-plot result idx 'b))))
