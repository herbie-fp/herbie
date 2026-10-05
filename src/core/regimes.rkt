#lang racket

;;;; Module principles
;; - The core of this file is infer-option and infer-option-prefixes.
;;   Both are extremely performance-sensitive.
;; - Therefore almost everything is vector-based with few copies.
;; - Everything else is overhead and should be minimized.

(require math/bigfloat
         math/flonum
         "../core/alternative.rkt"
         "../utils/common.rkt"
         "../utils/pretty-print.rkt"
         "../utils/pareto.rkt"
         "../syntax/float.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../utils/timeline.rkt"
         "../syntax/types.rkt"
         "../syntax/block.rkt"
         "compiler.rkt"
         "points.rkt"
         "programs.rkt")
(provide pareto-regimes
         (struct-out option)
         (struct-out si)
         combine-alts)

(module+ test
  (require rackunit
           "../syntax/syntax.rkt"
           "../syntax/sugar.rkt"))

(struct option (split-indices alts pts expr)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc opt port mode)
     (fprintf port "#<option ~a>" (option-split-indices opt)))])

;; CONSIDER: move initial-v and the "branch-vs" computation into caller.
(define (pareto-regimes block sorted initial-v pcontext spec-block)
  (timeline-event! 'regimes)
  (define alts-vec (list->vector sorted))
  (define alt-count (vector-length alts-vec))
  (define err-cols (block-errors block (map alt-expr sorted) pcontext))
  (define (real-v? v)
    (equal? (representation-type (block-repr-of v)) 'real))
  (define var-vs (map (curry block-add! block) (block-vars block)))
  (define branch-vs
    (filter real-v?
            (if (flag-set? 'reduce 'branch-expressions)
                (block-reachable block (cons initial-v (map alt-expr sorted)))
                var-vs)))
  (define candidates (branch-candidates block branch-vs err-cols pcontext))
  (define var-candidates (filter (lambda (c) (member (candidate-expr c) var-vs)) candidates))
  (define branches
    (remove-duplicates (append var-candidates (list (argmin candidate-score candidates)))))
  (define pts-vec (pcontext-points pcontext))

  ;; For timeline
  (define block-jsexpr
    (block->jsexpr block spec-block (append (map alt-expr sorted) (map candidate-expr branches))))
  (timeline-push! 'block block-jsexpr)
  (define branch-roots (drop (hash-ref block-jsexpr 'roots) alt-count))

  (define option-curves
    (for/list ([c (in-list branches)]
               [root (in-list branch-roots)])
      (define v (candidate-expr c))
      (define timeline-stop! (timeline-start! 'times (block->jsexpr block spec-block (list v))))
      (define curve (branch-options c alts-vec err-cols pts-vec))
      (define last-point (last curve))
      (timeline-stop!)
      (timeline-push! 'branch
                      root
                      (option-error last-point)
                      (length (option-split-indices (pareto-point-data last-point)))
                      (~a (representation-name (block-repr-of v))))
      curve))
  (define combined-option-curve
    (for/fold ([curve '()]) ([branch-curve (in-list option-curves)])
      (pareto-union curve branch-curve #:combine (lambda (old _new) old))))

  ;; Timeline
  (timeline-push! 'inputs (block->jsexpr block spec-block (map alt-expr sorted)))
  (timeline-push!
   'outputs
   (block->jsexpr block
                  spec-block
                  (remove-duplicates
                   (for*/list ([ppt (in-list combined-option-curve)]
                               [sidx (in-list (option-split-indices (pareto-point-data ppt)))])
                     (alt-expr (list-ref (option-alts (pareto-point-data ppt)) (si-cidx sidx)))))))
  (timeline-push! 'accuracy
                  (errors-score (first (block-errors block (list initial-v) pcontext)))
                  (baseline-errors-score err-cols alt-count)
                  (for/fold ([best +inf.0]) ([ppt (in-list combined-option-curve)])
                    (min best (option-error ppt)))
                  (oracle-errors-score err-cols alt-count))
  (for/list ([ppt (in-list combined-option-curve)])
    (define opt (pareto-point-data ppt))
    (timeline-push! 'count (length (option-alts opt)) (length (option-split-indices opt)))
    opt))

(define (option-error ppt)
  (- (pareto-point-error ppt) (length (option-split-indices (pareto-point-data ppt)))))

;; A branch expression, the point order along it, and its optimal split using every alt.
(struct candidate (expr sorted-indices can-split-vec splits score))

(define (branch-candidates block vs err-cols pcontext)
  ;; Expressions that sort the points identically give identical splits.
  (define orders
    (remove-duplicates (for/list ([v (in-list vs)]
                                  [v-vals-vec (in-list (v-values* block vs pcontext))])
                         (cons v (branch-order v-vals-vec (block-repr-of v))))
                       #:key cdr))
  (for/list ([order (in-list orders)])
    (match-define (cons v (cons sorted-indices can-split-vec)) order)
    (define-values (splits score) (infer-option err-cols sorted-indices can-split-vec))
    (candidate v sorted-indices can-split-vec splits score)))

(define (baseline-errors-score err-cols count)
  (for/fold ([best +inf.0]) ([err-col (in-list (take err-cols count))])
    (min best (errors-score err-col))))

(define (oracle-errors-score err-cols count)
  (define num-points (flvector-length (first err-cols)))
  (/ (for/sum ([point-idx (in-range num-points)])
              (for/fold ([best-err +inf.0]) ([err-col (in-list (take err-cols count))])
                (min best-err (flvector-ref err-col point-idx))))
     num-points))

(define (v-values* block vs pcontext)
  (define count (length vs))
  (define fn (compile-block block vs))
  (define num-points (pcontext-length pcontext))
  (define vals (build-vector count (lambda (_) (make-vector num-points))))
  (for ([pt (in-vector (pcontext-points pcontext))]
        [p (in-naturals)])
    (for ([out (in-vector (fn pt))]
          [i (in-naturals)])
      (vector-set! (vector-ref vals i) p out)))
  (vector->list vals))

;; The point order along a branch expression, and where splitting it is legal.
(define (branch-order v-vals-vec repr)
  (define order
    (vector-sort (build-vector (vector-length v-vals-vec) values)
                 (lambda (i j) (</total (vector-ref v-vals-vec i) (vector-ref v-vals-vec j) repr))))
  (define can-split-vec
    (for/vector #:length (vector-length order)
                ([idx (in-vector order)]
                 [k (in-naturals)])
      (and (> k 0)
           (</total (vector-ref v-vals-vec (vector-ref order (sub1 k)))
                    (vector-ref v-vals-vec idx)
                    repr))))
  (cons order can-split-vec))

(define (branch-options c alts-vec err-cols pts-vec)
  (match-define (candidate v sorted-indices can-split-vec splits score) c)
  (define pts*
    (for/list ([i (in-vector sorted-indices)])
      (vector-ref pts-vec i)))

  (define-values (splitss scores)
    (infer-option-prefixes err-cols sorted-indices can-split-vec splits score))

  (define points
    (for/list ([count (in-range 1 (add1 (vector-length splitss)))])
      (define split-indices (vector-ref splitss (sub1 count)))
      (define alts (vector->list (vector-take alts-vec count)))
      (define error (+ (/ (flvector-ref scores (sub1 count)) (vector-length sorted-indices)) 1))
      (pareto-point count error (option split-indices alts pts* v))))
  (for/fold ([curve '()]) ([point (in-list points)])
    (pareto-union curve (list point) #:combine (lambda (old _new) old))))

(module+ test
  (require "../syntax/platform.rkt"
           "../syntax/load-platform.rkt")
  (activate-platform! "c")
  (define ctx (context '(x) <binary64> (list <binary64>)))
  (define pctx (mk-pcontext '(#(0.5) #(4.0)) '(1.0 1.0)))
  (define alts (map make-alt (list '(fmin.f64 x 1) '(fmax.f64 x 1))))
  (define err-cols (list (flvector 53.0 0.0) (flvector 0.0 53.0)))
  (define pts-vec (pcontext-points pctx))

  (define (test-regimes expr goal)
    (define-values (block vs) (progs->block (list expr) #:ctx ctx))
    (define c (first (branch-candidates block vs err-cols pctx)))
    (check (lambda (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (pareto-point-data (first (branch-options c (list->vector alts) err-cols pts-vec)))
           goal))

  (define (test-regimes/prefixes expr goals)
    (define-values (block vs) (progs->block (list expr) #:ctx ctx))
    (define c (first (branch-candidates block vs err-cols pctx)))
    (define options
      (map pareto-point-data (reverse (branch-options c (list->vector alts) err-cols pts-vec))))
    (for ([goal (in-list goals)]
          [opt (in-list options)])
      (check (lambda (x y) (equal? (map si-cidx (option-split-indices x)) y)) opt goal)))

  ;; This is a basic sanity test
  (test-regimes 'x '(1 0))
  (test-regimes/prefixes 'x '((0) (1 0)))

  ;; This test ensures we handle equal points correctly. All points
  ;; are equal along the `1` axis, so we should only get one
  ;; splitpoint (the second, since it is better at the further point).
  (test-regimes (literal 1 'binary64) '(0))

  (test-regimes `(if.f64 (==.f64 x ,(literal 0.5 'binary64)) ,(literal 1 'binary64) (NAN.f64)) '(1 0))

  (check-equal? (baseline-errors-score err-cols 2) 26.5)
  (check-equal? (oracle-errors-score err-cols 2) 0.0))

(module core typed/racket
  (provide (struct-out si)
           infer-option
           infer-option-prefixes)
  (require math/flonum)

  ;; Struct representing a splitindex
  ;; cidx = Candidate index: the index candidate program that should be used to the left of this splitindex
  ;; pidx = Point index: The index of the point to the left of which we should split.
  (struct si ([cidx : Integer] [pidx : Integer]) #:prefab)

  ;; argmin_a scores[a]
  (: argmin (-> FlVector Integer))
  (define (argmin scores)
    (let loop ([alt-idx 0]
               [best 0]
               [best-score +inf.0])
      (cond
        [(= alt-idx (flvector-length scores)) best]
        [(< (flvector-ref scores alt-idx) best-score)
         (loop (add1 alt-idx) alt-idx (flvector-ref scores alt-idx))]
        [else (loop (add1 alt-idx) best best-score)])))

  ;; This is the core main loop of the regimes algorithm.
  ;; Takes in alt-major error columns, point-sorting indices, and a vector
  ;; of booleans to determine when it's ok to split for another alt.
  ;; Returns a list of split indices saying which alt to use for which
  ;; range of points, starting at 1 going up to num-points, and the score
  ;; of that split. Alts are indexed 0 and points are index 1. The optimal
  ;; regimes split is calculated using the following DP recurrence:
  ;; best[p][a] = error[p][a] + min(best[p-1][a], penalty + min_b best[p-1][b])
  (: infer-option
     (-> (Listof FlVector) (Vectorof Integer) (Vectorof Boolean) (Values (Listof si) Float)))
  (define (infer-option err-cols sorted-indices can-split-vec)
    (define number-of-alts (length err-cols))
    (define number-of-points (vector-length sorted-indices))
    (define split-penalty (fl number-of-points))

    ;; error[p][a] = (flvector-ref (vector-ref err-vec a) (vector-ref sorted-indices p))
    (: err-vec (Vectorof FlVector))
    (define err-vec (list->vector err-cols))

    ;; row = best[p-1] going in to point p, best[p] coming out; updated in place
    (: row FlVector)
    (define row ; best[0][a] = error[0][a]
      (for/flvector #:length number-of-alts
                    ([err-col (in-vector err-vec)])
                    (flvector-ref err-col (vector-ref sorted-indices 0))))
    ;; previous-alts[p][a] = the alt at p-1 on the best path ending on a at p
    (: previous-alts (Vectorof (Vectorof Integer)))
    (define previous-alts (build-vector number-of-points (lambda (_) (make-vector number-of-alts 0))))

    (for ([point-idx (in-range 1 number-of-points)])
      (define best-alt (argmin row))
      ;; penalty + min_b best[p-1][b]
      (define switched-score (+ split-penalty (flvector-ref row best-alt)))
      (define original-idx (vector-ref sorted-indices point-idx))
      (define previous-here (vector-ref previous-alts point-idx))
      (define can-split? (vector-ref can-split-vec point-idx))
      (for ([alt-idx (in-naturals)]
            [err-col (in-vector err-vec)])
        (define continued-score (flvector-ref row alt-idx)) ; best[p-1][a]
        (define switch? (and can-split? (< switched-score continued-score)))
        (flvector-set! row
                       alt-idx
                       (+ (flvector-ref err-col original-idx) ; error[p][a]
                          (if switch? switched-score continued-score)))
        (vector-set! previous-here alt-idx (if switch? best-alt alt-idx))))

    ;; score = min_a best[P][a]
    (define last-point (sub1 number-of-points))
    (define last-alt (argmin row))

    (: alts (Vectorof Integer))
    (define alts (make-vector number-of-points 0))
    (vector-set! alts last-point last-alt)
    (for ([point-idx (in-range last-point 0 -1)])
      (vector-set! alts
                   (sub1 point-idx)
                   (vector-ref (vector-ref previous-alts point-idx) (vector-ref alts point-idx))))

    (: splits (Listof si))
    (define splits
      (for/list ([point-idx (in-range 1 (add1 number-of-points))]
                 #:unless (and (< point-idx number-of-points)
                               (= (vector-ref alts point-idx) (vector-ref alts (sub1 point-idx)))))
        (si (vector-ref alts (sub1 point-idx)) point-idx)))
    (values splits (flvector-ref row last-alt))) ; (splits, score)

  ;; Repeatedly calculate the optimal regimes split, removing the costliest
  ;; alt (and any alts costlier) one at a time, starting from a provided
  ;; optimal split using every alt. Doing so until no alts remain yields the
  ;; full set of Pareto optimal regime splits.
  (: infer-option-prefixes
     (-> (Listof FlVector)
         (Vectorof Integer)
         (Vectorof Boolean)
         (Listof si)
         Float
         (Values (Vectorof (Listof si)) FlVector)))
  (define (infer-option-prefixes err-cols sorted-indices can-split-vec splits score)
    (define number-of-alts (length err-cols))
    (: splitss (Vectorof (Listof si)))
    (define splitss (make-vector number-of-alts (ann null (Listof si))))
    (define scores (make-flvector number-of-alts +inf.0))
    (let loop ([max-alt (sub1 number-of-alts)]
               [splits splits]
               [score score])
      (define highest-used-alt (apply max (map (inst si-cidx Integer) splits)))
      (for ([alt-idx (in-range highest-used-alt (add1 max-alt))])
        (vector-set! splitss alt-idx splits)
        (flvector-set! scores alt-idx score))
      (when (> highest-used-alt 0)
        (define-values (splits* score*)
          (infer-option (take err-cols highest-used-alt) sorted-indices can-split-vec))
        (loop (sub1 highest-used-alt) splits* score*)))
    (values splitss scores)))

(require (submod "." core))

(define (combine-alts block best-option)
  (match-define (option splitindices alts pts v) best-option)
  (define repr (block-repr-of v))
  (define eval-expr (compose (curryr vector-ref 0) (compile-block block (list v))))
  (define splitpoints
    (for/list ([si1 (in-list (drop-right splitindices 1))])
      (define p1 (eval-expr (list-ref pts (sub1 (si-pidx si1)))))
      (define p2 (eval-expr (list-ref pts (si-pidx si1))))
      (sp (si-cidx si1) v (left-point repr p1 p2))))
  (define v*
    (for/fold ([v (alt-expr (list-ref alts (si-cidx (last splitindices))))])
              ([splitpoint (in-list (reverse splitpoints))])
      (define repr (block-repr-of (sp-bexpr splitpoint)))
      (define if-impl (get-fpcore-impl 'if '() (list (get-representation 'bool) repr repr)))
      (define <=-impl (get-fpcore-impl '<= '() (list repr repr)))
      (define lit-v
        (block-add! block
                    (literal (repr->real (sp-point splitpoint) repr) (representation-name repr))))
      (define cmp-v (block-add! block (list <=-impl (sp-bexpr splitpoint) lit-v)))
      (block-add! block (list if-impl cmp-v (alt-expr (list-ref alts (sp-cidx splitpoint))) v))))

  ;; We don't want unused alts in our history!
  (define-values (alts* splitpoints**)
    (remove-unused-alts alts (append splitpoints (list (sp (si-cidx (last splitindices)) v +nan.0)))))
  (alt v* (list 'regimes splitpoints**) alts*))

(define (left-point repr p1 p2)
  (define left ((representation-repr->bf repr) p1))
  (define right ((representation-repr->bf repr) p2))
  (define out ; TODO: Try using bigfloat-pick-point here?
    (if (bfnegative? left)
        (bigfloat-interval-shortest left (bfmin (bf/ left 2.bf) right))
        (bigfloat-interval-shortest left (bfmin (bf* left 2.bf) right))))
  ;; It's important to return something strictly less than right
  (if (bf= out right)
      p1
      ((representation-bf->repr repr) out)))

(define (remove-unused-alts alts splitpoints)
  (for/fold ([alts* '()]
             [splitpoints* '()])
            ([splitpoint (in-list splitpoints)])
    (define alt (list-ref alts (sp-cidx splitpoint)))
    ;; It's important to snoc the alt in order for the indices not to change
    (define alts** (remove-duplicates (append alts* (list alt))))
    (define splitpoint* (struct-copy sp splitpoint [cidx (index-of alts** alt)]))
    (define splitpoints** (append splitpoints* (list splitpoint*)))
    (values alts** splitpoints**)))
