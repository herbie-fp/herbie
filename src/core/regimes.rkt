#lang racket

;;;; Module principles
;; - The core of this file is infer-option and infer-option-prefixes.
;;   Both are extremely performance-sensitive.
;; - Therefore almost everything is vector-based with few copies.
;;   Except critical-subexpressions. Converting it to vectors makes it slow.
;; - Everything else is overhead and should be minimized.

(require math/flonum
         "../core/alternative.rkt"
         "../utils/common.rkt"
         "../utils/pareto.rkt"
         "../syntax/float.rkt"
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
         critical-subexpression?)

(module+ test
  (require rackunit
           "../syntax/syntax.rkt"
           "../syntax/sugar.rkt")

  (define (check-critical expr subexpr)
    (define ctx
      (context (free-variables expr)
               <binary64>
               (make-list (length (free-variables expr)) <binary64>)))
    (define-values (block vs) (progs->block (list expr) #:ctx ctx))
    (critical-subexpression? block (first vs) (block-add! block subexpr))))

(struct option (split-indices alts pts expr)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc opt port mode)
     (fprintf port "#<option ~a>" (option-split-indices opt)))])

;; CONSIDER: move start-prog and the "branch-vs" computation into caller.
(define (pareto-regimes block sorted start-prog pcontext spec-block)
  (timeline-event! 'regimes)
  (define alts-vec (list->vector sorted))
  (define alt-count (vector-length alts-vec))
  (define err-cols (block-errors block (map alt-expr sorted) pcontext))
  (define (real-v? v)
    (equal? (representation-type (block-repr-of v)) 'real))
  (define branch-vs
    (filter real-v?
            (if (flag-set? 'reduce 'branch-expressions)
                (critical-subexpressions block start-prog)
                (map (curry block-add! block) (block-vars block)))))

  (define v-vals (v-values* block branch-vs pcontext))
  (define pts-vec (pcontext-points pcontext))

  ;; For timeline
  (define block-jsexpr (block->jsexpr block spec-block (append (map alt-expr sorted) branch-vs)))
  (timeline-push! 'block block-jsexpr)
  (define branch-roots (drop (hash-ref block-jsexpr 'roots) alt-count))
  (define branch-root-map (make-immutable-hash (map cons branch-vs branch-roots)))

  (define option-curves
    (for/list ([v (in-list branch-vs)]
               [v-vals-vec (in-list v-vals)])
      (define timeline-stop! (timeline-start! 'times (block->jsexpr block spec-block (list v))))
      (define repr (block-repr-of v))
      (define curve (branch-options block alts-vec err-cols pts-vec v v-vals-vec repr))
      (define last-point (last curve))
      (timeline-stop!)
      (timeline-push! 'branch
                      (hash-ref branch-root-map v)
                      (- (pareto-point-error last-point)
                         (length (option-split-indices (pareto-point-data last-point))))
                      (length (option-split-indices (pareto-point-data last-point)))
                      (~a (representation-name repr)))
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
  (for/list ([ppt (in-list combined-option-curve)])
    (define opt (pareto-point-data ppt))
    (timeline-push! 'count (length (option-alts opt)) (length (option-split-indices opt)))
    (timeline-push! 'accuracy
                    (- (pareto-point-error ppt) (length (option-split-indices opt)))
                    (oracle-errors-score err-cols (pareto-point-cost ppt))
                    (baseline-errors-score err-cols (pareto-point-cost ppt)))
    opt))

(define (critical-subexpression? block root-v sub-v)
  (set-member? (critical-subexpressions block root-v) sub-v))

(define (critical-subexpressions block root-v)
  (define var-vs (map (curry block-add! block) (block-vars block)))
  (define free-vars (block-free-vars block))
  (define dom-parent (build-dominator-tree block root-v))
  (define (dominates? parent-v child-v)
    (cond
      [(equal? parent-v child-v) #t]
      [(equal? child-v root-v) #f]
      [else (dominates? parent-v (dom-parent child-v))]))
  (define (extractable? v)
    (for/and ([var (in-set (free-vars v))])
      (dominates? v (block-add! block var))))
  (reap [sow]
        (define seen-vs (mutable-set root-v))
        (sow root-v)
        (for ([v (in-list var-vs)])
          (when (dom-parent v)
            (let loop ([v v])
              (unless (set-member? seen-vs v)
                (set-add! seen-vs v)
                (when (extractable? v)
                  (sow v))
                (loop (dom-parent v))))))))

(define (build-dominator-tree block root-v)
  (define reachable-vs (reverse (block-reachable block (list root-v))))
  (define dom-parents (make-vector (block-length block) #f))
  (define (dom-parent v)
    (vector-ref dom-parents (val-idx v)))
  (define (update-child! v child-v)
    (define old-parent (dom-parent child-v))
    (define new-parent
      (if old-parent
          (dominator-lca v old-parent dom-parent)
          v))
    (vector-set! dom-parents (val-idx child-v) new-parent))
  (vector-set! dom-parents (val-idx root-v) root-v)
  (for ([v (in-list reachable-vs)])
    (expr-recurse (val-def v) (lambda (child) (update-child! v child))))
  dom-parent)

(define (dominator-lca v1 v2 dom-parent)
  (let loop ([v1 v1]
             [v2 v2])
    (define idx1 (val-idx v1))
    (define idx2 (val-idx v2))
    (cond
      [(= idx1 idx2) v1]
      [(< idx1 idx2) (loop (dom-parent v1) v2)]
      [else (loop v1 (dom-parent v2))])))

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

(define (branch-options block alts-vec err-cols pts-vec v v-vals-vec repr)
  (define sorted-indices
    (vector-sort (build-vector (vector-length v-vals-vec) values)
                 (lambda (i j) (</total (vector-ref v-vals-vec i) (vector-ref v-vals-vec j) repr))))
  (define pts*
    (for/list ([i (in-vector sorted-indices)])
      (vector-ref pts-vec i)))
  (define can-split?
    (cons #f
          (for/list ([idx (in-vector sorted-indices 1)]
                     [prev-idx (in-vector sorted-indices 0)])
            (</total (vector-ref v-vals-vec prev-idx) (vector-ref v-vals-vec idx) repr))))

  (define-values (splitss scores) (infer-option-prefixes err-cols sorted-indices can-split?))

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
    (define v (car vs))
    (define v-vals (car (v-values* block (list v) pctx)))
    (check
     (lambda (x y) (equal? (map si-cidx (option-split-indices x)) y))
     (pareto-point-data
      (first (branch-options block (list->vector alts) err-cols pts-vec v v-vals (block-repr-of v))))
     goal))

  (define (test-regimes/prefixes expr goals)
    (define-values (block vs) (progs->block (list expr) #:ctx ctx))
    (define v (car vs))
    (define v-vals (car (v-values* block (list v) pctx)))
    (define options
      (map pareto-point-data
           (reverse
            (branch-options block (list->vector alts) err-cols pts-vec v v-vals (block-repr-of v)))))
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
  (check-equal? (oracle-errors-score err-cols 2) 0.0)

  (check-true (check-critical '(+.f64 (sin.f64 x) y) '(sin.f64 x)))
  (check-false (check-critical '(+.f64 (sin.f64 x) x) '(sin.f64 x)))
  (check-true (check-critical '(+.f64 x x) 'x))
  (check-true (check-critical '(+.f64 x x) '(+.f64 x x)))
  (check-true (check-critical '(sin.f64 x) '(sin.f64 x)))

  (let ()
    (define xy-ctx (context '(x y) <binary64> (list <binary64> <binary64>)))
    (define-values (block vs) (progs->block (list 'x) #:ctx xy-ctx))
    (check-true (critical-subexpression? block (first vs) (block-add! block 'x)))
    (check-false (critical-subexpression? block (first vs) (block-add! block 'y))))

  (let ()
    (define xyz-ctx (context '(x y z) <binary64> (list <binary64> <binary64> <binary64>)))
    (define-values (block vs) (progs->block (list '(* (+ x y) (/ x z))) #:ctx xyz-ctx))
    (check-false (critical-subexpression? block (first vs) (block-add! block '(+ x y)))))

  (let ()
    (define vec2-ctx
      (context '(a b)
               <binary64>
               (list (make-array-representation #:elem <binary64> #:len 2)
                     (make-array-representation #:elem <binary64> #:len 2))))
    (define dot-product
      '(+.f64 (*.f64 (ref.f64 a #s(literal 0 binary64)) (ref.f64 b #s(literal 0 binary64)))
              (*.f64 (ref.f64 a #s(literal 1 binary64)) (ref.f64 b #s(literal 1 binary64)))))
    (define-values (block vs) (progs->block (list dot-product) #:ctx vec2-ctx))
    (check-true (set-member? (critical-subexpressions block (first vs)) (first vs)))))

(define (valid-splitindices? can-split? split-indices)
  (and (for/and ([pidx (map si-pidx (drop-right split-indices 1))])
         (and (> pidx 0) (list-ref can-split? pidx)))
       (= (si-pidx (last split-indices)) (length can-split?))))

(module core typed/racket
  (provide (struct-out si)
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

  ;; Calculate the optimal regimes split using the following DP recurrence:
  ;; best[p][a] = error[p][a] + min(best[p-1][a], penalty + min_b best[p-1][b])
  (: infer-option
     (-> (Listof FlVector) (Vectorof Integer) (Vectorof Boolean) (Values (Listof si) Float)))
  (define (infer-option err-cols sorted-indices can-split-vec)
    (define number-of-alts (length err-cols))
    (define number-of-points (vector-length sorted-indices))
    (define split-penalty (fl number-of-points))

    ;; errors[p][a]
    (: errors (Vectorof FlVector))
    (define errors
      (for/vector #:length number-of-points
                  ([original-idx (in-vector sorted-indices)])
        :
        FlVector
        (define row (make-flvector number-of-alts))
        (for ([alt-idx (in-naturals)]
              [err-col (in-list err-cols)])
          (flvector-set! row alt-idx (flvector-ref err-col original-idx)))
        row))

    ;; row = best[p-1] going in to point p, best[p] coming out; updated in place
    (: row FlVector)
    (define row (flvector-copy (vector-ref errors 0))) ; best[0][a] = error[0][a]
    ;; previous-alts[p][a] = the alt at p-1 on the best path ending on a at p
    (: previous-alts (Vectorof (Vectorof Integer)))
    (define previous-alts (build-vector number-of-points (lambda (_) (make-vector number-of-alts 0))))

    (for ([point-idx (in-range 1 number-of-points)])
      (define best-alt (argmin row))
      ;; penalty + min_b best[p-1][b]
      (define switched-score (+ split-penalty (flvector-ref row best-alt)))
      (define errors-here (vector-ref errors point-idx)) ; error[p]
      (define previous-here (vector-ref previous-alts point-idx))
      (define can-split? (vector-ref can-split-vec point-idx))
      (for ([alt-idx (in-range number-of-alts)])
        (define continued-score (flvector-ref row alt-idx)) ; best[p-1][a]
        (define switch? (and can-split? (< switched-score continued-score)))
        (flvector-set! row
                       alt-idx
                       (+ (flvector-ref errors-here alt-idx)
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

    (define splits
      (for/list :
        (Listof si)
        ([point-idx (in-range 1 (add1 number-of-points))]
         #:unless (and (< point-idx number-of-points)
                       (= (vector-ref alts point-idx) (vector-ref alts (sub1 point-idx)))))
        (si (vector-ref alts (sub1 point-idx)) point-idx)))
    (values splits (flvector-ref row last-alt))) ; (splits, score)

  ;; Repeatedly calculate the optimal regimes split, removing the costliest
  ;; alt (and any alts costlier) one at a time. Doing so until no alts
  ;; remain yields the full set of Pareto optimal regime splits.
  (: infer-option-prefixes
     (-> (Listof FlVector)
         (Vectorof Integer)
         (Listof Boolean)
         (Values (Vectorof (Listof si)) FlVector)))
  (define (infer-option-prefixes err-cols sorted-indices can-split)
    (define can-split-vec (list->vector can-split))
    (define number-of-alts (length err-cols))
    (: splitss (Vectorof (Listof si)))
    (define splitss (make-vector number-of-alts (ann null (Listof si))))
    (define scores (make-flvector number-of-alts +inf.0))
    (let loop ([max-alt (sub1 number-of-alts)])
      (when (>= max-alt 0)
        (define-values (splits score)
          (infer-option (take err-cols (add1 max-alt)) sorted-indices can-split-vec))
        (define highest-used-alt
          (apply max
                 (for/list :
                   (Listof Integer)
                   ([split (in-list splits)])
                   (si-cidx split))))
        (for ([alt-idx (in-range highest-used-alt (add1 max-alt))])
          (vector-set! splitss alt-idx splits)
          (flvector-set! scores alt-idx score))
        (loop (sub1 highest-used-alt))))
    (values splitss scores)))

(require (submod "." core))
