#lang racket

;;;; Module principles
;; - The core of this file is infer-option-prefixes.
;;   It is a giant dynamic programming algorithm.
;;   It is extremely performance-sensitive.
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
    (define vec2 (make-array-representation #:slots (list <binary64> <binary64>)))
    (define vec2-ctx (context '(a b) <binary64> (list vec2 vec2)))
    (define dot-product
      '(+.f64 (*.f64 (ref.0.array<binary64:binary64> a) (ref.0.array<binary64:binary64> b))
              (*.f64 (ref.1.array<binary64:binary64> a) (ref.1.array<binary64:binary64> b))))
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

  (: resort-errors (-> FlVector (Vectorof Integer) FlVector))
  (define (resort-errors alt-errors sorted-indices)
    (for/flvector #:length (vector-length sorted-indices)
                  ([point-idx (in-vector sorted-indices)])
                  (flvector-ref alt-errors point-idx)))

  ;; This is the core main loop of the regimes algorithm.
  ;; Takes in alt-major error columns, point-sorting indices, and a list of
  ;; split indices to determine when it's ok to split for another alt.
  ;; Returns a list of split indices saying which alt to use for which
  ;; range of points. Starting at 1 going up to num-points.
  ;; Alts are indexed 0 and points are index 1.
  (: infer-option-prefixes
     (-> (Listof FlVector)
         (Vectorof Integer)
         (Listof Boolean)
         (Values (Vectorof (Listof si)) FlVector)))
  (define (infer-option-prefixes err-cols sorted-indices can-split)
    (define can-split-vec (list->vector can-split))
    (define number-of-alts (length err-cols))
    (: flvec-psums (Vectorof FlVector))
    (define flvec-psums
      (for/vector #:length number-of-alts
                  ([err-col (in-list err-cols)])
        :
        FlVector
        (flvector-sums (resort-errors err-col sorted-indices))))

    ;; Set up data needed for algorithm
    (define number-of-points (vector-length can-split-vec))
    ;; min-weight is used as penalty to favor not adding split points
    (define min-weight (fl number-of-points))

    (: result-error-sums (Vectorof FlVector))
    (: result-alt-idxs (Vectorof (Vectorof Integer)))
    (: result-prev-idxs (Vectorof (Vectorof Integer)))
    (define result-error-sums
      (for/vector #:length number-of-alts
                  ([alt-idx (in-range number-of-alts)])
        :
        FlVector
        (make-flvector number-of-points +inf.0)))
    (define result-alt-idxs
      (for/vector #:length number-of-alts
                  ([alt-idx (in-range number-of-alts)])
        :
        (Vectorof Integer)
        (make-vector number-of-points 0)))
    (define result-prev-idxs
      (for/vector #:length number-of-alts
                  ([alt-idx (in-range number-of-alts)])
        :
        (Vectorof Integer)
        (make-vector number-of-points number-of-points)))

    ;; Vectors used to determine the best final segment for each possible split
    ;; when adding alts in increasing cost order.
    (: best-alt-idxs (Vectorof Integer))
    (: best-alt-costs FlVector)
    (define best-alt-idxs (make-vector number-of-points number-of-alts))
    (define best-alt-costs (make-flvector number-of-points))

    (for ([point-idx (in-range number-of-points)])
      (define current-best-alt 0)
      (define current-best-cost +inf.0)

      (for ([prev-split-idx (in-range number-of-points)])
        (vector-set! best-alt-idxs prev-split-idx number-of-alts)
        (flvector-set! best-alt-costs prev-split-idx +inf.0))

      (for ([alt-idx (in-range number-of-alts)])
        (define alt-error-sums (vector-ref flvec-psums alt-idx))
        (define single-alt-error (flvector-ref alt-error-sums point-idx))
        (when (< single-alt-error current-best-cost)
          (set! current-best-cost single-alt-error)
          (set! current-best-alt alt-idx))

        (define current-alt-error current-best-cost)
        (define current-alt-idx current-best-alt)
        (define current-prev-idx number-of-points)

        ;; Update the best last segment for each split point with the newly
        ;; available alt.
        (for ([prev-split-idx (in-range point-idx)]
              [prev-alt-error-sum (in-flvector alt-error-sums)]
              [can-split (in-vector can-split-vec 1)]
              #:when can-split)
          (define best-alt-idx (vector-ref best-alt-idxs prev-split-idx))
          (define best-alt-cost (flvector-ref best-alt-costs prev-split-idx))
          (define segment-error (- single-alt-error prev-alt-error-sum))
          (when (or (= best-alt-idx number-of-alts) (< segment-error best-alt-cost))
            (flvector-set! best-alt-costs prev-split-idx segment-error)
            (vector-set! best-alt-idxs prev-split-idx alt-idx)))

        ;; Compare against the best already-computed prefix result for this alt
        ;; budget.
        (define alt-result-error-sums (vector-ref result-error-sums alt-idx))
        (for ([prev-split-idx (in-range point-idx)]
              [r-error-sum (in-flvector alt-result-error-sums)]
              [best-alt-idx (in-vector best-alt-idxs)]
              [best-alt-cost (in-flvector best-alt-costs)]
              [can-split (in-vector can-split-vec 1)]
              #:when can-split)
          (define alt-error-sum (+ r-error-sum best-alt-cost min-weight))
          (define set-cond
            (cond
              [(< alt-error-sum current-alt-error) #t]
              [(and (= alt-error-sum current-alt-error) (> current-alt-idx best-alt-idx)) #t]
              [(and (= alt-error-sum current-alt-error)
                    (= current-alt-idx best-alt-idx)
                    (> current-prev-idx prev-split-idx))
               #t]
              [else #f]))
          (when set-cond
            (set! current-alt-error alt-error-sum)
            (set! current-alt-idx best-alt-idx)
            (set! current-prev-idx prev-split-idx)))

        (flvector-set! (vector-ref result-error-sums alt-idx) point-idx current-alt-error)
        (vector-set! (vector-ref result-alt-idxs alt-idx) point-idx current-alt-idx)
        (vector-set! (vector-ref result-prev-idxs alt-idx) point-idx current-prev-idx)))

    (define splitss
      (for/vector #:length number-of-alts
                  ([alt-idx (in-range number-of-alts)])
        :
        (Listof si)
        (let loop ([i (- number-of-points 1)]
                   [rest (ann null (Listof si))])
          (define alt-idx* (vector-ref (vector-ref result-alt-idxs alt-idx) i))
          (define next (vector-ref (vector-ref result-prev-idxs alt-idx) i))
          (define sis (cons (si alt-idx* (+ i 1)) rest))
          (if (< next i)
              (loop next sis)
              sis))))

    (define scores
      (for/flvector #:length number-of-alts
                    ([alt-idx (in-range number-of-alts)])
                    (flvector-ref (vector-ref result-error-sums alt-idx) (sub1 number-of-points))))
    (values splitss scores)))

(require (submod "." core))
