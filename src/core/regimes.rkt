#lang racket

;;;; Module principles
;; - The core of this file is infer-option.
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

(struct order (indices can-split?) #:transparent)
(struct candidate (v order error splits))

;; CONSIDER: move start-prog and the "branch-vs" computation into caller.
(define (pareto-regimes block sorted start-prog pcontext spec-block)
  (timeline-event! 'regimes)
  (define alt-count (length sorted))
  (define err-cols (block-errors block (map alt-expr sorted) pcontext))
  (define free-vars (block-free-vars block))
  (define branch-vs
    (for/list ([v (in-list (if (flag-set? 'reduce 'branch-expressions)
                               (block-reachable block (cons start-prog (map alt-expr sorted)))
                               (map (curry block-add! block) (block-vars block))))]
               #:when (equal? (representation-type (block-repr-of v)) 'real)
               #:unless (set-empty? (free-vars v)))
      v))
  (define pts-vec (pcontext-points pcontext))

  ;; For timeline
  (define block-jsexpr (block->jsexpr block spec-block (append (map alt-expr sorted) branch-vs)))
  (timeline-push! 'block block-jsexpr)
  (define branch-roots (drop (hash-ref block-jsexpr 'roots) alt-count))
  (define branch-root-map (make-immutable-hash (map cons branch-vs branch-roots)))

  (define orders
    (remove-duplicates (for/list ([v (in-list branch-vs)]
                                  [v-vals-vec (in-list (v-values* block branch-vs pcontext))])
                         (cons v (branch-order v-vals-vec (block-repr-of v))))
                       #:key cdr))
  (define curve (pareto-curve err-cols orders))

  ;; Timeline
  (timeline-push! 'inputs (block->jsexpr block spec-block (map alt-expr sorted)))
  (timeline-push! 'accuracy
                  (errors-score (first (block-errors block (list start-prog) pcontext)))
                  (baseline-errors-score err-cols alt-count)
                  (apply min (map (compose candidate-mean-error pareto-point-data) curve))
                  (oracle-errors-score err-cols alt-count))
  (define options
    (for/list ([ppt (in-list curve)])
      (match-define (pareto-point cost _ (and c (candidate v ord _ splits))) ppt)
      (timeline-push! 'branch
                      (hash-ref branch-root-map v)
                      (candidate-mean-error c)
                      (length splits)
                      (~a (representation-name (block-repr-of v))))
      (timeline-push! 'count cost (length splits))
      (define pts*
        (for/list ([i (in-vector (order-indices ord))])
          (vector-ref pts-vec i)))
      (option splits (take sorted cost) pts* v)))
  (timeline-push! 'outputs
                  (block->jsexpr block
                                 spec-block
                                 (remove-duplicates
                                  (for*/list ([opt (in-list options)]
                                              [sidx (in-list (option-split-indices opt))])
                                    (alt-expr (list-ref (option-alts opt) (si-cidx sidx)))))))
  options)

(define (candidate-mean-error c)
  (- (candidate-error c) (length (candidate-splits c))))

(define (candidate-alts c)
  (match (candidate-splits c)
    ['() +inf.0]
    [splits (add1 (apply max (map si-cidx splits)))]))

(define (branch-order v-vals-vec repr)
  (define special? (representation-special-value? repr))
  (define ->ordinal (representation-repr->ordinal repr))
  (define keys
    (for/vector #:length (vector-length v-vals-vec)
                ([x (in-vector v-vals-vec)])
      (if (special? x)
          +inf.0
          (->ordinal x))))
  (define indices
    (vector-sort (build-vector (vector-length keys) values) < #:key (curry vector-ref keys)))
  (define can-split?
    (for/vector #:length (vector-length indices)
                ([idx (in-vector indices)]
                 [k (in-naturals)])
      (and (> k 0) (< (vector-ref keys (vector-ref indices (sub1 k))) (vector-ref keys idx)))))
  (order indices can-split?))

(define (pareto-curve err-cols orders)
  (define num-points (flvector-length (first err-cols)))
  (define errors
    (for/vector #:length num-points
                ([point-idx (in-range num-points)])
      (for/flvector #:length (length err-cols)
                    ([err-col (in-list err-cols)])
                    (flvector-ref err-col point-idx))))
  (define totals (for/flvector ([err-col (in-list err-cols)]) (flvector-sum err-col)))
  (define (error-of score)
    (add1 (/ score num-points)))

  (define (splittable? level best-alt)
    (define gains
      (sort (for/list ([alt-idx (in-range level)])
              (for/sum ([row (in-vector errors)])
                       (max 0.0 (- (flvector-ref row best-alt) (flvector-ref row alt-idx)))))
            >))
    (let loop ([gains (rest gains)]
               [total (first gains)]
               [switches 1])
      (and (pair? gains)
           (or (> (+ total (first gains)) (* switches num-points))
               (loop (rest gains) (+ total (first gains)) (add1 switches))))))

  (define (evaluate c level)
    (match-define (candidate v ord _ _) c)
    (define-values (splits score)
      (infer-option errors level (order-indices ord) (order-can-split? ord)))
    (candidate v ord (error-of score) splits))

  (let loop ([candidates (for/list ([(v ord) (in-dict orders)])
                           (candidate v ord -inf.0 '()))]
             [level (length err-cols)]
             [curve '()])
    (cond
      [(zero? level) curve]
      [else
       (define best-alt (argmin (curry flvector-ref totals) (range level)))
       (define single
         (struct-copy candidate
                      (first candidates)
                      [error (error-of (flvector-ref totals best-alt))]
                      [splits (list (si best-alt num-points))]))
       (define-values (best candidates*)
         (cond
           [(splittable? level best-alt)
            (for/fold ([best single]
                       [done '()])
                      ([c (in-list (sort candidates < #:key candidate-error))])
              (define threshold (candidate-error best))
              (define c*
                (if (or (>= (candidate-error c) threshold) (<= (candidate-alts c) level))
                    c
                    (evaluate c level)))
              (values (if (< (candidate-error c*) threshold) c* best) (cons c* done)))]
           [else (values single candidates)]))
       (define alts (candidate-alts best))
       (loop candidates*
             (sub1 alts)
             (pareto-union curve
                           (list (pareto-point alts (candidate-error best) best))
                           #:combine (lambda (old _new) old)))])))

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

(module+ test
  (require "../syntax/platform.rkt"
           "../syntax/load-platform.rkt")
  (activate-platform! "c")
  (define ctx (context '(x) <binary64> (list <binary64>)))
  (define pctx (mk-pcontext '(#(0.5) #(4.0)) '(1.0 1.0)))
  (define alts (map make-alt (list '(fmin.f64 x 1) '(fmax.f64 x 1))))
  (define err-cols (list (flvector 53.0 0.0) (flvector 0.0 53.0)))

  (define (regimes-splits expr)
    (define-values (block vs) (progs->block (list expr) #:ctx ctx))
    (define v (car vs))
    (define v-vals (car (v-values* block (list v) pctx)))
    (for/list ([ppt (in-list (pareto-curve err-cols
                                           (list (cons v (branch-order v-vals (block-repr-of v))))))])
      (map si-cidx (candidate-splits (pareto-point-data ppt)))))

  (define (test-regimes expr goal)
    (check-equal? (first (regimes-splits expr)) goal))

  (define (test-regimes/prefixes expr goals)
    (check-equal? (reverse (regimes-splits expr)) goals))

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
           infer-option)
  (require math/flonum)

  ;; Struct representing a splitindex
  ;; cidx = Candidate index: the index candidate program that should be used to the left of this splitindex
  ;; pidx = Point index: The index of the point to the left of which we should split.
  (struct si ([cidx : Integer] [pidx : Integer]) #:prefab)

  ;; argmin_a scores[a] over the first n alts
  (: argmin (-> FlVector Integer Integer))
  (define (argmin scores n)
    (let loop ([alt-idx 0]
               [best 0]
               [best-score +inf.0])
      (cond
        [(= alt-idx n) best]
        [(< (flvector-ref scores alt-idx) best-score)
         (loop (add1 alt-idx) alt-idx (flvector-ref scores alt-idx))]
        [else (loop (add1 alt-idx) best best-score)])))

  ;; This is the core main loop of the regimes algorithm.
  ;; Takes in point-major error rows, the number of alts to consider,
  ;; point-sorting indices, and a vector of booleans to determine when
  ;; it's ok to split for another alt.
  ;; Returns a list of split indices saying which alt to use for which
  ;; range of points, starting at 1 going up to num-points, and the score
  ;; of that split. Alts are indexed 0 and points are index 1. The optimal
  ;; regimes split is calculated using the following DP recurrence:
  ;; best[p][a] = error[p][a] + min(best[p-1][a], penalty + min_b best[p-1][b])
  (:
   infer-option
   (-> (Vectorof FlVector) Integer (Vectorof Integer) (Vectorof Boolean) (Values (Listof si) Float)))
  (define (infer-option errors number-of-alts sorted-indices can-split-vec)
    (define number-of-points (vector-length sorted-indices))
    (define split-penalty (fl number-of-points))

    ;; row = best[p-1] going in to point p, best[p] coming out; updated in place
    (: row FlVector)
    (define row
      (flvector-copy (vector-ref errors (vector-ref sorted-indices 0))
                     0
                     number-of-alts)) ; best[0][a] = error[0][a]
    ;; best-alts[p] = argmin_b best[p-1][b]
    (define best-alts (make-vector number-of-points 0))
    ;; switched[p][a] = 1 when the best path ending on a at p switches from best-alts[p]
    (define switched (make-bytes (* number-of-points number-of-alts) 0))

    (for ([point-idx (in-range 1 number-of-points)])
      (define best-alt (argmin row number-of-alts))
      ;; penalty + min_b best[p-1][b]
      (define switched-score (+ split-penalty (flvector-ref row best-alt)))
      (define errors-here (vector-ref errors (vector-ref sorted-indices point-idx))) ; error[p]
      (define can-split? (vector-ref can-split-vec point-idx))
      (vector-set! best-alts point-idx best-alt)
      (for ([alt-idx (in-range number-of-alts)])
        (define continued-score (flvector-ref row alt-idx)) ; best[p-1][a]
        (define switch? (and can-split? (< switched-score continued-score)))
        (flvector-set! row
                       alt-idx
                       (+ (flvector-ref errors-here alt-idx)
                          (if switch? switched-score continued-score)))
        (when switch?
          (bytes-set! switched (+ (* point-idx number-of-alts) alt-idx) 1))))

    ;; score = min_a best[P][a]
    (define last-point (sub1 number-of-points))
    (define last-alt (argmin row number-of-alts))

    (: alts (Vectorof Integer))
    (define alts (make-vector number-of-points 0))
    (vector-set! alts last-point last-alt)
    (for ([point-idx (in-range last-point 0 -1)])
      (define alt (vector-ref alts point-idx))
      (vector-set! alts
                   (sub1 point-idx)
                   (if (= 1 (bytes-ref switched (+ (* point-idx number-of-alts) alt)))
                       (vector-ref best-alts point-idx)
                       alt)))

    (define splits
      (for/list :
        (Listof si)
        ([point-idx (in-range 1 (add1 number-of-points))]
         #:unless (and (< point-idx number-of-points)
                       (= (vector-ref alts point-idx) (vector-ref alts (sub1 point-idx)))))
        (si (vector-ref alts (sub1 point-idx)) point-idx)))
    (values splits (flvector-ref row last-alt)))) ; (splits, score)

(require (submod "." core))
