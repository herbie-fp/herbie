#lang racket

;;;; Module principles
;; - The core of this file is infer-max-option.
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
    (if (flag-set? 'reduce 'branch-expressions)
        (branch-candidates block (cons start-prog (map alt-expr sorted)) err-cols pcontext)
        (filter real-v? (map (curry block-add! block) (block-vars block)))))

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
                      (option-error last-point)
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
  (timeline-push! 'accuracy
                  (errors-score (first (block-errors block (list start-prog) pcontext)))
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

;; Choose the branch expression whose best-accuracy regimes solution has the
;; lowest error, out of every subexpression of the original program and the alts.
(define (branch-candidates block roots err-cols pcontext)
  (define pool
    (for/list ([v (in-list (block-reachable block roots))]
               #:when (equal? (representation-type (block-repr-of v)) 'real))
      v))
  (define orders
    (for/list ([v (in-list pool)]
               [v-vals-vec (in-list (v-values* block pool pcontext))])
      (define-values (order can-split-vec) (branch-order v-vals-vec (block-repr-of v)))
      (cons order can-split-vec)))
  ;; Expressions that sort the points identically are cached.
  (define candidates (remove-duplicates (map cons pool orders) #:key cdr))
  (define scored
    (for/list ([candidate (in-list candidates)])
      (match-define (cons v (cons order can-split-vec)) candidate)
      (define-values (_splits score) (infer-max-option err-cols order can-split-vec))
      (cons score v)))
  (define ranked (map cdr (sort scored < #:key car)))
  (take ranked (min (*branch-expr-limit*) (length ranked))))

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

(define (branch-order v-vals-vec repr)
  (define order
    (vector-sort (build-vector (vector-length v-vals-vec) values)
                 (lambda (i j) (</total (vector-ref v-vals-vec i) (vector-ref v-vals-vec j) repr))))
  (define can-split-vec (make-vector (vector-length order) #f))
  (for ([idx (in-vector order 1)]
        [prev-idx (in-vector order 0)]
        [k (in-naturals 1)])
    (vector-set! can-split-vec
                 k
                 (</total (vector-ref v-vals-vec prev-idx) (vector-ref v-vals-vec idx) repr)))
  (values order can-split-vec))

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
  (define-values (sorted-indices can-split-vec) (branch-order v-vals-vec repr))
  (define pts*
    (for/list ([i (in-vector sorted-indices)])
      (vector-ref pts-vec i)))

  (define-values (splitss scores) (infer-option-prefixes err-cols sorted-indices can-split-vec))

  (define points
    (for/list ([count (in-range 1 (add1 (vector-length splitss)))])
      (define split-indices (vector-ref splitss (sub1 count)))
      (define alts (vector->list (vector-take alts-vec count)))
      (define error (+ (/ (flvector-ref scores (sub1 count)) (vector-length sorted-indices)) 1))
      (pareto-point count error (option split-indices alts pts* v))))
  (for/fold ([curve '()]) ([point (in-list points)])
    (pareto-union curve (list point) #:combine (lambda (old _new) old))))

;; Repeatedly solve the maximum-accuracy problem. If the maximum used alt is
;; m, that solution is optimal for every prefix from m through the prefix just
;; solved, so one solve fills an entire Pareto plateau.
(define (infer-option-prefixes err-cols sorted-indices can-split-vec)
  (define number-of-alts (length err-cols))
  (define splitss (make-vector number-of-alts null))
  (define scores (make-flvector number-of-alts +inf.0))
  (let loop ([max-alt (sub1 number-of-alts)])
    (when (>= max-alt 0)
      (define-values (splits score)
        (infer-max-option (take err-cols (add1 max-alt)) sorted-indices can-split-vec))
      (define highest-used-alt (apply max (map si-cidx splits)))
      (for ([alt-idx (in-range highest-used-alt (add1 max-alt))])
        (vector-set! splitss alt-idx splits)
        (flvector-set! scores alt-idx score))
      (loop (sub1 highest-used-alt))))
  (values splitss scores))

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
           infer-max-option)
  (require math/flonum)

  ;; Struct representing a splitindex
  ;; cidx = Candidate index: the index candidate program that should be used to the left of this splitindex
  ;; pidx = Point index: The index of the point to the left of which we should split.
  (struct si ([cidx : Integer] [pidx : Integer]) #:prefab)

  ;; Solve regimes with one fixed prefix of alts. The state is the alt used on
  ;; the final segment, so every possible prior split is represented without
  ;; explicitly scanning all prior split points.
  (: infer-max-option
     (-> (Listof FlVector) (Vectorof Integer) (Vectorof Boolean) (Values (Listof si) Float)))
  (define (infer-max-option err-cols sorted-indices can-split-vec)
    (define number-of-alts (length err-cols))
    (define number-of-points (vector-length can-split-vec))
    (define min-weight (fl number-of-points))
    (: err-vec (Vectorof FlVector))
    (define err-vec (list->vector err-cols))
    (: previous FlVector)
    (: current FlVector)
    (define previous (make-flvector number-of-alts))
    (define current (make-flvector number-of-alts))
    (: previous-alts (Vectorof Integer))
    (define previous-alts (make-vector (* number-of-points number-of-alts) 0))
    (: previous-index (-> Integer Integer Integer))
    (define (previous-index point-idx alt-idx)
      (+ (* point-idx number-of-alts) alt-idx))

    (for ([alt-idx (in-range number-of-alts)])
      (flvector-set! previous
                     alt-idx
                     (flvector-ref (vector-ref err-vec alt-idx) (vector-ref sorted-indices 0))))

    (for ([point-idx (in-range 1 number-of-points)])
      (define best-alt 0)
      (define best-score +inf.0)
      (define second-best-alt 0)
      (define second-best-score +inf.0)
      (for ([alt-idx (in-range number-of-alts)])
        (define score (flvector-ref previous alt-idx))
        (cond
          [(< score best-score)
           (set! second-best-alt best-alt)
           (set! second-best-score best-score)
           (set! best-score score)
           (set! best-alt alt-idx)]
          [(< score second-best-score)
           (set! second-best-alt alt-idx)
           (set! second-best-score score)]))
      (define original-idx (vector-ref sorted-indices point-idx))
      (define can-split? (vector-ref can-split-vec point-idx))
      (for ([alt-idx (in-range number-of-alts)])
        (define continued-score (flvector-ref previous alt-idx))
        (define switched-score (+ min-weight (if (= alt-idx best-alt) second-best-score best-score)))
        (define switch? (and can-split? (< switched-score continued-score)))
        (flvector-set! current
                       alt-idx
                       (+ (flvector-ref (vector-ref err-vec alt-idx) original-idx)
                          (if switch? switched-score continued-score)))
        (vector-set! previous-alts
                     (previous-index point-idx alt-idx)
                     (if switch?
                         (if (= alt-idx best-alt) second-best-alt best-alt)
                         alt-idx)))
      (define temporary previous)
      (set! previous current)
      (set! current temporary))

    (define end-alt 0)
    (define score +inf.0)
    (for ([alt-idx (in-range number-of-alts)])
      (define alt-score (flvector-ref previous alt-idx))
      (when (< alt-score score)
        (set! score alt-score)
        (set! end-alt alt-idx)))
    (values
     (let loop ([point-idx (sub1 number-of-points)]
                [alt-idx end-alt]
                [end number-of-points]
                [rest (ann null (Listof si))])
       (if (= point-idx 0)
           (cons (si alt-idx end) rest)
           (let ([previous-alt (vector-ref previous-alts (previous-index point-idx alt-idx))])
             (if (= previous-alt alt-idx)
                 (loop (sub1 point-idx) alt-idx end rest)
                 (loop (sub1 point-idx) previous-alt point-idx (cons (si alt-idx end) rest))))))
     score)))

(require (submod "." core))
