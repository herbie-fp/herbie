#lang racket

;;;; Module principles
;; - The core of this file is infer-option-prefixes.
;;   It is a giant dynamic programming algorithm.
;;   It is extremely performance-sensitive.
;; - Therefore almost everything is vector-based with few copies.
;;   Except exprs-to-branch-on. Converting it to vectors makes it slow.
;; - Everything else is overhead and should be minimized.

(require math/flonum
         "../core/alternative.rkt"
         "../utils/common.rkt"
         "../utils/pareto.rkt"
         "../syntax/float.rkt"
         "../syntax/syntax.rkt"
         "../utils/timeline.rkt"
         "../syntax/types.rkt"
         "../syntax/batch.rkt"
         "compiler.rkt"
         "points.rkt"
         "programs.rkt")
(provide pareto-regimes
         (struct-out option)
         (struct-out si)
         critical-subexpression?)

(define *critical-subexpressions-cache* (make-hash))

(module+ test
  (require rackunit
           "../syntax/syntax.rkt"
           "../syntax/sugar.rkt"))

(struct option (split-indices alts pts expr)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc opt port mode)
     (fprintf port "#<option ~a>" (option-split-indices opt)))])

;; CONSIDER: move start-prog and the "branch-brfs" computation into caller.
(define (pareto-regimes batch sorted start-prog ctx pcontext)
  (timeline-event! 'regimes)
  (define alts-vec (list->vector sorted))
  (define alt-count (vector-length alts-vec))
  (define err-cols (batch-errors batch (map alt-expr sorted) pcontext ctx))
  (define branch-brfs
    (if (flag-set? 'reduce 'branch-expressions)
        (exprs-to-branch-on batch start-prog ctx)
        (map (curry batch-add! batch) (context-vars ctx))))

  (define brf-vals (brf-values* batch branch-brfs ctx pcontext))
  (define reprs (batch-reprs batch ctx))
  (define pts-vec (pcontext-points pcontext))

  ;; For timeline
  (define batch-jsexpr (batch->jsexpr batch (append (map alt-expr sorted) branch-brfs)))
  (timeline-push! 'batch batch-jsexpr)
  (define branch-roots (drop (hash-ref batch-jsexpr 'roots) alt-count))
  (define branch-root-map (make-immutable-hash (map cons branch-brfs branch-roots)))

  (define option-curves
    (for/list ([brf (in-list branch-brfs)]
               [brf-vals-vec (in-list brf-vals)])
      (define timeline-stop! (timeline-start! 'times (batch->jsexpr batch (list brf))))
      (define repr (reprs brf))
      (define curve (branch-options batch alts-vec err-cols pts-vec brf brf-vals-vec repr))
      (define last-point (last curve))
      (timeline-stop!)
      (timeline-push! 'branch
                      (hash-ref branch-root-map brf)
                      (- (pareto-point-error last-point)
                         (length (option-split-indices (pareto-point-data last-point))))
                      (length (option-split-indices (pareto-point-data last-point)))
                      (~a (representation-name repr)))
      curve))
  (define combined-option-curve
    (for/fold ([curve '()]) ([branch-curve (in-list option-curves)])
      (pareto-union curve branch-curve #:combine (lambda (old _new) old))))

  ;; Timeline
  (for/list ([ppt (in-list combined-option-curve)])
    (define opt (pareto-point-data ppt))
    (define output-brfs
      (for/list ([sidx (in-list (option-split-indices opt))])
        (alt-expr (list-ref (option-alts opt) (si-cidx sidx)))))
    (timeline-push! 'inputs (batch->jsexpr batch (map alt-expr (option-alts opt))))
    (timeline-push! 'count (length (option-alts opt)) (length (option-split-indices opt)))
    (timeline-push! 'outputs (batch->jsexpr batch output-brfs))
    (timeline-push! 'baseline (baseline-errors-score err-cols (pareto-point-cost ppt)))
    (timeline-push! 'accuracy (- (pareto-point-error ppt) (length (option-split-indices opt))))
    (timeline-push! 'oracle (oracle-errors-score err-cols (pareto-point-cost ppt)))
    opt))

(define (exprs-to-branch-on batch start-prog ctx)
  (define exprs (batch-exprs batch))
  (define start-expr (exprs start-prog))
  (define reprs (batch-reprs batch ctx))
  ;; We can only binary search if the branch expression is critical
  ;; for the start program and is real-typed.
  (for/list ([subexpr (in-list (critical-subexpressions start-expr))]
             #:do [(define sub-brf (batch-add! batch subexpr))]
             #:when (equal? (representation-type (reprs sub-brf)) 'real))
    sub-brf))

;; Requires that expr is not a λ expression
(define (critical-subexpression? expr subexpr)
  (and (member subexpr (critical-subexpressions expr) equal?) #t))

(define (critical-subexpressions expr)
  (hash-ref!
   *critical-subexpressions-cache*
   expr
   (lambda ()
     (define-values (batch brfs) (progs->batch (list expr)))
     (define root-brf (first brfs))
     (define root-idx (batchref-idx root-brf))
     (define dom-parents (build-dominator-tree batch root-brf))
     (define free-vars (batch-free-vars batch))
     (define exprs (batch-exprs batch))

     (for/list ([subexpr (in-list (all-subexpressions expr))]
                #:do [(define sub-brf (batch-add! batch subexpr))
                      (define sub-idx (batchref-idx sub-brf))
                      (define subexpr* (exprs sub-brf))
                      (define vars* (set->list (free-vars sub-brf)))]
                #:when (equal? subexpr subexpr*)
                #:when (not (= sub-idx root-idx))
                #:when (pair? vars*)
                #:do [(define lca-idx
                        (for/fold ([lca-idx (batchref-idx (batch-add! batch (first vars*)))])
                                  ([var (in-list (rest vars*))])
                          (dominator-lca lca-idx (batchref-idx (batch-add! batch var)) dom-parents)))]
                #:when (dominates? sub-idx lca-idx dom-parents))
       subexpr))))

(define (build-dominator-tree batch root-brf)
  (define root-idx (batchref-idx root-brf))
  (define dom-parents (make-vector (batch-length batch) #f))
  (vector-set! dom-parents root-idx root-idx)
  (for ([idx (in-range root-idx -1 -1)]
        [node (in-batch batch root-idx -1 -1)]
        #:when (vector-ref dom-parents idx))
    (for ([child-idx (in-list (batch-node-children node))])
      (define old-parent (vector-ref dom-parents child-idx))
      (define new-parent
        (if old-parent
            (dominator-lca idx old-parent dom-parents)
            idx))
      (vector-set! dom-parents child-idx new-parent)))
  dom-parents)

(define (batch-node-children node)
  (match node
    [(? number?) '()]
    [(? literal?) '()]
    [(? symbol?) '()]
    [(approx _ impl) (list impl)]
    [(hole _ spec) (list spec)]
    [(list _ args ...) args]))

(define (dominator-lca idx1 idx2 dom-parents)
  (let loop ([idx1 idx1]
             [idx2 idx2])
    (cond
      [(= idx1 idx2) idx1]
      [(< idx1 idx2) (loop (vector-ref dom-parents idx1) idx2)]
      [else (loop idx1 (vector-ref dom-parents idx2))])))

(define (dominates? ancestor-idx idx dom-parents)
  (= ancestor-idx (dominator-lca ancestor-idx idx dom-parents)))

(define (baseline-errors-score err-cols count)
  (for/fold ([best +inf.0]) ([err-col (in-list (take err-cols count))])
    (min best (errors-score err-col))))

(define (oracle-errors-score err-cols count)
  (define num-points (flvector-length (first err-cols)))
  (/ (for/sum ([point-idx (in-range num-points)])
              (for/fold ([best-err +inf.0]) ([err-col (in-list (take err-cols count))])
                (min best-err (flvector-ref err-col point-idx))))
     num-points))

(define (brf-values* batch brfs ctx pcontext)
  (define count (length brfs))
  (define fn (compile-batch batch brfs ctx))
  (define num-points (pcontext-length pcontext))
  (define vals (build-vector count (lambda (_) (make-vector num-points))))
  (for ([pt (in-vector (pcontext-points pcontext))]
        [p (in-naturals)])
    (for ([out (in-vector (fn pt))]
          [i (in-naturals)])
      (vector-set! (vector-ref vals i) p out)))
  (vector->list vals))

(define (branch-options batch alts-vec err-cols pts-vec brf brf-vals-vec repr)
  (define sorted-indices
    (vector-sort (build-vector (vector-length brf-vals-vec) values)
                 (lambda (i j)
                   (</total (vector-ref brf-vals-vec i) (vector-ref brf-vals-vec j) repr))))
  (define pts*
    (for/list ([i (in-vector sorted-indices)])
      (vector-ref pts-vec i)))
  (define can-split?
    (cons #f
          (for/list ([idx (in-vector sorted-indices 1)]
                     [prev-idx (in-vector sorted-indices 0)])
            (</total (vector-ref brf-vals-vec prev-idx) (vector-ref brf-vals-vec idx) repr))))

  (define-values (splitss scores) (infer-option-prefixes err-cols sorted-indices can-split?))

  (define points
    (for/list ([count (in-range 1 (add1 (vector-length splitss)))])
      (define split-indices (vector-ref splitss (sub1 count)))
      (define alts (vector->list (vector-take alts-vec count)))
      (define error (+ (/ (flvector-ref scores (sub1 count)) (vector-length sorted-indices)) 1))
      (pareto-point count error (option split-indices alts pts* brf))))
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
    (define-values (batch brfs) (progs->batch (list expr)))
    (define brf (car brfs))
    (define brf-vals (car (brf-values* batch (list brf) ctx pctx)))
    (define reprs (batch-reprs batch ctx))
    (check
     (lambda (x y) (equal? (map si-cidx (option-split-indices x)) y))
     (pareto-point-data
      (first (branch-options batch (list->vector alts) err-cols pts-vec brf brf-vals (reprs brf))))
     goal))

  (define (test-regimes/prefixes expr goals)
    (define-values (batch brfs) (progs->batch (list expr)))
    (define brf (car brfs))
    (define brf-vals (car (brf-values* batch (list brf) ctx pctx)))
    (define reprs (batch-reprs batch ctx))
    (define options
      (map pareto-point-data
           (reverse
            (branch-options batch (list->vector alts) err-cols pts-vec brf brf-vals (reprs brf)))))
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

  (check-true (critical-subexpression? '(+.f64 (sin.f64 x) y) '(sin.f64 x)))
  (check-false (critical-subexpression? '(+.f64 (sin.f64 x) x) '(sin.f64 x)))
  (check-true (critical-subexpression? '(+.f64 x x) 'x))
  (check-false (critical-subexpression? '(+.f64 x x) '(+.f64 x x)))
  (check-false (critical-subexpression? '(sin.f64 x) '(sin.f64 x))))

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
