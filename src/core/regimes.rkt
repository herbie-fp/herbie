#lang racket

(require math/flonum
         "../core/alternative.rkt"
         "../utils/common.rkt"
         "../utils/pareto.rkt"
         "../syntax/float.rkt"
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

(module+ test
  (require rackunit
           "../syntax/syntax.rkt"
           "../syntax/sugar.rkt"))

(struct option (split-indices alts pts expr errors)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc opt port mode)
     (fprintf port "#<option ~a>" (option-split-indices opt)))])

(define (pareto-regimes batch sorted start-prog ctx pcontext)
  (timeline-event! 'regimes)
  (define alts-vec (list->vector sorted))
  (define alt-count (vector-length alts-vec))
  (define err-cols-vec (list->vector (batch-errors batch (map alt-expr sorted) pcontext ctx)))
  (define branches
    (if (null? sorted)
        '()
        (exprs-to-branch-on batch start-prog ctx)))
  (define branch-brfs
    (if (flag-set? 'reduce 'branch-expressions)
        branches
        (map (curry batch-add! batch) (context-vars ctx))))
  (define batch-jsexpr (batch->jsexpr batch (append (map alt-expr sorted) branch-brfs)))
  (timeline-push! 'batch batch-jsexpr)
  (define branch-roots (drop (hash-ref batch-jsexpr 'roots) alt-count))
  (define branch-root-map
    (for/hash ([brf (in-list branch-brfs)]
               [root (in-list branch-roots)])
      (values brf root)))
  (define brf-vals (brf-values* batch branch-brfs ctx pcontext))
  (define reprs (batch-reprs batch ctx))
  (define pts-vec (pcontext-points pcontext))
  (define option-curve
    (for/list ([brf (in-list branch-brfs)]
               [brf-vals-vec (in-list brf-vals)])
      (branch-option-curve batch
                           alts-vec
                           err-cols-vec
                           pts-vec
                           brf
                           (hash-ref branch-root-map brf)
                           brf-vals-vec
                           ctx
                           reprs)))
  (define combined-option-curve
    (for/fold ([curve '()]) ([branch-curve (in-list option-curve)])
      (pareto-union curve branch-curve #:combine (lambda (old _new) old))))
  (let loop ([count alt-count])
    (if (zero? count)
        '()
        (let* ([ppt (select-curve-point combined-option-curve count)]
               [opt (pareto-point-data ppt)]
               [output-brfs (for/list ([sidx (in-list (option-split-indices opt))])
                              (alt-expr (list-ref (option-alts opt) (si-cidx sidx))))]
               [next-count (si-cidx (argmax (lambda (sidx) (si-cidx sidx))
                                            (option-split-indices opt)))])
          (timeline-push! 'inputs (batch->jsexpr batch (map alt-expr (option-alts opt))))
          (timeline-push! 'count (length (option-alts opt)) (length (option-split-indices opt)))
          (timeline-push! 'outputs (batch->jsexpr batch output-brfs))
          (timeline-push! 'baseline
                          (baseline-errors-score (prefix-error-columns err-cols-vec
                                                                       (pareto-point-cost ppt))))
          (timeline-push! 'accuracy (errors-score (option-errors opt)))
          (timeline-push! 'repr (~a (representation-name (context-repr ctx))))
          (timeline-push! 'oracle
                          (oracle-errors-score (prefix-error-columns err-cols-vec
                                                                     (pareto-point-cost ppt))))
          (cons opt (loop next-count))))))

(define (exprs-to-branch-on batch start-prog ctx)
  (define exprs (batch-exprs batch))
  (define start-expr (exprs start-prog))
  ;; We can only binary search if the branch expression is critical
  ;; for the start program and is real-typed.
  (for/list ([subexpr (set-union (context-vars ctx) (all-subexpressions start-expr))]
             #:when (critical-subexpression? start-expr subexpr)
             #:when (equal? (representation-type (repr-of subexpr ctx)) 'real))
    (batch-add! batch subexpr)))

;; Requires that expr is not a λ expression
(define (critical-subexpression? expr subexpr)
  (define crit-vars (free-variables subexpr))
  (define replaced-expr (replace-expression expr subexpr 1))
  (define non-crit-vars (free-variables replaced-expr))
  (and (not (null? crit-vars)) (null? (set-intersect crit-vars non-crit-vars))))

(define (brf-values* batch brfs ctx pcontext)
  (define count (length brfs))
  (define fn (compile-batch batch brfs ctx))
  (define num-points (pcontext-length pcontext))
  (define vals (build-vector count (lambda (_) (make-vector num-points))))
  (for ([pt (in-vector (pcontext-points pcontext))]
        [p (in-naturals)])
    (define outs (fn pt))
    (for ([out (in-vector outs)]
          [i (in-naturals)])
      (vector-set! (vector-ref vals i) p out)))
  (vector->list vals))

(define (prefix-error-columns err-cols-vec count)
  (for/list ([idx (in-range count)])
    (vector-ref err-cols-vec idx)))

(define (select-curve-point curve count)
  (findf (lambda (ppt) (<= (pareto-point-cost ppt) count)) curve))

(define (branch-option-curve batch
                             alts-vec
                             err-cols-vec
                             pts-vec
                             brf
                             branch-root
                             brf-vals-vec
                             ctx
                             reprs)
  (define timeline-stop! (timeline-start! 'times (batch->jsexpr batch (list brf))))
  (define repr (reprs brf))
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
                     [prev-idx (in-vector sorted-indices 0 (sub1 (vector-length sorted-indices)))])
            (</total (vector-ref brf-vals-vec prev-idx) (vector-ref brf-vals-vec idx) repr))))
  (define-values (splitss scores)
    (infer-option-prefixes (vector->list err-cols-vec) sorted-indices can-split?))
  (define full-count-splits (vector-ref splitss (sub1 (vector-length splitss))))
  (timeline-stop!)
  (timeline-push! 'branch
                  branch-root
                  (/ (flvector-ref scores (sub1 (flvector-length scores)))
                     (vector-length sorted-indices))
                  (length full-count-splits)
                  (~a (representation-name repr)))
  (for/fold ([curve '()]) ([count (in-range 1 (add1 (vector-length splitss)))])
    (define split-indices (vector-ref splitss (sub1 count)))
    (define alts
      (for/list ([idx (in-range count)])
        (vector-ref alts-vec idx)))
    (define error (+ (/ (flvector-ref scores (sub1 count)) (vector-length sorted-indices)) 1))
    (define opt
      (option split-indices alts pts* brf (pick-errors split-indices sorted-indices err-cols-vec)))
    (pareto-union curve (list (pareto-point count error opt)) #:combine (lambda (old _new) old))))

(define/contract (pick-errors split-indices sorted-indices err-cols)
  (-> (listof si?) vector? vector? flvector?)
  (for/flvector #:length (vector-length sorted-indices)
                ([i (in-naturals)] [point-idx (in-vector sorted-indices)])
                (define alt-idx (si-cidx (findf (lambda (x) (< i (si-pidx x))) split-indices)))
                (flvector-ref (vector-ref err-cols alt-idx) point-idx)))

(define (baseline-errors-score err-cols)
  (apply min (map errors-score err-cols)))

(define (oracle-errors-score err-cols)
  (define num-points (flvector-length (first err-cols)))
  (/ (for/sum ([point-idx (in-range num-points)])
              (for/fold ([max-err 0.0]) ([err-col (in-list err-cols)])
                (max max-err (flvector-ref err-col point-idx))))
     num-points))

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
    (define err-cols-vec (list->vector err-cols))
    (define brf-root (first (hash-ref (batch->jsexpr batch (list brf)) 'roots)))
    (define reprs (batch-reprs batch ctx))
    (check (lambda (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (pareto-point-data (first (branch-option-curve batch
                                                          (list->vector alts)
                                                          err-cols-vec
                                                          pts-vec
                                                          brf
                                                          brf-root
                                                          brf-vals
                                                          ctx
                                                          reprs)))
           goal))

  (define (test-regimes/prefixes expr goals)
    (define-values (batch brfs) (progs->batch (list expr)))
    (define brf (car brfs))
    (define brf-vals (car (brf-values* batch (list brf) ctx pctx)))
    (define err-cols-vec (list->vector err-cols))
    (define brf-root (first (hash-ref (batch->jsexpr batch (list brf)) 'roots)))
    (define reprs (batch-reprs batch ctx))
    (define options
      (map pareto-point-data
           (reverse (branch-option-curve batch
                                         (list->vector alts)
                                         err-cols-vec
                                         pts-vec
                                         brf
                                         brf-root
                                         brf-vals
                                         ctx
                                         reprs))))
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

  (test-regimes `(if.f64 (==.f64 x ,(literal 0.5 'binary64)) ,(literal 1 'binary64) (NAN.f64))
                '(1 0)))

;; Given error-lsts, returns a list of sp objects representing where the optimal splitpoints are.
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
    ;; Converts the list to vector form for faster processing
    (define can-split-vec (list->vector can-split))
    (define number-of-alts (length err-cols))
    (define flvec-psums
      :
      (Vectorof FlVector)
      (for/vector: #:length number-of-alts
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
      (for/vector: #:length number-of-alts
                   ([alt-idx (in-range number-of-alts)])
                   :
                   FlVector
                   (make-flvector number-of-points +inf.0)))
    (define result-alt-idxs
      (for/vector: #:length number-of-alts
                   ([alt-idx (in-range number-of-alts)])
                   :
                   (Vectorof Integer)
                   (make-vector number-of-points 0)))
    (define result-prev-idxs
      (for/vector: #:length number-of-alts
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
      (for/vector: #:length number-of-alts
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
