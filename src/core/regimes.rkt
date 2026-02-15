#lang racket

(require math/flonum
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
         "../utils/pareto.rkt"
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

(define (option-score opt)
  (+ (errors-score (option-errors opt)) (length (option-split-indices opt))))

(define (option-max-alt-index opt)
  (si-cidx (argmax si-cidx (option-split-indices opt))))

(define (option->pareto-point opt)
  (pareto-point (option-max-alt-index opt) (option-score opt) (list opt)))

(define (option-curve options)
  (for/fold ([curve '()]) ([opt (in-vector options)])
    (pareto-union (list (option->pareto-point opt)) curve #:combine append)))

(define (log-option! batch err-cols-vec opt ctx)
  (define alts* (option-alts opt))
  (define alt-count (length alts*))
  (timeline-push! 'inputs (batch->jsexpr batch (map alt-expr alts*)))
  (timeline-push! 'count alt-count (length (option-split-indices opt)))
  (define output-brfs
    (for/list ([sidx (option-split-indices opt)])
      (alt-expr (list-ref alts* (si-cidx sidx)))))
  (timeline-push! 'outputs (batch->jsexpr batch output-brfs))
  (define err-lsts*
    (for/list ([errs (in-vector err-cols-vec 0 alt-count)])
      (vector->list errs)))
  (timeline-push! 'baseline (apply min (map errors-score err-lsts*)))
  (timeline-push! 'accuracy (errors-score (option-errors opt)))
  (define repr (context-repr ctx))
  (timeline-push! 'repr (~a (representation-name repr)))
  (timeline-push! 'oracle (errors-score (apply map max err-lsts*)))
  (define brf-repr ((batch-reprs batch ctx) (option-expr opt)))
  (timeline-push! 'branch
                  (batch->jsexpr batch (list (option-expr opt)))
                  (errors-score (option-errors opt))
                  (length (option-split-indices opt))
                  (~a (representation-name brf-repr))))

(define (pareto-regimes batch sorted start-prog ctx pcontext)
  (timeline-event! 'regimes)
  (define err-lsts (batch-errors batch (map alt-expr sorted) pcontext ctx))
  (define err-cols-vec (list->vector (map list->vector err-lsts)))
  (define err-bits-cols-vec
    (for/vector ([errs (in-vector err-cols-vec)])
      (vector->flvector (vector-map ulps->bits errs))))
  (define branches
    (if (null? sorted)
        '()
        (exprs-to-branch-on batch start-prog ctx)))
  (define branch-brfs
    (if (flag-set? 'reduce 'branch-expressions)
        branches
        (map (curry batch-add! batch) (context-vars ctx))))
  (define brf-vals (brf-values* batch branch-brfs ctx pcontext))
  (define pts-vec (pcontext-points pcontext))
  (define curve
    (for/fold ([curve '()]) ([(brf brf-vals) (in-dict (map cons branch-brfs brf-vals))])
      (define options
        (option-table-on-brf batch sorted err-cols-vec err-bits-cols-vec pts-vec brf brf-vals ctx))
      (pareto-union (option-curve options) curve #:combine append)))
  (define opts
    (for/list ([ppt (in-list curve)])
      (car (pareto-point-data ppt))))
  (for ([opt (in-list opts)])
    (log-option! batch err-cols-vec opt ctx))
  opts)

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

(define (option-table-on-brf batch alts err-cols-vec err-bits-cols-vec pts-vec brf brf-vals-vec ctx)
  (define timeline-stop! (timeline-start! 'times (batch->jsexpr batch (list brf))))
  (define repr ((batch-reprs batch ctx) brf))
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
  (define split-indices-table (infer-split-indices/all err-bits-cols-vec sorted-indices can-split?))
  (define prefix-alts
    (for/list ([alt-count (in-range 1 (add1 (length alts)))])
      (take alts alt-count)))
  (define out
    (for/vector ([alts* (in-list prefix-alts)]
                 [split-indices (in-vector split-indices-table)])
      (option split-indices alts* pts* brf (pick-errors split-indices sorted-indices err-cols-vec))))
  (timeline-stop!)
  out)

(define/contract (pick-errors split-indices sorted-indices err-cols-vec)
  (-> (listof si?) vector? vector? (listof nonnegative-integer?))
  (for/list ([i (in-naturals)]
             [point-idx (in-vector sorted-indices)])
    (define alt-idx (si-cidx (findf (lambda (x) (< i (si-pidx x))) split-indices)))
    (vector-ref (vector-ref err-cols-vec alt-idx) point-idx)))

(module+ test
  (require "../syntax/platform.rkt"
           "../syntax/load-platform.rkt")
  (activate-platform! "c")
  (define ctx (context '(x) <binary64> (list <binary64>)))
  (define pctx (mk-pcontext '(#(0.5) #(4.0)) '(1.0 1.0)))
  (define alts (map make-alt (list '(fmin.f64 x 1) '(fmax.f64 x 1))))
  (define err-lsts `((,(expt 2 53) 1) (1 ,(expt 2 53))))
  (define err-cols-vec (list->vector (map list->vector err-lsts)))
  (define err-bits-cols-vec
    (for/vector ([errs (in-vector err-cols-vec)])
      (vector->flvector (vector-map ulps->bits errs))))
  (define pts-vec (pcontext-points pctx))

  (define (test-regimes expr goal)
    (define-values (batch brfs) (progs->batch (list expr)))
    (define brf (car brfs))
    (define brf-vals (car (brf-values* batch (list brf) ctx pctx)))
    (define options
      (option-table-on-brf batch alts err-cols-vec err-bits-cols-vec pts-vec brf brf-vals ctx))
    (check (lambda (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (vector-ref options (sub1 (vector-length options)))
           goal))

  ;; This is a basic sanity test
  (test-regimes 'x '(1 0))

  ;; This test ensures we handle equal points correctly. All points
  ;; are equal along the `1` axis, so we should only get one
  ;; splitpoint (the second, since it is better at the further point).
  (test-regimes (literal 1 'binary64) '(0))

  (test-regimes `(if.f64 (==.f64 x ,(literal 0.5 'binary64)) ,(literal 1 'binary64) (NAN.f64))
                '(1 0)))

(module core typed/racket
  (provide (struct-out si)
           infer-split-indices
           infer-split-indices/all)
  (require math/flonum)

  ;; Struct representing a splitindex
  ;; cidx = Candidate index: the index candidate program that should be used to the left of this splitindex
  ;; pidx = Point index: The index of the point to the left of which we should split.
  (struct si ([cidx : Integer] [pidx : Integer]) #:prefab)

  (: sorted-errors (-> FlVector (Vectorof Integer) FlVector))
  (define (sorted-errors alt-errors sorted-indices)
    (vector->flvector (vector-map (lambda ([point-idx : Integer]) (flvector-ref alt-errors point-idx))
                                  sorted-indices)))

  (: reconstruct-split-indices (-> (Vectorof Integer) (Vectorof Integer) Integer (Listof si)))
  (define (reconstruct-split-indices result-alt-idxs result-prev-idxs number-of-points)
    (let loop ([i (- number-of-points 1)]
               [rest (ann null (Listof si))])
      (define alt-idx (vector-ref result-alt-idxs i))
      (define next (vector-ref result-prev-idxs i))
      (define sis (cons (si alt-idx (+ i 1)) rest))
      (if (< next i)
          (loop next sis)
          sis)))

  ;; Computes regimes for every alt prefix in one pass. The output at
  ;; index i is the optimal split list when only alts [0..i] are
  ;; available.
  (: infer-split-indices/all
     (-> (Vectorof FlVector) (Vectorof Integer) (Listof Boolean) (Vectorof (Listof si))))
  (define (infer-split-indices/all err-bits-cols sorted-indices can-split)
    ;; Converts the list to vector form for faster processing
    (define can-split-vec (list->vector can-split))
    (define flvec-psums
      (vector-map (lambda ([alt-errors : FlVector])
                    (flvector-sums (sorted-errors alt-errors sorted-indices)))
                  err-bits-cols))

    ;; Set up data needed for algorithm
    (define number-of-points (vector-length can-split-vec))
    (define number-of-alts (vector-length flvec-psums))
    ;; min-weight is used as penalty to favor not adding split points
    (define min-weight (fl number-of-points))

    (: result-error-sums* (Vectorof FlVector))
    (: result-alt-idxs* (Vectorof (Vectorof Integer)))
    (: result-prev-idxs* (Vectorof (Vectorof Integer)))
    (define result-error-sums*
      (for/vector :
        (Vectorof FlVector)
        ([_ (in-range number-of-alts)])
        (make-flvector number-of-points +inf.0)))
    (define result-alt-idxs*
      (for/vector :
        (Vectorof (Vectorof Integer))
        ([_ (in-range number-of-alts)])
        (make-vector number-of-points 0)))
    (define result-prev-idxs*
      (for/vector :
        (Vectorof (Vectorof Integer))
        ([_ (in-range number-of-alts)])
        (make-vector number-of-points number-of-points)))

    ;; Initialize with the best single-alt option for each alt prefix.
    (for ([point-idx (in-range number-of-points)])
      (define best-error +inf.0)
      (define best-alt-idx 0)
      (for ([alt-idx (in-range number-of-alts)])
        (define alt-errors (vector-ref flvec-psums alt-idx))
        (define err (flvector-ref alt-errors point-idx))
        (when (< err best-error)
          (set! best-error err)
          (set! best-alt-idx alt-idx))
        (flvector-set! (vector-ref result-error-sums* alt-idx) point-idx best-error)
        (vector-set! (vector-ref result-alt-idxs* alt-idx) point-idx best-alt-idx)
        (vector-set! (vector-ref result-prev-idxs* alt-idx) point-idx number-of-points)))

    ;; Main DP loop. For each point and split, scan alt prefixes once
    ;; while carrying the best suffix-alt candidate.
    (for ([point-idx (in-range number-of-points)])
      (for ([prev-split-idx (in-range point-idx)]
            [can-split (in-vector can-split-vec 1)]
            #:when can-split)
        (define best-segment-cost +inf.0)
        (define best-segment-alt-idx number-of-alts)
        (for ([alt-idx (in-range number-of-alts)])
          (define alt-error-sums (vector-ref flvec-psums alt-idx))
          (define segment-error
            (- (flvector-ref alt-error-sums point-idx) (flvector-ref alt-error-sums prev-split-idx)))
          (when (< segment-error best-segment-cost)
            (set! best-segment-cost segment-error)
            (set! best-segment-alt-idx alt-idx))

          (define result-error-sums (vector-ref result-error-sums* alt-idx))
          (define result-alt-idxs (vector-ref result-alt-idxs* alt-idx))
          (define result-prev-idxs (vector-ref result-prev-idxs* alt-idx))

          (define current-alt-error (flvector-ref result-error-sums point-idx))
          (define current-alt-idx (vector-ref result-alt-idxs point-idx))
          (define current-prev-idx (vector-ref result-prev-idxs point-idx))
          (define alt-error-sum
            (+ (flvector-ref result-error-sums prev-split-idx) best-segment-cost min-weight))

          (define set-cond
            (cond
              [(< alt-error-sum current-alt-error) #t]
              [(and (= alt-error-sum current-alt-error) (> current-alt-idx best-segment-alt-idx)) #t]
              [(and (= alt-error-sum current-alt-error)
                    (= current-alt-idx best-segment-alt-idx)
                    (> current-prev-idx prev-split-idx))
               #t]
              [else #f]))
          (when set-cond
            (flvector-set! result-error-sums point-idx alt-error-sum)
            (vector-set! result-alt-idxs point-idx best-segment-alt-idx)
            (vector-set! result-prev-idxs point-idx prev-split-idx)))))

    (for/vector :
      (Vectorof (Listof si))
      ([alt-idx (in-range number-of-alts)])
      (reconstruct-split-indices (vector-ref result-alt-idxs* alt-idx)
                                 (vector-ref result-prev-idxs* alt-idx)
                                 number-of-points)))

  ;; This is the core main loop of the regimes algorithm.
  ;; Takes in alt-major error columns, point-sorting indices, and a list of
  ;; split indices to determine when it's ok to split for another alt.
  ;; Returns a list of split indices saying which alt to use for which
  ;; range of points. Starting at 1 going up to num-points.
  ;; Alts are indexed 0 and points are index 1.
  (: infer-split-indices (-> (Vectorof FlVector) (Vectorof Integer) (Listof Boolean) (Listof si)))
  (define (infer-split-indices err-bits-cols sorted-indices can-split)
    (define all-split-indices (infer-split-indices/all err-bits-cols sorted-indices can-split))
    (if (zero? (vector-length all-split-indices))
        null
        (vector-ref all-split-indices (sub1 (vector-length all-split-indices))))))

(require (submod "." core))
