#lang racket

(require math/flonum
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
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
  (define err-lsts (batch-errors batch (map alt-expr sorted) pcontext ctx))
  (define branches
    (if (null? sorted)
        '()
        (exprs-to-branch-on batch start-prog ctx)))
  (define branch-brfs
    (if (flag-set? 'reduce 'branch-expressions)
        branches
        (map (curry batch-add! batch) (context-vars ctx))))
  (define brf-vals (brf-values* batch branch-brfs ctx pcontext))
  (let loop ([alts sorted]
             [errs (hash)]
             [err-lsts err-lsts])
    (cond
      [(null? alts) '()]
      ; Only return one option if not pareto mode
      [else
       (define-values (opt new-errs)
         (infer-splitpoints batch branch-brfs brf-vals alts err-lsts #:errs errs ctx pcontext))
       (define high (si-cidx (argmax (λ (x) (si-cidx x)) (option-split-indices opt))))
       (cons opt (loop (take alts high) new-errs (take err-lsts high)))])))

;; `infer-splitpoints` and `combine-alts` are split so the mainloop
;; can insert a timeline break between them.

(define (infer-splitpoints batch
                           branch-brfs
                           brf-vals
                           alts
                           err-lsts*
                           #:errs [cerrs (hash)]
                           ctx
                           pcontext)
  (timeline-push! 'inputs (batch->jsexpr batch (map alt-expr alts)))
  (define brf-data (map cons branch-brfs brf-vals))
  (define sorted-brfs
    (sort brf-data (lambda (x y) (< (hash-ref cerrs (car x) -1) (hash-ref cerrs (car y) -1)))))
  (define err-cols-vec (list->vector (map list->vector err-lsts*)))
  (define err-bits-cols-vec
    (for/vector ([errs (in-vector err-cols-vec)])
      (vector->flvector (vector-map ulps->bits errs))))
  (define pts-vec (pcontext-points pcontext))

  ;; invariant:
  ;; errs[bexpr] is some best option on branch expression bexpr computed on more alts than we have right now.
  (define-values (best best-err errs)
    (for/fold ([best '()]
               [best-err +inf.0]
               [errs cerrs]
               #:result (values best best-err errs))
              ([(brf brf-vals) (in-dict sorted-brfs)]
               ;; stop if we've computed this (and following) branch-brf on more alts and it's still worse
               #:break (> (hash-ref cerrs brf -1) best-err))
      (define opt (option-on-brf batch alts err-cols-vec err-bits-cols-vec pts-vec brf brf-vals ctx))
      (define err
        (+ (errors-score (option-errors opt))
           (length (option-split-indices opt)))) ;; one-bit penalty per split
      (define new-errs (hash-set errs brf err))
      (if (< err best-err)
          (values opt err new-errs)
          (values best best-err new-errs))))

  (timeline-push! 'count (length alts) (length (option-split-indices best)))
  (define output-brfs
    (for/list ([sidx (option-split-indices best)])
      (alt-expr (list-ref alts (si-cidx sidx)))))
  (timeline-push! 'outputs (batch->jsexpr batch output-brfs))
  (timeline-push! 'baseline (apply min (map errors-score err-lsts*)))
  (timeline-push! 'accuracy (errors-score (option-errors best)))
  (define repr (context-repr ctx))
  (timeline-push! 'repr (~a (representation-name repr)))
  (timeline-push! 'oracle (errors-score (apply map max err-lsts*)))
  (values best errs))

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

(define (option-on-brf batch alts err-cols-vec err-bits-cols-vec pts-vec brf brf-vals-vec ctx)
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
  (define split-indices (infer-split-indices err-bits-cols-vec sorted-indices can-split?))
  (define out
    (option split-indices alts pts* brf (pick-errors split-indices sorted-indices err-cols-vec)))
  (timeline-stop!)
  (timeline-push! 'branch
                  (batch->jsexpr batch (list brf))
                  (errors-score (option-errors out))
                  (length split-indices)
                  (~a (representation-name repr)))
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
    (check (lambda (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-brf batch alts err-cols-vec err-bits-cols-vec pts-vec brf brf-vals ctx)
           goal))

  ;; This is a basic sanity test
  (test-regimes 'x '(1 0))

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
           infer-split-indices)
  (require math/flonum)

  ;; Struct representing a splitindex
  ;; cidx = Candidate index: the index candidate program that should be used to the left of this splitindex
  ;; pidx = Point index: The index of the point to the left of which we should split.
  (struct si ([cidx : Integer] [pidx : Integer]) #:prefab)

  (: sorted-errors (-> FlVector (Vectorof Integer) FlVector))
  (define (sorted-errors alt-errors sorted-indices)
    (vector->flvector (vector-map (lambda ([point-idx : Integer]) (flvector-ref alt-errors point-idx))
                                  sorted-indices)))

  ;; This is the core main loop of the regimes algorithm.
  ;; Takes in alt-major error columns, point-sorting indices, and a list of
  ;; split indices to determine when it's ok to split for another alt.
  ;; Returns a list of split indices saying which alt to use for which
  ;; range of points. Starting at 1 going up to num-points.
  ;; Alts are indexed 0 and points are index 1.
  (: infer-split-indices (-> (Vectorof FlVector) (Vectorof Integer) (Listof Boolean) (Listof si)))
  (define (infer-split-indices err-bits-cols sorted-indices can-split)
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

    ;; These 3 vectors are will contain the output data and be used for
    ;; determining which alt is best for a given point
    (define result-error-sums (make-flvector number-of-points +inf.0))
    (define result-alt-idxs (make-vector number-of-points 0))
    (define result-prev-idxs (make-vector number-of-points number-of-points))

    (for ([alt-idx (in-naturals)]
          [alt-errors (in-vector flvec-psums)])
      (for ([point-idx (in-range number-of-points)]
            [err (in-flvector alt-errors)]
            #:when (< err (flvector-ref result-error-sums point-idx)))
        (flvector-set! result-error-sums point-idx err)
        (vector-set! result-alt-idxs point-idx alt-idx)))

    ;; Vectors are now filled with starting data. Beginning main loop of the
    ;; regimes algorithm.

    ;; Vectors used to determine if our current alt is better than our running
    ;; best alt.
    (: best-alt-idxs (Vectorof Integer))
    (: best-alt-costs FlVector)
    (define best-alt-idxs (make-vector number-of-points number-of-alts))
    (define best-alt-costs (make-flvector number-of-points))

    (for ([point-idx (in-range number-of-points)]
          [current-alt-error (in-flvector result-error-sums)]
          [current-alt-idx (in-vector result-alt-idxs)]
          [current-prev-idx (in-vector result-prev-idxs)])
      ;; Set and fill temporary vectors with starting data
      ;; #f for best index and positive infinite for best cost
      (vector-fill! best-alt-idxs -1)
      (for ([i (in-range number-of-points)])
        (flvector-set! best-alt-costs i +inf.0))

      ;; For each alt loop over its vector of errors
      (for ([alt-idx (in-naturals)]
            [alt-error-sums (in-vector flvec-psums)])
        ;; Loop over the points up to our current point
        (for ([prev-split-idx (in-range point-idx)]
              [prev-alt-error-sum (in-flvector alt-error-sums)]
              [best-alt-idx (in-vector best-alt-idxs)]
              [best-alt-cost (in-flvector best-alt-costs)]
              [can-split (in-vector can-split-vec 1)]
              #:when can-split)
          ;; Check if we can add a split point
          ;; compute the difference between the current error-sum and previous
          (let ([current-error (- (flvector-ref alt-error-sums point-idx) prev-alt-error-sum)])
            ;; if we have not set the best alt yet or
            ;; the current alt-error-sum is less then previous
            (when (or (= best-alt-idx number-of-alts) (< current-error best-alt-cost))
              ;; update best cost and best index
              (flvector-set! best-alt-costs prev-split-idx current-error)
              (vector-set! best-alt-idxs prev-split-idx alt-idx)))))
      ;; We have now have the index of the best alt and its error up to our
      ;; current point-idx.
      ;; Now we compare against our current best saved in the 3 vectors above
      (for ([prev-split-idx (in-range point-idx)]
            [r-error-sum (in-flvector result-error-sums)]
            [best-alt-idx (in-vector best-alt-idxs)]
            [best-alt-cost (in-flvector best-alt-costs)]
            [can-split (in-vector can-split-vec 1)]
            #:when can-split)
        ;; Re compute the error sum for a potential better alt
        (define alt-error-sum (+ r-error-sum best-alt-cost min-weight))
        ;; Check if the new alt-error-sum is better then the current
        (define set-cond
          ;; give benefit to previous best alt
          (cond
            [(< alt-error-sum current-alt-error) #t]
            ;; Tie breaker if error are the same favor first alt
            [(and (= alt-error-sum current-alt-error) (> current-alt-idx best-alt-idx)) #t]
            ;; Tie breaker for if error and alt is the same
            [(and (= alt-error-sum current-alt-error)
                  (= current-alt-idx best-alt-idx)
                  (> current-prev-idx prev-split-idx))
             #t]
            [else #f]))
        (when set-cond
          (set! current-alt-error alt-error-sum)
          (set! current-alt-idx best-alt-idx)
          (set! current-prev-idx prev-split-idx)))
      (flvector-set! result-error-sums point-idx current-alt-error)
      (vector-set! result-alt-idxs point-idx current-alt-idx)
      (vector-set! result-prev-idxs point-idx current-prev-idx))

    ;; Loop over results vectors in reverse and build the output split index list
    (let loop ([i (- number-of-points 1)]
               [rest (ann null (Listof si))])
      (define alt-idx (vector-ref result-alt-idxs i))
      (define next (vector-ref result-prev-idxs i))
      (define sis (cons (si alt-idx (+ i 1)) rest))
      (if (< next i)
          (loop next sis)
          sis))))

(require (submod "." core))
