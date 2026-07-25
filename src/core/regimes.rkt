#lang typed/racket

;;;; Module principles
;; - The core of this file is infer-option-prefixes.
;;   It is a giant dynamic programming algorithm.
;;   It is extremely performance-sensitive.
;; - Therefore almost everything is vector-based with few copies.
;;   Except critical-subexpressions. Converting it to vectors makes it slow.
;; - Everything else is overhead and should be minimized.

(require math/flonum
         typed/json
         typed/racket/unsafe)

(define-type RepresentationSpec (U Symbol (List 'array RepresentationSpec Positive-Integer)))
(define-type RepresentationValue (U Real Boolean (Vectorof RepresentationValue)))

(require/typed "../utils/common.rkt" [flag-set? (-> Symbol Symbol Boolean)])
(unsafe-require/typed "../utils/pareto.rkt"
                      [#:struct pareto-point ([cost : Integer] [error : Flonum] [data : Any])]
                      [pareto-union
                       (->* ((Listof pareto-point) (Listof pareto-point))
                            (#:combine (-> Any Any Any))
                            (Listof pareto-point))])
(require/typed "../utils/timeline.rkt"
               [timeline-event! (-> Symbol Void)]
               [timeline-push! (->* (Symbol) #:rest JSExpr Void)]
               [timeline-start! (->* (Symbol) #:rest JSExpr (-> Void))])
(require/typed
 "../syntax/types.rkt"
 [#:opaque Representation representation?]
 [representation-type (-> Representation RepresentationSpec)]
 [representation-name (-> Representation RepresentationSpec)]
 [<binary64> Representation]
 [make-array-representation (->* (#:elem Representation #:len Positive-Integer) () Representation)]
 [#:struct context
  ([vars : (Listof Symbol)] [repr : Representation] [var-reprs : (Listof Representation)])])
(require/typed "../syntax/float.rkt"
               [</total (-> RepresentationValue RepresentationValue Representation Boolean)])
(require/typed "../syntax/syntax.rkt"
               [#:struct literal ([value : Any] [precision : Any])]
               [#:struct approx ([spec : Any] [impl : Any])])
(require/typed "../syntax/batch.rkt"
               [#:opaque Batch batch?]
               [#:struct batchref ([batch : Batch] [idx : Index])])
(define-type Expr (U Symbol Boolean Real literal approx batchref (Listof Expr)))
(unsafe-require/typed "../syntax/batch.rkt" [batch-free-vars (-> Batch (-> batchref (Setof Expr)))])
(unsafe-require/typed racket/set [in-set (-> (Setof Expr) (Sequenceof Expr))])
(require/typed "../syntax/batch.rkt"
               [batch-add! (-> Batch Expr batchref)]
               [batch-length (-> Batch Index)]
               [batch-vars (-> Batch (Listof Symbol))]
               [batch-reachable/impl (-> Batch (Listof batchref) (Listof batchref))]
               [deref (-> batchref Expr)]
               [expr-recurse-impl (-> Expr (-> batchref Any) Any)]
               [progs->batch (->* ((Listof Expr) #:ctx context) () (Values Batch (Listof batchref)))]
               [batch->jsexpr (-> Batch (Listof Expr) (Immutable-HashTable Symbol JSExpr))])
(require/typed
 "compiler.rkt"
 [compile-batch
  (-> Batch (Listof batchref) (-> (Vectorof RepresentationValue) (Vectorof RepresentationValue)))])
(require/typed "points.rkt"
               [#:opaque PContext pcontext?]
               [pcontext-points (-> PContext (Vectorof (Vectorof RepresentationValue)))]
               [pcontext-length (-> PContext Index)]
               [mk-pcontext
                (-> (Listof (Vectorof RepresentationValue)) (Listof RepresentationValue) PContext)]
               [batch-errors (-> Batch (Listof Expr) PContext (Listof FlVector))]
               [errors-score (-> FlVector Flonum)])
(require/typed "programs.rkt"
               [batch-repr-of (-> batchref Representation)]
               [free-variables (-> Expr (Listof Symbol))])
(require/typed "../syntax/load-platform.rkt" [activate-platform! (-> String Void)])
(require/typed "../core/alternative.rkt"
               [#:opaque Alt alt?]
               [make-alt (-> Expr Alt)]
               [alt-expr (-> Alt Expr)])
(provide pareto-regimes
         (struct-out option)
         (struct-out si)
         critical-subexpression?)

(module+ test
  (require typed/rackunit)

  (define (check-critical [expr : Expr] [subexpr : Expr])
    (define ctx
      (context (free-variables expr)
               <binary64>
               (make-list (length (free-variables expr)) <binary64>)))
    (define-values (batch brfs) (progs->batch (list expr) #:ctx ctx))
    (critical-subexpression? batch (first brfs) (batch-add! batch subexpr))))

(struct si ([cidx : Integer] [pidx : Integer]) #:prefab)

(struct option
        ([split-indices : (Listof si)] [alts : (Listof Alt)]
                                       [pts : (Listof (Vectorof RepresentationValue))]
                                       [expr : batchref])
  #:transparent
  #:property prop:custom-write
  (lambda ([opt : option] [port : Output-Port] [mode : Any])
    (fprintf port "#<option ~a>" (option-split-indices opt))))

(: option-exprs (-> option (Listof Expr)))
(define (option-exprs opt)
  (for/list ([sidx (in-list (option-split-indices opt))])
    (alt-expr (list-ref (option-alts opt) (si-cidx sidx)))))

(: pareto-union-keep-old (-> (Listof pareto-point) (Listof pareto-point) (Listof pareto-point)))
(define (pareto-union-keep-old old new)
  (pareto-union old new #:combine (lambda (old-data _new-data) old-data)))

;; CONSIDER: move start-prog and the "branch-brfs" computation into caller.
(: pareto-regimes (-> Batch (Listof Alt) batchref PContext (Listof option)))
(define (pareto-regimes batch sorted start-prog pcontext)
  (timeline-event! 'regimes)
  (define alts-vec (list->vector sorted))
  (define alt-count (vector-length alts-vec))
  (define err-cols (batch-errors batch (map alt-expr sorted) pcontext))
  (: real-brf? (-> batchref Boolean))
  (define (real-brf? brf)
    (equal? (representation-type (batch-repr-of brf)) 'real))
  (define branch-brfs
    (filter real-brf?
            (if (flag-set? 'reduce 'branch-expressions)
                (critical-subexpressions batch start-prog)
                (map (lambda ([var : Symbol]) (batch-add! batch var)) (batch-vars batch)))))

  (define brf-vals (brf-values* batch branch-brfs pcontext))
  (define pts-vec (pcontext-points pcontext))

  ;; For timeline
  (: batch-jsexpr (Immutable-HashTable Symbol JSExpr))
  (define batch-jsexpr (batch->jsexpr batch (append (map alt-expr sorted) branch-brfs)))
  (timeline-push! 'batch batch-jsexpr)
  (: branch-roots (Listof JSExpr))
  (define branch-roots
    (match (hash-ref batch-jsexpr 'roots)
      [(list roots ...) (drop roots alt-count)]
      [_ (error 'pareto-regimes "batch JSON roots are not a list")]))
  (: branch-root-map (Immutable-HashTable batchref JSExpr))
  (define branch-root-map
    (make-immutable-hash
     (map (lambda ([brf : batchref] [root : JSExpr]) (cons brf root)) branch-brfs branch-roots)))

  (: option-curves (Listof (Listof pareto-point)))
  (define option-curves
    (for/list ([brf (in-list branch-brfs)]
               [brf-vals-vec (in-list brf-vals)])
      (define timeline-stop! (timeline-start! 'times (batch->jsexpr batch (list brf))))
      (define repr (batch-repr-of brf))
      (define curve (branch-options batch alts-vec err-cols pts-vec brf brf-vals-vec repr))
      (define last-point (last curve))
      (define last-opt (assert (pareto-point-data last-point) option?))
      (timeline-stop!)
      (timeline-push! 'branch
                      (hash-ref branch-root-map brf)
                      (- (pareto-point-error last-point) (length (option-split-indices last-opt)))
                      (length (option-split-indices last-opt))
                      (~a (representation-name repr)))
      curve))
  (: combined-option-curve (Listof pareto-point))
  (define combined-option-curve
    (for/fold ([curve '()]) ([branch-curve (in-list option-curves)])
      (pareto-union-keep-old curve branch-curve)))

  ;; Timeline
  (timeline-push! 'inputs (batch->jsexpr batch (map alt-expr sorted)))
  (timeline-push! 'outputs
                  (batch->jsexpr batch
                                 (remove-duplicates
                                  (append-map (lambda ([ppt : pareto-point])
                                                (define opt (assert (pareto-point-data ppt) option?))
                                                (option-exprs opt))
                                              combined-option-curve))))
  (for/list ([ppt (in-list combined-option-curve)])
    (define opt (assert (pareto-point-data ppt) option?))
    (timeline-push! 'count (length (option-alts opt)) (length (option-split-indices opt)))
    (timeline-push! 'accuracy
                    (- (pareto-point-error ppt) (length (option-split-indices opt)))
                    (oracle-errors-score err-cols (pareto-point-cost ppt))
                    (baseline-errors-score err-cols (pareto-point-cost ppt)))
    opt))

(: critical-subexpression? (-> Batch batchref batchref Boolean))
(define (critical-subexpression? batch root-brf sub-brf)
  (set-member? (critical-subexpressions batch root-brf) sub-brf))

(: critical-subexpressions (-> Batch batchref (Listof batchref)))
(define (critical-subexpressions batch root-brf)
  (define var-brfs (map (lambda ([var : Symbol]) (batch-add! batch var)) (batch-vars batch)))
  (define free-vars (batch-free-vars batch))
  (define dom-parent (build-dominator-tree batch root-brf))
  (: dominates? (-> batchref batchref Boolean))
  (define (dominates? parent-brf child-brf)
    (cond
      [(equal? parent-brf child-brf) #t]
      [(equal? child-brf root-brf) #f]
      [else (dominates? parent-brf (assert (dom-parent child-brf) batchref?))]))
  (: extractable? (-> batchref Boolean))
  (define (extractable? brf)
    (for/and ([var (in-set (free-vars brf))])
      (dominates? brf (batch-add! batch var))))
  (: seen-brfs (Mutable-HashTable batchref Boolean))
  (define seen-brfs (make-hash (list (cons root-brf #t))))
  (: result (Listof batchref))
  (define result (list root-brf))
  (for ([brf (in-list var-brfs)])
    (when (dom-parent brf)
      (let loop ([brf brf])
        (unless (hash-has-key? seen-brfs brf)
          (hash-set! seen-brfs brf #t)
          (when (extractable? brf)
            (set! result (cons brf result)))
          (loop (assert (dom-parent brf) batchref?))))))
  (reverse result))

(: build-dominator-tree (-> Batch batchref (-> batchref (U False batchref))))
(define (build-dominator-tree batch root-brf)
  (define reachable-brfs (reverse (batch-reachable/impl batch (list root-brf))))
  (: dom-parents (Vectorof (U False batchref)))
  (define dom-parents (make-vector (batch-length batch) #f))
  (: dom-parent (-> batchref (U False batchref)))
  (define (dom-parent brf)
    (vector-ref dom-parents (batchref-idx brf)))
  (: update-child! (-> batchref batchref Void))
  (define (update-child! brf child-brf)
    (define old-parent (dom-parent child-brf))
    (define new-parent
      (if old-parent
          (dominator-lca brf (assert old-parent batchref?) dom-parent)
          brf))
    (vector-set! dom-parents (batchref-idx child-brf) new-parent))
  (vector-set! dom-parents (batchref-idx root-brf) root-brf)
  (for ([brf (in-list reachable-brfs)])
    (expr-recurse-impl (deref brf) (lambda (child) (update-child! brf (assert child batchref?)))))
  dom-parent)

(: dominator-lca (-> batchref batchref (-> batchref (U False batchref)) batchref))
(define (dominator-lca brf1 brf2 dom-parent)
  (let loop ([brf1 brf1]
             [brf2 brf2])
    (define idx1 (batchref-idx brf1))
    (define idx2 (batchref-idx brf2))
    (cond
      [(= idx1 idx2) brf1]
      [(< idx1 idx2) (loop (assert (dom-parent brf1) batchref?) brf2)]
      [else (loop brf1 (assert (dom-parent brf2) batchref?))])))

(: baseline-errors-score (-> (Listof FlVector) Integer Flonum))
(define (baseline-errors-score err-cols count)
  (for/fold ([best (ann +inf.0 Flonum)]) ([err-col (in-list (take err-cols count))])
    (min best (errors-score err-col))))

(: oracle-errors-score (-> (Listof FlVector) Integer Flonum))
(define (oracle-errors-score err-cols count)
  (define num-points (flvector-length (first err-cols)))
  (/ (foldl (lambda ([point-idx : Integer] [total : Flonum])
              (+ total (oracle-point-error err-cols count point-idx)))
            0.0
            (range num-points))
     num-points))

(: oracle-point-error (-> (Listof FlVector) Integer Integer Flonum))
(define (oracle-point-error err-cols count point-idx)
  (foldl (lambda ([err-col : FlVector] [best-err : Flonum])
           (min best-err (flvector-ref err-col point-idx)))
         +inf.0
         (take err-cols count)))
(: brf-values* (-> Batch (Listof batchref) PContext (Listof (Vectorof RepresentationValue))))
(define (brf-values* batch brfs pcontext)
  (define count (length brfs))
  (define fn (compile-batch batch brfs))
  (define num-points (pcontext-length pcontext))
  (: vals (Vectorof (Vectorof RepresentationValue)))
  (define vals
    (build-vector count
                  (lambda ([_ : Index])
                    (ann (make-vector num-points #f) (Vectorof RepresentationValue)))))
  (for ([pt (in-vector (pcontext-points pcontext))]
        [p (in-naturals)])
    (for ([out (in-vector (fn pt))]
          [i (in-naturals)])
      (vector-set! (vector-ref vals i) p out)))
  (vector->list vals))

(: branch-options
   (-> Batch
       (Vectorof Alt)
       (Listof FlVector)
       (Vectorof (Vectorof RepresentationValue))
       batchref
       (Vectorof RepresentationValue)
       Representation
       (Listof pareto-point)))
(define (branch-options batch alts-vec err-cols pts-vec brf brf-vals-vec repr)
  (: sorted-indices (Vectorof Integer))
  (define sorted-indices
    (vector-sort (build-vector (ann (vector-length brf-vals-vec) Integer) (lambda ([i : Index]) i))
                 (lambda ([i : Integer] [j : Integer])
                   (</total (vector-ref brf-vals-vec i) (vector-ref brf-vals-vec j) repr))))
  (: pts* (Listof (Vectorof RepresentationValue)))
  (define pts*
    (for/list ([i (in-vector sorted-indices)])
      (vector-ref pts-vec i)))
  (: can-split? (Listof Boolean))
  (define can-split?
    (cons #f
          (map (lambda ([idx : Integer] [prev-idx : Integer])
                 (</total (vector-ref brf-vals-vec prev-idx) (vector-ref brf-vals-vec idx) repr))
               (cdr (vector->list sorted-indices))
               (take (vector->list sorted-indices) (sub1 (vector-length sorted-indices))))))

  (define-values (splitss scores) (infer-option-prefixes err-cols sorted-indices can-split?))

  (: points (Listof pareto-point))
  (define points
    (for/list ([count (in-range 1 (add1 (vector-length splitss)))])
      (define split-indices (vector-ref splitss (sub1 count)))
      (define alts (vector->list (vector-take alts-vec count)))
      (define error (+ (/ (flvector-ref scores (sub1 count)) (vector-length sorted-indices)) 1))
      (pareto-point count error (option split-indices alts pts* brf))))
  (for/fold ([curve '()]) ([point (in-list points)])
    (pareto-union-keep-old curve (list point))))

(module+ test
  (activate-platform! "c")
  (define ctx (context '(x) <binary64> (list <binary64>)))
  (define pctx (mk-pcontext '(#(0.5) #(4.0)) '(1.0 1.0)))
  (define alts (map make-alt (list '(fmin.f64 x 1) '(fmax.f64 x 1))))
  (define err-cols (list (flvector 53.0 0.0) (flvector 0.0 53.0)))
  (define pts-vec (pcontext-points pctx))

  (define (test-regimes [expr : Expr] [goal : Any])
    (define-values (batch brfs) (progs->batch (list expr) #:ctx ctx))
    (define brf (car brfs))
    (define brf-vals (car (brf-values* batch (list brf) pctx)))
    (define opt
      (assert (pareto-point-data (first (branch-options batch
                                                        (list->vector alts)
                                                        err-cols
                                                        pts-vec
                                                        brf
                                                        brf-vals
                                                        (batch-repr-of brf))))
              option?))
    (check (lambda ([x : option] [y : Any])
             (equal? (map (lambda ([sidx : si]) (si-cidx sidx)) (option-split-indices x)) y))
           opt
           goal))

  (define (test-regimes/prefixes [expr : Expr] [goals : Any])
    (define-values (batch brfs) (progs->batch (list expr) #:ctx ctx))
    (define brf (car brfs))
    (define brf-vals (car (brf-values* batch (list brf) pctx)))
    (: options (Listof option))
    (define options
      (map (lambda ([ppt : pareto-point]) (assert (pareto-point-data ppt) option?))
           (reverse (branch-options batch
                                    (list->vector alts)
                                    err-cols
                                    pts-vec
                                    brf
                                    brf-vals
                                    (batch-repr-of brf)))))
    (for ([goal (in-list goals)]
          [opt (in-list options)])
      (check (lambda ([x : option] [y : Any])
               (equal? (map (lambda ([sidx : si]) (si-cidx sidx)) (option-split-indices x)) y))
             opt
             goal)))

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
    (define-values (batch brfs) (progs->batch (list 'x) #:ctx xy-ctx))
    (check-true (critical-subexpression? batch (first brfs) (batch-add! batch 'x)))
    (check-false (critical-subexpression? batch (first brfs) (batch-add! batch 'y))))

  (let ()
    (define xyz-ctx (context '(x y z) <binary64> (list <binary64> <binary64> <binary64>)))
    (define-values (batch brfs) (progs->batch (list '(* (+ x y) (/ x z))) #:ctx xyz-ctx))
    (check-false (critical-subexpression? batch (first brfs) (batch-add! batch '(+ x y)))))

  (let ()
    (define vec2-ctx
      (context '(a b)
               <binary64>
               (list (make-array-representation #:elem <binary64> #:len 2)
                     (make-array-representation #:elem <binary64> #:len 2))))
    (: dot-product Expr)
    (define dot-product
      (list
       '+.f64
       (list '*.f64 (list 'ref.f64 'a (literal 0 'binary64)) (list 'ref.f64 'b (literal 0 'binary64)))
       (list '*.f64
             (list 'ref.f64 'a (literal 1 'binary64))
             (list 'ref.f64 'b (literal 1 'binary64)))))
    (define-values (batch brfs) (progs->batch (list dot-product) #:ctx vec2-ctx))
    (check-true (set-member? (critical-subexpressions batch (first brfs)) (first brfs)))))

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
(:
 infer-option-prefixes
 (-> (Listof FlVector) (Vectorof Integer) (Listof Boolean) (Values (Vectorof (Listof si)) FlVector)))
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

  (: splitss (Vectorof (Listof si)))
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
  (values splitss scores))
