#lang racket

;; This module selects the expressions that regimes can branch on.
;; Dominance is necessary only for binary-search refinement, not for a branch itself.

(require math/flonum
         "../config.rkt"
         "../core/alternative.rkt"
         "../syntax/block.rkt"
         "../syntax/float.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "compiler.rkt"
         "points.rkt"
         "programs.rkt")

(provide branch-candidates
         v-values*)

;; Scores how well one split along this expression separates the alts; lower is better.
(define (branch-separability err-cols vals repr)
  (define n (vector-length vals))
  (define order
    (vector-sort (build-vector n values)
                 (lambda (i j) (</total (vector-ref vals i) (vector-ref vals j) repr))))

  (define best-prefix (make-flvector (add1 n) +inf.0))
  (define best-suffix (make-flvector (add1 n) +inf.0))
  (define acc (make-flvector (add1 n) 0.0))
  (for ([err-col (in-list err-cols)])
    (for ([k (in-range n)])
      (flvector-set! acc (add1 k) (+ (flvector-ref acc k) (flvector-ref err-col (vector-ref order k)))))
    (define total (flvector-ref acc n))
    (for ([k (in-range (add1 n))])
      (define p (flvector-ref acc k))
      (when (< p (flvector-ref best-prefix k))
        (flvector-set! best-prefix k p))
      (define s (- total p))
      (when (< s (flvector-ref best-suffix k))
        (flvector-set! best-suffix k s))))

  ;; A split is legal only where the branch value changes; k = 0 and k = n are no-splits.
  (for/fold ([best +inf.0]) ([k (in-range (add1 n))])
    (define splittable?
      (or (= k 0)
          (= k n)
          (</total (vector-ref vals (vector-ref order (sub1 k)))
                   (vector-ref vals (vector-ref order k))
                   repr)))
    (if splittable?
        (min best (+ (flvector-ref best-prefix k) (flvector-ref best-suffix k)))
        best)))

;; Evenly spaced indices that cover n, at most want of them.
(define (stride-indices n want)
  (define step (max 1 (quotient n (max 1 want))))
  (for/list ([i (in-range 0 n step)])
    i))

;; Chooses the branch expressions for the regimes DP; the keep list always stays.
(define (branch-candidates block alts start-prog err-cols pcontext keep)
  (define free-vars (block-free-vars block))
  ;; A usable branch expression has an ordered real value that varies with the input.
  (define (usable? v)
    (and (equal? (representation-type (block-repr-of v)) 'real)
         (not (literal? (val-def v)))
         (not (set-empty? (free-vars v)))))

  (define kept (filter usable? (remove-duplicates keep #:key val-idx)))
  (define kept-idxs (for/seteqv ([v (in-list kept)]) (val-idx v)))

  (define pool
    (for/list ([v (in-list (block-reachable block
                                            (cons start-prog (map alt-expr alts))
                                            #:condition node-is-impl?))]
               #:when (usable? v)
               #:unless (set-member? kept-idxs (val-idx v)))
      v))

  (cond
    [(null? pool) kept]
    [else
     ;; The pool can hold thousands of expressions, so ranking, not the dynamic program,
     ;; is what costs the time on a large program: one candidate costs a pass over every
     ;; alt's error column. Score everything exactly when that fits in the budget, and
     ;; subsample only when it does not -- always ranking every candidate against the same
     ;; points and alts, so the comparison stays fair. Ranking on a fixed subsample instead
     ;; measured worse on both probes, so the fidelity is worth keeping where it is free.
     ;; Only the ranking degrades; the DP still sees every point and every alt.
     (define n-pts (pcontext-length pcontext))
     (define n-alts (length err-cols))
     (define work (* (length pool) n-alts n-pts))
     (define budget (*branch-score-budget*))
     (define scale
       (if (<= work budget)
           1.0
           (sqrt (/ (exact->inexact budget) work))))
     ;; Below these floors the ranking is noise, so they outrank the budget.
     (define pt-idxs (stride-indices n-pts (max 32 (inexact->exact (floor (* scale n-pts))))))
     (define alt-idxs (stride-indices n-alts (max 8 (inexact->exact (floor (* scale n-alts))))))
     (define err-cols*
       (for/list ([col (in-list err-cols)]
                  [i (in-naturals)]
                  #:when (memv i alt-idxs))
         (for/flvector #:length (length pt-idxs) ([p (in-list pt-idxs)]) (flvector-ref col p))))

     (define vals (v-values* block pool pcontext))
     (define scored
       (for/list ([v (in-list pool)]
                  [vs (in-list vals)])
         (define vs* (for/vector #:length (length pt-idxs) ([p (in-list pt-idxs)]) (vector-ref vs p)))
         (cons (branch-separability err-cols* vs* (block-repr-of v)) v)))
     ;; The sort is stable, so equal scores keep block order and the choice is deterministic.
     (define ranked (sort scored < #:key car))
     (append kept (map cdr (take ranked (min (*branch-expr-limit*) (length ranked)))))]))

;; Values of each expression in vs at every point, as one vector per expression.
(define (v-values* block vs pcontext)
  (define count (length vs))
  (define fn (compile-block block vs))
  (define num-points (pcontext-length pcontext))
  (define out (build-vector count (lambda (_) (make-vector num-points))))
  (for ([pt (in-vector (pcontext-points pcontext))]
        [p (in-naturals)])
    (for ([o (in-vector (fn pt))]
          [i (in-naturals)])
      (vector-set! (vector-ref out i) p o)))
  (vector->list out))
