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
     ;; Score every candidate against every alt on every point. No subsampling.
     (define vals (v-values* block pool pcontext))
     (define scored
       (for/list ([v (in-list pool)]
                  [vs (in-list vals)])
         (cons (branch-separability err-cols vs (block-repr-of v)) v)))
     ;; The sort is stable, so equal scores keep block order and the choice is deterministic.
     (define ranked (sort scored < #:key car))
     (append kept (map cdr (take ranked (min (*branch-expr-limit*) (length ranked)))))]))

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
