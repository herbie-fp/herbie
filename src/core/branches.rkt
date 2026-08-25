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

;; Sorts the points by this expression and groups equal values.
;; Returns the sort order, the legal split positions, and a hashable key.
;; Two expressions with the same key always get the same separability score.
(define (ordering-key vals repr)
  (define n (vector-length vals))
  (define order
    (vector-sort (build-vector n values)
                 (lambda (i j) (</total (vector-ref vals i) (vector-ref vals j) repr))))
  (define can-split (make-vector (add1 n) #t))
  (define key (make-vector n 0))
  (for ([k (in-range 1 n)])
    (define new-group?
      (</total (vector-ref vals (vector-ref order (sub1 k)))
               (vector-ref vals (vector-ref order k))
               repr))
    (unless new-group?
      (vector-set! can-split k #f))
    (vector-set! key
                 (vector-ref order k)
                 (+ (vector-ref key (vector-ref order (sub1 k)))
                    (if new-group? 1 0))))
  (values order can-split (vector->immutable-vector key)))

;; Scores how well one split along this point order separates the alts; lower is better.
(define (branch-separability err-cols order can-split)
  (define n (vector-length order))
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
    (if (vector-ref can-split k)
        (min best (+ (flvector-ref best-prefix k) (flvector-ref best-suffix k)))
        best)))

;; Chooses the branch expressions for the regimes DP; the keep list always stays.
;; dp-score gives a candidate's true regimes DP error, for re-ranking the shortlist.
(define (branch-candidates block alts start-prog err-cols pcontext keep dp-score)
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
     ;; The score depends only on how a candidate orders the points, so all
     ;; candidates that sort the points the same way share one computation.
     (define vals (v-values* block pool pcontext))
     (define score-cache (make-hash))
     (define scored
       (for/list ([v (in-list pool)]
                  [vs (in-list vals)])
         (define-values (order can-split key) (ordering-key vs (block-repr-of v)))
         (define score
           (hash-ref! score-cache key (lambda () (branch-separability err-cols order can-split))))
         (list score v key vs)))
     ;; The sort is stable, so equal scores keep block order and the choice is deterministic.
     (define ranked (sort scored < #:key first))
     (define picks (take ranked (min (*branch-expr-limit*) (length ranked))))
     (define outsiders
       (drop (take ranked (min (*branch-shortlist*) (length ranked))) (length picks)))
     ;; The one-split score cannot see candidates that only pay off with several
     ;; splits, so re-rank the shortlist with the true DP error. An outsider must
     ;; win by a clear margin before it displaces a pick; small wins are noise.
     (define dp-cache (make-hash))
     (define (dp-of entry)
       (hash-ref! dp-cache (third entry) (lambda () (dp-score (second entry) (fourth entry)))))
     (define final
       (for/fold ([picks picks]) ([out (in-list (sort outsiders < #:key dp-of))])
         (cond
           [(< (dp-of out) (- (apply min (map dp-of picks)) (*branch-dp-margin*)))
            (define worst (argmax dp-of picks))
            (for/list ([p (in-list picks)])
              (if (eq? p worst) out p))]
           [else picks])))
     (append kept (map second final))]))

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
