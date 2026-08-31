#lang racket

(require math/flonum
         "../config.rkt"
         "../syntax/block.rkt"
         "../syntax/float.rkt"
         "../syntax/types.rkt"
         "compiler.rkt"
         "points.rkt"
         "programs.rkt")

(provide branch-candidates
         v-values*)

(struct candidate (score v order vals))

;; Cheap heuristic score how well one split along this point order separates the alts.
(define (branch-separability err-cols order)
  (define n (vector-length order))
  (define best-prefix (make-flvector (add1 n) +inf.0))
  (define best-suffix (make-flvector (add1 n) +inf.0))
  (define acc (make-flvector (add1 n) 0.0))
  (for ([err-col (in-list err-cols)])
    ;; Accumulate errors for this alt.
    (for ([k (in-range n)])
      (define err (flvector-ref err-col (vector-ref order k)))
      (flvector-set! acc (add1 k) (+ (flvector-ref acc k) err)))
    (define total (flvector-ref acc n))
    ;; Record best prefixes and suffixes for every point.
    (for ([k (in-range (add1 n))])
      (define prefix (flvector-ref acc k))
      (define suffix (- total prefix))
      (when (< prefix (flvector-ref best-prefix k))
        (flvector-set! best-prefix k prefix))
      (when (< suffix (flvector-ref best-suffix k))
        (flvector-set! best-suffix k suffix))))
  (for/fold ([best +inf.0])
            ([prefix (in-flvector best-prefix)]
             [suffix (in-flvector best-suffix)])
    (min best (+ prefix suffix))))

;; Chooses the branch expressions for the regimes DP; the keep list always stays.
;; dp-score gives a candidate's true regimes DP error, for re-ranking the shortlist.
(define (branch-candidates block roots err-cols pcontext keep dp-score)
  (define free-vars (block-free-vars block))
  (define (usable? v)
    (and (equal? (representation-type (block-repr-of v)) 'real) (not (set-empty? (free-vars v)))))
  (define kept (filter usable? keep))
  (define kept-idxs (map val-idx kept))
  ;; All possible subexpressions across all alts and the original program.
  (define pool
    (for/list ([v (in-list (block-reachable block roots))]
               #:when (usable? v)
               #:unless (memv (val-idx v) kept-idxs))
      v))
  ;; Expressions that sort the points identically are cached.
  (define score-cache (make-hash))
  (define scored
    (for/list ([v (in-list pool)]
               [v-vals-vec (in-list (v-values* block pool pcontext))])
      (define repr (block-repr-of v))
      (define order
        (vector-sort (build-vector (vector-length v-vals-vec) values)
                     (lambda (i j)
                       (</total (vector-ref v-vals-vec i) (vector-ref v-vals-vec j) repr))))
      (define score (hash-ref! score-cache order (lambda () (branch-separability err-cols order))))
      (candidate score v order v-vals-vec)))
  (define ranked (sort scored < #:key candidate-score))
  (define shortlist (take ranked (min (*branch-shortlist*) (length ranked))))
  (define-values (picks outsiders)
    (split-at shortlist (min (*branch-expr-limit*) (length shortlist))))
  ;; The one-split score misses candidates that only pay off with several splits,
  ;; so an outsider may displace a pick, but only on a clear win over all of them.
  (define dp-cache (make-hash))
  (define (dp-of c)
    (hash-ref! dp-cache
               (candidate-order c)
               (lambda () (dp-score (candidate-v c) (candidate-vals c)))))
  (define final
    (for/fold ([picks picks]) ([out (in-list (sort outsiders < #:key dp-of))])
      (define best (apply min (map dp-of picks)))
      (if (< (dp-of out) (- best (*branch-dp-margin*)))
          (cons out (remq (argmax dp-of picks) picks))
          picks)))
  (append kept (map candidate-v final)))

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
