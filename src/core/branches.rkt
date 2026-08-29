#lang racket

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

(struct candidate (score v key vals))

;; Sorts the points by this expression's value; expressions with the same key
;; sort the points the same way, so they share one separability score.
(define (ordering-key vals repr)
  (define n (vector-length vals))
  (define order
    (vector-sort (build-vector n values)
                 (lambda (i j) (</total (vector-ref vals i) (vector-ref vals j) repr))))
  (define can-split (make-vector (add1 n) #t))
  (define key (make-vector n 0))
  (for ([k (in-range 1 n)])
    (define prev (vector-ref order (sub1 k)))
    (define here (vector-ref order k))
    (define new-group? (</total (vector-ref vals prev) (vector-ref vals here) repr))
    (unless new-group?
      (vector-set! can-split k #f))
    (vector-set! key here (+ (vector-ref key prev) (if new-group? 1 0))))
  (values order can-split (vector->immutable-vector key)))

;; An expression and its negation order the points in reverse, inducing the same
;; splits, so a key and its mirror image share one canonical form: whichever of
;; the two is lexicographically least.
(define (canonical-key key)
  (define groups
    (for/fold ([m 0]) ([g (in-vector key)])
      (max m g)))
  (define (mirror g)
    (- groups g))
  (define flip?
    (for/first ([g (in-vector key)]
                #:unless (= g (mirror g)))
      (< (mirror g) g)))
  (if flip?
      (vector->immutable-vector (vector-map mirror key))
      key))

;; Scores how well one split along this point order separates the alts; lower is better.
;; can-split is #t at k = 0 and k = n, so the no-split baseline is always on offer.
(define (branch-separability err-cols order can-split)
  (define n (vector-length order))
  (define best-prefix (make-flvector (add1 n) +inf.0))
  (define best-suffix (make-flvector (add1 n) +inf.0))
  (define acc (make-flvector (add1 n) 0.0))
  (for ([err-col (in-list err-cols)])
    (for ([k (in-range n)])
      (define err (flvector-ref err-col (vector-ref order k)))
      (flvector-set! acc (add1 k) (+ (flvector-ref acc k) err)))
    (define total (flvector-ref acc n))
    (for ([k (in-range (add1 n))])
      (define prefix (flvector-ref acc k))
      (define suffix (- total prefix))
      (when (< prefix (flvector-ref best-prefix k))
        (flvector-set! best-prefix k prefix))
      (when (< suffix (flvector-ref best-suffix k))
        (flvector-set! best-suffix k suffix))))
  (for/fold ([best +inf.0])
            ([k (in-range (add1 n))]
             #:when (vector-ref can-split k))
    (min best (+ (flvector-ref best-prefix k) (flvector-ref best-suffix k)))))

;; Chooses the branch expressions for the regimes DP; the keep list always stays.
;; dp-score gives a candidate's true regimes DP error, for re-ranking the shortlist.
(define (branch-candidates block alts start-prog err-cols pcontext keep dp-score)
  (define free-vars (block-free-vars block))
  (define (usable? v)
    (and (equal? (representation-type (block-repr-of v)) 'real)
         (not (literal? (val-def v)))
         (not (set-empty? (free-vars v)))))
  (define kept (filter usable? (remove-duplicates keep #:key val-idx)))
  (define kept-idxs
    (for/seteqv ([v (in-list kept)])
      (val-idx v)))
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
     (define score-cache (make-hash))
     (define scored
       (for/list ([v (in-list pool)]
                  [vs (in-list (v-values* block pool pcontext))])
         (define-values (order can-split key) (ordering-key vs (block-repr-of v)))
         (define ckey (canonical-key key))
         (define score
           (hash-ref! score-cache ckey (lambda () (branch-separability err-cols order can-split))))
         (candidate score v ckey vs)))
     ;; Stable sort: equal scores keep block order, so results are deterministic.
     (define ranked (sort scored < #:key candidate-score))
     (define shortlist (take ranked (min (*branch-shortlist*) (length ranked))))
     (define-values (picks outsiders)
       (split-at shortlist (min (*branch-expr-limit*) (length shortlist))))
     ;; The one-split score misses candidates that only pay off with several splits,
     ;; so an outsider may displace a pick, but only on a clear win over all of them.
     (define dp-cache (make-hash))
     (define (dp-of c)
       (hash-ref! dp-cache
                  (candidate-key c)
                  (lambda () (dp-score (candidate-v c) (candidate-vals c)))))
     (define final
       (for/fold ([picks picks]) ([out (in-list (sort outsiders < #:key dp-of))])
         (define best (apply min (map dp-of picks)))
         (if (< (dp-of out) (- best (*branch-dp-margin*)))
             (cons out (remq (argmax dp-of picks) picks))
             picks)))
     (append kept (map candidate-v final))]))

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
