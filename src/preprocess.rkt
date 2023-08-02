#lang racket

(require "core/egg-herbie.rkt" "core/simplify.rkt"
         "syntax/syntax.rkt" "syntax/types.rkt" "alternative.rkt" "common.rkt"
         "programs.rkt" "points.rkt" "timeline.rkt" "float.rkt")

(provide find-preprocessing preprocess-pcontext remove-unnecessary-preprocessing)

;; See https://pavpanchekha.com/blog/symmetric-expressions.html
(define (find-preprocessing initial specification context rules
                            node-limit simplify?)
  (define egraph (make-egraph))
  (define specification-id (egraph-add-expr egraph specification))
  (define initial-id (and simplify? (egraph-add-expr egraph initial)))
  (define even-ids
    (for/list ([variable (in-list (context-vars context))]
               [representation (in-list (context-var-reprs context))])
      ;; TODO: Handle case where neg isn't supported for this representation
      (define negate (get-parametric-operator 'neg representation))
      (egraph-add-expr
       egraph
       (replace-vars (list (cons variable (list negate variable))) specification))))
  (define pairs (combinations (context-vars context) 2))
  (define swap-ids
    (for/list ([pair (in-list pairs)])
      (match-define (list a b) pair)
      (egraph-add-expr
       egraph
       (replace-vars (list (cons a b) (cons b a)) specification))))
  (match-define (list _ ffi-rules _) (expand-rules rules))
  (define iteration-data (egraph-run egraph node-limit ffi-rules #t))
  (define simplified
    (if simplify?
        (remove-duplicates
         (for/list ([iteration (in-range (length iteration-data))])
           (egraph-get-simplest egraph initial-id iteration)))
        (list initial)))
  (define (equivalent? id)
    (= (egraph-find egraph specification-id)
       (egraph-find egraph id)))
  (define abs-instructions
    (for/list ([variable (in-list (context-vars context))]
               [even-id (in-list even-ids)]
               #:when (equivalent? even-id))
      (list 'abs variable)))
  (define components
    (connected-components
     (context-vars context)
     (filter-map
      (lambda (id pair) (and (equivalent? id) pair))
      swap-ids
      pairs)))
  (define sort-instructions
    (for/list ([component (in-list components)]
               #:when (> (length component) 1))
      (cons 'sort component)))
  ;; Absolute value should happen before sorting
  (define instructions (append abs-instructions sort-instructions))
  (values simplified instructions))

(define (connected-components variables swaps)
  (define components (disjoint-set (length variables)))
  (for ([swap (in-list swaps)])
    (match-define (list a b) swap)
    (disjoint-set-union! components
                         (disjoint-set-find! components (index-of variables a))
                         (disjoint-set-find! components (index-of variables b))))
  (group-by
   (compose (curry disjoint-set-find! components) (curry index-of variables))
   variables))

(define (preprocess-pcontext context pcontext preprocessing)
  (define preprocess
    (apply compose (map
                    (curry instruction->operator context)
                    ;; Function composition applies the rightmost function first
                    (reverse preprocessing))))
  (pcontext-map preprocess pcontext))

(define (instruction->operator context instruction)
  (define variables (context-vars context))
  (define sort* (curryr sort (curryr </total (context-repr context))))
  (match instruction
    [(list 'sort component ...)
     (unless (subsequence? component variables)
       (error 'instruction->operator
              "component should always be a subsequence of variables"))
     (define indices (indexes-where variables (curryr member component)))
     (lambda (points)
       (let* ([subsequence (map (curry list-ref points) indices)]
              [sorted (sort* subsequence)])
         (list-set* points indices sorted)))]
    [(list 'abs variable)
     (define index (index-of variables variable))
     (define abs (operator-info
                  (get-parametric-operator
                   'fabs
                   (list-ref (context-var-reprs context) index))
                  'fl))
     (curryr list-update index abs)]))

; until fixed point, iterate through preprocessing attempting to drop preprocessing with no effect on error
(define (remove-unnecessary-preprocessing expression context pcontext preprocessing #:removed [removed empty])
  (define-values (result newly-removed)
    (let loop ([preprocessing preprocessing] [i 0] [removed removed])
      (cond
        [(>= i (length preprocessing))
         (values preprocessing removed)]
        [(preprocessing-<=? expression context pcontext (drop-at preprocessing i) preprocessing)
         (loop (drop-at preprocessing i) i (cons (list-ref preprocessing i) removed))]
        [else
         (loop preprocessing (+ i 1) removed)])))
  (cond
    [(< (length result) (length preprocessing))
     (remove-unnecessary-preprocessing expression context pcontext result #:removed newly-removed)]
    [else
     (timeline-push! 'remove-preprocessing (map ~a newly-removed))
     result]))

(define (preprocessing-<=? expression context pcontext preprocessing1 preprocessing2)
  (define pcontext1 (preprocess-pcontext context pcontext preprocessing1))
  (define pcontext2 (preprocess-pcontext context pcontext preprocessing2))
  (<= (errors-score (errors expression pcontext1 context))
      (errors-score (errors expression pcontext2 context))))
