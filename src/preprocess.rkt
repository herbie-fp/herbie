#lang racket

(require "core/egg-herbie.rkt" "core/simplify.rkt"
         "syntax/syntax.rkt" "syntax/types.rkt" "alternative.rkt" "common.rkt"
         "programs.rkt" "points.rkt" "timeline.rkt" "float.rkt")

(provide find-preprocessing preprocess-pcontext remove-unnecessary-preprocessing)

;; See https://pavpanchekha.com/blog/symmetric-expressions.html
(define (find-preprocessing expression context rules)
  ;; Here `*` means a test identity that *may* be equal to `expression`, and
  ;; `~` means the simplest form of an expression.
  (define variables (context-vars context))
  (define variable-representations (context-var-reprs context))
  (define evens*
    (for/list ([variable (in-list variables)]
               [representation (in-list variable-representations)])
      ;; TODO: Handle case where neg isn't supported for this representation
      (define negate (get-parametric-operator 'neg representation))
      (replace-vars (list (cons variable (list negate variable))) expression)))
  (define pairs (combinations variables 2))
  (define swaps*
    (for/list ([pair (in-list pairs)])
      (match-define (list a b) pair)
      (replace-vars (list (cons a b) (cons b a)) expression)))
  (define query (make-egg-query (cons expression (append evens* swaps*)) rules))
  (match-define (cons expression~ rest~) (map last (simplify-batch query)))
  (define-values (evens~ swaps~) (split-at rest~ (length evens*)))
  (define swaps (filter-map
                 (lambda (pair swap~) (and (equal? expression~ swap~) pair))
                 pairs
                 swaps~))
  (define components (connected-components variables swaps))
  (define abs-instructions
    (for/list ([variable (in-list variables)] [even~ (in-list evens~)]
               #:when (equal? expression~ even~))
      (list 'abs variable)))
  (define sort-instructions
    (for/list ([component (in-list components)]
               #:when (> (length component) 1))
      (cons 'sort component)))
  ;; Absolute value should happen before sorting
  (append abs-instructions sort-instructions))

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
; (remove-unnecessary-preprocessing (alt-expr best) context pcontext preprocessing))
; (remove-unnecessary-preprocessing (alt-expr curr) context pcontext)
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
