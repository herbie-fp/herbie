#lang racket

(require "core/egg-herbie.rkt" "core/simplify.rkt"
         "syntax/syntax.rkt" "syntax/types.rkt" "alternative.rkt" "common.rkt"
         "programs.rkt" "points.rkt" "timeline.rkt" "float.rkt")

(provide find-preprocessing preprocess-pcontext preprocessing-<=? remove-unnecessary-preprocessing)

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
  ;; TODO: If egraph detects unsoundness, abort preprocessing. Would need
  ;; explicit access to egraph constructed by run-egg though
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
    [(list 'sort component ...) #:when (equal? component variables)
     sort*]
    [(list 'sort component ...) #:when (list-prefix? component variables)
     (define position (length component))
     (lambda (points)
       (let-values ([(prefix suffix) (split-at points position)])
         (append (sort* prefix) suffix)))]
    [(list 'sort component ...) #:when (list-suffix? component variables)
     (define position (- (length variables) (length component)))
     (lambda (points)
       (let-values ([(prefix suffix) (split-at points position)])
         (append prefix (sort* suffix))))]
    [(list 'sort component ...)
     (unless (subsequence? component variables)
       (error 'instruction->operator
              "component should always be a subsequence of variables"))
     (define indices (indexes-where variables (curryr member component)))
     (lambda (points)
       (let* ([subsequence (list-ref* points indices)]
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

(define (preprocessing-<=? alt pcontext preprocessing1 preprocessing2 context)
  (define pcontext1 (preprocess-pcontext context pcontext preprocessing1))
  (define pcontext2 (preprocess-pcontext context pcontext preprocessing2))
  (<= (errors-score (errors (alt-expr alt) pcontext1 context))
      (errors-score (errors (alt-expr alt) pcontext2 context))))

; until fixed point, iterate through preprocessing attempting to drop preprocessing with no effect on error
(define (remove-unnecessary-preprocessing alt context pcontext preprocessing #:removed [removed empty])
  (define-values (result newly-removed)
    (let loop ([preprocessing preprocessing] [i 0] [removed removed])
      (cond
        [(>= i (length preprocessing))
         (values preprocessing removed)]
        [(preprocessing-<=? alt pcontext (drop-at preprocessing i) preprocessing context)
         (loop (drop-at preprocessing i) i (cons (list-ref preprocessing i) removed))]
        [else
         (loop preprocessing (+ i 1) removed)])))
  (cond
    [(< (length result) (length preprocessing))
     (remove-unnecessary-preprocessing alt context pcontext result #:removed newly-removed)]
    [else
     (timeline-push! 'remove-preprocessing (map ~a newly-removed))
     result]))
