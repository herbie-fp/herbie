#lang racket

(require "core/egg-herbie.rkt" "core/simplify.rkt"
         "syntax/syntax.rkt" "syntax/types.rkt" "common.rkt" "programs.rkt"
         "points.rkt" "float.rkt")

(provide find-preprocessing preprocess-pcontext)

;; See https://pavpanchekha.com/blog/symmetric-expressions.html
(define (find-preprocessing expression context rules)
  ;; Here `*` means a test identity that *may* be equal to `expression`, and
  ;; `~` means the simplest form of an expression.
  (define variables (context-vars context))
  (define variable-representations (context-var-reprs context))
  (define pairs (combinations variables 2))
  (define swaps*
    (for/list ([pair (in-list pairs)])
      (match-define (list a b) pair)
      (replace-vars (list (cons a b) (cons b a)) expression)))
  (define evens*
    (for/list ([variable (in-list variables)]
               [representation (in-list variable-representations)])
      ;; TODO: Handle case where neg isn't supported for this representation
      (define negate (get-parametric-operator 'neg representation))
      (replace-vars (list (cons variable (list negate variable))) expression)))
  ;; TODO: If egraph detects unsoundness, abort preprocessing. Would need
  ;; explicit access to egraph constructed by run-egg though
  (define query (make-egg-query (cons expression (append swaps* evens*)) rules))
  (match-define (cons expression~ rest~) (map last (simplify-batch query)))
  (define-values (swaps~ evens~) (split-at rest~ (length swaps*)))
  (define abs-instructions
    (for/list ([variable (in-list variables)] [even~ (in-list evens~)]
               #:when (equal? expression~ even~))
      (list 'abs variable)))
  (define swaps (filter-map
                 (lambda (pair swap~) (and (equal? expression~ swap~) pair))
                 pairs
                 swaps~))
  (define sort-instructions
    (for/list ([component (connected-components variables swaps)]
               #:when (> (length component) 1))
      (cons 'sort component)))
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
    (apply compose (map (curry instruction->operator context) preprocessing)))
  (pcontext-map preprocess pcontext))

(define (instruction->operator context instruction)
  (define variables (context-vars context))
  (define representation (context-repr context))
  (define sort* (curryr sort (curryr </total representation)))
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
     (define abs (operator-info (get-parametric-operator
                                 'fabs
                                 (list-ref (context-var-reprs context) index))
                                'fl))
     (curryr list-update index abs)]))
