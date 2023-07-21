#lang racket

(require "core/egg-herbie.rkt" "core/simplify.rkt"
         "syntax/syntax.rkt" "syntax/types.rkt" "alternative.rkt" "common.rkt"
         "programs.rkt" "points.rkt" "timeline.rkt" "float.rkt")

(provide (struct-out preprocessing-instruction) add-preprocessing-tests
         find-preprocessing preprocess-pcontext remove-unnecessary-preprocessing)

;; See https://pavpanchekha.com/blog/symmetric-expressions.html

;; TODO: Remove
;; tests : Expression -> Context -> List (Expression, A)
;; instructions : List A -> Context -> Instruction
;; operator : Instruction -> (Point -> Point)
(struct preprocessing-step (tests instructions operator) #:transparent)

(struct preprocessing-instruction (type data) #:transparent)

(define (add-preprocessing-tests egraph expression context)
  (for/list ([step (in-dict-values preprocessing-steps)])
    (match-define (preprocessing-step tests _ _) step)
    (for/list ([test (in-list (tests expression context))])
      (match-define (cons expression* item) test)
      (cons (egraph-add-expr egraph expression*) item))))

(define (find-preprocessing egraph test-groups specification-id context)
  (define (equivalent? id)
    (= (egraph-find egraph specification-id)
       (egraph-find egraph id)))
  (apply
   append
   (for/list ([(type step) (in-dict preprocessing-steps)]
              [tests (in-list test-groups)])
     (match-define (preprocessing-step _ instructions _) step)
     (map
      (curry preprocessing-instruction type)
      (instructions
       (filter-map
        (match-lambda [(cons id item) (and (equivalent? id) item)])
        tests)
       context)))))

(define (preprocess-pcontext context pcontext instructions)
  (define (instruction->operator context instruction)
    (match-define (preprocessing-instruction type data) instruction)
    (match-define (preprocessing-step _ _ operator)
      (dict-ref preprocessing-steps type))
    (operator data context))
  (define preprocess
    (apply
     compose
     (map
      (curry instruction->operator context)
      ;; Function composition applies the rightmost function first
      (reverse instructions))))
  (pcontext-map preprocess pcontext))

; until fixed point, iterate through preprocessing attempting to drop preprocessing with no effect on error
(define (remove-unnecessary-preprocessing expression context pcontext
                                          instructions #:removed [removed empty])
  (define-values (result newly-removed)
    (let loop ([instructions instructions] [i 0] [removed removed])
      (cond
        [(>= i (length instructions))
         (values instructions removed)]
        [(preprocessing-<=? expression context pcontext (drop-at instructions i) instructions)
         (loop (drop-at instructions i) i (cons (list-ref instructions i) removed))]
        [else
         (loop instructions (+ i 1) removed)])))
  (cond
    [(< (length result) (length instructions))
     (remove-unnecessary-preprocessing expression context pcontext result #:removed newly-removed)]
    [else
     (timeline-push! 'remove-preprocessing (map ~a newly-removed))
     result]))

(define (preprocessing-<=? expression context pcontext preprocessing1 preprocessing2)
  (define pcontext1 (preprocess-pcontext context pcontext preprocessing1))
  (define pcontext2 (preprocess-pcontext context pcontext preprocessing2))
  (<= (errors-score (errors expression pcontext1 context))
      (errors-score (errors expression pcontext2 context))))

(define preprocessing-steps
  `((sort ,preprocessing-sort)
    (abs ,preprocessing-abs)))

(define preprocessing-sort
  (preprocessing-step
   (lambda (expression context)
     (for/list ([pair (in-combinations (context-vars context) 2)])
       (match-define (list a b) pair)
       (cons
        (replace-vars (list (cons a b) (cons b a)) expression)
        pair)))
   (lambda (pairs context)
     (filter
      (lambda (component) (> (length component) 1))
      (connected-components (context-vars context) pairs)))
   (lambda (component context)
     (define sort* (curryr sort (curryr </total (context-repr context))))
     (define variables (context-vars context))
     (unless (subsequence? component variables)
       (error 'instruction->operator
              "component should always be a subsequence of variables"))
     (define indices (indexes-where variables (curryr member component)))
     (lambda (points)
       (let* ([subsequence (map (curry list-ref points) indices)]
              [sorted (sort* subsequence)])
         (list-set* points indices sorted))))))

(define (connected-components variables pairs)
  (define components (disjoint-set (length variables)))
  (for ([pair (in-list pairs)])
    (match-define (list a b) pair)
    (disjoint-set-union! components
                         (disjoint-set-find! components (index-of variables a))
                         (disjoint-set-find! components (index-of variables b))))
  (group-by
   (compose (curry disjoint-set-find! components) (curry index-of variables))
   variables))

(define preprocessing-abs
  (preprocessing-step
   (lambda (expression context)
     (for/list ([variable (in-list (context-vars context))]
                [representation (in-list (context-var-reprs context))])
       ;; TODO: Handle case where neg isn't supported for this representation
       (define negate (get-parametric-operator 'neg representation))
       (cons
        (replace-vars
         (list (cons variable (list negate variable)))
         expression)
        variable)))
   (lambda (variables _) variables)
   (lambda (variable context)
     (define index (index-of (context-vars context) variable))
     (define abs (operator-info
                  (get-parametric-operator
                   'fabs
                   (list-ref (context-var-reprs context) index))
                  'fl))
     (curryr list-update index abs))))
