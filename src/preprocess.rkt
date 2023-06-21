#lang racket

(require "syntax/types.rkt" "points.rkt" "float.rkt")

(provide preprocess-pcontext *herbie-preprocess* apply-preprocess)

(define (connected-components variables swaps)
  (define rules
    (for/list ([swap swaps])
      (match-define (list a b) swap)
      (rule (string->symbol (format "swap-~a-~a" a b))
            (index-of variables a)
            (index-of variables b)
            '()
            'real)))
  (define query (make-egg-query (range (length variables)) rules #:const-folding? #f))
  (define components (map last (simplify-batch query)))
  (map (lambda (component) (map car component)) (group-by cdr (map cons variables components))))

(define (connected-components* variables swaps)
  (define indices (range (length variables)))
  (define union-find (list->vector indices))
  (define (find! u x)
    (define p (vector-ref u x))
    (if (= p x)
        x
        (let ([g (parent u p)])
          (vector-set! u x g)
          (find! u g))))
  (define (union! u x y) (parent-set! u y x))
  (for ([swap (in-list swaps)])
    (match-define (list a b) swap)
    (union!
     union-find
     (find! union-find (index-of variables a))
     (find! union-find (index-of variables b))))
  (group-by
   (compose (curry list-ref variables) (curry find! union-find))
   indices))

;; See https://pavpanchekha.com/blog/symmetric-expressions.html
(define (find-identities expression variables rules)
  ;; Here `*` means a test identity that *may* be equal to `expression`, and
  ;; `~` means the simplest form of an expression.
  (define pairs (combinations variables 2))
  (define swaps*
    (for/list ([pair (in-list pairs)])
      (match-define (list a b) pair)
      (replace-vars (list (cons a b) (cons b a)) expression)))
  (define evens*
    (for/list ([variable (in-list variables)])
      ;; TODO: Use negate for vars repr here
      (replace-vars (list (cons variable `(fabs.f64 ,variable))) expression)))
  ;; TODO: Check for unsoundness, abort all preprocessing if so. Would need
  ;; explicit access to egraph constructed by run-egg though
  (define query (make-egg-query (cons expression (append swaps* evens*)) rules))
  (match-define (cons expression~ rest~) (map last (simplify-batch query)))
  (define-values (swaps~ evens~) (split-at rest~ (length swaps*)))
  (define preprocess-abs
    (for/list ([variable variables]
               [even~ (in-list evens~)]
               #:when (equal? expression~ even~))
      (list 'abs variable)))
  (define swaps
    (filter-map
     (lambda (pair swap~) (and (equal? expression~ swap~) pair))
     pairs
     swaps~))
  (define preprocess-sort
    ;; TODO: connected-components vs connected-components*?
    (for/list ([component (connected-components* swaps)]
               #:when (> (length component) 1))
      (cons 'sort component)))

  (append preprocess-abs preprocess-sort))

(define (preprocess->operator context preprocess)
  (match-define (context variables representation _) context)
  ;; TODO: Does </total need individual reprs?
  (define sort* (curryr sort (curryr </total representation)))
  (define (list-suffix? l r) (list-prefix? (reverse l) (reverse r)))
  (match preprocess
    [('sort component ...) #:when (equal? component variables)
     sort*]
    [('sort component ...) #:when (list-prefix? component variables)
     (define position (length component))
     (lambda (points)
       (let-values ([(prefix suffix) (split-at points position)])
         (append (sort* prefix) suffix)))]
    [('sort component ...) #:when (list-suffix? component variables)
     (define position (- (length variables) (length component)))
     (lambda (points)
       (let-values ([(prefix suffix) (split-at points position)])
         (append prefix (sort* suffix))))]
    [('sort component ...)
     ;; Assertion: `component` is a subsequence of `variables`
     ;;
     ;; This gets a bit tricky: given a description of a subsequence of points
     ;; (`component`), we have to extract the subsequence, sort it, and glue it
     ;; back together with the other points.
     (define should-take
       (for/fold ([result empty]
                  [subsequence component]
                  #:result result)
                 ([variable variables])
         (if (equal? variable (first subsequence))
             (values (cons #t result) (rest subsequence))
             (values (cons #f result) subsequence))))
     (lambda (points)
       (let ([sorted (sort*
                      (filter-map
                       (lambda (point take?) (and take? point))
                       points should-take))])
         (for/fold ([result empty]
                    [sorted sorted]
                    #:result result)
                   ([point points]
                    [take? should-take])
           (if take?
               (values (cons (first sorted) result) (rest sorted))
               (values (cons point result) sorted)))))]
    [('abs variable)
     (define index (index-of variables variable))
     ;; TODO: Make abs an actually correct operation
     (lambda (points) (list-update points index abs))]))
