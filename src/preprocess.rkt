#lang racket

;; TODO: The exact same replace-vars exists in both syntax.rkt and program.rkt
(require "core/egg-herbie.rkt" "core/simplify.rkt" "syntax/rules.rkt"
         "syntax/types.rkt" "programs.rkt" "points.rkt" "float.rkt")

(provide find-preprocessing preprocess-pcontext)

;; See https://pavpanchekha.com/blog/symmetric-expressions.html
(define (find-preprocessing expression context rules)
  ;; Here `*` means a test identity that *may* be equal to `expression`, and
  ;; `~` means the simplest form of an expression.
  (define variables (context-vars context))
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
  (define abs-instructions
    (for/list ([variable (in-list variables)] [even~ (in-list evens~)]
               #:when (equal? expression~ even~))
      (list 'abs variable)))
  (define swaps (filter-map
                 (lambda (pair swap~) (and (equal? expression~ swap~) pair))
                 pairs
                 swaps~))
  ;; TODO: `connected-components` vs `connected-components*`?
  (define sort-instructions
    (for/list ([component (connected-components* variables swaps)]
               #:when (> (length component) 1))
      (cons 'sort component)))
  (append abs-instructions sort-instructions))

(define (preprocess-pcontext context pcontext* preprocessing)
  (define preprocess
    (apply compose (map (curry instruction->operator context) preprocessing)))
  (pcontext
   (vector-map preprocess (pcontext-points pcontext*))
   (pcontext-exacts pcontext*)))

(define (instruction->operator context instruction)
  (define variables (context-vars context))
  (define representation (context-repr context))
  ;; TODO: Does `</total` need individual reprs?
  (define sort* (curryr sort (curryr </total representation)))
  (define (list-suffix? l r) (list-prefix? (reverse l) (reverse r)))
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
       (error 'instruction->operator "component should always be a subsequence of variables"))
     ;; This gets a bit tricky: given a description of a subsequence of points
     ;; (`component`), we have to extract the subsequence, sort it, and glue it
     ;; back together with the other points.
     (define should-take
       (for/fold ([result empty] [subsequence component]
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
         (for/fold ([result empty] [sorted sorted]
                    #:result result)
                   ([point points] [take? should-take])
           (if take?
               (values (cons (first sorted) result) (rest sorted))
               (values (cons point result) sorted)))))]
    [(list 'abs variable)
     (define index (index-of variables variable))
     ;; TODO: Make abs an actually correct operation
     (lambda (points) (list-update points index abs))]))

(define (subsequence? v l)
  (cond
    [(empty? v) #t]
    [(empty? l) #f]
    [else (subsequence? (rest v) (member (first v) l) (rest v))]))

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
  (define union-find (list->vector (range (length variables))))
  (define (find! u x)
    (define p (vector-ref u x))
    (if (= p x)
        x
        (let ([g (vector-ref u p)])
          (vector-set! u x g)
          (find! u g))))
  (define (union! u x y) (vector-set! u y x))
  (for ([swap (in-list swaps)])
    (match-define (list a b) swap)
    (union! union-find
            (find! union-find (index-of variables a))
            (find! union-find (index-of variables b))))
  (group-by
   (compose (curry find! union-find) (curry index-of variables))
   variables))
