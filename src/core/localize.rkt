#lang racket

(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../ground-truth.rkt" "../syntax/types.rkt" "../syntax/sugar.rkt"
         "../syntax/syntax.rkt" "../alternative.rkt" "../platform.rkt" 
         "simplify.rkt" "egg-herbie.rkt" "../syntax/rules.rkt")

(provide batch-localize-error batch-localize-cost local-error-as-tree compute-local-errors)

;; Returns a list of expressions sorted by increasing local error
(define (batch-localize-error exprs ctx)
  (define errss
    (if (null? exprs) empty (compute-local-errors exprs ctx)))

  (for/list ([expr (in-list exprs)] [errs (in-list errss)])
    (sort
     (sort
      (for/list ([(expr err) (in-hash errs)]
                 #:when (list? expr))
                (cons err expr))
      expr<? #:key cdr)
     > #:key (compose errors-score car))))


;;Returns a list of lists of subexpresions and their cost different sorted by increasing local cost in the form (diff, expr)
(define (batch-localize-cost exprs ctx)

  ;;Creates a function to get the cost of a expr
  (define expr->cost  (platform-cost-proc (*active-platform*)))
  ;;For each expression takes the subexpressions and a the simplified version of those subexpression then for each subexpression it computes the difference between the two and return a sorted list of pairs of (subexpr and diff).
  (for/list ([expr (in-list exprs)])
    (define subexprs (all-subexpressions expr))
    (define simple-subexprs (simplify-batch (make-egg-query subexprs (*simplify-rules*))))

      (sort 
        (for/list ([subexpr (in-list subexprs)]
                  [simple-subexpr (in-list simple-subexprs)]
                  #:when (list? subexpr))
                  (cons (- (expr->cost subexpr (context-repr ctx)) (expr->cost (last simple-subexpr) (context-repr ctx)))
                         subexpr)
                  )
      > #:key car)  
    )
  )
      



; Compute local error or each sampled point at each node in `prog`.
(define (compute-local-errors exprs ctx)
  (define subexprss
    (for/list ([expr (in-list exprs)])
      (all-subexpressions expr)))
  (define spec-list
    (for*/list ([subexprs (in-list subexprss)] [subexpr (in-list subexprs)])
      (prog->spec subexpr)))
  (define ctx-list
    (for*/list ([subexprs (in-list subexprss)] [subexpr (in-list subexprs)])
      (struct-copy context ctx [repr (repr-of subexpr ctx)])))

  (define subexprs-fn (eval-progs-real spec-list ctx-list))

  ; Mutable error hack, this is bad
  (define errs
    (make-hash
     (for*/list ([subexprs (in-list subexprss)] [subexpr (in-list subexprs)])
       (cons subexpr '()))))

  (for ([(pt ex) (in-pcontext (*pcontext*))])
    (define exacts (apply subexprs-fn pt))
    (define exacts-hash
      (make-immutable-hash (map cons (apply append subexprss) exacts)))
    (for* ([subexprs (in-list subexprss)] [expr (in-list subexprs)])
      (define err
        (match expr
          [(? literal?) 1]
          [(? variable?) 1]
          [`(if ,c ,ift ,iff) 1]
          [(list f args ...)
           (define repr (impl-info f 'otype))
           (define argapprox
             (for/list ([arg (in-list args)])
               (hash-ref exacts-hash arg)))
           (define approx (apply (impl-info f 'fl) argapprox))
           (ulp-difference (hash-ref exacts-hash expr) approx repr)]))
      (hash-update! errs expr (curry cons err))))

  (for/list ([expr (in-list exprs)] [subexprs (in-list subexprss)])
    (for/hash ([subexpr (in-list subexprs)])
      (values subexpr (reverse (hash-ref errs subexpr))))))

;; Compute the local error of every subexpression of `prog`
;; and returns the error information as an S-expr in the
;; same shape as `prog`
(define (local-error-as-tree expr ctx)
  (define errs (first (compute-local-errors (list expr) ctx)))
  (let loop ([expr expr])
    (match expr
      [(list op args ...) (cons (hash-ref errs expr) (map loop args))]
      [_ (list (hash-ref errs expr))])))
