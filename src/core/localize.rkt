#lang racket

(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../ground-truth.rkt" "../syntax/types.rkt" "../syntax/sugar.rkt"
         "../syntax/syntax.rkt")

(provide batch-localize-error local-error-as-tree compute-local-errors
         all-subexpressions all-subexpressions-rev)

(define (all-subexpressions expr repr)
  (remove-duplicates
    (reap [sow]
          (let loop ([expr expr] [repr repr])
            (sow (cons expr repr))
            (match expr
              [(? number?) (void)]
              [(? variable?) (void)]
              [`(if ,c ,t ,f)
               (loop c (get-representation 'bool))
               (loop t repr)
               (loop f repr)]
              [(list op args ...)
               (define atypes (impl-info op 'itype))
               (for ([arg args] [atype atypes])
                 (loop arg atype))])))))

(define (all-subexpressions-rev expr repr)
  (remove-duplicates (reverse
                      (reap [sow]
                            (let loop ([expr expr] [repr repr])
                              (sow (cons expr repr))
                              (match expr
                                [(? number?) (void)]
                                [(? variable?) (void)]
                                [`(if ,c ,t ,f)
                                 (loop c (get-representation 'bool))
                                 (loop t repr)
                                 (loop f repr)]
                                [(list op args ...)
                                 (define atypes (impl-info op 'itype))
                                 (for ([arg args] [atype atypes])
                                   (loop arg atype))]))))))

;; Returns a list of expressions sorted by increasing local error
(define (batch-localize-error exprs ctx)
  (define errss
    (if (null? exprs) empty (compute-local-errors exprs ctx)))
  (for/list ([expr (in-list exprs)] [errs (in-list errss)])
    (sort
     (sort
      (reap [sow]
        (for ([(expr err) (in-hash errs)])
          (unless (andmap (curry = 1) err)
            (sow (cons err expr)))))
      expr<? #:key cdr)
     > #:key (compose errors-score car))))

; Compute local error or each sampled point at each node in `prog`.
(define (compute-local-errors exprs ctx)
  (define subexprss
    (for/list ([expr (in-list exprs)])
      (all-subexpressions expr (context-repr ctx))))
  (define spec-list
    (for*/list ([subexprs (in-list subexprss)] [subexpr (in-list subexprs)])
      (prog->spec (car subexpr))))
  (define ctx-list
    (for*/list ([subexprs (in-list subexprss)] [subexpr (in-list subexprs)])
      (struct-copy context ctx [repr (cdr subexpr)])))

  (define subexprs-fn (eval-progs-real spec-list ctx-list))

  ; Mutable error hack, this is bad
  (define errs
    (make-hash
     (for*/list ([subexprs (in-list subexprss)] [subexpr (in-list subexprs)])
       (cons (car subexpr) '()))))

  (for ([(pt ex) (in-pcontext (*pcontext*))])
    (define exacts (apply subexprs-fn pt))
    (define exacts-hash
      (make-immutable-hash (map cons (apply append subexprss) exacts)))
    (for* ([subexprs (in-list subexprss)] [expr (in-list subexprs)])
      (define err
        (match (car expr)
          [(? number?) 1]
          [(? variable?) 1]
          [`(if ,c ,ift ,iff) 1]
          [(list f args ...)
           (define repr (impl-info f 'otype))
           (define argapprox
             (for/list ([arg (in-list args)]
                        [repr (in-list (impl-info f 'itype))])
               (hash-ref exacts-hash
                         (cons arg repr))))
           (define approx (apply (impl-info f 'fl) argapprox))
           (ulp-difference (hash-ref exacts-hash expr) approx repr)]))
      (hash-update! errs (car expr) (curry cons err))))

  (for/list ([expr (in-list exprs)] [subexprs (in-list subexprss)])
    (for/hash ([subexpr (in-list subexprs)])
      (values (car subexpr) (reverse (hash-ref errs (car subexpr)))))))

;; Compute the local error of every subexpression of `prog`
;; and returns the error information as an S-expr in the
;; same shape as `prog`
(define (local-error-as-tree expr ctx)
  (define errs (first (compute-local-errors (list expr) ctx)))
  (let loop ([expr expr])
    (match expr
      [(list op args ...) (cons (hash-ref errs expr) (map loop args))]
      [_ (list (hash-ref errs expr))])))
