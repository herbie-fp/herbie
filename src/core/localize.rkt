#lang racket

(require math/bigfloat)
(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt" "../syntax/types.rkt")

(provide localize-error)

(define (all-subexpressions expr)
  (remove-duplicates
   (reap [sow]
         (let loop ([expr expr])
           (sow expr)
           (match expr
             [(? number?) (void)]
             [(? variable?) (void)]
             [(list op args ...)
              (for-each loop args)])))))

(define (localize-error prog ctx)
  (define expr (program-body prog))
  (define subexprs (all-subexpressions expr))
  (define subprogs
    (for/list ([expr (in-list subexprs)])
      `(λ ,(program-variables prog) ,expr)))
  (define exact-fn (batch-eval-progs subprogs 'bf ctx))
  (define errs (make-hash (map (curryr cons '()) subexprs)))
  (for ([(pt ex) (in-pcontext (*pcontext*))])
    (define bf-values (apply exact-fn pt))
    (define bfhash (make-hash (map cons subexprs bf-values)))
    (for ([expr (in-list subexprs)])
      (define err
        (match expr
          [(? number?) 1]
          [(? variable?) 1]
          [`(if ,c ,ift ,iff) 1]
          [(list f args ...)
           (define repr (operator-info f 'otype))
           (define <-bf (representation-bf->repr repr))
           (define argapprox
             (for/list ([arg (in-list args)] [repr (in-list (operator-info f 'itype))])
               ((representation-bf->repr repr) (hash-ref bfhash arg))))
           (ulp-difference (<-bf (hash-ref bfhash expr))
                           (apply (operator-info f 'fl) argapprox) repr)]))
      (hash-update! errs expr (curry cons err))))

  (sort
   (reap [sow]
         (for ([(expr err) (in-hash errs)])
           (unless (andmap (curry = 1) err)
             (sow (cons err expr)))))
   > #:key (compose errors-score car)))
