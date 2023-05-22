#lang racket

(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../ground-truth.rkt" "../syntax/types.rkt" "../syntax/syntax.rkt")

(provide localize-error local-error-as-tree)

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

; Compute local error or each sampled point at each node in `prog`.
; Uses math/bigfloat rather than rival for speed.
(define (compute-local-errors-fast prog ctx)
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

  errs)

;; Returns a list of expressions sorted by increasing local error
(define (localize-error prog ctx)
  (define errs (compute-local-errors prog ctx))
  (sort
    (reap [sow]
          (for ([(expr err) (in-hash errs)])
            (unless (andmap (curry = 1) err)
              (sow (cons err expr)))))
    > #:key (compose errors-score car)))

(define (compute-local-errors prog ctx)
  (define expr (program-body prog)) 
  (define subexprs (all-subexpressions expr))
  (define prog-list 
    (for/list ([sexpr (in-list subexprs)])
      `(λ ,(context-vars ctx) ,sexpr)))
  (define subexprs-fn (eval-prog-list-real prog-list ctx))
  (define errs (make-hash (map (curryr cons '()) subexprs)))
  (for ([(pt ex) (in-pcontext (*pcontext*))])
    (eprintf "~a -> ~a\n" pt ex)
    (eprintf "~a ~a\n" (flonum? (first pt)) (flonum? ex))
    (define exacts (apply subexprs-fn pt))
    (define exacts-hash
      (for/hash ([expr (in-list subexprs)] 
        [ex (in-list exacts)])
        (values expr ex)))
    (eprintf "got here\n")
    (for ([expr (in-list subexprs)])
      (eprintf "expr ~a\n" expr)
      (define err
        (match expr
          [(? number?) 1]
          [(? variable?) 1]
          [`(if ,c ,ift ,iff) 1]
          [(list f args ...)
           (define repr (operator-info f 'otype))
           (define argapprox
             (for/list ([arg (in-list args)] [repr (in-list (operator-info f 'itype))])
               (hash-ref exacts-hash arg)))
           (eprintf "~a ~a\n" argapprox (flonum? (first argapprox)))
           (eprintf "~a\n" (apply (operator-info f 'fl) argapprox))
           (ulp-difference (hash-ref exacts-hash expr)
                           (apply (operator-info f 'fl) argapprox) repr)]))
      (hash-update! errs expr (curry cons err))))
  errs)

;; Compute the local error of every subexpression of `prog`
;; and returns the error information as an S-expr in the
;; same shape as `prog`
(define (local-error-as-tree prog ctx)
  (define errs (compute-local-errors prog ctx))
  (define expr (program-body prog))
  (let loop ([expr expr])
    (match expr
      [(list op args ...) (cons (hash-ref errs expr) (map loop args))]
      [_ (list (hash-ref errs expr))])))
