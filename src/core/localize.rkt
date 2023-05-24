#lang racket

(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../ground-truth.rkt" "../syntax/types.rkt" "../syntax/syntax.rkt")

(provide localize-error local-error-as-tree)

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
              (define atypes (operator-info op 'itype))
              (for ([arg args] [atype atypes])
                (loop arg atype))])))))

; Compute local error or each sampled point at each node in `prog`.
; Uses math/bigfloat rather than rival for speed.
(define (compute-local-errors-fast prog ctx)
  (define expr (program-body prog))
  (define subexprs (all-subexpressions expr (context-repr ctx)))
  (define subprogs
    (for/list ([sexpr (in-list subexprs)])
      `(λ ,(program-variables prog) ,(first sexpr))))
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

; expr = (posit16->binary32 1)
; ctx = () () -> binary32

; subexpr = (posit16->binary32 1)
; ctx = () () -> binary32

; subexpr = 1 repr = posit16

(define (compute-local-errors prog ctx)
  (define expr (program-body prog))
  (define subexprs (all-subexpressions expr (context-repr ctx)))
  (define prog-list 
    (for/list ([sexpr (in-list subexprs)])
      `(λ ,(context-vars ctx) ,(car sexpr))))
  (define ctx-list 
    (for/list ([sexpr (in-list subexprs)])
      (struct-copy context ctx [repr (cdr sexpr)])))
  (define subexprs-fn (eval-prog-list-real prog-list ctx-list))

  (define errs
    (for/hash ([sexpr (in-list subexprs)])
      (values (car sexpr) '())))

  (for ([(pt ex) (in-pcontext (*pcontext*))])
    (define exacts (apply subexprs-fn pt))
    (define exacts-hash
      (for/hash (
        [expr (in-list subexprs)] 
        [ex (in-list exacts)])
        (values (car expr) ex)))
    (for ([expr (in-list subexprs)])
      (define err
        (match (car expr)
          [(? number?) 1]
          [(? variable?) 1]
          [`(if ,c ,ift ,iff) 1]
          [(list f args ...)
            (define repr (operator-info f 'otype))
            (define argapprox
              (for/list (
                [arg (in-list args)] 
                [repr (in-list (operator-info f 'itype))])
                (hash-ref exacts-hash arg)
              )
            )
            (ulp-difference
              (hash-ref exacts-hash (car expr))
              (apply (operator-info f 'fl) argapprox) repr)
          ]))
      (hash-update! errs (car expr) (curry cons err))))
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
