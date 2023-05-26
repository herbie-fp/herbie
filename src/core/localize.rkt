#lang racket

(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../ground-truth.rkt" "../syntax/types.rkt" "../syntax/syntax.rkt")

(provide localize-error local-error-as-tree)

;; Returns a list of expressions sorted by increasing local error
(define (localize-error prog ctx)
  (define errs (compute-local-errors prog ctx))
  (sort
    (reap [sow]
          (for ([(expr err) (in-hash errs)])
            (unless (andmap (curry = 1) err)
              (sow (cons err expr)))))
    > #:key (compose errors-score car)))

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
(define (compute-local-errors prog ctx)
  (define expression (program-body prog))
  (define sub-expressions (all-subexpressions expression (context-repr ctx)))
  (define prog-list
    (for/list ([subexpr (in-list sub-expressions)])
      `(λ ,(context-vars ctx) ,(car subexpr))))
  (define ctx-list
    (for/list ([subexpr (in-list sub-expressions)])
      (struct-copy context ctx [repr (cdr subexpr)])))

  (define subexprs-fn (eval-progs-real prog-list ctx-list))

  ; Mutable error hack, this is bad
  (define temp-errs
    (make-hash
     (for/list ([subexpr (in-list sub-expressions)])
       (cons (car subexpr) '()))))
  (define errs (make-hash))
  (for ([(k v) temp-errs])
    (hash-set! errs k v))

  (for ([(pt ex) (in-pcontext (*pcontext*))])
    (define exacts (apply subexprs-fn pt))
    (define exacts-hash
      (make-immutable-hash (map cons sub-expressions exacts)))
    (for ([expr (in-list sub-expressions)])
      (define err
        (match (car expr)
          [(? number?) 1]
          [(? variable?) 1]
          [`(if ,c ,ift ,iff) 1]
          [(list f args ...)
           (define repr (operator-info f 'otype))
           (define argapprox
             (for/list ([arg (in-list args)]
                        [repr (in-list (operator-info f 'itype))])
               (hash-ref exacts-hash
                         (cons arg repr))))
           (ulp-difference
            (hash-ref exacts-hash expr)
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
