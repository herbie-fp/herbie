#lang racket

(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../ground-truth.rkt" "../syntax/types.rkt" "../syntax/sugar.rkt"
         "../syntax/syntax.rkt" "../alternative.rkt" "../platform.rkt" "simplify.rkt" "egg-herbie.rkt" "../syntax/rules.rkt")

(provide batch-localize-error batch-localize-cost local-error-as-tree compute-local-errors
         all-subexpressions )

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

;; Returns a list of expressions sorted by increasing local error
(define (batch-localize-error exprs ctx)
  (define errss
    (if (null? exprs) empty (compute-local-errors exprs ctx)))
  (define result
  (for/list ([expr (in-list exprs)] [errs (in-list errss)])
    (sort
     (sort
      (reap [sow]
        (for ([(expr err) (in-hash errs)])
          (sow (cons err expr))))
      expr<? #:key cdr)
     > #:key (compose errors-score car))))
     result) 


;;Returns a list of expressions sorted by increasing local cost in the form (subexpression, cost)
(define (batch-localize-cost exprs ctx)
  ;;Create a list of lists of subexpressions
  (define subexprss
    (for/list ([expr (in-list exprs)])
      (all-subexpressions expr (context-repr ctx))))

  (define costs '())
  (define globalRepr "")
  ;;Sort through each each subexpression and append a pair of expression and cost to the list costs
  (for-each (lambda (subexprs)
    (for-each (lambda (subexpr)
      (define expr(car subexpr));;This subexpr is of form:(+.f64 N 1)
      (define repr (cdr subexpr));;The repr is in the form #<representation binary64>
      (set! globalRepr repr)
      (define cost((platform-cost-proc (*active-platform*)) expr repr))
      (set! costs (cons (cons expr cost) costs))
      )
    subexprs))
  subexprss)

  (define simplified-exprs  (simplify-batch (make-egg-query (reverse (map car costs)) (*simplify-rules*))))

(define simple-costs '())
(for-each (lambda (simple-subexpr)
  (if (= (length simple-subexpr) 2)
    (let ((cost ((platform-cost-proc (*active-platform*)) (cadr simple-subexpr) globalRepr)))
      ;(displayln "Test1")
      ;(displayln (cadr simple-subexpr))
      (set! simple-costs (cons (cons (cadr simple-subexpr) cost) simple-costs)))
    (let ((cost ((platform-cost-proc (*active-platform*)) (car simple-subexpr) globalRepr)))
      ;;(displayln "Test2")
      ;;(displayln (car simple-subexpr))
      (set! simple-costs (cons (cons (car simple-subexpr) cost) simple-costs)))))
        simplified-exprs)

  (define cost-opportunity '())
  (for-each 
    (lambda (costs-pair simple-costs-pair)
      (define diff (- (cdr costs-pair) (cdr simple-costs-pair)))
      (if (> diff 5)
      (displayExpr (car costs-pair) (car simple-costs-pair) diff)
        ;;(set! cost-opportunity (cons (cons (car costs-pair) diff ) cost-opportunity))
        (display ""))
      )
    costs simple-costs)

  (define sorted-cost-opportunity
    (sort cost-opportunity
      (lambda (a b) (> (cdr a) (cdr b)))))


  (define cost-error (append sorted-cost-opportunity (batch-localize-error exprs ctx)))

  sorted-cost-opportunity)

(define (displayExpr before after diff)
  (displayln "Before")
  (displayln before)
  (displayln "after")
  (displayln after)
  (displayln "diff")
  (displayln diff)
  (displayln "____"))






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
