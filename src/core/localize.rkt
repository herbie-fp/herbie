#lang racket

(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../ground-truth.rkt" "../syntax/types.rkt" "../syntax/sugar.rkt"
         "../syntax/syntax.rkt" "../alternative.rkt" "../platform.rkt" 
         "simplify.rkt" "egg-herbie.rkt" "../syntax/rules.rkt")

(provide batch-localize-costs batch-localize-errors
         local-error-as-tree compute-local-errors
         all-subexpressions)

(define (all-subexpressions expr)
  (remove-duplicates
    (reap [sow]
          (let loop ([expr expr])
            (sow expr)
            (match expr
              [(? literal?) (void)]
              [(? variable?) (void)]
              [`(if ,c ,t ,f)
               (loop c)
               (loop t)
               (loop f)]
              [(list op args ...)
               (for ([arg args]) (loop arg))])))))

(define (regroup-nested inputss outputs)
  (match* (inputss outputs)
    [((cons (cons fhead ftail) rest)
      (cons head tail))
     (match-define (cons fout out) (regroup-nested (cons ftail rest) tail))
     (cons (cons head fout) out)]
    [((cons '() rest) outputs)
     (cons '() (regroup-nested rest outputs))]
    [('() '())
     '()]))

(define (batch-localize-costs exprs ctx)
  (define subexprss (map all-subexpressions exprs))
  (define expr->cost  (platform-cost-proc (*active-platform*)))
  
  (define simplifiedss
    (regroup-nested
     subexprss
     (map last
          (simplify-batch (make-egg-query (apply append subexprss) (*simplify-rules*))))))

  (define localize-costss
    (for/list ([subexprs (in-list subexprss)] [simplifieds (in-list simplifiedss)])
      (sort 
       (for/list ([subexpr (in-list subexprs)]
                  [simplified (in-list simplifieds)]
                  #:when (list? subexpr))
         (cons (- (expr->cost subexpr (repr-of subexpr ctx))
                  (expr->cost simplified (repr-of subexpr ctx)))
               subexpr))
       > #:key car)))

  (values simplifiedss localize-costss))

(define (batch-localize-errors exprs ctx)
  (define subexprss (map all-subexpressions exprs))
  (define errss (compute-local-errors subexprss ctx))

  (for/list ([expr (in-list exprs)] [errs (in-list errss)])
    (sort
     (sort
      (for/list ([(subexpr err) (in-hash errs)]
                 #:when (list? subexpr))
        (cons err subexpr))
      expr<? #:key cdr)
     > #:key (compose errors-score car))))

; Compute local error or each sampled point at each node in `prog`.
(define (compute-local-errors subexprss ctx)
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

  (for/list ([subexprs (in-list subexprss)])
    (for*/hash ([subexpr (in-list subexprs)])
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
