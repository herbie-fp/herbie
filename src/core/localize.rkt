#lang racket

(require "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "rules.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
         "../syntax/platform.rkt"
         "points.rkt"
         "programs.rkt"
         "sampling.rkt"
         "simplify.rkt"
         "egg-herbie.rkt"
         "batch.rkt")

(provide batch-localize-costs
         batch-localize-errors
         compute-local-errors
         local-error-as-tree)

(define (regroup-nested inputss outputs)
  (match* (inputss outputs)
    [((cons (cons fhead ftail) rest) (cons head tail))
     (match-define (cons fout out) (regroup-nested (cons ftail rest) tail))
     (cons (cons head fout) out)]
    [((cons '() rest) outputs) (cons '() (regroup-nested rest outputs))]
    [('() '()) '()]))

(define (batch-localize-costs exprs ctx)
  (define subexprss (map all-subexpressions exprs))
  (define progs (apply append subexprss))

  ; inputs to egg
  (define reprs (map (lambda (prog) (repr-of prog ctx)) progs))
  (define rules (real-rules (*simplify-rules*)))
  (define lifting-rules (platform-lifting-rules))
  (define lowering-rules (platform-lowering-rules))

  ; egg runner (2-phases for real rewrites and implementation selection)
  (define runner
    (make-egg-runner progs
                     reprs
                     `((,lifting-rules . ((iteration . 1) (scheduler . simple)))
                       (,rules . ((node . ,(*node-limit*))))
                       (,lowering-rules . ((iteration . 1) (scheduler . simple))))))

  ; run egg
  (define simplified
    (simplify-batch runner
                    (typed-egg-extractor
                     (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc))))

  ; run egg
  (define simplifiedss (regroup-nested subexprss (map last simplified)))

  ; build map from starting expr to simplest
  (define expr->simplest (make-hash))
  (for ([subexprs (in-list subexprss)]
        [simplifieds (in-list simplifiedss)]
        #:when #t
        [subexpr (in-list subexprs)]
        [simplified (in-list simplifieds)])
    (hash-set! expr->simplest subexpr simplified))

  ; platform-based expression cost
  (define cost-proc (platform-cost-proc (*active-platform*)))
  (define (expr->cost expr)
    (cost-proc expr (repr-of expr ctx)))

  (define (cost-opportunity subexpr children)
    ; start and end cost of roots
    (define start-cost (expr->cost subexpr))
    (define best-cost (expr->cost (hash-ref expr->simplest subexpr)))
    ; start and end cost of children
    (define start-child-costs (map expr->cost children))
    (define best-child-costs
      (for/list ([child (in-list children)])
        (expr->cost (hash-ref expr->simplest child))))
    ; compute cost opportunity
    (- (apply - start-cost start-child-costs) (apply - best-cost best-child-costs)))

  ; rank subexpressions by cost opportunity
  (define localize-costss
    (for/list ([subexprs (in-list subexprss)])
      (sort (reap [sow]
                  (for ([subexpr (in-list subexprs)])
                    (match subexpr
                      [(? literal?) (void)]
                      [(? symbol?) (void)]
                      [(approx _ impl)
                       (define cost-opp (cost-opportunity subexpr (list impl)))
                       (sow (cons cost-opp subexpr))]
                      [(list _ args ...)
                       (define cost-opp (cost-opportunity subexpr args))
                       (sow (cons cost-opp subexpr))])))
            >
            #:key car)))

  localize-costss)

(define (batch-localize-errors exprs ctx)
  (define subexprss (map all-subexpressions exprs))
  (define errss (compute-local-errors subexprss ctx))

  (define pruned-list
    (for/list ([h (in-list errss)])
      (define pruned (make-hash))
      (for ([(k v) (in-hash h)])
        (hash-set! pruned k (hash-ref v 'errs)))
      pruned))

  (for/list ([_ (in-list exprs)]
             [errs (in-list pruned-list)])
    (sort (sort (for/list ([(subexpr err) (in-hash errs)]
                           #:when (or (list? subexpr) (approx? subexpr)))
                  (cons err subexpr))
                expr<?
                #:key cdr)
          >
          #:key (compose errors-score car))))

; Compute local error or each sampled point at each node in `prog`.
(define (compute-local-errors subexprss ctx)
  (define exprs-list (append* subexprss)) ; unroll subexprss
  (define ctx-list
    (for/list ([subexpr (in-list exprs-list)])
      (struct-copy context ctx [repr (repr-of subexpr ctx)])))

  (define expr-batch (progs->batch exprs-list))
  (define nodes (batch-nodes expr-batch))
  (define roots (batch-roots expr-batch))

  (define subexprs-fn (eval-progs-real (map prog->spec exprs-list) ctx-list))

  (define errs
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define exacts-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (for ([(pt ex) (in-pcontext (*pcontext*))]
        [pt-idx (in-naturals)])
    (define exacts (list->vector (apply subexprs-fn pt)))
    (for ([expr (in-list exprs-list)]
          [root (in-vector roots)]
          [exact (in-vector exacts)]
          [expr-idx (in-naturals)])
      (define err
        (match (vector-ref nodes root)
          [(? literal?) 1]
          [(? variable?) 1]
          [(approx _ impl)
           (define repr (repr-of expr ctx))
           (ulp-difference exact (vector-ref exacts (vector-member impl roots)) repr)]
          [`(if ,c ,ift ,iff) 1]
          [(list f args ...)
           (define repr (impl-info f 'otype))
           (define argapprox
             (for/list ([idx (in-list args)])
               (vector-ref exacts (vector-member idx roots)))) ; arg's index mapping to exact
           (define approx (apply (impl-info f 'fl) argapprox))
           (ulp-difference exact approx repr)]))
      (vector-set! (vector-ref exacts-out expr-idx) pt-idx exact)
      (vector-set! (vector-ref errs expr-idx) pt-idx err)))

  (define n 0)
  (for/list ([subexprs (in-list subexprss)])
    (for*/hash ([subexpr (in-list subexprs)])
      (begin0 (values subexpr
                      (hasheq 'errs
                              (vector->list (vector-ref errs n))
                              'exacts
                              (vector->list (vector-ref exacts-out n))))
        (set! n (add1 n))))))

;; Compute the local error of every subexpression of `prog`
;; and returns the error information as an S-expr in the
;; same shape as `prog`
(define (local-error-as-tree expr ctx)
  (define errs (first (compute-local-errors (list (all-subexpressions expr)) ctx)))
  (let loop ([expr expr])
    (define expr-info (hash-ref errs expr))
    (define err-list (hash-ref expr-info 'errs))
    (define exacts-list (hash-ref expr-info 'errs))
    (match expr
      [(list op args ...) (cons err-list (map loop args))]
      [_ (list err-list)])))
