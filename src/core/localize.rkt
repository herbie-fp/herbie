#lang racket

(require math/bigfloat
         rival)
(require "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "rules.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
         "../syntax/platform.rkt"
         "../syntax/read.rkt"
         "../syntax/read.rkt"
         "../core/rival.rkt"
         "points.rkt"
         "programs.rkt"
         "sampling.rkt"
         "simplify.rkt"
         "egg-herbie.rkt"
         "compiler.rkt"
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
  (define batch (progs->batch progs))
  (define runner
    (make-egg-runner batch
                     (batch-roots batch)
                     reprs
                     `((,lifting-rules . ((iteration . 1) (scheduler . simple)))
                       (,rules . ((node . ,(*node-limit*))))
                       (,lowering-rules . ((iteration . 1) (scheduler . simple))))))

  ; run egg
  (define simplified
    (map (compose debatchref last)
         (simplify-batch runner
                         (typed-egg-batch-extractor
                          (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc)
                          batch))))

  ; run egg
  (define simplifiedss (regroup-nested subexprss simplified))

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
        (hash-set! pruned k (hash-ref v 'ulp-errs)))
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

  (define all-vars (context-vars ctx))
  (define spec-list (map prog->spec exprs-list))
  (define spec-vec (list->vector spec-list))
  (define expr-batch (progs->batch exprs-list))
  (define nodes (batch-nodes expr-batch))
  (define roots (batch-roots expr-batch))
  (eprintf "nodes: ~a\n" nodes)
  (eprintf "spec-vec: ~a\n" spec-vec)
  (eprintf "roots: ~a\n" roots)

  ; TODO don't ignore the status code from make-real-compiler in eval-progs-real
  (define subexprs-fn (eval-progs-real (map prog->spec exprs-list) ctx-list))
  (define actual-value-fn (compile-progs exprs-list ctx))

  (define ulp-errs
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define exacts-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define approx-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define true-error-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  ; Points are ordered in the ordering that they appear in the spec.
  (for ([(pt ex) (in-pcontext (*pcontext*))]
        [pt-idx (in-naturals)])

    (define exacts (list->vector (apply subexprs-fn pt)))
    (define actuals (apply actual-value-fn pt))

    (for ([spec (in-vector spec-vec)]
          [expr (in-list exprs-list)]
          [root (in-vector roots)]
          [exact (in-vector exacts)]
          [actual (in-vector actuals)]
          [expr-idx (in-naturals)])
      (define true-err
        ;; ??? Whats the default values for true error literal, variable approx and if?
        (match (vector-ref nodes root)
          [(? literal?) 0]
          [(? variable?) 0]
          [(approx aprx-spec impl)
           (eprintf "TRUEERR: APPROX, ~a ~a ~a\n" approx aprx-spec impl)
           0] ;; TODO understand approx nodes.
          [`(if ,c ,ift ,iff) 0]
          [(list f args-roots ...)
           ;; Find the index of the variables we need to substitute.
           (eprintf "EXPR[node: ~a, exact: ~a, root: ~a, sepc: ~a]\n"
                    (vector-ref nodes root)
                    exact
                    root
                    spec)
           ; __exact double underscore to avoid conflicts with user provided
           ; variables. Could use name mangling long term.
           (define modifed-vars (append all-vars `(__exact)))
           (define true-error-expr (list `(- ,spec __exact)))
           (eprintf "true-error-expr: ~a\n" true-error-expr)
           (define diffMachine
             (rival-compile true-error-expr modifed-vars (list flonum-discretization)))
           (define inputs (map bf (append pt (list exact)))) ; TODO remove bf hack
           ;; ??? Is this always length 1, as we are asking about exact?
           (eprintf "inputs: ~a\n" inputs)
           (define true-error (vector-ref (rival-apply diffMachine (list->vector inputs)) 0))
           (eprintf "true-error: ~a, pt: ~a, exact ~a\n" true-error pt exact)
           true-error]))

      (define ulp-err ; ??? Is this upls of error?
        (match (vector-ref nodes root)
          [(? literal?) 1]
          [(? variable?) 1]
          [(approx _ impl)
           (define repr (repr-of expr ctx))
           (ulp-difference exact (vector-ref exacts (vector-member impl roots)) repr)]
          [`(if ,c ,ift ,iff) 1]
          [(list f args-roots ...)
           (define repr (impl-info f 'otype))
           (define argapprox
             (for/list ([idx (in-list args-roots)])
               (vector-ref exacts (vector-member idx roots)))) ; arg's index mapping to exact
           (define approx (apply (impl-info f 'fl) argapprox))
           ;; ??? Should we compute `exact` against `actual` now?
           (ulp-difference exact approx repr)]))

      (vector-set! (vector-ref exacts-out expr-idx) pt-idx exact)
      (vector-set! (vector-ref approx-out expr-idx) pt-idx actual)
      (vector-set! (vector-ref true-error-out expr-idx) pt-idx true-err)
      (vector-set! (vector-ref ulp-errs expr-idx) pt-idx ulp-err)))
  (eprintf "\n\n")

  (define n 0)
  (for/list ([subexprs (in-list subexprss)])
    (for*/hash ([subexpr (in-list subexprs)])
      (begin0 (values subexpr
                      (hasheq 'ulp-errs
                              (vector->list (vector-ref ulp-errs n))
                              'exact-values
                              (vector->list (vector-ref exacts-out n))
                              'approx-values
                              (vector->list (vector-ref approx-out n))
                              'true-error-values
                              (vector->list (vector-ref true-error-out n))))
        (set! n (add1 n))))))

;; Compute the local error of every subexpression of `prog`
;; and returns the error information as an S-expr in the
;; same shape as `prog`
(define (local-error-as-tree test ctx)
  (define errs (first (compute-local-errors (list (all-subexpressions (test-input test))) ctx)))

  (define local-error
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref errs expr))
      (define err-list (hash-ref expr-info 'ulp-errs))
      (match expr
        [(list op args ...) (cons err-list (map loop args))]
        [_ (list err-list)])))

  (define exact-values
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref errs expr))
      (define exacts-list (hash-ref expr-info 'exact-values))
      (match expr
        [(list op args ...) (cons exacts-list (map loop args))]
        [_ (list exacts-list)])))

  (define approx-values
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref errs expr))
      (define exacts-list (hash-ref expr-info 'approx-values))
      (match expr
        [(list op args ...) (cons exacts-list (map loop args))]
        [_ (list exacts-list)])))

  (define true-error-values
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref errs expr))
      (define actual-list (hash-ref expr-info 'true-error-values))
      (match expr
        [(list op args ...) (cons actual-list (map loop args))]
        [_ (list actual-list)])))

  (define tree
    (let loop ([expr (prog->fpcore (test-input test) (test-context test))]
               [ulp-err local-error]
               [exact exact-values]
               [approx approx-values]
               [t-err true-error-values])
      (match expr
        [(list op args ...)
         ;; err => (List (listof Integer) List ...)
         (hasheq 'e
                 (~a op)
                 'ulps-error ;; TODO Not sure on this
                 (first ulp-err)
                 'avg-error
                 (format-bits (errors-score (first ulp-err)))
                 'exact-value
                 (map ~s (first exact))
                 'approx-value
                 (map ~s (first approx))
                 'true-error-value
                 (map ~s (first t-err))
                 'children
                 (map loop args (rest ulp-err) (rest exact) (rest approx) (rest t-err)))]
        ;; err => (List (listof Integer))
        [_
         (hasheq 'e
                 (~a expr)
                 'ulps-error ;; TODO Not sure on this
                 (first ulp-err)
                 'avg-error
                 (format-bits (errors-score (first ulp-err)))
                 'exact-value
                 (map ~s (first exact))
                 'approx-value
                 (map ~s (first approx))
                 'true-error-value
                 (map ~s (first t-err))
                 'children
                 '())])))
  tree)
