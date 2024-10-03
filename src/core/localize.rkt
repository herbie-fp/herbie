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
  (define diffMachine (rival-compile (list `(- e a)) '(e a) (list flonum-discretization)))

  (define errs
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define exacts-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define diffs-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define true-error-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  ; Save variables root location for later.
  (define variables (make-hash))
  ; Save literals and other leaf nodes.
  (define literals (make-hash))
  ; not sure if we need to save expr or spec
  (define var-count 0)
  (for ([subexpr (in-list exprs-list)]
        [spec (in-vector spec-vec)]
        [root (in-vector roots)])
    (match (vector-ref nodes root)
      [(? literal?) (hash-set! literals root spec)]
      [(? variable?)
       (hash-set! variables root (cons spec var-count))
       (set! var-count (+ var-count 1))]
      [_ empty]))
  (eprintf "Variables: ~a\n" variables)

  ;; Function to expand the node to find all nested variable names
  (define (find-variables args-roots)
    ; Maybe just pass the node in and recuse on that instead of looking stuff up.
    (for/list ([idx (in-list args-roots)])
      (cond
        [(hash-has-key? variables idx)
         (match-define (cons var-name point-idx) (hash-ref variables idx))
         var-name]
        [(hash-has-key? literals idx)
         #|skip we are only looking for variables|#
         empty]
        [else
         #|get node and recurse|#
         (eprintf "other: ~a\n" (vector-ref nodes idx))
         (match (vector-ref nodes idx)
           [(? literal?) empty]
           [(? variable?)
            (match-define (cons var-name point-idx) (hash-ref variables idx))
            var-name]
           [(approx _ impl) empty] ;; TODO ??? IDK
           [`(if ,c ,ift ,iff) empty]
           [(list f nested-args-roots ...) (find-variables nested-args-roots)])])))

  ; Points are ordered in the ordering that they appear in the spec.
  (for ([(pt ex) (in-pcontext (*pcontext*))]
        [pt-idx (in-naturals)])

    (define exacts (list->vector (apply subexprs-fn pt)))
    (define actuals (apply actual-value-fn pt))
    ; (define actuals-map
    ;   (for/hash ([subexpr (in-list exprs-list)]
    ;              [actual (in-vector actuals)])
    ;     (values subexpr actual)))
    ; (define exacts-map
    ;   (for/hash ([subexpr (in-list exprs-list)]
    ;              [exact (in-vector exacts)])
    ;     (values subexpr exact)))

    (define diff-map (make-hash))

    (for ([spec (in-vector spec-vec)]
          [expr (in-list exprs-list)]
          [root (in-vector roots)]
          [exact (in-vector exacts)]
          [actual (in-vector actuals)]
          [expr-idx (in-naturals)])
      (define diff
        (vector-ref (rival-apply diffMachine (list->vector `(,(bf exact) ,(bf actual)))) 0))
      (define true-err
        (match (vector-ref nodes root)
          [(? literal?) 1]
          [(? variable?) 1]
          [(approx _ impl)
           (define repr (repr-of expr ctx))
           1] ;; TODO
          [`(if ,c ,ift ,iff) 1]
          [(list f args-roots ...)
           ;; Find the index of the variables we need to substitute.
           (eprintf "EXPR[node: ~a, exact: ~a, root: ~a, sepc: ~a]\n"
                    (vector-ref nodes root)
                    exact
                    root
                    spec)
           (eprintf "pt: ~a\n" pt)
           ;; HACK flatten to fix my bad recusion.
           (define var-list (flatten (find-variables args-roots)))
           ; ??? Does variable order mater?
           ; __e double underscore to avoid conflicts with user provided
           ; variables. Could use name mangling long term.
           (define modifed-vars (append var-list `(__e)))
           (eprintf "modifed-vars: ~a\n" modifed-vars)
           (define input-expr (list `(- ,spec __e)))
           (eprintf "input-expr: ~a\n" input-expr)
           (define diffMachine (rival-compile input-expr modifed-vars (list flonum-discretization)))
           ; TODO only pass in points that match variables.
           (define input-points pt) 
           (define inputs (map bf (append input-points (list exact)))) ; TODO remove bf hack
           ;  (eprintf "inputs: ~a\n" inputs)
           (define true-error (vector-ref (rival-apply diffMachine (list->vector inputs)) 0))
           (eprintf "true-error: ~a, pt: ~a, exact ~a\n" true-error pt exact)
           true-error]))

      (define err
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
           (ulp-difference exact approx repr)]))

      (vector-set! (vector-ref exacts-out expr-idx) pt-idx exact)
      (vector-set! (vector-ref errs expr-idx) pt-idx err)
      (vector-set! (vector-ref diffs-out expr-idx) pt-idx diff)
      (vector-set! (vector-ref true-error-out expr-idx) pt-idx true-err)))

  (define n 0)
  (for/list ([subexprs (in-list subexprss)])
    (for*/hash ([subexpr (in-list subexprs)])
      (begin0 (values subexpr
                      (hasheq 'errs
                              (vector->list (vector-ref errs n))
                              'exact-values
                              (vector->list (vector-ref exacts-out n))
                              'diff-values
                              (vector->list (vector-ref diffs-out n))
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
      (define err-list (hash-ref expr-info 'errs))
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

  (define diff-values
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref errs expr))
      (define actual-list (hash-ref expr-info 'diff-values))
      (match expr
        [(list op args ...) (cons actual-list (map loop args))]
        [_ (list actual-list)])))

  (define true-error-values
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref errs expr))
      (define actual-list (hash-ref expr-info 'true-error-values))
      (match expr
        [(list op args ...) (cons actual-list (map loop args))]
        [_ (list actual-list)])))

  (define tree
    (let loop ([expr (prog->fpcore (test-input test) (test-context test))]
               [err local-error]
               [exact exact-values]
               [diff diff-values]
               [t-err true-error-values])
      (match expr
        [(list op args ...)
         ;; err => (List (listof Integer) List ...)
         (hasheq 'e
                 (~a op)
                 'avg-error
                 (format-bits (errors-score (first err)))
                 'exact-value
                 (map ~s (first exact))
                 'diff-value
                 (map ~s (first diff))
                 'true-error-value
                 (map ~s (first t-err))
                 'children
                 (map loop args (rest err) (rest exact) (rest diff) (rest t-err)))]
        ;; err => (List (listof Integer))
        [_
         (hasheq 'e
                 (~a expr)
                 'avg-error
                 (format-bits (errors-score (first err)))
                 'exact-value
                 (map ~s (first exact))
                 'diff-value
                 (map ~s (first diff))
                 'true-error-value
                 (map ~s (first t-err))
                 'children
                 '())])))
  tree)
