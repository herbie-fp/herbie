#lang racket

(require math/bigfloat)
(require "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "rules.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
         "../syntax/platform.rkt"
         "../syntax/read.rkt"
         "../syntax/read.rkt"
         "rival.rkt"
         "points.rkt"
         "programs.rkt"
         "sampling.rkt"
         "simplify.rkt"
         "egg-herbie.rkt"
         "compiler.rkt"
         "batch.rkt")

(module+ test
  (require rackunit
           "../syntax/load-plugin.rkt"
           "../syntax/syntax.rkt"
           "../syntax/sugar.rkt")
  (load-herbie-builtins))

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

  (for/list ([_ (in-list exprs)]
             [errs (in-list errss)])
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
  (define actual-value-fn (compile-progs exprs-list ctx))

  (define errs
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define exacts-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define actuals-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (for ([(pt ex) (in-pcontext (*pcontext*))]
        [pt-idx (in-naturals)])

    (define exacts (list->vector (apply subexprs-fn pt)))
    (define actuals (apply actual-value-fn pt))

    (for ([expr (in-list exprs-list)]
          [root (in-vector roots)]
          [exact (in-vector exacts)]
          [actual (in-vector actuals)]
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
      (vector-set! (vector-ref errs expr-idx) pt-idx err)
      (vector-set! (vector-ref actuals-out expr-idx) pt-idx actual)))

  (define n 0)
  (for/list ([subexprs (in-list subexprss)])
    (for*/hash ([subexpr (in-list subexprs)])
      (begin0 (values subexpr (vector->list (vector-ref errs n)))
        (set! n (add1 n))))))

(module+ test
  (define ctx (make-debug-context '(x y)))
  (define spec `(- (sqrt (+ x 1)) (sqrt y)))
  (define pt `(1e-100 1e-100))
  (define exact 1e-50)
  (check-equal? (first (apply (eval-progs-real (list `(- ,spec ,exact)) (list ctx)) pt)) 1.0))

;; Compute the local error of every subexpression of `prog`
;; and returns the error information as an S-expr in the
;; same shape as `prog`
(define (local-error-as-tree test ctx)
  (define subexprss (list (all-subexpressions (test-input test))))
  (define exprs-list (append* subexprss)) ; unroll subexprss
  (define ctx-list
    (for/list ([subexpr (in-list exprs-list)])
      (struct-copy context ctx [repr (repr-of subexpr ctx)])))

  (define repr-hash
    (make-immutable-hash (map (lambda (e ctx) (cons e (context-repr ctx))) exprs-list ctx-list)))

  (define ctx-vec (list->vector ctx-list))
  (define spec-list (map prog->spec exprs-list))
  (define spec-vec (list->vector spec-list))
  (define expr-batch (progs->batch exprs-list))
  (define nodes (batch-nodes expr-batch))
  (define roots (batch-roots expr-batch))

  ; TODO don't ignore the status code from make-real-compiler in eval-progs-real
  (define subexprs-fn (eval-progs-real spec-list ctx-list))
  (define actual-value-fn (compile-progs exprs-list ctx))

  ;; TODO clean up allocations
  (define local-errors
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define abs-error-outs
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define exacts-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  (define actuals-out
    (for/vector #:length (vector-length roots)
                ([node (in-vector roots)])
      (make-vector (pcontext-length (*pcontext*)))))

  ;; TODO combine loops over pcontext for/vectors?
  (define exacts-from-points
    (for/vector #:length (pcontext-length (*pcontext*))
                ([(pt ex) (in-pcontext (*pcontext*))])
      (list->vector (apply subexprs-fn pt))))
  (define actuals-from-points
    (for/vector #:length (pcontext-length (*pcontext*))
                ([(pt ex) (in-pcontext (*pcontext*))])
      (apply actual-value-fn pt)))

  (define (compute-true-error spec exact ctx pt)
    ;; TODO compute in batches and evalutate propigated errors from rival.
    (first (apply (eval-progs-real (list `(- ,spec ,exact)) (list ctx)) pt)))

  (define (absolute-error-for i exact pt)
    (define approx-spec (vector-ref spec-vec i))
    (define approx-ctx (vector-ref ctx-vec i))
    (compute-true-error approx-spec exact approx-ctx pt))

  (define previous_node_if? #f)

  (for ([(pt ex) (in-pcontext (*pcontext*))]
        [exacts (in-vector exacts-from-points)]
        [actuals (in-vector actuals-from-points)]
        [pt-idx (in-naturals)])
    (for ([expr (in-list exprs-list)]
          [current-spec (in-vector spec-vec)]
          [current-ctx (in-list ctx-list)]
          [root (in-vector roots)]
          [actual (in-vector actuals)]
          [exact (in-vector exacts)]
          [expr-idx (in-naturals)])
      (define local-error
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
               (vector-ref exacts (vector-member idx roots))))
           (define approx (apply (impl-info f 'fl) argapprox))
           (ulp-difference exact approx repr)]))
      (define true-error
        (match (vector-ref nodes root)
          [(? literal?) (compute-true-error current-spec exact current-ctx pt)]
          [(? variable?) 0]
          [(approx _ impl) (absolute-error-for impl exact pt)]
          [`(if ,c ,ift ,iff)
           (set! previous_node_if? #t)
           (if exact
               (absolute-error-for ift exact pt)
               (absolute-error-for iff exact pt))]
          [(list f args ...)
           (define local previous_node_if?)
           (when previous_node_if?
             (set! previous_node_if? #f))
           (if local
               exact
               (compute-true-error current-spec exact current-ctx pt))]))
      (define current-repr (hash-ref repr-hash expr))
      (define abs-error (bfabs ((representation-repr->bf current-repr) true-error)))
      (define abs-error-out (value->json (bigfloat->flonum abs-error) current-repr))
      (vector-set! (vector-ref exacts-out expr-idx) pt-idx exact)
      (vector-set! (vector-ref local-errors expr-idx) pt-idx local-error)
      (vector-set! (vector-ref actuals-out expr-idx) pt-idx actual)
      (vector-set! (vector-ref abs-error-outs expr-idx) pt-idx abs-error-out)))

  (define n 0)
  (define err-tree
    (first (for/list ([subexprs (in-list subexprss)])
             (for*/hash ([subexpr (in-list subexprs)])
               (begin0 (values subexpr
                               (hasheq 'local-errors
                                       (vector->list (vector-ref local-errors n))
                                       'exact-values
                                       (vector->list (vector-ref exacts-out n))
                                       'actual-values
                                       (vector->list (vector-ref actuals-out n))
                                       'absolute-error
                                       (vector->list (vector-ref abs-error-outs n))))
                 (set! n (add1 n)))))))

  (define local-error-values
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref err-tree expr))
      (define err-list (hash-ref expr-info 'local-errors))
      (match expr
        [(list op args ...) (cons err-list (map loop args))]
        [_ (list err-list)])))

  (define exact-values
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref err-tree expr))
      (define exacts-list (hash-ref expr-info 'exact-values))
      (match expr
        [(list op args ...) (cons exacts-list (map loop args))]
        [_ (list exacts-list)])))

  (define actual-values
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref err-tree expr))
      (define actual-list (hash-ref expr-info 'actual-values))
      (match expr
        [(list op args ...) (cons actual-list (map loop args))]
        [_ (list actual-list)])))

  (define true-error-values
    (let loop ([expr (test-input test)])
      (define expr-info (hash-ref err-tree expr))
      (define true-error-list (hash-ref expr-info 'absolute-error))
      (match expr
        [(list op args ...) (cons true-error-list (map loop args))]
        [_ (list true-error-list)])))

  (define tree
    (let loop ([expr (prog->fpcore (test-input test) (test-context test))]
               [local-error local-error-values]
               [exact exact-values]
               [actual actual-values]
               [true-error true-error-values])
      (match expr
        [(list op args ...)
         ;; err => (List (listof Integer) List ...)
         (hasheq 'e
                 (~a op)
                 'ulps-error
                 (map ~s (first local-error))
                 'avg-error
                 (format-bits (errors-score (first local-error)))
                 'exact-value
                 (map ~s (first exact))
                 'actual-value
                 (map ~s (first actual))
                 'absolute-error
                 (map ~s (first true-error))
                 'children
                 (map loop args (rest local-error) (rest exact) (rest actual) (rest true-error)))]
        ;; err => (List (listof Integer))
        [_
         (hasheq 'e
                 (~a expr)
                 'ulps-error
                 (map ~s (first local-error))
                 'avg-error
                 (format-bits (errors-score (first local-error)))
                 'exact-value
                 (map ~s (first exact))
                 'actual-value
                 (map ~s (first actual))
                 'absolute-error
                 (map ~s (first true-error))
                 'children
                 '())])))
  tree)
