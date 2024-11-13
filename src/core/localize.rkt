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
         "../core/rival.rkt"
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

  (define errs
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
      (vector-set! (vector-ref errs expr-idx) pt-idx err)))

  (define n 0)
  (for/list ([subexprs (in-list subexprss)])
    (for*/hash ([subexpr (in-list subexprs)])
      (begin0 (values subexpr (vector->list (vector-ref errs n)))
        (set! n (add1 n))))))

(define (check-for-invalid-exact input)
  (match input
    ['+inf.0 #t]
    ['-inf.0 #t]
    ['+nan.0 #t]
    [value (or (bfnan? value) (boolean? value))]))

; Compute local error or each sampled point at each node in `prog`.
(define (compute-errors subexprss ctx)
  ;; We compute the actual (float) result
  (define exprs-list (append* subexprss)) ; unroll subexprss
  (define actual-value-fn (compile-progs exprs-list ctx))

  ;; And the real result
  (define spec-list (map prog->spec exprs-list))
  (define ctx-list
    (for/list ([subexpr (in-list exprs-list)])
      (struct-copy context ctx [repr (repr-of subexpr ctx)])))
  (define subexprs-fn (eval-progs-real spec-list ctx-list))

  ;; And the absolute difference between the two
  (define exact-var-names
    (for/list ([expr (in-list exprs-list)] [n (in-naturals)])
      ;; HACK: should generate unique, not just rare, symbol
      (string->symbol (format "-exact-for-~a" n))))
  (define delta-ctx
    (context (append (context-vars ctx) exact-var-names)
             (get-representation 'binary64)
             (append (context-var-reprs ctx)
                     (for/list ([expr (in-list exprs-list)])
                       (repr-of expr ctx)))))
  (define compare-specs
    (for/list ([spec (in-list spec-list)]
               [expr (in-list exprs-list)]
               [var (in-list exact-var-names)])
      (cond
        [(number? spec)
         0] ; HACK: unclear why numbers don't work in Rival but :shrug:
        [(equal? (representation-type (repr-of expr ctx)) 'boolean)
         0] ; HACK: just ignore differences in booleans
        [else
         `(fabs (- ,spec ,var))])))
  (define delta-fn (eval-progs-real compare-specs (map (const delta-ctx) compare-specs)))

  (define expr-batch (progs->batch exprs-list))
  (define nodes (batch-nodes expr-batch))
  (define roots (batch-roots expr-batch))

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

  (define spec-vec (list->vector spec-list))
  (define ctx-vec (list->vector ctx-list))
  (for ([(pt ex) (in-pcontext (*pcontext*))]
        [pt-idx (in-naturals)])

    (define exacts (list->vector (apply subexprs-fn pt)))
    (define actuals (apply actual-value-fn pt))

    (define pt* (append pt (vector->list actuals)))
    (define deltas (list->vector (apply delta-fn pt*)))

    (for [[spec (in-list spec-list)]
          [expr (in-list exprs-list)]
          [root (in-vector roots)]
          [exact (in-vector exacts)]
          [actual (in-vector actuals)]
          [delta (in-vector deltas)]
          [expr-idx (in-naturals)]]
      (define ulp-err
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
      (vector-set! (vector-ref approx-out expr-idx) pt-idx actual)
      (vector-set! (vector-ref ulp-errs expr-idx) pt-idx ulp-err)
      (vector-set! (vector-ref true-error-out expr-idx) pt-idx delta)))

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
                              'absolute-error
                              (vector->list (vector-ref true-error-out n))))
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
(define (local-error-as-tree expr ctx)
  (define data-hash (first (compute-errors (list (all-subexpressions expr)) ctx)))

  (define (translate-booleans value)
    (match value
      [#t 'true]
      [#f 'false]
      [v v]))

  (define (make-hash-for expr)
    (define data (hash-ref data-hash expr))
    (define abs-error (hash-ref data 'absolute-error))
    (define ulp-error (map ~s (list (ulps->bits (hash-ref data 'ulps-error)))))
    (define avg-error (format-bits (errors-score (list (hash-ref data 'ulps-error)))))
    (define exact-error (map ~s (list (translate-booleans (hash-ref data 'exact-value)))))
    (define actual-error (map ~s (list (translate-booleans (hash-ref data 'actual-value)))))
    (match expr
      [(list op args ...)
       (hasheq 'e
               expr
               'ulps-error
               ulp-error
               'avg-error
               avg-error
               'exact-value
               exact-error
               'actual-value
               actual-error
               'absolute-error
               abs-error
               'children
               (map make-hash-for args))]
      [_
       (hasheq 'e
               expr
               'ulps-error
               ulp-error
               'avg-error
               avg-error
               'exact-value
               exact-error
               'actual-value
               actual-error
               'absolute-error
               abs-error
               'children
               '())])

  (make-hash-for expr))
