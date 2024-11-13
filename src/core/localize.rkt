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
  (define exprs-list (append* subexprss)) ; unroll subexprss
  (define spec-list (map prog->spec exprs-list))
  (define ctx-list
    (for/list ([subexpr (in-list exprs-list)])
      (struct-copy context ctx [repr (repr-of subexpr ctx)])))
  (define exact-var-name '__exact)
  (define extended
    (for/list ([ctx (in-list ctx-list)])
      (context-extend ctx exact-var-name (context-repr ctx))))
  (define compare-specs
    (for/list ([spec (in-list spec-list)])
      `(- ,spec ,exact-var-name)))

  (define expr-batch (progs->batch exprs-list))
  (define nodes (batch-nodes expr-batch))
  (define roots (batch-roots expr-batch))

  (define subexprs-fn (eval-progs-real spec-list ctx-list))
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

  (define (error-for cur-ctx cur-sepc pt exact)
    (define extended (context-append cur-ctx exact-var-name (context-repr cur-ctx)))
    (define compare-specs `(- ,cur-sepc ,exact-var-name))
    (define new-compare (eval-progs-real (list compare-specs) (list extended)))
    (define inputs (append pt (list exact)))
    (define true-errors (list->vector (apply new-compare inputs)))
    (vector-ref true-errors 0))

  (define spec-vec (list->vector spec-list))
  (define ctx-vec (list->vector ctx-list))
  (for ([(pt ex) (in-pcontext (*pcontext*))]
        [pt-idx (in-naturals)])

    (define exacts (list->vector (apply subexprs-fn pt)))
    (define actuals (apply actual-value-fn pt))
    (define index 0)
    ; (for ([i (in-naturals)]
    ;       [node (in-vector nodes)])
    ;   (eprintf "node[~a] ~a\n" i node))

    (define (parse-true-error i pt pt-idx)
      (define root (vector-ref roots i))
      (define node (vector-ref nodes root))
      (define cur-ctx (vector-ref ctx-vec i))
      (define cur-sepc (vector-ref spec-vec i))
      (define exact (vector-ref exacts i))
      (define true-error
        (match node
          [(? literal?)
          ;  (eprintf "literal?: ~a\n" node)
           ;  (define inputs (append (list exact) (vector->list (make-vector (length pt) 0))))
           ;  (define compare-fn (eval-progs-real compare-specs extended))
           ;  (define true-errors (list->vector (apply compare-fn inputs)))
           ; TODO not correct because of eval-progs-real and literals being different.
           ;  (define extended (context-append cur-ctx exact-var-name (context-repr cur-ctx)))
           ;  (define compare-specs `(- ,cur-sepc ,exact-var-name))
           ;  (define new-compare (eval-progs-real (list compare-specs) (list extended)))
           ;  (define inputs (append pt (list exact)))
           ;  (define true-errors (list->vector (apply new-compare inputs)))
           ;  (vector-ref true-errors 0)
           (if (check-for-invalid-exact exact)
               #f
               (error-for cur-ctx cur-sepc pt exact))]
          [(? variable?)
          ;  (eprintf "variable?: ~a\n" node)
           0]
          [(approx approx-spec impl)
          ;  (green "approx")
           0]
          [`(if ,c ,ift ,iff)
          ;  (eprintf "if: ~a\n" node)
           (parse-true-error (vector-member c roots) pt pt-idx)
           (parse-true-error (vector-member ift roots) pt pt-idx)
           (parse-true-error (vector-member iff roots) pt pt-idx)
           0]
          [(list f args-roots ...)
          ;  (eprintf "func[~a]: ~a, count: ~a\n" root node (length args-roots))
           (for ([idx (in-list args-roots)])
             (define node (vector-member idx roots))
            ;  (eprintf "root: ~a, idx: ~a, node: ~a\n" root idx node)
             (parse-true-error node pt pt-idx))
          ;  (eprintf "~a: ~a ~a\n" root cur-sepc exact)
           (if (check-for-invalid-exact exact)
               #f
               (error-for cur-ctx cur-sepc pt exact))]))
      (vector-set! (vector-ref true-error-out i) pt-idx true-error))

    (parse-true-error index pt pt-idx)

    (for ([cur-sepc (in-list spec-list)]
          [cur-ctx (in-list ctx-list)]
          [expr (in-list exprs-list)]
          [root (in-vector roots)]
          [exact (in-vector exacts)]
          [actual (in-vector actuals)]
          [expr-idx (in-naturals)])
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
      (vector-set! (vector-ref ulp-errs expr-idx) pt-idx ulp-err)))

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
(define (local-error-as-tree expr ctx)
  (define errs (first (compute-errors (list (all-subexpressions expr)) ctx)))

  (define local-error
    (let loop ([expr expr])
      (define expr-info (hash-ref errs expr))
      (define err-list (hash-ref expr-info 'ulp-errs))
      (match expr
        [(list op args ...) (cons err-list (map loop args))]
        [_ (list err-list)])))

  (define exact-values
    (let loop ([expr expr])
      (define expr-info (hash-ref errs expr))
      (define exacts-list (hash-ref expr-info 'exact-values))
      (match expr
        [(list op args ...) (cons exacts-list (map loop args))]
        [_ (list exacts-list)])))

  (define approx-values
    (let loop ([expr expr])
      (define expr-info (hash-ref errs expr))
      (define exacts-list (hash-ref expr-info 'approx-values))
      (match expr
        [(list op args ...) (cons exacts-list (map loop args))]
        [_ (list exacts-list)])))

  (define true-error-values
    (let loop ([expr expr])
      (define expr-info (hash-ref errs expr))
      (define actual-list (hash-ref expr-info 'true-error-values))
      (match expr
        [(list op args ...) (cons actual-list (map loop args))]
        [_ (list actual-list)])))

  (define tree
    (let loop ([expr (prog->fpcore expr ctx)]
               [ulp-err local-error]
               [exact exact-values]
               [approx approx-values]
               [t-err true-error-values])
      (match expr
        [(list op args ...)
         ;; err => (List (listof Integer) List ...)
         (hasheq 'e
                 (~a op)
                 'ulps-error
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
                 'ulps-error
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
