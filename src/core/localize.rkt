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

(define (fraction-with-odd-denominator? frac)
  (and (rational? frac) (let ([denom (denominator frac)]) (and (> denom 1) (odd? denom)))))

(define (pow-impl-args impl args)
  (define vars (impl-info impl 'vars))
  (match (impl-info impl 'spec)
    [(list 'pow b e)
     #:when (set-member? vars e)
     (define env (map cons vars args))
     (define b* (dict-ref env b b))
     (define e* (dict-ref env e e))
     (cons b* e*)]
    [_ #f]))

(define (default-cost-proc expr _)
  (let rec ([expr expr])
    (match expr
      [(literal _ _) 1]
      [(? symbol?) 1]
      ; approx node
      [(approx _ impl) (rec impl)]
      [(list 'if cond ift iff) (+ 1 (rec cond) (rec ift) (rec iff))]
      [(list (? impl-exists? impl) args ...)
       (match (pow-impl-args impl args)
         [(cons _ (literal e _))
          #:when (fraction-with-odd-denominator? e)
          +inf.0]
         [_ (apply + 1 (map rec args))])]
      [(list _ args ...) (apply + 1 (map rec args))])))

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
  (define cost-proc
    (if (*egraph-platform-cost*)
        (platform-cost-proc (*active-platform*))
        default-cost-proc))
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
    (unless (>= start-cost best-cost)
      (error 'cost-opportunity
             "Initial expression ~a is better than final expression ~a\n"
             subexpr
             (hash-ref expr->simplest subexpr)))

    ; Cost opportunity would normally be:
    ;   (start cost - start child costs) - (best cost - best child costs)
    ; However, we rearrange to handle infinities:
    (define a (apply + start-cost best-child-costs))
    (define b (apply + best-cost start-child-costs))
    (if (= a b)
        0
        (- a b))) ; This `if` statement handles `inf - inf`

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
  (define fpcore (prog->fpcore (test-input test) (test-context test)))
  (define exprs-list (all-subexpressions (test-input test)))
  (define ctx-list
    (for/list ([subexpr (in-list exprs-list)])
      (struct-copy context ctx [repr (repr-of subexpr ctx)])))

  (define ctx-vec (list->vector ctx-list))
  (define spec-list (map prog->spec exprs-list))
  (define spec-vec (list->vector spec-list))
  (define expr-batch (progs->batch exprs-list))
  (define nodes (batch-nodes expr-batch))
  (define roots (batch-roots expr-batch))

  (define subexprs-fn (eval-progs-real spec-list ctx-list))
  (define actual-value-fn (compile-progs exprs-list ctx))

  (define exacts-from-points
    (for/vector #:length (pcontext-length (*pcontext*))
                ([(pt ex) (in-pcontext (*pcontext*))])
      (list->vector (apply subexprs-fn pt))))
  (define actuals-from-points
    (for/vector #:length (pcontext-length (*pcontext*))
                ([(pt ex) (in-pcontext (*pcontext*))])
      (apply actual-value-fn pt)))

  (define (compute-abs-error actual exact ctx pt repr)
    ;; TODO compute in batches and evalutate propigated errors from rival.
    (define true-error (first (apply (eval-progs-real (list `(- ,actual ,exact)) (list ctx)) pt)))
    (define bf-true-error ((representation-repr->bf repr) true-error))
    (define abs-error (bfabs bf-true-error))
    (define abs-error-out (value->json (bigfloat->flonum abs-error) repr))
    abs-error-out)

  (define (absolute-error-for i exact pt repr)
    (define approx-spec (vector-ref spec-vec i))
    (define approx-ctx (vector-ref ctx-vec i))
    (compute-abs-error approx-spec exact approx-ctx pt repr))

  (define data-hash (make-hash))

  (for ([(pt ex) (in-pcontext (*pcontext*))]
        [exacts (in-vector exacts-from-points)]
        [actuals (in-vector actuals-from-points)])
    (for ([expr-syntax (in-list (all-subexpressions fpcore))]
          [expr (in-list exprs-list)]
          [current-ctx (in-list ctx-list)]
          [root (in-vector roots)]
          [actual (in-vector actuals)]
          [exact (in-vector exacts)])
      (define node (vector-ref nodes root))
      (define node-ulp-difference
        (match node
          [(? literal?) (ulp-difference exact actual (repr-of expr ctx))]
          [(? variable?) 1]
          [(approx _ impl) (ulp-difference exact actual (repr-of expr ctx))]
          [`(if ,c ,ift ,iff)
           (if exact
               (ulp-difference exact actual (impl-info (first (vector-ref nodes ift)) 'otype))
               (ulp-difference exact actual (impl-info (first (vector-ref nodes iff)) 'otype)))]
          [(list f args ...)
           (if (equal? (representation-type (impl-info f 'otype)) 'bool)
               1 ; return 1 for the condition node `c` in an if function.
               (ulp-difference exact actual (impl-info f 'otype)))]))
      (define abs-error
        (match node
          [(? literal?) (compute-abs-error actual exact current-ctx pt (repr-of expr ctx))]
          [(? variable?) 0]
          [(approx _ impl) (absolute-error-for impl exact pt (repr-of expr ctx))]
          [`(if ,c ,ift ,iff)
           (if exact
               (absolute-error-for ift exact pt (repr-of expr ctx))
               (absolute-error-for iff exact pt (repr-of expr ctx)))]
          [(list f args ...)
           (if (equal? (representation-type (impl-info f 'otype)) 'bool)
               exact
               (compute-abs-error actual exact current-ctx pt (repr-of expr ctx)))]))
      (hash-set! data-hash
                 root
                 (hasheq 'e
                         (~s (if (pair? expr-syntax)
                                 (first expr-syntax)
                                 expr-syntax))
                         'ulps-error
                         node-ulp-difference
                         'exact-value
                         exact
                         'actual-value
                         actual
                         'absolute-error
                         abs-error
                         'percent-accuracy
                         (* (- 1
                               (/ (ulps->bits node-ulp-difference)
                                  (representation-total-bits (repr-of expr ctx))))
                            100)))))

  (define (translate-booleans value)
    (match value
      [#t 'true]
      [#f 'false]
      [v v]))

  (define (make-hash-for root)
    ;; TODO Remove extra array from JSON output so we don't need `(map ~s (list ...)) -> (~s ...)`
    (define data (hash-ref data-hash root))
    (define expr (hash-ref data 'e))
    (define abs-error (translate-booleans (hash-ref data 'absolute-error)))
    (define ulp-error (map ~s (list (translate-booleans (ulps->bits (hash-ref data 'ulps-error))))))
    (define avg-error (format-bits (errors-score (list (hash-ref data 'ulps-error)))))
    (define exact-error (map ~s (list (translate-booleans (hash-ref data 'exact-value)))))
    (define actual-error (map ~s (list (translate-booleans (hash-ref data 'actual-value)))))
    (define precent-accurate (map ~s (list (hash-ref data 'percent-accuracy))))
    (match abs-error ; check for errors and send as string
      [(? hash? abs-error-hash) (set! abs-error (list (hash-ref abs-error-hash 'value)))]
      [error-value (set! abs-error (map ~s (list error-value)))])

    (match (vector-ref nodes root)
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
               'percent-accuracy
               precent-accurate
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
               'percent-accuracy
               precent-accurate
               'children
               '())]))

  (make-hash-for (vector-ref roots 0)))
