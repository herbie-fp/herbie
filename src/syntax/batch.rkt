#lang racket

(require "syntax.rkt"
         "types.rkt"
         "../utils/common.rkt"
         "../utils/dvector.rkt")

(provide progs->batch ; List<Expr> -> (Batch, List<Batchref>)

         expr-recurse
         expr-recurse-spec
         (struct-out batch)
         batch-empty ; Batch
         batch-empty-extend
         batch-push!
         batch-add! ; Batch -> (or Expr Batchref Expr<Batchref>) -> Batchref
         batch-copy-only!
         batch-length ; Batch -> Integer
         batch-free-vars ; Batch -> (Batchref -> Set<Var>)
         in-batch ; Batch -> Sequence<Node>
         batch-reachable ; Batch -> List<Batchref> -> (Node -> Boolean) -> List<Batchref>
         batch-exprs
         batch-recurse
         batch->jsexpr
         jsexpr->batch-exprs

         (struct-out batchref)
         deref) ; Batchref -> Expr

;; Batches store these recursive structures, flattened
(struct batch ([nodes #:mutable] [index #:mutable] vars var-reprs))

(struct batchref (batch idx) #:transparent)

;; --------------------------------- CORE BATCH FUNCTION ------------------------------------

(define (batch-empty ctx)
  (match-define (context vars _ var-reprs) ctx)
  (batch (make-dvector) (make-hash) vars var-reprs))

(define (batch-empty-extend b var repr)
  (define out
    (batch (make-dvector) (make-hash) (cons var (batch-vars b)) (cons repr (batch-var-reprs b))))
  (values out (batch-push! out var)))

(define (in-batch batch [start 0] [end #f] [step 1])
  (in-dvector (batch-nodes batch) start end step))

;; This function recurses through implementation children of expressions.
(define (expr-recurse expr f)
  (match expr
    [(approx spec impl) (approx spec (f impl))]
    [(list op) (list op)]
    [(list op arg1) (list op (f arg1))]
    [(list op arg1 arg2) (list op (f arg1) (f arg2))]
    [(list op arg1 arg2 arg3) (list op (f arg1) (f arg2) (f arg3))]
    [(list op args ...) (cons op (map f args))]
    [_ expr]))

;; This function recurses through the specification child of approximate expressions.
(define (expr-recurse-spec f expr)
  (match expr
    [(approx spec impl) (approx (f spec) impl)]
    [_ expr]))

(define (batch-length b)
  (dvector-length (batch-nodes b)))

(define (batch-push! b term)
  (define hashcons (batch-index b))
  (batchref b (hash-ref! hashcons term (lambda () (dvector-add! (batch-nodes b) term)))))

(define (batch-add! b expr)
  (define (munge prog)
    (match prog
      [(batchref b* idx*)
       (assert-batch-brf! b prog)
       idx*]
      [_ (batchref-idx (batch-push! b (expr-recurse prog munge)))]))
  (batchref b (munge expr)))

(define (deref x)
  (match-define (batchref b idx) x)
  (expr-recurse (dvector-ref (batch-nodes b) idx) (lambda (ref) (batchref b ref))))

(define (progs->batch exprs #:ctx ctx)
  (define out (batch-empty ctx))
  (for ([var (in-list (context-vars ctx))])
    (batch-push! out var))
  (define brfs
    (for/list ([expr (in-list exprs)])
      (batch-add! out expr)))
  (values out brfs))

;; batch-recurse iterates only over its children
;; A lot of parts of Herbie rely on that
(define (batch-recurse batch f)
  (define out (make-dvector (batch-length batch)))
  (define visited (make-dvector (batch-length batch) #f))
  (λ (brf)
    (assert-batch-brf! batch brf)
    (let loop ([brf brf])
      (define idx (batchref-idx brf))
      (cond
        [(and (> (dvector-capacity visited) idx) (dvector-ref visited idx)) (dvector-ref out idx)]
        [else
         (define res (f brf loop))
         (dvector-set! out idx res)
         (dvector-set! visited idx #t)
         res]))))

(define (assert-batch-brf! batch . brfs)
  (unless (andmap (compose (curry equal? batch) batchref-batch) brfs)
    (error 'assert-batch-brf! "One of batchrefs does not belong to the provided batch")))

;; Function returns indices of children nodes within a batch for given roots,
;;   where a child node is a child of a root + meets a condition - (condition node)
(define (batch-reachable batch brfs #:condition [condition (const #t)])
  ; Little check
  (apply assert-batch-brf! batch brfs)
  (define len (batch-length batch))
  (define child-mask (make-vector len #f))
  (for ([brf (in-list brfs)])
    (vector-set! child-mask (batchref-idx brf) #t))
  (for ([i (in-range (sub1 len) -1 -1)]
        [node (in-batch batch (sub1 len) -1 -1)]
        [child (in-vector child-mask (sub1 len) -1 -1)]
        #:when child)
    (cond
      [(condition node) (expr-recurse node (λ (n) (vector-set! child-mask n #t)))]
      [else (vector-set! child-mask i #f)]))
  ; Return batchrefs of children nodes in ascending order
  (for/list ([child (in-vector child-mask)]
             [i (in-naturals)]
             #:when child)
    (batchref batch i)))

;; Function constructs a vector of expressions for the given nodes of a batch
(define (batch-exprs batch #:spec-f [spec-f void])
  (batch-recurse batch
                 (lambda (brf recurse)
                   (expr-recurse-spec spec-f (expr-recurse (deref brf) recurse)))))

;; Function constructs a vector of expressions for the given nodes of a batch
(define (batch-copy-only! batch batch*)
  (batch-recurse batch*
                 (lambda (brf recurse)
                   (batch-push! batch (expr-recurse (deref brf) (compose batchref-idx recurse))))))

(define (batch-free-vars batch)
  (batch-recurse batch
                 (lambda (brf recurse)
                   (define node (deref brf))
                   (cond
                     [(symbol? node) (set node)]
                     [else
                      (define arg-free-vars (mutable-set))
                      (expr-recurse node (lambda (i) (set-union! arg-free-vars (recurse i))))
                      arg-free-vars]))))

;; Converts a batch + roots to a JSON-compatible structure
;; Returns: (hash 'nodes [...] 'roots [idx1 idx2 ...])
;; Nodes are: atoms (symbols->strings, numbers) or [op-string idx1 idx2 ...]
(define (batch->jsexpr b spec-batch brfs)
  (define batch* (batch-empty (context (batch-vars b) #f (batch-var-reprs b))))
  (for ([var (in-list (batch-vars b))])
    (batch-push! batch* var))
  (define (add-expr expr)
    (batch-push! batch*
                 (expr-recurse-spec (compose batchref-idx add-expr)
                                    (expr-recurse expr (compose batchref-idx add-expr)))))
  (define spec-f (batch-exprs spec-batch))
  (define exprs (batch-exprs b #:spec-f spec-f))
  (define brfs* (map add-expr (map exprs brfs)))
  (define nodes
    (for/list ([node (in-batch batch*)])
      (match node
        [(? symbol?) (~a node)]
        [(? number?) (~a node)]
        [(approx spec impl) (list "approx" spec impl)]
        [(list op args ...) (cons (~a op) args)]
        [_ (~a node)])))
  (hash 'nodes nodes 'roots (map batchref-idx brfs*)))

;; Converts a jsexpr batch to a single SSA-style string with O(n) size
(define (jsexpr->batch-exprs jsexpr)
  (define nodes (hash-ref jsexpr 'nodes))
  (define roots (hash-ref jsexpr 'roots))
  (define node-vec (list->vector nodes))

  ;; Pass 0: mark only the part of the graph reachable from roots.
  (define reachable? (make-vector (vector-length node-vec) #f))
  (let loop ([stack roots])
    (cond
      [(null? stack) #t]
      [(vector-ref reachable? (car stack)) (loop (cdr stack))]
      [else
       (define idx (car stack))
       (vector-set! reachable? idx #t)
       (match (vector-ref node-vec idx)
         [(list _ args ...) (loop (append args (cdr stack)))]
         [_ (loop (cdr stack))])]))

  ;; Pass 1: count references to each node
  (define ref-counts (make-vector (vector-length node-vec) 0))
  (for ([root roots])
    (vector-set! ref-counts root (+ 1 (vector-ref ref-counts root))))
  (for ([i (in-naturals)]
        [node (in-vector node-vec)]
        [reachable (in-vector reachable?)]
        #:when reachable)
    (match node
      [(list _ args ...)
       (for ([arg (in-list args)])
         (vector-set! ref-counts arg (+ 1 (vector-ref ref-counts arg))))]
      ;; Never dedup constants & variables
      [_ (vector-set! ref-counts i -inf.0)]))

  ;; Pass 2: build expressions, using %N for multiply-referenced nodes
  (define exprs (make-vector (vector-length node-vec) #f))
  (for ([i (in-naturals)]
        [node (in-vector node-vec)]
        [reachable (in-vector reachable?)]
        #:when reachable)
    (vector-set! exprs
                 i
                 (match node
                   [(list op args ...)
                    (format "(~a ~a)"
                            op
                            (string-join (for/list ([arg (in-list args)])
                                           (if (> (vector-ref ref-counts arg) 1)
                                               (format "%~a" arg)
                                               (vector-ref exprs arg)))))]
                   [_ (~a node)])))

  ;; Output: one line per multi-ref node, then root expressions
  (define bindings
    (for/list ([i (in-naturals)]
               [reachable (in-vector reachable?)]
               #:when reachable
               #:when (> (vector-ref ref-counts i) 1))
      (format "%~a = ~a" i (vector-ref exprs i))))
  (define return-exprs
    (for/list ([root roots])
      (if (> (vector-ref ref-counts root) 1)
          (format "%~a" root)
          (vector-ref exprs root))))
  (string-join (append bindings return-exprs) "\n"))
;; --------------------------------- TESTS ---------------------------------------

; Tests for progs->batch and batch-exprs
(module+ test
  (require rackunit)
  (define test-empty-ctx (context '() #f '()))
  (define (test-munge-unmunge expr [expected expr] #:spec-f [spec-f (void)])
    (define-values (batch brfs) (progs->batch (list expr) #:ctx test-empty-ctx))
    (check-equal? (list expected) (map (batch-exprs batch #:spec-f spec-f) brfs)))

  (define (f64 x)
    (literal x 'binary64))

  (test-munge-unmunge '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))))
  (test-munge-unmunge
   '(+ 1 (neg (* 1/2 (+ (exp (/ (sin 3) (cos 3))) (/ 1 (exp (/ (sin 3) (cos 3)))))))))
  (test-munge-unmunge '(cbrt x))
  (test-munge-unmunge (list 'x))
  (define spec-expr '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))))
  (define-values (spec-batch spec-brfs) (progs->batch (list spec-expr) #:ctx test-empty-ctx))
  (define approx-expr
    `(+.f64 (sin.f64 ,(approx (first spec-brfs)
                              '(+.f64 ,(f64 3) (*.f64 ,(f64 25) (sin.f64 ,(f64 6))))))
            ,(f64 4)))
  (define expected-approx
    `(+.f64 (sin.f64 ,(approx spec-expr '(+.f64 ,(f64 3) (*.f64 ,(f64 25) (sin.f64 ,(f64 6))))))
            ,(f64 4)))
  (test-munge-unmunge approx-expr expected-approx #:spec-f (batch-exprs spec-batch)))

; Tests for remove-zombie-nodes
(module+ test
  (require rackunit)
  (define (zombie-test #:specs [specs '()] #:nodes nodes #:expected expected #:roots roots)
    (define spec-batch (batch-empty test-empty-ctx))
    (define spec-brfs (map (curry batch-add! spec-batch) specs))
    (define (segregate nodes)
      (apply create-dvector
             (for/list ([node (in-dvector nodes)])
               (match node
                 [(approx spec impl) (approx (list-ref spec-brfs spec) impl)]
                 [_ node]))))
    (define in-batch (batch (segregate nodes) (make-hash) '() '()))
    (define brfs (map (curry batchref in-batch) roots))
    (define out-batch (batch-empty test-empty-ctx))
    (define copy-f (batch-copy-only! out-batch in-batch))
    (define brfs* (map copy-f brfs))
    (define spec-f (batch-exprs spec-batch))
    (check-equal? (map (batch-exprs out-batch #:spec-f spec-f) brfs*)
                  (map (batch-exprs in-batch #:spec-f spec-f) brfs))
    (check-equal? (dvector->vector (batch-nodes out-batch)) (dvector->vector (segregate expected))))

  (zombie-test #:expected (create-dvector 2 0 '(sqrt 1) '(pow 0 2))
               #:nodes (create-dvector 0 1 '(sqrt 0) 2 '(pow 3 2))
               #:roots (list 4))
  (zombie-test #:expected (create-dvector 0 '(sqrt 0) '(exp 1))
               #:nodes (create-dvector 0 6 '(pow 0 1) '(* 2 0) '(sqrt 0) '(exp 4))
               #:roots (list 5))
  (zombie-test #:expected (create-dvector 0 1/2 '(+ 0 1))
               #:nodes (create-dvector 0 1/2 '(+ 0 1) '(* 2 0))
               #:roots (list 2))

  (zombie-test #:specs (list '(exp 1))
               #:expected (create-dvector 0 (approx 0 0))
               #:nodes (create-dvector 0 1/2 '(+ 0 1) '(* 2 0) '(exp 1) (approx 0 0))
               #:roots (list 5))
  (zombie-test #:specs (list '(* x x))
               #:expected (create-dvector 1/2 2 (approx 0 1) '(pow 0 2))
               #:nodes (create-dvector 'x 2 1/2 '(sqrt 1) '(cbrt 1) '(* 0 0) (approx 0 1) '(pow 2 6))
               #:roots (list 7))
  (zombie-test #:specs (list '(* x x))
               #:expected (create-dvector 1/2 2 (approx 0 1) '(pow 0 2) '(sqrt 1))
               #:nodes (create-dvector 'x 2 1/2 '(sqrt 1) '(cbrt 1) '(* 0 0) (approx 0 1) '(pow 2 6))
               #:roots (list 7 3)))

; Tests for batch->jsexpr and jsexpr->batch-exprs
(module+ test
  (require rackunit)
  (define (test-json-tostring expr expected)
    (define-values (batch brfs) (progs->batch (list expr) #:ctx test-empty-ctx))
    (define jsexpr (batch->jsexpr batch batch brfs))
    (define str (jsexpr->batch-exprs jsexpr))
    (check-equal? str expected))

  ; No sharing - just the expression
  (test-json-tostring '(+ x y) "(+ x y)")
  ; Shared subexpressions get their own bindings
  (test-json-tostring '(* 1/2 (+ (exp x) (neg (/ 1 (exp x)))))
                      "%2 = (exp x)\n(* 1/2 (+ %2 (neg (/ 1 %2))))")
  ; Shared constants/variables are inlined
  (test-json-tostring '(sqrt (+ (* x x) (* y y))) "(sqrt (+ (* x x) (* y y)))"))
