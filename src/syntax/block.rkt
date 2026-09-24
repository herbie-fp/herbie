#lang racket

(require "syntax.rkt"
         "types.rkt"
         "../utils/common.rkt"
         "../utils/dvector.rkt")

(provide progs->block ; List<Expr> -> (Block, List<Val>)

         expr-recurse
         expr-recurse-spec
         (struct-out block)
         block-empty ; Block
         block-empty-extend
         block-push!
         block-add! ; Block -> (or Expr Val Expr<Val>) -> Val
         block-copy-only!
         block-length ; Block -> Integer
         block-free-vars ; Block -> (Val -> Set<Var>)
         in-block ; Block -> Sequence<Node>
         block-reachable ; Block -> List<Val> -> (Node -> Boolean) -> List<Val>
         block-exprs
         block-recurse
         block->jsexpr
         jsexpr->block-exprs

         (struct-out val)
         val-def) ; Val -> Expr

;; Blocks store these recursive structures, flattened
(struct block ([nodes #:mutable] [index #:mutable] vars var-reprs))

(struct val (block idx) #:transparent)

;; --------------------------------- CORE BLOCK FUNCTION ------------------------------------

(define (block-empty ctx)
  (match-define (context vars _ var-reprs) ctx)
  (block (make-dvector) (make-hash) vars var-reprs))

(define (block-empty-extend b var repr)
  (define out
    (block (make-dvector) (make-hash) (cons var (block-vars b)) (cons repr (block-var-reprs b))))
  (values out (block-push! out var)))

(define (in-block block [start 0] [end #f] [step 1])
  (in-dvector (block-nodes block) start end step))

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

(define (block-length b)
  (dvector-length (block-nodes b)))

(define (block-push! b term)
  (define hashcons (block-index b))
  (val b (hash-ref! hashcons term (lambda () (dvector-add! (block-nodes b) term)))))

(define (block-add! b expr)
  (define (munge prog)
    (match prog
      [(val b* idx*)
       (assert-block-v! b prog)
       idx*]
      [_ (val-idx (block-push! b (expr-recurse prog munge)))]))
  (val b (munge expr)))

(define (val-def x)
  (match-define (val b idx) x)
  (expr-recurse (dvector-ref (block-nodes b) idx) (lambda (ref) (val b ref))))

(define (progs->block exprs #:ctx ctx)
  (define out (block-empty ctx))
  (for ([var (in-list (context-vars ctx))])
    (block-push! out var))
  (define vs
    (for/list ([expr (in-list exprs)])
      (block-add! out expr)))
  (values out vs))

;; block-recurse iterates only over its children
;; A lot of parts of Herbie rely on that
(define (block-recurse block f)
  (define out (make-dvector (block-length block)))
  (define visited (make-dvector (block-length block) #f))
  (λ (v)
    (assert-block-v! block v)
    (let loop ([v v])
      (define idx (val-idx v))
      (cond
        [(and (> (dvector-capacity visited) idx) (dvector-ref visited idx)) (dvector-ref out idx)]
        [else
         (define res (f v loop))
         (dvector-set! out idx res)
         (dvector-set! visited idx #t)
         res]))))

(define (assert-block-v! block . vs)
  (unless (andmap (compose (curry equal? block) val-block) vs)
    (error 'assert-block-v! "One of vals does not belong to the provided block")))

;; Function returns indices of children nodes within a block for given roots,
;;   where a child node is a child of a root + meets a condition - (condition node)
(define (block-reachable block vs #:condition [condition (const #t)])
  ; Little check
  (apply assert-block-v! block vs)
  (define len (block-length block))
  (define child-mask (make-vector len #f))
  (for ([v (in-list vs)])
    (vector-set! child-mask (val-idx v) #t))
  (for ([i (in-range (sub1 len) -1 -1)]
        [node (in-block block (sub1 len) -1 -1)]
        [child (in-vector child-mask (sub1 len) -1 -1)]
        #:when child)
    (cond
      [(condition node) (expr-recurse node (λ (n) (vector-set! child-mask n #t)))]
      [else (vector-set! child-mask i #f)]))
  ; Return vals of children nodes in ascending order
  (for/list ([child (in-vector child-mask)]
             [i (in-naturals)]
             #:when child)
    (val block i)))

;; Function constructs a vector of expressions for the given nodes of a block
(define (block-exprs block #:spec-f [spec-f void])
  (block-recurse block
                 (lambda (v recurse) (expr-recurse-spec spec-f (expr-recurse (val-def v) recurse)))))

;; Function constructs a vector of expressions for the given nodes of a block
(define (block-copy-only! block block*)
  (block-recurse block*
                 (lambda (v recurse)
                   (block-push! block (expr-recurse (val-def v) (compose val-idx recurse))))))

(define (block-free-vars block)
  (block-recurse block
                 (lambda (v recurse)
                   (define node (val-def v))
                   (cond
                     [(symbol? node) (set node)]
                     [else
                      (define arg-free-vars (mutable-set))
                      (expr-recurse node (lambda (i) (set-union! arg-free-vars (recurse i))))
                      arg-free-vars]))))

;; Converts a block + roots to a JSON-compatible structure
;; Returns: (hash 'nodes [...] 'roots [idx1 idx2 ...])
;; Nodes are: atoms (symbols->strings, numbers) or [op-string idx1 idx2 ...]
(define (block->jsexpr b spec-block vs)
  (define block* (block-empty (context (block-vars b) #f (block-var-reprs b))))
  (for ([var (in-list (block-vars b))])
    (block-push! block* var))
  (define (add-expr expr)
    (block-push! block*
                 (expr-recurse-spec (compose val-idx add-expr)
                                    (expr-recurse expr (compose val-idx add-expr)))))
  (define spec-f (block-exprs spec-block))
  (define exprs (block-exprs b #:spec-f spec-f))
  (define vs* (map add-expr (map exprs vs)))
  (define nodes
    (for/list ([node (in-block block*)])
      (match node
        [(? symbol?) (~a node)]
        [(? number?) (~a node)]
        [(approx spec impl) (list "approx" spec impl)]
        [(list op args ...) (cons (~a op) args)]
        [_ (~a node)])))
  (hash 'nodes nodes 'roots (map val-idx vs*)))

;; Converts a jsexpr block to a single SSA-style string with O(n) size
(define (jsexpr->block-exprs jsexpr)
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

; Tests for progs->block and block-exprs
(module+ test
  (require rackunit)
  (define test-empty-ctx (context '() #f '()))
  (define (test-munge-unmunge expr [expected expr] #:spec-f [spec-f (void)])
    (define-values (block vs) (progs->block (list expr) #:ctx test-empty-ctx))
    (check-equal? (list expected) (map (block-exprs block #:spec-f spec-f) vs)))

  (define (f64 x)
    (literal x 'binary64))

  (test-munge-unmunge '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))))
  (test-munge-unmunge
   '(+ 1 (neg (* 1/2 (+ (exp (/ (sin 3) (cos 3))) (/ 1 (exp (/ (sin 3) (cos 3)))))))))
  (test-munge-unmunge '(cbrt x))
  (test-munge-unmunge (list 'x))
  (define spec-expr '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))))
  (define-values (spec-block spec-vs) (progs->block (list spec-expr) #:ctx test-empty-ctx))
  (define approx-expr
    `(+.f64 (sin.f64 ,(approx (first spec-vs) '(+.f64 ,(f64 3) (*.f64 ,(f64 25) (sin.f64 ,(f64 6))))))
            ,(f64 4)))
  (define expected-approx
    `(+.f64 (sin.f64 ,(approx spec-expr '(+.f64 ,(f64 3) (*.f64 ,(f64 25) (sin.f64 ,(f64 6))))))
            ,(f64 4)))
  (test-munge-unmunge approx-expr expected-approx #:spec-f (block-exprs spec-block)))

; Tests for remove-zombie-nodes
(module+ test
  (require rackunit)
  (define (zombie-test #:specs [specs '()] #:nodes nodes #:expected expected #:roots roots)
    (define spec-block (block-empty test-empty-ctx))
    (define spec-vs (map (curry block-add! spec-block) specs))
    (define (segregate nodes)
      (apply create-dvector
             (for/list ([node (in-dvector nodes)])
               (match node
                 [(approx spec impl) (approx (list-ref spec-vs spec) impl)]
                 [_ node]))))
    (define in-block (block (segregate nodes) (make-hash) '() '()))
    (define vs (map (curry val in-block) roots))
    (define out-block (block-empty test-empty-ctx))
    (define copy-f (block-copy-only! out-block in-block))
    (define vs* (map copy-f vs))
    (define spec-f (block-exprs spec-block))
    (check-equal? (map (block-exprs out-block #:spec-f spec-f) vs*)
                  (map (block-exprs in-block #:spec-f spec-f) vs))
    (check-equal? (dvector->vector (block-nodes out-block)) (dvector->vector (segregate expected))))

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

; Tests for block->jsexpr and jsexpr->block-exprs
(module+ test
  (require rackunit)
  (define (test-json-tostring expr expected)
    (define-values (block vs) (progs->block (list expr) #:ctx test-empty-ctx))
    (define jsexpr (block->jsexpr block block vs))
    (define str (jsexpr->block-exprs jsexpr))
    (check-equal? str expected))

  ; No sharing - just the expression
  (test-json-tostring '(+ x y) "(+ x y)")
  ; Shared subexpressions get their own bindings
  (test-json-tostring '(* 1/2 (+ (exp x) (neg (/ 1 (exp x)))))
                      "%2 = (exp x)\n(* 1/2 (+ %2 (neg (/ 1 %2))))")
  ; Shared constants/variables are inlined
  (test-json-tostring '(sqrt (+ (* x x) (* y y))) "(sqrt (+ (* x x) (* y y)))"))
