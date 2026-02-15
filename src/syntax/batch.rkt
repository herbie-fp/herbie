#lang racket

(require "syntax.rkt"
         "../utils/common.rkt"
         "../utils/dvector.rkt")

(provide progs->batch ; List<Expr> -> (Batch, List<Batchref>)

         expr-recurse
         (struct-out batch)
         batch-empty ; Batch
         batch-push!
         batch-add! ; Batch -> (or Expr Batchref Expr<Batchref>) -> Batchref
         batch-copy-only!
         batch-length ; Batch -> Integer
         batch-tree-size ; Batch -> List<Batchref> -> Integer
         batch-free-vars ; Batch -> (Batchref -> Set<Var>)
         in-batch ; Batch -> Sequence<Node>
         batch-reachable ; Batch -> List<Batchref> -> (Node -> Boolean) -> List<Batchref>
         batch-exprs
         batch-recurse
         batch-get-nodes
         batch->jsexpr
         jsexpr->batch-exprs

         (struct-out batchref)
         batchref<?
         deref) ; Batchref -> Expr

;; Batches store these recursive structures, flattened
(struct batch ([nodes #:mutable] [index #:mutable]))

(struct batchref (batch idx) #:transparent)

;; --------------------------------- CORE BATCH FUNCTION ------------------------------------

(define (batch-empty)
  (batch (make-dvector) (make-hash)))

(define (in-batch batch [start 0] [end #f] [step 1])
  (in-dvector (batch-nodes batch) start end step))

(define (batch-get-nodes b)
  (dvector->vector (batch-nodes b)))

(define (batchref<? brf1 brf2)
  (< (batchref-idx brf1) (batchref-idx brf2)))

;; This function defines the recursive structure of expressions
(define (expr-recurse expr f)
  (match expr
    [(approx spec impl) (approx (f spec) (f impl))]
    [(hole precision spec) (hole precision (f spec))]
    [(list op) (list op)]
    [(list op arg1) (list op (f arg1))]
    [(list op arg1 arg2) (list op (f arg1) (f arg2))]
    [(list op arg1 arg2 arg3) (list op (f arg1) (f arg2) (f arg3))]
    [(list op args ...) (cons op (map f args))]
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
       (unless (equal? b b*)
         (error 'batch-add! "Batchref belongs to a different batch"))
       idx*]
      [_ (batchref-idx (batch-push! b (expr-recurse prog munge)))]))
  (batchref b (munge expr)))

(define (deref x)
  (match-define (batchref b idx) x)
  (expr-recurse (dvector-ref (batch-nodes b) idx) (lambda (ref) (batchref b ref))))

(define (progs->batch exprs #:vars [vars '()])
  (define out (batch-empty))
  (for ([var (in-list vars)])
    (batch-push! out var))
  (define brfs
    (for/list ([expr (in-list exprs)])
      (batch-add! out expr)))
  (values out brfs))

;; batch-recurse iterates only over its children
;; A lot of parts of Herbie rely on that
;; batch-recurse panics if user provides different arguments for the same calls
;; TODO: what if user provides the same object but it is changed inside? "equal?" may not distinguish it
(define (batch-recurse batch f)
  (define out (make-dvector (batch-length batch)))
  (define visited (make-dvector (batch-length batch) #f))
  (λ (brf . args)
    (match-define (batchref b idx) brf)
    (unless (eq? b batch)
      (error 'batch-recurse "Batchref belongs to a different batch"))
    (let loop ([brf (batchref batch idx)]
               [args args])
      (define idx (batchref-idx brf))
      (cond
        [(and (> (dvector-capacity visited) idx) (dvector-ref visited idx))
         (unless (equal? args (dvector-ref visited idx))
           (error 'batch-recurse
                  "Cache violation for ~a, cached with ~a, provided with ~a"
                  brf
                  (dvector-ref visited idx)
                  args))
         (dvector-ref out idx)]
        [else
         (define res (apply f brf (λ (brf . args) (loop brf args)) args))
         (dvector-set! out idx res)
         (dvector-set! visited idx args)
         res]))))

(define (brfs-belong-to-batch? batch brfs)
  (unless (andmap (compose (curry equal? batch) batchref-batch) brfs)
    (error 'brfs-belong-to-batch? "One of batchrefs does not belong to the provided batch")))

;; Function returns indices of children nodes within a batch for given roots,
;;   where a child node is a child of a root + meets a condition - (condition node)
(define (batch-reachable batch brfs #:condition [condition (const #t)])
  ; Little check
  (brfs-belong-to-batch? batch brfs)
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
(define (batch-exprs batch)
  (batch-recurse batch (lambda (brf recurse) (expr-recurse (deref brf) recurse))))

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
                     [(approx? node) (recurse (approx-impl node))]
                     [else
                      (define arg-free-vars (mutable-set))
                      (expr-recurse node (lambda (i) (set-union! arg-free-vars (recurse i))))
                      arg-free-vars]))))

(define (batch-tree-size batch brfs)
  (define counts
    (batch-recurse batch
                   (lambda (brf recurse)
                     (define args (reap [sow] (expr-recurse (deref brf) sow)))
                     (apply + 1 (map recurse args)))))
  (apply + (map counts brfs)))

;; Converts a batch + roots to a JSON-compatible structure
;; Returns: (hash 'nodes [...] 'roots [idx1 idx2 ...])
;; Nodes are: atoms (symbols->strings, numbers) or [op-string idx1 idx2 ...]
(define (batch->jsexpr b brfs)
  (define batch* (batch-empty))
  (define copy-f (batch-copy-only! batch* b))
  (define brfs* (map copy-f brfs))
  (define nodes
    (for/list ([node (in-batch batch*)])
      (match node
        [(? symbol?) (~a node)]
        [(? number?) (~a node)]
        [(approx spec impl) (list "approx" spec impl)]
        [(hole precision spec) (list "hole" (~a precision) spec)]
        [(list op args ...) (cons (~a op) args)]
        [_ (~a node)])))
  (hash 'nodes nodes 'roots (map batchref-idx brfs*)))

;; Converts a jsexpr batch to a single SSA-style string with O(n) size
(define (jsexpr->batch-exprs jsexpr)
  (define nodes (hash-ref jsexpr 'nodes))
  (define roots (hash-ref jsexpr 'roots))

  ;; Pass 1: count references to each node
  (define ref-counts (make-vector (length nodes) 0))
  (for ([root roots])
    (vector-set! ref-counts root (+ 1 (vector-ref ref-counts root))))
  (for ([i (in-naturals)]
        [node (in-list nodes)])
    (match node
      [(list op args ...)
       (for ([arg args])
         (vector-set! ref-counts arg (+ 1 (vector-ref ref-counts arg))))]
      ;; Never dedup constants & variables
      [_ (vector-set! ref-counts i -inf.0)]))

  ;; Pass 2: build expressions, using %N for multiply-referenced nodes
  (define exprs (make-vector (length nodes) #f))
  (for ([node (in-list nodes)]
        [i (in-naturals)])
    (vector-set! exprs
                 i
                 (match node
                   [(list op args ...)
                    (format "(~a ~a)"
                            op
                            (string-join (for/list ([arg args])
                                           (if (> (vector-ref ref-counts arg) 1)
                                               (format "%~a" arg)
                                               (vector-ref exprs arg)))))]
                   [_ (~a node)])))

  ;; Output: one line per multi-ref node, then root expressions
  (define bindings
    (for/list ([i (in-naturals)]
               [node (in-list nodes)]
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
  (define (test-munge-unmunge expr)
    (define-values (batch brfs) (progs->batch (list expr)))
    (check-equal? (list expr) (map (batch-exprs batch) brfs)))

  (define (f64 x)
    (literal x 'binary64))

  (test-munge-unmunge '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))))
  (test-munge-unmunge
   '(+ 1 (neg (* 1/2 (+ (exp (/ (sin 3) (cos 3))) (/ 1 (exp (/ (sin 3) (cos 3)))))))))
  (test-munge-unmunge '(cbrt x))
  (test-munge-unmunge (list 'x))
  (test-munge-unmunge `(+.f64 (sin.f64 ,(approx '(* 1/2 (+ (exp x) (neg (/ 1 (exp x)))))
                                                '(+.f64 ,(f64 3)
                                                        (*.f64 ,(f64 25) (sin.f64 ,(f64 6))))))
                              ,(f64 4))))

; Tests for remove-zombie-nodes
(module+ test
  (require rackunit)
  (define (zombie-test #:nodes nodes #:roots roots)
    (define in-batch (batch nodes (make-hash)))
    (define brfs (map (curry batchref in-batch) roots))
    (define out-batch (batch-empty))
    (define copy-f (batch-copy-only! out-batch in-batch))
    (define brfs* (map copy-f brfs))
    (check-equal? (map (batch-exprs out-batch) brfs*) (map (batch-exprs in-batch) brfs))
    (batch-nodes out-batch))

  (check-equal? (create-dvector 2 0 '(sqrt 1) '(pow 0 2))
                (zombie-test #:nodes (create-dvector 0 1 '(sqrt 0) 2 '(pow 3 2)) #:roots (list 4)))
  (check-equal? (create-dvector 0 '(sqrt 0) '(exp 1))
                (zombie-test #:nodes (create-dvector 0 6 '(pow 0 1) '(* 2 0) '(sqrt 0) '(exp 4))
                             #:roots (list 5)))
  (check-equal? (create-dvector 0 1/2 '(+ 0 1))
                (zombie-test #:nodes (create-dvector 0 1/2 '(+ 0 1) '(* 2 0)) #:roots (list 2)))

  (check-equal? (create-dvector 1/2 '(exp 0) 0 (approx 1 2))
                (zombie-test #:nodes (create-dvector 0 1/2 '(+ 0 1) '(* 2 0) '(exp 1) (approx 4 0))
                             #:roots (list 5)))
  (check-equal?
   (create-dvector 1/2 'x '(* 1 1) 2 (approx 2 3) '(pow 0 4))
   (zombie-test #:nodes (create-dvector 'x 2 1/2 '(sqrt 1) '(cbrt 1) '(* 0 0) (approx 5 1) '(pow 2 6))
                #:roots (list 7)))
  (check-equal?
   (create-dvector 1/2 'x '(* 1 1) 2 (approx 2 3) '(pow 0 4) '(sqrt 3))
   (zombie-test #:nodes (create-dvector 'x 2 1/2 '(sqrt 1) '(cbrt 1) '(* 0 0) (approx 5 1) '(pow 2 6))
                #:roots (list 7 3))))

; Tests for batch->jsexpr and jsexpr->batch-exprs
(module+ test
  (require rackunit)
  (define (test-json-tostring expr expected)
    (define-values (batch brfs) (progs->batch (list expr)))
    (define jsexpr (batch->jsexpr batch brfs))
    (define str (jsexpr->batch-exprs jsexpr))
    (check-equal? str expected))

  ; No sharing - just the expression
  (test-json-tostring '(+ x y) "(+ x y)")
  ; Shared subexpressions get their own bindings
  (test-json-tostring '(* 1/2 (+ (exp x) (neg (/ 1 (exp x)))))
                      "%2 = (exp x)\n(* 1/2 (+ %2 (neg (/ 1 %2))))")
  ; Shared constants/variables are inlined
  (test-json-tostring '(sqrt (+ (* x x) (* y y))) "(sqrt (+ (* x x) (* y y)))"))
