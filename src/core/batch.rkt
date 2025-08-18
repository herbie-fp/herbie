#lang racket

(require "../syntax/syntax.rkt"
         "../utils/common.rkt"
         "dvector.rkt")

(provide progs->batch ; List<Expr> -> (Batch, List<Batchref>)
         batch->progs ; Batch -> List<Batchref> -> List<Expr>

         expr-recurse
         (struct-out batch)
         batch-empty ; Batch
         batch-push!
         batch-add! ; Batch -> (or Expr Batchref Expr<Batchref>) -> Batchref
         batch-copy ; Batch -> Batch
         batch-copy-only ; Batch -> List<Batchref> -> (Batch, List<Batchref>)
         batch-copy-only!
         batch-length ; Batch -> Integer
         batch-tree-size ; Batch -> List<Batchref> -> Integer
         batch-free-vars ; Batch -> (Batchref -> Set<Var>)
         in-batch ; Batch -> Sequence<Node>
         batch-ref ; Batch -> Idx -> Node
         batch-pull ; Batchref -> Expr
         batch-apply ; Batch -> List<Batchref> -> (Expr<Batchref> -> Expr<Batchref>) -> (Batch, List<Batchref>)
         batch-apply! ; Batch -> (Expr<Batchref> -> Expr<Batchref>) -> (Batchref -> Batchref)
         batch-reachable ; Batch -> List<Batchref> -> (Node -> Boolean) -> List<Batchref>
         batch-exprs
         batch-map

         (struct-out batchref)
         batchref<?
         batchref<=?
         batchref>?
         batchref>=?
         batchref=?
         deref) ; Batchref -> Expr

;; Batches store these recursive structures, flattened
(struct batch ([nodes #:mutable] [index #:mutable] [cache #:mutable]))

(struct batchref (batch idx) #:transparent)

;; --------------------------------- CORE BATCH FUNCTION ------------------------------------

(define (batch-empty)
  (batch (make-dvector) (make-hash) (make-hasheq)))

(define (in-batch batch [start 0] [end #f] [step 1])
  (in-dvector (batch-nodes batch) start end step))

(define (batchref<? brf1 brf2)
  (< (batchref-idx brf1) (batchref-idx brf2)))
(define (batchref<=? brf1 brf2)
  (<= (batchref-idx brf1) (batchref-idx brf2)))
(define (batchref>? brf1 brf2)
  (> (batchref-idx brf1) (batchref-idx brf2)))
(define (batchref>=? brf1 brf2)
  (>= (batchref-idx brf1) (batchref-idx brf2)))
(define (batchref=? brf1 brf2)
  (= (batchref-idx brf1) (batchref-idx brf2)))

;; This function defines the recursive structure of expressions
(define (expr-recurse expr f)
  (match expr
    [(approx spec impl) (approx (f spec) (f impl))]
    [(hole precision spec) (hole precision (f spec))]
    [(list op args ...) (cons op (map f args))]
    [_ expr]))

(define (batch-length b)
  (dvector-length (batch-nodes b)))

(define (batch-push! b term)
  (define hashcons (batch-index b))
  (hash-ref! hashcons
             term
             (lambda ()
               (define idx (hash-count hashcons))
               (hash-set! hashcons term idx)
               (dvector-add! (batch-nodes b) term)
               (batchref b idx))))

(define (batch-add! b expr)
  (define cache (batch-cache b))
  (define (munge prog)
    (match prog
      [(batchref b* idx*)
       (unless (equal? b b*)
         (error 'batch-add! "Batchref belongs to a different batch"))
       idx*]
      [_
       (hash-ref! cache prog (lambda () (batchref-idx (batch-push! b (expr-recurse prog munge)))))]))
  (batchref b (munge expr)))

(define (batch-copy b)
  (batch (dvector-copy (batch-nodes b)) (hash-copy (batch-index b)) (hash-copy (batch-cache b))))

(define (deref x)
  (match-define (batchref b idx) x)
  (expr-recurse (batch-ref b idx) (lambda (ref) (batchref b ref))))

(define (progs->batch exprs #:vars [vars '()])
  (define out (batch-empty))
  (for ([var (in-list vars)])
    (batch-push! out var))
  (define brfs
    (for/list ([expr (in-list exprs)])
      (batch-add! out expr)))
  (values out brfs))

(define (batch->progs b brfs)
  (map (batch-exprs b) brfs))

;; batch-map does not iterate over nodes that are not a child of brf
;; A lot of parts of Herbie rely on that
(define (batch-map batch f)
  (define out (make-dvector))
  (define visited (make-dvector))
  (λ (brf)
    (match-define (batchref b idx) brf)
    (cond
      ; Little check
      [(not (equal? b batch)) (error 'batch-map "Batchref is is from a different batch")]
      [else
       (let loop ([idx idx])
         (match (and (> (dvector-capacity visited) idx) (dvector-ref visited idx))
           [#t (dvector-ref out idx)]
           [_
            (define node (batch-ref batch idx))
            (define res (f (λ (x) (loop x)) node))
            (dvector-set! out idx res)
            (dvector-set! visited idx #t)
            res]))])))

(define (batch-ref batch reg)
  (dvector-ref (batch-nodes batch) reg))

(define (batch-pull brf)
  (define (unmunge brf)
    (expr-recurse (deref brf) unmunge))
  (unmunge brf))

(define (brfs-belong-to-batch? batch brfs)
  (unless (andmap (compose (curry equal? batch) batchref-batch) brfs)
    (error 'brfs-belong-to-batch? "One of batchrefs does not belong to the provided batch")))

;; --------------------------------- CUSTOM BATCH FUNCTION ------------------------------------

;; out - batch to where write new nodes
;; b - batch from which to read nodes
;; f - function to modify nodes from b
(define (batch-apply-internal out b f)
  (batch-map b
             (λ (remap node)
               (define node-with-batchrefs (expr-recurse node (lambda (ref) (batchref b ref))))
               (define node* (f node-with-batchrefs))
               (define brf*
                 (let loop ([node* node*])
                   (match node*
                     [(? batchref? brf) (remap (batchref-idx brf))]
                     [_ (batch-push! out (expr-recurse node* (compose batchref-idx loop)))])))
               brf*)))

;; Allocates new batch
(define (batch-apply b brfs f)
  (define out (batch-empty))
  (define apply-f (batch-apply-internal out b f))
  (define brfs* (map apply-f brfs))
  (values out brfs*))

;; Modifies batch in-place
(define (batch-apply! b f)
  (batch-apply-internal b b f))

;; Function returns indices of children nodes within a batch for given roots,
;;   where a child node is a child of a root + meets a condition - (condition node)
(define (batch-reachable batch brfs #:condition [condition (const #t)])
  (match brfs
    [(? null?) '()]
    [_
     ; Little check
     (brfs-belong-to-batch? batch brfs)
     ; Define len to be as small as possible
     (define max-idx (apply max (map batchref-idx brfs)))
     (define len (add1 max-idx))
     (define child-mask (make-vector len #f))

     (for ([brf brfs])
       (vector-set! child-mask (batchref-idx brf) #t))
     (for ([i (in-range max-idx -1 -1)]
           [node (in-batch batch max-idx -1 -1)]
           [child (in-vector child-mask max-idx -1 -1)]
           #:when child)
       (unless (condition node)
         (vector-set! child-mask i #f))
       (expr-recurse node (λ (n) (vector-set! child-mask n #t))))
     ; Return batchrefs of children nodes in ascending order
     (for/list ([child (in-vector child-mask)]
                [i (in-naturals)]
                #:when child)
       (batchref batch i))]))

;; Function constructs a vector of expressions for the given nodes of a batch
(define (batch-exprs batch)
  (batch-map batch (lambda (children-exprs node) (expr-recurse node children-exprs))))

;; Function constructs a vector of expressions for the given nodes of a batch
(define (batch-copy-only! batch batch*)
  (batch-map batch*
             (lambda (remap node)
               (batch-push! batch (expr-recurse node (compose batchref-idx remap))))))

(define (batch-free-vars batch)
  (batch-map batch
             (lambda (get-children-free-vars node)
               (cond
                 [(symbol? node) (set node)]
                 [else
                  (define arg-free-vars (mutable-set))
                  (expr-recurse node
                                (lambda (i) (set-union! arg-free-vars (get-children-free-vars i))))
                  arg-free-vars]))))

(define (batch-tree-size batch brfs)
  (define counts
    (batch-map batch
               (lambda (get-children-counts node)
                 (define args (reap [sow] (expr-recurse node sow)))
                 (apply + 1 (map get-children-counts args)))))
  (apply + (map counts brfs)))

;; The function removes any zombie nodes from batch with respect to the brfs
(define (batch-copy-only batch brfs)
  (batch-apply batch brfs identity))

;; --------------------------------- TESTS ---------------------------------------

; Tests for progs->batch and batch->progs
(module+ test
  (require rackunit)
  (define (test-munge-unmunge expr)
    (define-values (batch brfs) (progs->batch (list expr)))
    (check-equal? (list expr) (batch->progs batch brfs)))

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
    (define in-batch (batch nodes (make-hash) (make-hasheq)))
    (define brfs (map (curry batchref in-batch) roots))
    (define-values (out-batch brfs*) (batch-copy-only in-batch brfs))
    (check-equal? (batch->progs out-batch brfs*) (batch->progs in-batch brfs))
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
