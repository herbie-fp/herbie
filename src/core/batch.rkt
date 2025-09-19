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
         batch-recurse
         batch-iterate
         batch-get-nodes

         (struct-out batchref)
         batchref<?
         batchref<=?
         batchref>?
         batchref>=?
         batchref=?
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
  (define (munge prog)
    (match prog
      [(batchref b* idx*)
       (unless (equal? b b*)
         (error 'batch-add! "Batchref belongs to a different batch"))
       idx*]
      [_ (batchref-idx (batch-push! b (expr-recurse prog munge)))]))
  (batchref b (munge expr)))

(define (batch-copy b)
  (batch (dvector-copy (batch-nodes b)) (hash-copy (batch-index b))))

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

;; batch-recurse does not iterate over nodes that are not a child of brf
;; A lot of parts of Herbie rely on that
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
        [(and (> (dvector-capacity visited) idx) (dvector-ref visited idx)) (dvector-ref out idx)]
        [else
         (define res (apply f brf (λ (brf . args) (loop brf args)) args))
         (dvector-set! out idx res)
         (dvector-set! visited idx #t)
         res]))))

;; Same as batch-recurse but without using additional arguments inside a recurse function
(define (batch-iterate batch f)
  (define out (make-dvector))
  (define pt -1)
  (λ (brf)
    (match-define (batchref b idx) brf)
    (unless (eq? b batch)
      (error 'batch-iterate "Batchref belongs to a different batch"))

    (when (< pt idx)
      (for ([n (in-range (add1 pt) (add1 idx))])
        (dvector-set! out n (f (batchref batch n) (compose (curry dvector-ref out) batchref-idx))))
      (set! pt idx))
    (dvector-ref out idx)))

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
  (batch-recurse b
                 (λ (brf recurse)
                   (define node (deref brf))
                   (define node* (f node))
                   (define brf*
                     (let loop ([node* node*])
                       (match node*
                         [(? batchref? brf) (recurse brf)]
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
    (define in-batch (batch nodes (make-hash)))
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
