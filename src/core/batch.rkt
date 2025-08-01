#lang racket

(require "../syntax/syntax.rkt"
         "../utils/common.rkt"
         "../utils/alternative.rkt" ; for unbatchify-alts
         "dvector.rkt")

(provide progs->batch ; List<Expr> -> Batch
         batch->progs ; Batch -> ?(or List<Root> Vector<Root>) -> List<Expr>

         (struct-out batch)
         make-batch ; Batch
         batch-push! ; Batch -> Node -> Idx
         batch-munge! ; Batch -> Expr -> Root
         batch-copy ; Batch -> Batch
         batch-length ; Batch -> Integer
         batch-tree-size ; Batch -> Integer
         batch-free-vars
         in-batch ; Batch -> Sequence<Node>
         batch-ref ; Batch -> Idx -> Node
         batch-pull ; Batchref -> Expr
         batch-replace ; Batch -> (Expr<Batchref> -> Expr<Batchref>) -> Batch
         batch-children ; Batch -> ?Vector<Root> -> Vector<Idx>
         batch-reconstruct-exprs ; Batch -> Vector<Expr>
         batch-remove-zombie ; Batch -> ?Vector<Root> -> Batch

         (struct-out batchref)
         deref ; Batchref -> Expr

         unbatchify-alts)

;; Batches store these recursive structures, flattened
(struct batch ([nodes #:mutable] [index #:mutable] cache))

(struct batchref (batch idx) #:transparent)

(define (make-batch)
  (batch (make-dvector) (make-hash) (make-hasheq)))

(define (in-batch batch [start 0] [end #f] [step 1])
  (in-dvector (batch-nodes batch) start end step))

;; This function defines the recursive structure of expressions
(define (expr-recurse expr f)
  (match expr
    [(approx spec impl) (approx (f spec) (f impl))]
    [(hole precision spec) (hole precision (f spec))]
    [(list op args ...) (cons op (map f args))]
    [_ expr]))

;; Converts batchrefs of altns into expressions, assuming that batchrefs refer to batch
(define (unbatchify-alts batch altns)
  (define exprs (batch-reconstruct-exprs batch))
  (define (unmunge altn)
    (define expr (alt-expr altn))
    (match expr
      [(? batchref?)
       (define expr* (vector-ref exprs (batchref-idx expr)))
       (struct-copy alt altn [expr expr*])]
      [_ altn]))
  (map (curry alt-map unmunge) altns))

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

(define (batch-copy b)
  (batch (dvector-copy (batch-nodes b)) (hash-copy (batch-index b)) (hash-copy (batch-cache b))))

(define (deref x)
  (match-define (batchref b idx) x)
  (expr-recurse (batch-ref b idx) (lambda (ref) (batchref b ref))))

(define (progs->batch exprs #:vars [vars '()])
  (define out (make-batch))
  (for ([var (in-list vars)])
    (batch-push! out var))
  (define brfs
    (for/list ([expr (in-list exprs)])
      (batch-munge! out expr)))
  (values out brfs))

(define (batch-tree-size b brfs)
  (define len (batch-length b))
  (define counts (make-vector len 0))
  (for ([i (in-naturals)]
        [node (in-batch b)])
    (define args (reap [sow] (expr-recurse node sow)))
    (vector-set! counts i (apply + 1 (map (curry vector-ref counts) args))))
  (apply + (map (compose (curry vector-ref counts) batchref-idx) brfs)))

(define (batch-munge! b expr)
  (define cache (batch-cache b))
  (define (munge prog)
    (hash-ref! cache prog (lambda () (batchref-idx (batch-push! b (expr-recurse prog munge))))))
  (batchref b (munge expr)))

(define (batch->progs b brfs)
  (define exprs (batch-reconstruct-exprs b))
  (for/list ([brf brfs])
    (vector-ref exprs (batchref-idx brf))))

(define (batch-free-vars batch)
  (define out (make-vector (batch-length batch)))
  (for ([i (in-naturals)]
        [node (in-batch batch)])
    (define fv
      (cond
        [(symbol? node) (set node)]
        [else
         (define arg-free-vars (mutable-set))
         (expr-recurse node (lambda (i) (set-union! arg-free-vars (vector-ref out i))))
         arg-free-vars]))
    (vector-set! out i fv))
  out)

(define (batch-replace b brfs f)
  (define out (make-batch))
  (define mapping (make-vector (batch-length b) -1))
  (for ([node (in-batch b)]
        [idx (in-naturals)])
    (define replacement (f (expr-recurse node (lambda (x) (batchref b x)))))
    (define final-idx
      (let loop ([expr replacement])
        (match expr
          [(batchref b* idx)
           (unless (eq? b* b)
             (error 'batch-replace "Replacement ~a references the wrong batch ~a" replacement b*))
           (when (= -1 (vector-ref mapping idx))
             (error 'batch-replace "Replacement ~a references unknown index ~a" replacement idx))
           (vector-ref mapping idx)]
          [_ (batchref-idx (batch-push! out (expr-recurse expr loop)))])))
    (vector-set! mapping idx final-idx))
  (define brfs* (map (compose (curry batchref out) (curry vector-ref mapping) batchref-idx) brfs))
  (values out brfs*))

;; Function returns indices of children nodes within a batch for given roots,
;;   where a child node is a child of a root + meets a condition - (condition node)
(define (batch-children batch
                        brfs
                        #:include-vars [include-vars #f]
                        #:condition [condition (const #t)])
  (define len (batch-length batch))
  (define child-mask (make-vector len #f))
  (for ([brf brfs])
    (vector-set! child-mask (batchref-idx brf) #t))
  (for ([i (in-range (- len 1) -1 -1)]
        [node (in-batch batch (- len 1) -1 -1)]
        [chld (in-vector child-mask (- len 1) -1 -1)]
        #:when (or (and chld (condition node)) (and include-vars (symbol? node))))
    (unless chld ; if include-vars then chld may not be #t, making sure it's #t
      (vector-set! child-mask i #t))
    (expr-recurse node
                  (λ (n)
                    (when (condition (batch-ref batch n))
                      (vector-set! child-mask n #t)))))
  ; Return indices of child nodes in ascending order
  (for/list ([chld (in-vector child-mask)]
             [i (in-naturals)]
             #:when chld)
    (batchref batch i)))

;; Function constructs a vector of expressions for the given nodes of a batch
(define (batch-reconstruct-exprs batch)
  (define exprs (make-vector (batch-length batch)))
  (for ([node (in-batch batch)]
        [idx (in-naturals)])
    (vector-set! exprs idx (expr-recurse node (lambda (x) (vector-ref exprs x)))))
  exprs)

;; The function removes any zombie nodes from batch with respect to the roots
;; Time complexity: O(|R| + |N|), where |R| - number of roots, |N| - length of nodes
;; Space complexity: O(|N| + |N*| + |R|), where |N*| is a length of nodes without zombie nodes
;; The flag keep-vars is used in compiler.rkt when vars should be preserved no matter what
(define (batch-remove-zombie batch brfs #:keep-vars [keep-vars #f])
  (define len (batch-length batch))
  (match (zero? len)
    [#f
     (define children-brfs (batch-children batch brfs #:include-vars keep-vars))

     (define mappings (make-hash))
     (define (map-ref brf)
       (hash-ref mappings brf))
     (define (map-set! brf brf*)
       (hash-set! mappings brf brf*))

     ; batch's children nodes get pushed+remapped into out batch
     (define out (make-batch))
     (for ([child children-brfs])
       (define node (deref child))
       (define node* (expr-recurse node (compose batchref-idx map-ref))) ; remapped node
       (define child* (batch-push! out node*))
       (map-set! child child*))
     (define brfs* (map map-ref brfs))

     (values out brfs*)]
    [#t
     (define out (batch-copy batch))
     (define brfs*
       (for/list ([brf brfs])
         (batchref out (batchref-idx brf))))
     (values out brfs*)]))

(define (batch-ref batch reg)
  (dvector-ref (batch-nodes batch) reg))

(define (batch-pull brf)
  (define (unmunge brf)
    (expr-recurse (deref brf) unmunge))
  (unmunge brf))

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
    (define-values (out-batch brfs*) (batch-remove-zombie in-batch brfs))
    (check-equal? (batch->progs out-batch brfs*) (batch->progs in-batch brfs))
    (batch-nodes out-batch))

  (check-equal? (create-dvector 0 '(sqrt 0) 2 '(pow 2 1))
                (zombie-test #:nodes (create-dvector 0 1 '(sqrt 0) 2 '(pow 3 2)) #:roots (list 4)))
  (check-equal? (create-dvector 0 '(sqrt 0) '(exp 1))
                (zombie-test #:nodes (create-dvector 0 6 '(pow 0 1) '(* 2 0) '(sqrt 0) '(exp 4))
                             #:roots (list 5)))
  (check-equal? (create-dvector 0 1/2 '(+ 0 1))
                (zombie-test #:nodes (create-dvector 0 1/2 '(+ 0 1) '(* 2 0)) #:roots (list 2)))
  (check-equal? (create-dvector 0 1/2 '(exp 1) (approx 2 0))
                (zombie-test #:nodes (create-dvector 0 1/2 '(+ 0 1) '(* 2 0) '(exp 1) (approx 4 0))
                             #:roots (list 5)))
  (check-equal?
   (create-dvector 'x 2 1/2 '(* 0 0) (approx 3 1) '(pow 2 4))
   (zombie-test #:nodes (create-dvector 'x 2 1/2 '(sqrt 1) '(cbrt 1) '(* 0 0) (approx 5 1) '(pow 2 6))
                #:roots (list 7)))
  (check-equal?
   (create-dvector 'x 2 1/2 '(sqrt 1) '(* 0 0) (approx 4 1) '(pow 2 5))
   (zombie-test #:nodes (create-dvector 'x 2 1/2 '(sqrt 1) '(cbrt 1) '(* 0 0) (approx 5 1) '(pow 2 6))
                #:roots (list 7 3))))
