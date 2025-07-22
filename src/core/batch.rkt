#lang racket

(require "../syntax/syntax.rkt"
         "../utils/common.rkt"
         "../utils/alternative.rkt") ; for unbatchify-alts

(provide progs->batch ; List<Expr> -> Batch
         batch->progs ; Batch -> ?(or List<Root> Vector<Root>) -> List<Expr>
         (struct-out batch)
         (struct-out batchref)
         (struct-out mutable-batch)
         batch-length ; Batch -> Integer
         batch-tree-size ; Batch -> Integer
         batch-free-vars
         batch-ref ; Batch -> Idx -> Expr
         deref ; Batchref -> Expr
         batch-replace ; Batch -> (Expr<Batchref> -> Expr<Batchref>) -> Batch
         debatchref ; Batchref -> Expr
         batch-alive-nodes ; Batch -> ?Vector<Root> -> Vector<Idx>
         batch-reconstruct-exprs ; Batch -> Vector<Expr>
         batch-remove-zombie ; Batch -> ?Vector<Root> -> Batch
         mutable-batch-munge! ; Mutable-batch -> Expr -> Root
         make-mutable-batch ; Mutable-batch
         batch->mutable-batch ; Batch -> Mutable-batch
         batch-copy-mutable-nodes! ; Batch -> Mutable-batch -> Void
         mutable-batch-push! ; Mutable-batch -> Node -> Idx
         batch-copy
         unbatchify-alts)

;; Batches store these recursive structures, flattened
(struct batch ([nodes #:mutable] [roots #:mutable]))

(struct mutable-batch ([nodes #:mutable] [index #:mutable] cache))

(struct batchref (batch idx) #:transparent)

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
  (cond
    [(batch? b) (vector-length (batch-nodes b))]
    [(mutable-batch? b) (hash-count (mutable-batch-index b))]
    [else (error 'batch-length "Invalid batch" b)]))

(define (make-mutable-batch)
  (mutable-batch '() (make-hash) (make-hasheq)))

(define (mutable-batch-push! b term)
  (define hashcons (mutable-batch-index b))
  (hash-ref! hashcons
             term
             (lambda ()
               (define new-idx (hash-count hashcons))
               (hash-set! hashcons term new-idx)
               (set-mutable-batch-nodes! b (cons term (mutable-batch-nodes b)))
               new-idx)))

(define (mutable-batch->batch b roots)
  (batch (list->vector (reverse (mutable-batch-nodes b))) roots))

(define (batch->mutable-batch b)
  (mutable-batch (reverse (vector->list (batch-nodes b))) (batch-restore-index b) (make-hasheq)))

(define (batch-copy-mutable-nodes! b mb)
  (set-batch-nodes! b (list->vector (reverse (mutable-batch-nodes mb)))))

(define (batch-copy b)
  (batch (vector-copy (batch-nodes b)) (vector-copy (batch-roots b))))

(define (deref x)
  (match-define (batchref b idx) x)
  (expr-recurse (vector-ref (batch-nodes b) idx) (lambda (ref) (batchref b ref))))

(define (debatchref x)
  (match-define (batchref b idx) x)
  (batch-ref b idx))

(define (progs->batch exprs #:vars [vars '()])
  (define out (make-mutable-batch))

  (for ([var (in-list vars)])
    (mutable-batch-push! out var))
  (define roots
    (for/vector #:length (length exprs)
                ([expr (in-list exprs)])
      (mutable-batch-munge! out expr)))

  (mutable-batch->batch out roots))

(define (batch-tree-size b)
  (define len (vector-length (batch-nodes b)))
  (define counts (make-vector len 0))
  (for ([i (in-naturals)]
        [node (in-vector (batch-nodes b))])
    (define args (reap [sow] (expr-recurse node sow)))
    (vector-set! counts i (apply + 1 (map (curry vector-ref counts) args))))
  (apply + (map (curry vector-ref counts) (vector->list (batch-roots b)))))

(define (mutable-batch-munge! b expr)
  (define cache (mutable-batch-cache b))
  (define (munge prog)
    (hash-ref! cache prog (lambda () (mutable-batch-push! b (expr-recurse prog munge)))))
  (munge expr))

(define (batch->progs b [roots (batch-roots b)])
  (define exprs (batch-reconstruct-exprs b))
  (for/list ([root roots])
    (vector-ref exprs root)))

(define (batch-free-vars batch)
  (define out (make-vector (vector-length (batch-nodes batch))))
  (for ([i (in-naturals)]
        [node (in-vector (batch-nodes batch))])
    (define fv
      (cond
        [(symbol? node) (set node)]
        [else
         (define arg-free-vars (mutable-set))
         (expr-recurse node (lambda (i) (set-union! arg-free-vars (vector-ref out i))))
         arg-free-vars]))
    (vector-set! out i fv))
  out)

(define (batch-replace b f)
  (define out (make-mutable-batch))
  (define mapping (make-vector (batch-length b) -1))
  (for ([node (in-vector (batch-nodes b))]
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
          [_ (mutable-batch-push! out (expr-recurse expr loop))])))
    (vector-set! mapping idx final-idx))
  (define roots (vector-map (curry vector-ref mapping) (batch-roots b)))
  (mutable-batch->batch out roots))

;; Function returns indices of alive nodes within a batch for given roots,
;;   where alive node is a child of a root + meets a condition - (condition node)
(define (batch-alive-nodes batch
                           [roots (batch-roots batch)]
                           #:keep-vars-alive [keep-vars-alive #f]
                           #:condition [condition (const #t)])
  (define nodes (batch-nodes batch))
  (define nodes-length (batch-length batch))
  (define alive-mask (make-vector nodes-length #f))
  (for ([root (in-vector roots)])
    (vector-set! alive-mask root #t))
  (for/vector ([i (in-range (- nodes-length 1) -1 -1)]
               [node (in-vector nodes (- nodes-length 1) -1 -1)]
               [alv (in-vector alive-mask (- nodes-length 1) -1 -1)]
               #:when (or (and alv (condition node)) (and keep-vars-alive (symbol? node))))
    (expr-recurse node (λ (n) (vector-set! alive-mask n #t)))
    i))

;; Function constructs a vector of expressions for the given nodes of a batch
(define (batch-reconstruct-exprs batch)
  (define exprs (make-vector (batch-length batch)))
  (for ([node (in-vector (batch-nodes batch))]
        [idx (in-naturals)])
    (vector-set! exprs idx (expr-recurse node (lambda (x) (vector-ref exprs x)))))
  exprs)

;; The function removes any zombie nodes from batch with respect to the roots
;; Time complexity: O(|R| + |N|), where |R| - number of roots, |N| - length of nodes
;; Space complexity: O(|N| + |N*| + |R|), where |N*| is a length of nodes without zombie nodes
;; The flag keep-vars is used in compiler.rkt when vars should be preserved no matter what
(define (batch-remove-zombie batch [roots (batch-roots batch)] #:keep-vars [keep-vars #f])
  (define nodes (batch-nodes batch))
  (define nodes-length (batch-length batch))
  (match (zero? nodes-length)
    [#f
     (define alive-nodes (batch-alive-nodes batch roots #:keep-vars-alive keep-vars))
     (vector-sort! alive-nodes <) ; nodes should be in ascending order

     (define mappings (make-vector nodes-length -1))
     (define (remap idx)
       (vector-ref mappings idx))

     (define out (make-mutable-batch))
     (for ([alv (in-vector alive-nodes)])
       (define node (vector-ref nodes alv))
       (vector-set! mappings alv (mutable-batch-push! out (expr-recurse node remap))))

     (define roots* (vector-map (curry vector-ref mappings) roots))
     (mutable-batch->batch out roots*)]
    [#t (batch-copy batch)]))

(define (batch-ref batch reg)
  (define (unmunge reg)
    (define node (vector-ref (batch-nodes batch) reg))
    (expr-recurse node unmunge))
  (unmunge reg))

(define (batch-restore-index batch)
  (make-hash (for/list ([node (in-vector (batch-nodes batch))]
                        [n (in-naturals)])
               (cons node n))))

; Tests for progs->batch and batch->progs
(module+ test
  (require rackunit)
  (define (test-munge-unmunge expr)
    (define batch (progs->batch (list expr)))
    (check-equal? (list expr) (batch->progs batch)))

  (define (f64 x)
    (literal x 'binary64))

  (test-munge-unmunge '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))))
  (test-munge-unmunge
   '(+ 1 (neg (* 1/2 (+ (exp (/ (sin 3) (cos 3))) (/ 1 (exp (/ (sin 3) (cos 3)))))))))
  (test-munge-unmunge '(cbrt x))
  (test-munge-unmunge '(x))
  (test-munge-unmunge `(+.f64 (sin.f64 ,(approx '(* 1/2 (+ (exp x) (neg (/ 1 (exp x)))))
                                                '(+.f64 ,(f64 3)
                                                        (*.f64 ,(f64 25) (sin.f64 ,(f64 6))))))
                              ,(f64 4))))

; Tests for remove-zombie-nodes
(module+ test
  (require rackunit)
  (define (zombie-test #:nodes nodes #:roots roots)
    (define in-batch (batch nodes roots))
    (define out-batch (batch-remove-zombie in-batch))
    (check-equal? (batch->progs out-batch) (batch->progs in-batch))
    (batch-nodes out-batch))

  (check-equal? (vector 0 '(sqrt 0) 2 '(pow 2 1))
                (zombie-test #:nodes (vector 0 1 '(sqrt 0) 2 '(pow 3 2)) #:roots (vector 4)))
  (check-equal? (vector 0 '(sqrt 0) '(exp 1))
                (zombie-test #:nodes (vector 0 6 '(pow 0 1) '(* 2 0) '(sqrt 0) '(exp 4))
                             #:roots (vector 5)))
  (check-equal? (vector 0 1/2 '(+ 0 1))
                (zombie-test #:nodes (vector 0 1/2 '(+ 0 1) '(* 2 0)) #:roots (vector 2)))
  (check-equal? (vector 0 1/2 '(exp 1) (approx 2 0))
                (zombie-test #:nodes (vector 0 1/2 '(+ 0 1) '(* 2 0) '(exp 1) (approx 4 0))
                             #:roots (vector 5)))
  (check-equal? (vector 'x 2 1/2 '(* 0 0) (approx 3 1) '(pow 2 4))
                (zombie-test #:nodes
                             (vector 'x 2 1/2 '(sqrt 1) '(cbrt 1) '(* 0 0) (approx 5 1) '(pow 2 6))
                             #:roots (vector 7)))
  (check-equal? (vector 'x 2 1/2 '(sqrt 1) '(* 0 0) (approx 4 1) '(pow 2 5))
                (zombie-test #:nodes
                             (vector 'x 2 1/2 '(sqrt 1) '(cbrt 1) '(* 0 0) (approx 5 1) '(pow 2 6))
                             #:roots (vector 7 3))))
