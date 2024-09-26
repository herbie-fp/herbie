#lang racket

(require "../utils/timeline.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt")

(provide progs->batch ; (Listof Expr) -> Batch
         batch->progs ; Batch -> (Listof Expr)
         (struct-out batch)
         (struct-out batchref) ; temporarily for patch.rkt
         batch-length ; Batch -> Integer
         batch-ref ; Batch -> Index -> Expr
         deref ; Batchref -> Expr
         batch-replace ; Batch -> (Expr<Batchref> -> Expr<Batchref>) -> Batch
         egg-nodes->batch ; Nodes -> Spec-maps -> Batch -> (Listof Batchref)
         batchref->expr ; Batchref -> Expr
         batch-extract-exprs ; Batch -> (Listof Root) -> (Listof Expr)
         remove-zombie-nodes) ; Batch -> Batch

;; This function defines the recursive structure of expressions
(define (expr-recurse expr f)
  (match expr
    [(approx spec impl) (approx spec (f impl))]
    [(list op args ...) (cons op (map f args))]
    [_ expr]))

;; Batches store these recursive structures, flattened
(struct batch ([nodes #:mutable] [roots #:mutable] vars))

(define (batch-length b)
  (cond
    [(batch? b) (vector-length (batch-nodes b))]
    [(mutable-batch? b) (hash-count (mutable-batch-index b))]
    [else (error 'batch-length "Invalid batch" b)]))

(struct mutable-batch ([nodes #:mutable] [index #:mutable] [vars #:mutable]))

(define (make-mutable-batch)
  (mutable-batch '() (make-hash) '()))

(define (batch-push! b term)
  (define hashcons (mutable-batch-index b))
  (hash-ref! hashcons
             term
             (lambda ()
               (let ([new-idx (hash-count hashcons)])
                 (hash-set! hashcons term new-idx)
                 (set-mutable-batch-nodes! b (cons term (mutable-batch-nodes b)))
                 (when (symbol? term)
                   (set-mutable-batch-vars! b (cons term (mutable-batch-vars b))))
                 new-idx))))

(define (mutable-batch->batch b roots)
  (batch (list->vector (reverse (mutable-batch-nodes b))) roots (reverse (mutable-batch-vars b))))

(define (batch->mutable-batch b)
  (mutable-batch (reverse (vector->list (batch-nodes b)))
                 (batch-restore-index b)
                 (reverse (batch-vars b))))

(struct batchref (batch idx))

(define (deref x)
  (match-define (batchref b idx) x)
  (expr-recurse (vector-ref (batch-nodes b) idx) (lambda (ref) (batchref b ref))))

(define (batchref->expr x)
  (match-define (batchref b idx) x)
  (batch-ref b idx))

(define (progs->batch exprs #:timeline-push [timeline-push #f] #:vars [vars '()])

  (define out (make-mutable-batch))
  (for ([var (in-list vars)])
    (batch-push! out var))

  (define size 0)
  (define (munge prog)
    (set! size (+ 1 size))
    (batch-push! out (expr-recurse prog munge)))

  (define roots (list->vector (map munge exprs)))
  (define final (mutable-batch->batch out roots))
  (when timeline-push
    (timeline-push! 'compiler size (batch-length final)))
  final)

(define (batch-extract-exprs b roots)
  (define exprs (make-vector (batch-length b)))
  (for ([node (in-vector (batch-nodes b))]
        [idx (in-naturals)])
    (vector-set! exprs idx (expr-recurse node (lambda (x) (vector-ref exprs x)))))
  (for/list ([root roots])
    (vector-ref exprs root)))

(define (batch->progs b)
  (define exprs (make-vector (batch-length b)))
  (for ([node (in-vector (batch-nodes b))]
        [idx (in-naturals)])
    (vector-set! exprs idx (expr-recurse node (lambda (x) (vector-ref exprs x)))))
  (for/list ([root (batch-roots b)])
    (vector-ref exprs root)))

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
          [_ (batch-push! out (expr-recurse expr loop))])))
    (vector-set! mapping idx final-idx))
  (define roots (vector-map (curry vector-ref mapping) (batch-roots b)))
  (mutable-batch->batch out roots))

; The function removes any zombie nodes from batch with respect to the roots
; Time complexity: O(|R| + |N|), where |R| - number of roots, |N| - length of nodes
; Space complexity: O(|N| + |N*| + |R|), where |N*| is a length of nodes without zombie nodes
(define (remove-zombie-nodes input-batch)
  (define nodes (batch-nodes input-batch))
  (define roots (batch-roots input-batch))
  (define nodes-length (batch-length input-batch))

  (define zombie-mask (make-vector nodes-length #t))
  (for ([root (in-vector roots)])
    (vector-set! zombie-mask root #f))
  (for ([node (in-vector nodes (- nodes-length 1) -1 -1)]
        [zmb (in-vector zombie-mask (- nodes-length 1) -1 -1)]
        #:when (not zmb))
    (expr-recurse node (λ (n) (vector-set! zombie-mask n #f))))

  (define mappings (make-vector nodes-length -1))
  (define (remap idx)
    (vector-ref mappings idx))

  (define out (make-mutable-batch))
  (for ([node (in-vector nodes)]
        [zmb (in-vector zombie-mask)]
        [n (in-naturals)]
        #:unless zmb)
    (vector-set! mappings n (batch-push! out (expr-recurse node remap))))

  (define roots* (vector-map (curry vector-ref mappings) roots))
  (mutable-batch->batch out roots*))

(define (batch-ref batch reg)
  (define (unmunge reg)
    (define node (vector-ref (batch-nodes batch) reg))
    (match node
      [(approx spec impl) (approx spec (unmunge impl))]
      [(list op regs ...) (cons op (map unmunge regs))]
      [_ node]))
  (unmunge reg))

(define (batch-restore-index batch)
  (make-hash (for/list ([node (in-vector (batch-nodes batch))]
                        [n (in-naturals)])
               (cons node n))))

(define (egg-nodes->batch egg-nodes id->spec input-batch rename-dict)
  (define out (batch->mutable-batch input-batch))

  ; This fuction here is only because of cycles in loads:( Can not be imported from egg-herbie.rkt
  (define (egg-parsed->expr expr rename-dict type)
    (let loop ([expr expr]
               [type type])
      (match expr
        [(? number?) (if (representation? type) (literal expr (representation-name type)) expr)]
        [(? symbol?)
         (if (hash-has-key? rename-dict expr) (car (hash-ref rename-dict expr)) (list expr))]
        [(list '$approx spec impl)
         (define spec-type (if (representation? type) (representation-type type) type))
         (approx (loop spec spec-type) (loop impl type))]
        [(list 'if cond ift iff)
         (if (representation? type)
             (list 'if (loop cond (get-representation 'bool)) (loop ift type) (loop iff type))
             (list 'if (loop cond 'bool) (loop ift type) (loop iff type)))]
        [(list (? impl-exists? impl) args ...) (cons impl (map loop args (impl-info impl 'itype)))]
        [(list op args ...) (cons op (map loop args (operator-info op 'itype)))])))

  (define (eggref id)
    (cdr (vector-ref egg-nodes id)))

  (define (add-enode node type)
    (define node*
      (match node
        [(? number?) (if (representation? type) (literal node (representation-name type)) node)]
        [(? symbol?) (if (hash-has-key? rename-dict node) (car (hash-ref rename-dict node)) node)]
        [(list '$approx spec impl)
         (define spec* (vector-ref id->spec spec))
         (unless spec*
           (error 'regraph-extract-variants "no initial approx node in eclass"))
         (define spec-type (if (representation? type) (representation-type type) type))
         (define final-spec (egg-parsed->expr spec* rename-dict spec-type))
         (approx final-spec (add-enode (eggref impl) type))]
        [(list 'if cond ift iff)
         (if (representation? type)
             (list 'if
                   (add-enode (eggref cond) (get-representation 'bool))
                   (add-enode (eggref ift) type)
                   (add-enode (eggref iff) type))
             (list 'if
                   (add-enode (eggref cond) 'bool)
                   (add-enode (eggref ift) type)
                   (add-enode (eggref iff) type)))]
        [(list (? impl-exists? impl) ids ...)
         (define args
           (for/list ([id (in-list ids)]
                      [type (in-list (impl-info impl 'itype))])
             (add-enode (eggref id) type)))
         (cons impl args)]
        [(list (? operator-exists? op) ids ...)
         (define args
           (for/list ([id (in-list ids)]
                      [type (in-list (operator-info op 'itype))])
             (add-enode (eggref id) type)))
         (cons op args)]))
    (batch-push! out node*))

  (define (finalize-batch roots)
    (set! input-batch (mutable-batch->batch out (list->vector roots)))
    (for/list ([root (in-list roots)])
      (batchref input-batch root)))

  (values add-enode finalize-batch))

; Tests for progs->batch and batch->progs
(module+ test
  (require rackunit)
  (define (test-munge-unmunge expr)
    (define batch (progs->batch (list expr)))
    (check-equal? (list expr) (batch->progs batch)))

  (test-munge-unmunge '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))))
  (test-munge-unmunge
   '(+ 1 (neg (* 1/2 (+ (exp (/ (sin 3) (cos 3))) (/ 1 (exp (/ (sin 3) (cos 3)))))))))
  (test-munge-unmunge '(cbrt x))
  (test-munge-unmunge '(x))
  (test-munge-unmunge
   `(+ (sin ,(approx '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))) '(+ 3 (* 25 (sin 6))))) 4)))

; Tests for remove-zombie-nodes
(module+ test
  (require rackunit)
  (define (zombie-test #:nodes nodes #:roots roots)
    (define in-batch (batch nodes roots '()))
    (define out-batch (remove-zombie-nodes in-batch))
    (check-equal? (batch->progs out-batch) (batch->progs in-batch))
    (batch-nodes out-batch))

  (check-equal? (vector 0 '(sqrt 0) 2 '(pow 2 1))
                (zombie-test #:nodes (vector 0 1 '(sqrt 0) 2 '(pow 3 2)) #:roots (vector 4)))
  (check-equal? (vector 0 '(sqrt 0) '(exp 1))
                (zombie-test #:nodes (vector 0 6 '(pow 0 1) '(* 2 0) '(sqrt 0) '(exp 4))
                             #:roots (vector 5)))
  (check-equal? (vector 0 1/2 '(+ 0 1))
                (zombie-test #:nodes (vector 0 1/2 '(+ 0 1) '(* 2 0)) #:roots (vector 2)))
  (check-equal? (vector 0 (approx '(exp 2) 0))
                (zombie-test #:nodes (vector 0 1/2 '(+ 0 1) '(* 2 0) (approx '(exp 2) 0))
                             #:roots (vector 4)))
  (check-equal? (vector 2 1/2 (approx '(* x x) 0) '(pow 1 2))
                (zombie-test #:nodes (vector 2 1/2 '(sqrt 0) '(cbrt 0) (approx '(* x x) 0) '(pow 1 4))
                             #:roots (vector 5)))
  (check-equal? (vector 2 1/2 '(sqrt 0) (approx '(* x x) 0) '(pow 1 3))
                (zombie-test #:nodes (vector 2 1/2 '(sqrt 0) '(cbrt 0) (approx '(* x x) 0) '(pow 1 4))
                             #:roots (vector 5 2))))
