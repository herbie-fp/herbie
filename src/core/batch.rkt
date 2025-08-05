#lang racket

(require "../syntax/syntax.rkt"
         "../utils/common.rkt"
         "../utils/alternative.rkt" ; for unbatchify-alts
         "dvector.rkt")

(provide progs->batch ; List<Expr> -> Batch
         batch->progs ; Batch -> ?(or List<Root> Vector<Root>) -> List<Expr>

         (struct-out batch)
         batch-empty ; Batch
         batch-push! ; Batch -> Node -> Idx
         batch-add! ; Batch -> Expr -> Root
         batch-insert!
         batch-copy ; Batch -> Batch
         batch-length ; Batch -> Integer
         batch-tree-size ; Batch -> Integer
         batch-free-vars
         in-batch ; Batch -> Sequence<Node>
         batch-ref ; Batch -> Idx -> Node
         batch-pull ; Batchref -> Expr
         batch-apply ; Batch -> (Expr<Batchref> -> Expr<Batchref>) -> Batch
         batch-children ; Batch -> ?Vector<Root> -> Vector<Idx>
         batch-reconstruct-exprs ; Batch -> Vector<Expr>
         batch-remove-zombie ; Batch -> ?Vector<Root> -> Batch

         (struct-out batchref)
         deref ; Batchref -> Expr

         unbatchify-alts)

;; Batches store these recursive structures, flattened
(struct batch ([nodes #:mutable] [index #:mutable] [cache #:mutable]))

(struct batchref (batch idx) #:transparent)

(define (batch-empty)
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
      [(? batchref? brf)
       (define expr* (exprs brf))
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

(define (batch-copy! b b*)
  (set-batch-nodes! b (dvector-copy (batch-nodes b*)))
  (set-batch-index! b (hash-copy (batch-index b*)))
  (set-batch-cache! b (hash-copy (batch-cache b*))))

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

(define (batchrefs-max-idx brfs)
  (apply max (map batchref-idx brfs)))

(define (batch-tree-size b brfs)
  (brfs-belong-to-batch? b brfs)
  (define len (batch-length b))
  (define counts (make-vector len 0))
  (for ([i (in-naturals)]
        [node (in-batch b)])
    (define args (reap [sow] (expr-recurse node sow)))
    (vector-set! counts i (apply + 1 (map (curry vector-ref counts) args))))
  (apply + (map (compose (curry vector-ref counts) batchref-idx) brfs)))

(define (batch-add! b expr)
  (define cache (batch-cache b))
  (define (munge prog)
    (hash-ref! cache prog (lambda () (batchref-idx (batch-push! b (expr-recurse prog munge))))))
  (batchref b (munge expr)))

(define (batch->progs b brfs)
  (match brfs
    [(? null?) '()]
    [_
     (brfs-belong-to-batch? b brfs)
     (define exprs (batch-reconstruct-exprs b))
     (for/list ([brf brfs])
       (exprs brf))]))

(define (batch-recursive-map batch f)
  (define len (batch-length batch))
  (define out (make-vector len))
  (define pt -1)
  (λ (brf)
    (match-define (batchref b* idx*) brf)
    (cond
      [(or (not (equal? b* batch)) (>= idx* len)) ; Little check
       (error 'batch-free-vars "Inappropriate batchref is passed")]
      [(>= pt (batchref-idx brf)) (vector-ref out idx*)]
      [(for ([node (in-batch batch (max 0 pt) (add1 idx*))]
             [i (in-naturals (max 0 pt))])
         (vector-set! out i (f (λ (x) (vector-ref out x)) node)))
       (set! pt idx*)
       (vector-ref out idx*)])))

; Very slow function, do not call it unless it is necessary
(define (batch-insert! batch brfs exprs)
  ; Little check
  (brfs-belong-to-batch? batch brfs)

  (define batch* (batch-empty))
  ; Adding new expressions first
  (define exprs-brfs
    (for/list ([expr exprs])
      (batch-add! batch* expr)))

  ; Copy nodes from batch
  (define mappings (make-vector (batch-length batch) -1))
  (define (re-map idx)
    (vector-ref mappings idx))
  (define (set-map! idx idx*)
    (vector-set! mappings idx idx*))
  (for ([node (in-batch batch)]
        [i (in-naturals)])
    (define brf (batch-push! batch* (expr-recurse node re-map)))
    (set-map! i (batchref-idx brf)))

  (define brfs* (map (compose (curry batchref batch) re-map batchref-idx) brfs))
  (batch-copy! batch batch*)
  (values brfs* exprs-brfs))

(define (batch-apply b brfs f)
  (define out (batch-empty))
  (match brfs
    [(? null?) (values out '())]
    [_
     (define children-brfs (batch-children b brfs))
     (define max-idx (batchrefs-max-idx children-brfs))

     (define mappings (make-vector (add1 max-idx) -1))
     (define (map-ref brf)
       (vector-ref mappings (batchref-idx brf)))
     (define (map-set! brf brf*)
       (vector-set! mappings (batchref-idx brf) brf*))

     (for ([brf (in-list children-brfs)])
       (define node (deref brf))
       (define node* (f node))
       (define brf*
         (let loop ([node* node*])
           (match node*
             [(? batchref? brf) (map-ref brf)]
             [_ (batch-push! out (expr-recurse node* (compose batchref-idx loop)))])))
       (map-set! brf brf*))
     (define brfs* (map map-ref brfs))
     (values out brfs*)]))

;; Function returns indices of children nodes within a batch for given roots,
;;   where a child node is a child of a root + meets a condition - (condition node)
(define (batch-children batch brfs #:condition [condition (const #t)])
  (match brfs
    [(? null?) '()]
    [_
     ; Little check
     (brfs-belong-to-batch? batch brfs)
     ; Define len to be as small as possible
     (define max-idx (batchrefs-max-idx brfs))
     (define len (add1 max-idx))
     (define child-mask (make-vector len #f))

     (for ([brf brfs])
       (vector-set! child-mask (batchref-idx brf) #t))
     (for ([i (in-range max-idx -1 -1)]
           [node (in-batch batch max-idx -1 -1)]
           [child (in-vector child-mask max-idx -1 -1)]
           #:when (and child (condition node)))
       (unless child ; if include-vars then chld may not be #t, making sure it's #t
         (vector-set! child-mask i #t))
       (expr-recurse node
                     (λ (n)
                       (when (condition (batch-ref batch n))
                         (vector-set! child-mask n #t)))))
     ; Return batchrefs of children nodes in ascending order
     (for/list ([child (in-vector child-mask)]
                [i (in-naturals)]
                #:when child)
       (batchref batch i))]))

;; Function constructs a vector of expressions for the given nodes of a batch
(define (batch-reconstruct-exprs batch)
  (batch-recursive-map batch (lambda (children-exprs node) (expr-recurse node children-exprs))))

(define (batch-free-vars batch)
  (batch-recursive-map
   batch
   (lambda (get-children-free-vars node)
     (cond
       [(symbol? node) (set node)]
       [else
        (define arg-free-vars (mutable-set))
        (expr-recurse node (lambda (i) (set-union! arg-free-vars (get-children-free-vars i))))
        arg-free-vars]))))

(define (brfs-belong-to-batch? batch brfs)
  (unless (andmap (compose (curry equal? batch) batchref-batch) brfs)
    (error 'brfs-belong-to-batch? "One of batchrefs does not belong to the provided batch")))

;; The function removes any zombie nodes from batch with respect to the roots
(define (batch-remove-zombie batch brfs)
  (define len (batch-length batch))
  (match (zero? len)
    [#f (batch-apply batch brfs identity)]
    [#t
     (unless (null? brfs)
       (error 'batch-remove-zombie "Non-empty batchrefs in an empty batch!"))
     (values (batch-copy batch) '())]))

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
