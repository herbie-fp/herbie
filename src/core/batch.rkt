#lang racket

(require "../utils/timeline.rkt"
         "../syntax/syntax.rkt")

(provide progs->batch
         batch->progs
         (struct-out batch)
         batch-length
         batch-ref
         batch-replace
         expand-taylor
         batch-remove-approx)

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

(define (mutable-batch->immutable b roots)
  (batch (list->vector (reverse (mutable-batch-nodes b))) roots (reverse (mutable-batch-vars b))))

(struct batchref (batch idx))

(define (deref x)
  (match-define (batchref b idx) x)
  (expr-recurse (vector-ref (batch-nodes b) idx) (lambda (ref) (batchref b ref))))

(define (progs->batch exprs #:timeline-push [timeline-push #f] #:vars [vars '()])

  (define out (make-mutable-batch))
  (for ([var (in-list vars)])
    (batch-push! out var))

  (define size 0)
  (define (munge prog)
    (set! size (+ 1 size))
    (batch-push! out (expr-recurse prog munge)))

  (define roots (list->vector (map munge exprs)))
  (define final (mutable-batch->immutable out roots))
  (when timeline-push
    (timeline-push! 'compiler size (batch-length final)))
  final)

(define (batch->progs b)
  (define exprs (make-vector (batch-length b)))
  (for ([node (in-vector (batch-nodes b))]
        [idx (in-naturals)])
    (vector-set! exprs idx (expr-recurse node (lambda (x) (vector-ref exprs x)))))
  (for/list ([root (batch-roots b)])
    (vector-ref exprs root)))

(define (batch-remove-approx batch)
  (batch-replace batch
                 (lambda (node)
                   (match node
                     [(approx spec impl) impl]
                     [node node]))))

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
  (mutable-batch->immutable out roots))

(define (expand-taylor input-batch)
  (batch-replace
   input-batch
   (lambda (node)
     (match node
       [(list '- ref1 ref2) `(+ ,ref1 (neg ,ref2))]
       [(list 'pow base (app deref 1/2)) `(sqrt ,base)]
       [(list 'pow base (app deref 1/3)) `(cbrt ,base)]
       [(list 'pow base (app deref 2/3)) `(cbrt (* ,base ,base))]
       [(list 'pow base (and power (app deref (? exact-integer?)))) (list 'pow base power)]
       [(list 'pow base power) `(exp (* ,power (log ,base)))]
       [(list 'tan arg) `(/ (sin ,arg) (cos ,arg))]
       [(list 'cosh arg) `(* 1/2 (+ (exp ,arg) (/ 1 (exp ,arg))))]
       [(list 'sinh arg) `(* 1/2 (+ (exp ,arg) (/ -1 (exp ,arg))))]
       [(list 'tanh arg) `(/ (+ (exp ,arg) (neg (/ 1 (exp ,arg)))) (+ (exp ,arg) (/ 1 (exp ,arg))))]
       [(list 'asinh arg) `(log (+ ,arg (sqrt (+ (* ,arg ,arg) 1))))]
       [(list 'acosh arg) `(log (+ ,arg (sqrt (+ (* ,arg ,arg) -1))))]
       [(list 'atanh arg) `(* 1/2 (log (/ (+ 1 ,arg) (+ 1 (neg ,arg)))))]
       [_ node]))))

; The function removes any zombie nodes from batch
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
    (match node
      [(list op args ...) (map (λ (n) (vector-set! zombie-mask n #f)) args)]
      [(approx spec impl) (vector-set! zombie-mask impl #f)]
      [_ void]))

  (define mappings (build-vector nodes-length values))

  (define nodes* '())
  (for ([node (in-vector nodes)]
        [zmb (in-vector zombie-mask)]
        [n (in-naturals)])
    (if zmb
        (for ([i (in-range n nodes-length)])
          (vector-set! mappings i (sub1 (vector-ref mappings i))))
        (set! nodes*
              (cons (match node
                      [(list op args ...) (cons op (map (curry vector-ref mappings) args))]
                      [(approx spec impl) (approx spec (vector-ref mappings impl))]
                      [_ node])
                    nodes*))))
  (set! nodes* (list->vector (reverse nodes*)))
  (define roots* (vector-map (curry vector-ref mappings) roots))
  (batch nodes* roots* (batch-vars input-batch)))

(define (batch-ref batch reg)
  (define (unmunge reg)
    (define node (vector-ref (batch-nodes batch) reg))
    (match node
      [(approx spec impl) (approx spec (unmunge impl))]
      [(list op regs ...) (cons op (map unmunge regs))]
      [_ node]))
  (unmunge reg))

; Tests for expand-taylor
(module+ test
  (require rackunit)

  (define (test-expand-taylor expr)
    (define batch (progs->batch (list expr)))
    (define batch* (expand-taylor batch))
    (car (batch->progs batch*)))

  (check-equal? '(* 1/2 (log (/ (+ 1 x) (+ 1 (neg x))))) (test-expand-taylor '(atanh x)))
  (check-equal? '(log (+ x (sqrt (+ (* x x) -1)))) (test-expand-taylor '(acosh x)))
  (check-equal? '(log (+ x (sqrt (+ (* x x) 1)))) (test-expand-taylor '(asinh x)))
  (check-equal? '(/ (+ (exp x) (neg (/ 1 (exp x)))) (+ (exp x) (/ 1 (exp x))))
                (test-expand-taylor '(tanh x)))
  (check-equal? '(* 1/2 (+ (exp x) (/ -1 (exp x)))) (test-expand-taylor '(sinh x)))
  (check-equal? '(+ 1 (neg (+ 2 (neg 3)))) (test-expand-taylor '(- 1 (- 2 3))))
  (check-equal? '(* 1/2 (+ (exp x) (/ 1 (exp x)))) (test-expand-taylor '(cosh x)))
  (check-equal? '(/ (sin x) (cos x)) (test-expand-taylor '(tan x)))
  (check-equal? '(+ 1 (neg (* 1/2 (+ (exp (/ (sin 3) (cos 3))) (/ 1 (exp (/ (sin 3) (cos 3))))))))
                (test-expand-taylor '(- 1 (cosh (tan 3)))))
  (check-equal? '(exp (* a (log x))) (test-expand-taylor '(pow x a)))
  (check-equal? '(+ x (sin a)) (test-expand-taylor '(+ x (sin a))))
  (check-equal? '(cbrt x) (test-expand-taylor '(pow x 1/3)))
  (check-equal? '(cbrt (* x x)) (test-expand-taylor '(pow x 2/3)))
  (check-equal? '(+ 100 (cbrt x)) (test-expand-taylor '(+ 100 (pow x 1/3))))
  (check-equal? `(+ 100 (cbrt (* x ,(approx 2 3))))
                (test-expand-taylor `(+ 100 (pow (* x ,(approx 2 3)) 1/3))))
  (check-equal? `(+ ,(approx 2 3) (cbrt x)) (test-expand-taylor `(+ ,(approx 2 3) (pow x 1/3))))
  (check-equal? `(+ (cbrt x) ,(approx 2 1/3)) (test-expand-taylor `(+ (pow x 1/3) ,(approx 2 1/3)))))

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
