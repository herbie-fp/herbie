#lang racket

(require "../utils/timeline.rkt"
         "../syntax/syntax.rkt"
         "../utils/alternative.rkt")

(provide progs->batch
         batch->progs
         (struct-out batch)
         batch-ref
         expand-taylor
         batch-add-expr!
         remove-dupicates-roots!
         empty-batch
         nodes->batch)

(struct batch
        ([nodes #:mutable] [roots #:mutable] vars [nodes-length #:mutable] [exprhash #:mutable]))

(define (progs->batch exprs
                      #:timeline-push [timeline-push #f]
                      #:vars [vars '()]
                      #:ignore-approx [ignore-approx #t])
  (define icache (reverse vars))
  (define exprhash
    (make-hash (for/list ([var vars]
                          [i (in-naturals)])
                 (cons var i))))
  ; Counts
  (define size 0)
  (define exprc 0)
  (define varc (length vars))

  ; Translates programs into an instruction sequence of operations
  (define (munge-ignore-approx prog)
    (set! size (+ 1 size))
    (match prog ; approx nodes are ignored
      [(approx _ impl) (munge-ignore-approx impl)]
      [_
       (define node ; This compiles to the register machine
         (match prog
           [(alt expr event prevs preprocessing)
            (alt (munge-ignore-approx expr) event (map munge-ignore-approx prevs) preprocessing)]
           [(list op args ...) (cons op (map munge-ignore-approx args))]
           [_ prog]))
       (hash-ref! exprhash
                  node
                  (lambda ()
                    (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                      (set! exprc (+ 1 exprc))
                      (set! icache (cons node icache)))))]))

  ; Translates programs into an instruction sequence of operations
  (define (munge-include-approx prog)
    (set! size (+ 1 size))
    (define node ; This compiles to the register machine
      (match prog
        [(alt expr event prevs preprocessing)
         (alt (munge-include-approx expr) event (map munge-include-approx prevs) preprocessing)]
        [(approx spec impl) (approx spec (munge-include-approx impl))]
        [(list op args ...) (cons op (map munge-include-approx args))]
        [_ prog]))
    (hash-ref! exprhash
               node
               (lambda ()
                 (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                   (set! exprc (+ 1 exprc))
                   (set! icache (cons node icache))))))

  (define roots
    (list->vector (map (if ignore-approx munge-ignore-approx munge-include-approx) exprs)))
  (define nodes (list->vector (reverse icache)))
  (define nodes-length (vector-length nodes))

  (when timeline-push
    (timeline-push! 'compiler (+ varc size) (+ exprc varc)))
  (batch nodes roots vars nodes-length exprhash))

(define (batch->progs batch)
  (define roots (batch-roots batch))
  (define nodes (batch-nodes batch))

  (define (unmunge reg)
    (define node (vector-ref nodes reg))
    (match node
      [(alt expr event prevs preprocessing)
       (alt (unmunge expr) event (map unmunge prevs) preprocessing)]
      [(approx spec impl) (approx spec (unmunge impl))]
      [(list op regs ...) (cons op (map unmunge regs))]
      [_ node]))

  (define exprs
    (for/list ([root (in-vector roots)])
      (unmunge root)))
  exprs)

; Function transforms nodes to a batch
; nodes: (listof '(id op arg1-index arg2-index))
(define (nodes->batch nodes id->spec)
  ; Mapping from nodes to nodes*
  (define icache '())
  (define exprhash (make-hash))
  (define exprc 0)
  (define roots '())

  ; Adding a node to hash
  (define (append-node node)
    (hash-ref! exprhash
               node
               (lambda ()
                 (begin0 exprc
                   (set! exprc (+ 1 exprc))
                   (set! icache (cons node icache))))))
  
  ; adds nodes to a batch 
  (define (add-node id)
    (match (cdr (vector-ref nodes id))
      [(? number? n) (append-node n)] ; number
      [(? symbol? s) (append-node s)] ; variable
      [(list '$approx spec impl) ; approx
       (match (vector-ref id->spec spec)
         [#f (error nodes->batch "no initial approx node in eclass ~a" id)]
         [spec-e
          (append-node (list '$approx spec-e (add-node impl)))])]
      ; if expression
      [(list 'if cond ift iff)
       (append-node (list 'if (add-node cond) (add-node ift) (add-node iff)))]
      ; expression of impls
      [(list (? impl-exists? impl) ids ...)
       (append-node (cons impl (map add-node ids)))]
      ; expression of operators
      [(list (? operator-exists? op) ids ...)
       (append-node (cons op (map add-node ids)))]))

  (define (build-batch ids)
    (set! roots (list->vector (map add-node ids)))
    (define nodes (list->vector (reverse icache)))
    (batch nodes roots '() (vector-length nodes) exprhash))
  build-batch)

(define (expand-taylor input-batch)
  (define vars (batch-vars input-batch))
  (define nodes (batch-nodes input-batch))
  (define roots (batch-roots input-batch))

  ; Hash to avoid duplications
  (define icache (reverse vars))
  (define exprhash
    (make-hash (for/list ([var vars]
                          [i (in-naturals)])
                 (cons var i))))
  (define exprc 0)
  (define varc (length vars))

  ; Mapping from nodes to nodes*
  (define mappings
    (make-hash (map (λ (n) (cons n n)) (build-list (batch-nodes-length input-batch) values))))

  ; Adding a node to hash
  (define (append-node node)
    (hash-ref! exprhash
               node
               (lambda ()
                 (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                   (set! exprc (+ 1 exprc))
                   (set! icache (cons node icache))))))

  ; Sequential rewriting
  (for ([node (in-vector nodes)]
        [n (in-naturals)])
    (match node
      [(list '- arg1 arg2)
       (define neg-index (append-node `(neg ,(hash-ref mappings arg2))))
       (hash-set! mappings n (append-node `(+ ,(hash-ref mappings arg1) ,neg-index)))]
      [(list 'pow base power)
       #:when (equal? (vector-ref nodes power) 1/2) ; 1/2 is to be removed from exprhash
       (hash-set! mappings n (append-node `(sqrt ,(hash-ref mappings base))))]
      [(list 'pow base power)
       #:when (equal? (vector-ref nodes power) 1/3) ; 1/3 is to be removed from exprhash
       (hash-set! mappings n (append-node `(cbrt ,(hash-ref mappings base))))]
      [(list 'pow base power)
       #:when (equal? (vector-ref nodes power) 2/3) ; 2/3 is to be removed from exprhash
       (define mult-index (append-node `(* ,(hash-ref mappings base) ,(hash-ref mappings base))))
       (hash-set! mappings n (append-node `(cbrt ,mult-index)))]
      [(list 'pow base power)
       #:when (exact-integer? (vector-ref nodes power))
       (hash-set! mappings
                  n
                  (append-node `(pow ,(hash-ref mappings base) ,(hash-ref mappings power))))]
      [(list 'pow base power)
       (define log-idx (append-node `(log ,(hash-ref mappings base))))
       (define mult-idx (append-node `(* ,(hash-ref mappings power) ,log-idx)))
       (hash-set! mappings n (append-node `(exp ,mult-idx)))]
      [(list 'tan args)
       (define sin-idx (append-node `(sin ,(hash-ref mappings args))))
       (define cos-idx (append-node `(cos ,(hash-ref mappings args))))
       (hash-set! mappings n (append-node `(/ ,sin-idx ,cos-idx)))]
      [(list 'cosh args)
       (define exp-idx (append-node `(exp ,(hash-ref mappings args))))
       (define one-idx (append-node 1)) ; should it be 1 or literal 1 or smth?
       (define inv-exp-idx (append-node `(/ ,one-idx ,exp-idx)))
       (define add-idx (append-node `(+ ,exp-idx ,inv-exp-idx)))
       (define half-idx (append-node 1/2))
       (hash-set! mappings n (append-node `(* ,half-idx ,add-idx)))]
      [(list 'sinh args)
       (define exp-idx (append-node `(exp ,(hash-ref mappings args))))
       (define one-idx (append-node 1))
       (define inv-exp-idx (append-node `(/ ,one-idx ,exp-idx)))
       (define neg-idx (append-node `(neg ,inv-exp-idx)))
       (define add-idx (append-node `(+ ,exp-idx ,neg-idx)))
       (define half-idx (append-node 1/2))
       (hash-set! mappings n (append-node `(* ,half-idx ,add-idx)))]
      [(list 'tanh args)
       (define exp-idx (append-node `(exp ,(hash-ref mappings args))))
       (define one-idx (append-node 1))
       (define inv-exp-idx (append-node `(/ ,one-idx ,exp-idx)))
       (define neg-idx (append-node `(neg ,inv-exp-idx)))
       (define add-idx (append-node `(+ ,exp-idx ,inv-exp-idx)))
       (define sub-idx (append-node `(+ ,exp-idx ,neg-idx)))
       (hash-set! mappings n (append-node `(/ ,sub-idx ,add-idx)))]
      [(list 'asinh args)
       (define mult-idx (append-node `(* ,(hash-ref mappings args) ,(hash-ref mappings args))))
       (define one-idx (append-node 1))
       (define add-idx (append-node `(+ ,mult-idx ,one-idx)))
       (define sqrt-idx (append-node `(sqrt ,add-idx)))
       (define add2-idx (append-node `(+ ,(hash-ref mappings args) ,sqrt-idx)))
       (hash-set! mappings n (append-node `(log ,add2-idx)))]
      [(list 'acosh args)
       (define mult-idx (append-node `(* ,(hash-ref mappings args) ,(hash-ref mappings args))))
       (define -one-idx (append-node -1))
       (define add-idx (append-node `(+ ,mult-idx ,-one-idx)))
       (define sqrt-idx (append-node `(sqrt ,add-idx)))
       (define add2-idx (append-node `(+ ,(hash-ref mappings args) ,sqrt-idx)))
       (hash-set! mappings n (append-node `(log ,add2-idx)))]
      [(list 'atanh args)
       (define neg-idx (append-node `(neg ,(hash-ref mappings args))))
       (define one-idx (append-node 1))
       (define add-idx (append-node `(+ ,one-idx ,(hash-ref mappings args))))
       (define sub-idx (append-node `(+ ,one-idx ,neg-idx)))
       (define div-idx (append-node `(/ ,add-idx ,sub-idx)))
       (define log-idx (append-node `(log ,div-idx)))
       (define half-idx (append-node 1/2))
       (hash-set! mappings n (append-node `(* ,half-idx ,log-idx)))]
      [(alt expr event prevs preprocessing)
       (hash-set!
        mappings
        n
        (append-node
         (alt (hash-ref mappings expr) event (map (curry hash-ref mappings) prevs) preprocessing)))]
      [(list op args ...)
       (hash-set! mappings n (append-node (cons op (map (curry hash-ref mappings) args))))]
      [(approx spec impl) (hash-set! mappings n (append-node (approx spec (hash-ref mappings impl))))]
      [_ (hash-set! mappings n (append-node node))]))
  (define roots* (vector-map (curry hash-ref mappings) roots))
  (define nodes* (list->vector (reverse icache)))

  ; This may be too expensive to handle simple 1/2, 1/3 and 2/3 zombie nodes..
  #;(remove-zombie-nodes (batch nodes* roots* vars (vector-length nodes*)))

  (batch nodes* roots* vars (vector-length nodes*) exprhash))

; Updates in-batch by adding new expressions
; Returns list of new roots
(define (batch-add-expr! in-batch expr #:ignore-approx [ignore-approx #f])
  (define exprhash (batch-exprhash in-batch))
  (define icache '())
  (define exprc (length (hash-keys exprhash)))

  ; Translates programs into an instruction sequence of operations
  (define (munge-ignore-approx prog)
    (match prog ; approx nodes are ignored
      [(approx _ impl) (munge-ignore-approx impl)]
      [_
       (define node ; This compiles to the register machine
         (match prog
           [(alt expr event prevs preprocessing)
            (alt (munge-ignore-approx expr) event (map munge-ignore-approx prevs) preprocessing)]
           [(list op args ...) (cons op (map munge-ignore-approx args))]
           [_ prog]))
       (hash-ref! exprhash
                  node
                  (lambda ()
                    (begin0 exprc ; store in cache, update exprs, exprc
                      (set! exprc (+ 1 exprc))
                      (set! icache (cons node icache)))))]))

  ; Translates programs into an instruction sequence of operations
  (define (munge-include-approx prog)
    (define node ; This compiles to the register machine
      (match prog
        [(alt expr event prevs preprocessing)
         (alt (munge-include-approx expr) event (map munge-include-approx prevs) preprocessing)]
        [(approx spec impl) (approx spec (munge-include-approx impl))]
        [(list op args ...) (cons op (map munge-include-approx args))]
        [_ prog]))
    (hash-ref! exprhash
               node
               (lambda ()
                 (begin0 exprc ; store in cache, update exprs, exprc
                   (set! exprc (+ 1 exprc))
                   (set! icache (cons node icache))))))

  (define root (if ignore-approx (munge-ignore-approx expr) (munge-include-approx expr)))
  (set-batch-roots! in-batch (vector-append (batch-roots in-batch) (vector root)))
  (set-batch-nodes! in-batch (vector-append (batch-nodes in-batch) (list->vector (reverse icache))))
  (set-batch-nodes-length! in-batch (vector-length (batch-nodes in-batch)))
  (set-batch-exprhash! in-batch exprhash)
  root)

; The function removes any zombie nodes from batch
#;(define (remove-zombie-nodes input-batch)
    (define nodes (batch-nodes input-batch))
    (define roots (batch-roots input-batch))
    (define nodes-length (batch-nodes-length input-batch))

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

    (define mappings (make-hash (map (λ (n) (cons n n)) (build-list nodes-length values))))

    (define nodes* '())
    (for ([node (in-vector nodes)]
          [zmb (in-vector zombie-mask)]
          [n (in-naturals)])
      (if zmb
          (map (λ (n) (hash-set! mappings n (sub1 (hash-ref mappings n)))) (range n nodes-length))
          (set! nodes*
                (cons (match node
                        [(list op args ...) (cons op (map (curry hash-ref mappings) args))]
                        [(approx spec impl) (approx spec (hash-ref mappings impl))]
                        [_ node])
                      nodes*))))
    (set! nodes* (list->vector (reverse nodes*)))
    (define roots* (vector-map (curry hash-ref mappings) roots))
    ; TODO: make exprhash for the new batch
    (batch nodes* roots* (batch-vars input-batch) (vector-length nodes*) (make-hash)))

(define (empty-batch)
  (batch (make-vector 0) (make-vector 0) '() 0 (make-hash)))

(define (batch-ref batch reg)
  (define (unmunge reg)
    (define node (vector-ref (batch-nodes batch) reg))
    (match node
      [(approx spec impl) (approx spec (unmunge impl))]
      [(list op regs ...) (cons op (map unmunge regs))]
      [_ node]))
  (unmunge reg))

(define (remove-dupicates-roots! batch)
  (set-batch-roots! batch (list->vector (remove-duplicates (vector->list (batch-roots batch))))))

; Tests for expand-taylor
(module+ test
  (require rackunit)
  (define (test-expand-taylor expr)
    (define batch (progs->batch (list expr) #:ignore-approx #f))
    (define batch* (expand-taylor batch))
    (car (batch->progs batch*)))

  (check-equal? '(* 1/2 (log (/ (+ 1 x) (+ 1 (neg x))))) (test-expand-taylor '(atanh x)))
  (check-equal? '(log (+ x (sqrt (+ (* x x) -1)))) (test-expand-taylor '(acosh x)))
  (check-equal? '(log (+ x (sqrt (+ (* x x) 1)))) (test-expand-taylor '(asinh x)))
  (check-equal? '(/ (+ (exp x) (neg (/ 1 (exp x)))) (+ (exp x) (/ 1 (exp x))))
                (test-expand-taylor '(tanh x)))
  (check-equal? '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))) (test-expand-taylor '(sinh x)))
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
  (check-equal? `(+ (cbrt x) ,(approx 2 1/3)) (test-expand-taylor `(+ (pow x 1/3) ,(approx 2 1/3))))
  (check-equal? (alt '(+ 2 (neg x)) 'something (list '(sqrt x)) 'something)
                (test-expand-taylor (alt '(- 2 x) 'something (list '(pow x 1/2)) 'something))))

; Tests for progs->batch and batch->progs
(module+ test
  (require rackunit)
  (define (test-munge-unmunge expr [ignore-approx #t])
    (define batch (progs->batch (list expr) #:ignore-approx ignore-approx))
    (check-equal? (list expr) (batch->progs batch)))

  (test-munge-unmunge '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))))
  (test-munge-unmunge
   '(+ 1 (neg (* 1/2 (+ (exp (/ (sin 3) (cos 3))) (/ 1 (exp (/ (sin 3) (cos 3)))))))))
  (test-munge-unmunge '(cbrt x))
  (test-munge-unmunge '(x))
  (test-munge-unmunge
   `(+ (sin ,(approx '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))) '(+ 3 (* 25 (sin 6))))) 4)
   #f)
  (test-munge-unmunge (alt '(* 223 (pow 2 x)) 'something (list '(/ (pow 2 x) 4)) 'something)))

; Tests for remove-zombie-nodes
#;
(module+ test
  (require rackunit)
  (define (zombie-test #:nodes nodes #:roots roots)
    (define in-batch (batch nodes roots '() (vector-length nodes) (make-hash)))
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

(module+ test
  (require rackunit)
  (define (batch-add!-test expr1 expr2)
    (define batch (progs->batch (list expr1)))
    (define root (batch-add-expr! batch expr2))
    (check-equal? (batch-ref batch root) expr2)
    (batch->progs batch))

  (check-equal? '((* 3 (pow 5 (tan x))) (* (pow 3) (tan x)))
                (batch-add!-test '(* 3 (pow 5 (tan x))) '(* (pow 3) (tan x))))
  (check-equal? '((* 2 3) (+ (* 2 3) 1)) (batch-add!-test '(* 2 3) '(+ (* 2 3) 1))))
