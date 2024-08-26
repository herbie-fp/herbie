#lang racket

(require "../utils/timeline.rkt"
         "../syntax/syntax.rkt")

(provide progs->batch
         batch->progs
         (struct-out batch)
         batch-get-expr
         batch-ref
         expand-taylor
         empty-batch
         nodes->batch
         batch-add-expr!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structure batch

;; - nodes: a vector of operations that expressions within batch contain, highly linked to avoid any duplications
;; - alt-exprs: pointers to a node from nodes that contain main expressions
;; - events: a vector of events that corresponding alt-expr has overcomed
;; - prevs: a vector of previous expressions that corresponding alt-expr has been derived from
;; - preprocessings: a vector of preprocessing history that corresponding alt-expr has overcomed
;; - vars: list of free variables inside batch
;; - nodes-length: length of nodes vector
;; - exprhash: a hash that maps a node to its index in nodes vector
;;
;; Constraints: lengths of alt-exprs/events/prevs/preprocessings should be equal
;;              node arguments [0,n) where n is a (hash-count exprhash) or (vector-length nodes)
(struct batch
        ([nodes #:mutable] [alt-exprs #:mutable]
                           [events #:mutable]
                           [prevs #:mutable]
                           [preprocessings #:mutable]
                           vars
                           [nodes-length #:mutable]
                           [exprhash #:mutable]))

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
        [(approx spec impl) (approx spec (munge-include-approx impl))]
        [(list op args ...) (cons op (map munge-include-approx args))]
        [_ prog]))
    (hash-ref! exprhash
               node
               (lambda ()
                 (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                   (set! exprc (+ 1 exprc))
                   (set! icache (cons node icache))))))

  (define alt-exprs
    (list->vector (map (if ignore-approx munge-ignore-approx munge-include-approx) exprs)))
  (define events (make-vector (vector-length alt-exprs) '()))
  (define prevs (make-vector (vector-length alt-exprs) '()))
  (define preprocessings (make-vector (vector-length alt-exprs) '()))
  (define nodes (list->vector (reverse icache)))
  (define nodes-length (vector-length nodes))

  (when timeline-push
    (timeline-push! 'compiler (+ varc size) (+ exprc varc)))
  (batch nodes alt-exprs events prevs preprocessings vars nodes-length exprhash))

; Simply recovering alt-expressions from a batch without considering prevs/preprocessings/events
(define (batch->progs batch)
  (define roots (batch-alt-exprs batch))
  (define nodes (batch-nodes batch))

  (define (unmunge reg)
    (define node (vector-ref nodes reg))
    (match node
      [(approx spec impl) (approx spec (unmunge impl))]
      [(list '$approx spec impl)
       (list '$approx
             spec
             (unmunge impl))] ; this row is to be deleted and needed only in egg-herbie.rkt
      [(list op regs ...) (cons op (map unmunge regs))]
      [_ node]))

  (define exprs
    (for/list ([root (in-vector roots)])
      (unmunge root)))
  exprs)

; Function transforms nodes to a batch, used in egg-herbie.rkt for egraph extraction
; nodes: (listof '(cost op arg1-index arg2-index))
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
         [spec-e (append-node (list '$approx spec-e (add-node impl)))])]
      ; if expression
      [(list 'if cond ift iff) (append-node (list 'if (add-node cond) (add-node ift) (add-node iff)))]
      ; expression of impls
      [(list (? impl-exists? impl) ids ...) (append-node (cons impl (map add-node ids)))]
      ; expression of operators
      [(list (? operator-exists? op) ids ...) (append-node (cons op (map add-node ids)))]))

  ; This function is better to be eliminated because it duplicates add-node at some point
  (define (add-root enode)
    (define idx
      (match enode
        [(? number?) (append-node enode)]
        [(? symbol?) (append-node enode)]
        [(list '$approx spec impl)
         (define spec* (vector-ref id->spec spec))
         (unless spec*
           (error 'regraph-extract-variants "no initial approx node in eclass"))
         (define impl-idx (add-node impl))
         (append-node (list '$approx spec* impl-idx))]
        [(list 'if cond ift iff)
         (define cond-idx (add-node cond))
         (define ift-idx (add-node ift))
         (define iff-idx (add-node iff))
         (append-node (list 'if cond-idx ift-idx iff-idx))]
        [(list (? impl-exists? impl) ids ...)
         (define args
           (for/list ([id (in-list ids)])
             (add-node id)))
         (append-node (cons impl args))]
        [(list (? operator-exists? op) ids ...)
         (define args
           (for/list ([id (in-list ids)])
             (add-node id)))
         (append-node (cons op args))]))
    (set! roots (cons idx roots)))

  (define (finalize-batch)
    (define alt-exprs (list->vector (remove-duplicates (reverse roots))))
    (define events (make-vector (vector-length alt-exprs) '()))
    (define prevs (make-vector (vector-length alt-exprs) '()))
    (define preprocessings (make-vector (vector-length alt-exprs) '()))
    (define nodes* (list->vector (reverse icache)))
    (batch nodes* alt-exprs events prevs preprocessings '() (length icache) exprhash))

  (define (clean-batch)
    (set! exprc 0)
    (set! icache '())
    (set! roots '())
    (set! exprhash (make-hash)))

  (values add-root clean-batch finalize-batch))

(define (expand-taylor input-batch)
  (define vars (batch-vars input-batch))
  (define nodes (batch-nodes input-batch))
  (define alt-exprs (batch-alt-exprs input-batch))

  ; Hash to avoid duplications
  (define icache (reverse vars))
  (define exprhash
    (make-hash (for/list ([var vars]
                          [i (in-naturals)])
                 (cons var i))))
  (define exprc 0)
  (define varc (length vars))

  ; Mapping from nodes to nodes*
  (define mappings (build-vector (batch-nodes-length input-batch) values))

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
       (define neg-index (append-node `(neg ,(vector-ref mappings arg2))))
       (vector-set! mappings n (append-node `(+ ,(vector-ref mappings arg1) ,neg-index)))]
      [(list 'pow base power)
       #:when (equal? (vector-ref nodes power) 1/2) ; 1/2 is to be removed from exprhash
       (vector-set! mappings n (append-node `(sqrt ,(vector-ref mappings base))))]
      [(list 'pow base power)
       #:when (equal? (vector-ref nodes power) 1/3) ; 1/3 is to be removed from exprhash
       (vector-set! mappings n (append-node `(cbrt ,(vector-ref mappings base))))]
      [(list 'pow base power)
       #:when (equal? (vector-ref nodes power) 2/3) ; 2/3 is to be removed from exprhash
       (define mult-index (append-node `(* ,(vector-ref mappings base) ,(vector-ref mappings base))))
       (vector-set! mappings n (append-node `(cbrt ,mult-index)))]
      [(list 'pow base power)
       #:when (exact-integer? (vector-ref nodes power))
       (vector-set! mappings
                    n
                    (append-node `(pow ,(vector-ref mappings base) ,(vector-ref mappings power))))]
      [(list 'pow base power)
       (define log-idx (append-node `(log ,(vector-ref mappings base))))
       (define mult-idx (append-node `(* ,(vector-ref mappings power) ,log-idx)))
       (vector-set! mappings n (append-node `(exp ,mult-idx)))]
      [(list 'tan args)
       (define sin-idx (append-node `(sin ,(vector-ref mappings args))))
       (define cos-idx (append-node `(cos ,(vector-ref mappings args))))
       (vector-set! mappings n (append-node `(/ ,sin-idx ,cos-idx)))]
      [(list 'cosh args)
       (define exp-idx (append-node `(exp ,(vector-ref mappings args))))
       (define one-idx (append-node 1)) ; should it be 1 or literal 1 or smth?
       (define inv-exp-idx (append-node `(/ ,one-idx ,exp-idx)))
       (define add-idx (append-node `(+ ,exp-idx ,inv-exp-idx)))
       (define half-idx (append-node 1/2))
       (vector-set! mappings n (append-node `(* ,half-idx ,add-idx)))]
      [(list 'sinh args)
       (define exp-idx (append-node `(exp ,(vector-ref mappings args))))
       (define one-idx (append-node 1))
       (define inv-exp-idx (append-node `(/ ,one-idx ,exp-idx)))
       (define neg-idx (append-node `(neg ,inv-exp-idx)))
       (define add-idx (append-node `(+ ,exp-idx ,neg-idx)))
       (define half-idx (append-node 1/2))
       (vector-set! mappings n (append-node `(* ,half-idx ,add-idx)))]
      [(list 'tanh args)
       (define exp-idx (append-node `(exp ,(vector-ref mappings args))))
       (define one-idx (append-node 1))
       (define inv-exp-idx (append-node `(/ ,one-idx ,exp-idx)))
       (define neg-idx (append-node `(neg ,inv-exp-idx)))
       (define add-idx (append-node `(+ ,exp-idx ,inv-exp-idx)))
       (define sub-idx (append-node `(+ ,exp-idx ,neg-idx)))
       (vector-set! mappings n (append-node `(/ ,sub-idx ,add-idx)))]
      [(list 'asinh args)
       (define mult-idx (append-node `(* ,(vector-ref mappings args) ,(vector-ref mappings args))))
       (define one-idx (append-node 1))
       (define add-idx (append-node `(+ ,mult-idx ,one-idx)))
       (define sqrt-idx (append-node `(sqrt ,add-idx)))
       (define add2-idx (append-node `(+ ,(vector-ref mappings args) ,sqrt-idx)))
       (vector-set! mappings n (append-node `(log ,add2-idx)))]
      [(list 'acosh args)
       (define mult-idx (append-node `(* ,(vector-ref mappings args) ,(vector-ref mappings args))))
       (define -one-idx (append-node -1))
       (define add-idx (append-node `(+ ,mult-idx ,-one-idx)))
       (define sqrt-idx (append-node `(sqrt ,add-idx)))
       (define add2-idx (append-node `(+ ,(vector-ref mappings args) ,sqrt-idx)))
       (vector-set! mappings n (append-node `(log ,add2-idx)))]
      [(list 'atanh args)
       (define neg-idx (append-node `(neg ,(vector-ref mappings args))))
       (define one-idx (append-node 1))
       (define add-idx (append-node `(+ ,one-idx ,(vector-ref mappings args))))
       (define sub-idx (append-node `(+ ,one-idx ,neg-idx)))
       (define div-idx (append-node `(/ ,add-idx ,sub-idx)))
       (define log-idx (append-node `(log ,div-idx)))
       (define half-idx (append-node 1/2))
       (vector-set! mappings n (append-node `(* ,half-idx ,log-idx)))]
      [(list op args ...)
       (vector-set! mappings n (append-node (cons op (map (curry vector-ref mappings) args))))]
      [(approx spec impl)
       (vector-set! mappings n (append-node (approx spec (vector-ref mappings impl))))]
      [_ (vector-set! mappings n (append-node node))]))

  (define alt-exprs* (vector-map (curry vector-ref mappings) alt-exprs))
  (define nodes* (list->vector (reverse icache)))

  ; This may be too expensive to handle simple 1/2, 1/3 and 2/3 zombie nodes..
  #;(remove-zombie-nodes (batch nodes* roots* vars (vector-length nodes*)))

  (batch nodes*
         alt-exprs*
         (batch-events input-batch)
         (batch-prevs input-batch)
         (batch-preprocessings input-batch)
         vars
         (vector-length nodes*)
         exprhash))

; Updates in-batch by adding new expressions
; Returns list of new roots
(define (batch-add-expr! in-batch expr #:ignore-approx [ignore-approx #f])
  (define exprhash (batch-exprhash in-batch))
  (define icache '())
  (define exprc (hash-count exprhash))

  (define (munge-ignore-approx prog)
    (match prog ; approx nodes are ignored
      [(approx _ impl) (munge-ignore-approx impl)]
      [_
       (define node ; This compiles to the register machine
         (match prog
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
  (set-batch-alt-exprs! in-batch (vector-append (batch-alt-exprs in-batch) (vector root)))
  (set-batch-nodes! in-batch (vector-append (batch-nodes in-batch) (list->vector (reverse icache))))
  (set-batch-nodes-length! in-batch (vector-length (batch-nodes in-batch)))
  (set-batch-exprhash! in-batch exprhash)
  root)

; The function removes any zombie nodes from batch
; TODO: reconstruct exprhash
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
    (batch nodes* roots* (batch-vars input-batch) (vector-length nodes*)))

(define (empty-batch)
  (batch (make-vector 0)
         (make-vector 0)
         (make-vector 0)
         (make-vector 0)
         (make-vector 0)
         '()
         0
         (make-hash)))

(define (in-batch batch)
  (for/stream ([alt-expr (in-vector (batch-alt-exprs batch))]
               [event (in-vector (batch-events batch))]
               [prev (in-vector (batch-prevs batch))]
               [preprocessing (in-vector (batch-preprocessings batch))]
               [n (in-naturals)])
              (values n alt-expr event prev preprocessing)))

(define (batch-ref batch idx)
  (vector (vector-ref (batch-alt-exprs batch) idx)
          (vector-ref (batch-events batch) idx)
          (vector-ref (batch-prevs batch) idx)
          (vector-ref (batch-preprocessings batch) idx)))

; Function returns a recovered expression from nodes with index idx
(define (batch-get-expr batch idx)
  (define (unmunge idx)
    (define node (vector-ref (batch-nodes batch) idx))
    (match node
      [(approx spec impl) (approx spec (unmunge impl))]
      [(list op regs ...) (cons op (map unmunge regs))]
      [_ node]))
  (unmunge idx))

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
  (check-equal? `(+ (cbrt x) ,(approx 2 1/3)) (test-expand-taylor `(+ (pow x 1/3) ,(approx 2 1/3)))))

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
   #f))

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
  (define (batch-add!-test exprs)
    (define batch (empty-batch))
    (for ([expr (in-list exprs)])
      (batch-add-expr! batch expr))
    (check-equal? exprs (batch->progs batch)))

  (batch-add!-test '((* 3 (pow 5 (tan x))) (* (pow 3) (tan x))))
  (batch-add!-test '((* 3 (pow 5 (tan x))) (* 3 (pow 5 (tan (* (pow 3) (tan x)))))
                                           (* (pow 3) (tan x))))
  (batch-add!-test '((* 2 3) (pow 2 3) (pow 2 (exp 3)) (+ (* 2 3) 1))))
