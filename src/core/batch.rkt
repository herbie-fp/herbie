#lang racket

(require "../utils/timeline.rkt"
         "../syntax/syntax.rkt")

(provide progs->batch
         batch->progs
         (struct-out batch)
         get-expr
         expand-taylor)

(struct batch ([nodes #:mutable] [roots #:mutable] vars [nodes-length #:mutable]))

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

  (define roots
    (list->vector (map (if ignore-approx munge-ignore-approx munge-include-approx) exprs)))
  (define nodes (list->vector (reverse icache)))
  (define nodes-length (vector-length nodes))

  (when timeline-push
    (timeline-push! 'compiler (+ varc size) (+ exprc varc)))
  (batch nodes roots vars nodes-length))

(define (batch->progs batch)
  (define roots (batch-roots batch))
  (define nodes (batch-nodes batch))

  (define (unmunge reg)
    (define node (vector-ref nodes reg))
    (match node
      [(approx spec impl) (approx spec (unmunge impl))]
      [(list op regs ...) (cons op (map unmunge regs))]
      [_ node]))

  (define exprs
    (for/list ([root (in-vector roots)])
      (unmunge root)))
  exprs)

; TODO: ADD APPROX NODE, REMOVE UNUSED NODES SUCH AS 1/2 FROM POW(x, 1/2)
(define (expand-taylor input-batch)
  (define vars (batch-vars input-batch))
  (define nodes (batch-nodes input-batch))
  (define roots (batch-roots input-batch))

  (define icache (reverse vars))
  (define exprhash
    (make-hash (for/list ([var vars]
                          [i (in-naturals)])
                 (cons var i))))

  (define exprc 0)
  (define varc (length vars))

  (define mappings
    (make-hash (map (λ (n) (cons n n)) (build-list (batch-nodes-length input-batch) values))))

  (define (append-node node)
    (hash-ref! exprhash
               node
               (lambda ()
                 (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                   (set! exprc (+ 1 exprc))
                   (set! icache (cons node icache))))))

  (for ([node (in-vector nodes)]
        [n (in-naturals)])
    (match node
      [(list '- arg1 arg2)
       (define neg-index (append-node `(neg ,(hash-ref mappings arg2))))
       (hash-set! mappings n (append-node `(+ ,(hash-ref mappings arg1) ,neg-index)))]

      [(list 'pow base power)
       #:when (equal? (vector-ref nodes power) 1/2) ; 1/2 to be removed from exprhash
       (hash-set! mappings n (append-node `(sqrt ,(hash-ref mappings base))))]

      [(list 'pow base power)
       #:when (equal? (vector-ref nodes power) 1/3) ; 1/3 to be removed from exprhash
       (hash-set! mappings n (append-node `(cbrt ,(hash-ref mappings base))))]

      [(list 'pow base power)
       #:when (equal? (vector-ref nodes power) 2/3) ; 2/3 to be removed from exprhash
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
       (define one-idx (append-node 1)) ; should it be 1 or literal 1 or smth?
       (define inv-exp-idx (append-node `(/ ,one-idx ,exp-idx)))
       (define neg-idx (append-node `(neg ,inv-exp-idx)))
       (define add-idx (append-node `(+ ,exp-idx ,neg-idx)))
       (define half-idx (append-node 1/2))
       (hash-set! mappings n (append-node `(* ,half-idx ,add-idx)))]

      [(list 'tanh args)
       (define exp-idx (append-node `(exp ,(hash-ref mappings args))))
       (define one-idx (append-node 1)) ; should it be 1 or literal 1 or smth?
       (define inv-exp-idx (append-node `(/ ,one-idx ,exp-idx)))
       (define neg-idx (append-node `(neg ,inv-exp-idx)))
       (define add-idx (append-node `(+ ,exp-idx ,inv-exp-idx)))
       (define sub-idx (append-node `(+ ,exp-idx ,neg-idx)))
       (hash-set! mappings n (append-node `(/ ,sub-idx ,add-idx)))]

      [(list 'asinh args)
       (define mult-idx (append-node `(* ,(hash-ref mappings args) ,(hash-ref mappings args))))
       (define one-idx (append-node 1)) ; should it be 1 or literal 1 or smth?
       (define add-idx (append-node `(+ ,mult-idx ,one-idx)))
       (define sqrt-idx (append-node `(sqrt ,add-idx)))
       (define add2-idx (append-node `(+ ,(hash-ref mappings args) ,sqrt-idx)))
       (hash-set! mappings n (append-node `(log ,add2-idx)))]

      [(list 'acosh args)
       (define mult-idx (append-node `(* ,(hash-ref mappings args) ,(hash-ref mappings args))))
       (define -one-idx (append-node -1)) ; should it be -1 or literal -1 or smth?
       (define add-idx (append-node `(+ ,mult-idx ,-one-idx)))
       (define sqrt-idx (append-node `(sqrt ,add-idx)))
       (define add2-idx (append-node `(+ ,(hash-ref mappings args) ,sqrt-idx)))
       (hash-set! mappings n (append-node `(log ,add2-idx)))]

      [(list 'atanh args)
       (define neg-idx (append-node `(neg ,(hash-ref mappings args))))
       (define one-idx (append-node 1)) ; should it be 1 or literal 1 or smth?
       (define add-idx (append-node `(+ ,one-idx ,(hash-ref mappings args))))
       (define sub-idx (append-node `(+ ,one-idx ,neg-idx)))
       (define div-idx (append-node `(/ ,add-idx ,sub-idx)))
       (define log-idx (append-node `(log ,div-idx)))
       (define half-idx (append-node 1/2)) ; should it be 1/2 or literal 1/2 or smth?
       (hash-set! mappings n (append-node `(* ,half-idx ,log-idx)))]

      [(list op args ...) (append-node (cons op (map (curry hash-ref mappings) args)))]
      #;[(approx spec impl) (approx spec (munge impl))]
      [_ (append-node node)]))

  (define roots* (vector-map (curry hash-ref mappings) roots))
  (define nodes* (list->vector (reverse icache)))
  (batch nodes* roots* vars (vector-length nodes*)))

(define (get-expr nodes reg)
  (define (unmunge reg)
    (define node (vector-ref nodes reg))
    (match node
      [(approx spec impl) (approx spec (unmunge impl))]
      [(list op regs ...) (cons op (map unmunge regs))]
      [_ node]))
  (unmunge reg))

(module+ test
  (require rackunit)
  (define (test-expand-taylor expr)
    (define batch (progs->batch (list expr)))
    (define batch* (expand-taylor batch))
    (car (batch->progs batch*)))

  (define (test-munge-unmunge expr [ignore-approx #t])
    (define batch (progs->batch (list expr) #:ignore-approx ignore-approx))
    (check-equal? (list expr) (batch->progs batch)))

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

  (test-munge-unmunge '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))))
  (test-munge-unmunge
   '(+ 1 (neg (* 1/2 (+ (exp (/ (sin 3) (cos 3))) (/ 1 (exp (/ (sin 3) (cos 3)))))))))
  (test-munge-unmunge '(cbrt x))
  (test-munge-unmunge '(x))
  (test-munge-unmunge
   `(+ (sin ,(approx '(* 1/2 (+ (exp x) (neg (/ 1 (exp x))))) '(+ 3 (* 25 (sin 6))))) 4)
   #f))
