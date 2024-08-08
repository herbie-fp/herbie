#lang racket

(require "../utils/timeline.rkt"
         "../syntax/syntax.rkt")

(provide progs->batch
         batch->progs
         (struct-out batch)
         get-expr
         expand-taylor)

(struct batch ([nodes #:mutable] [roots #:mutable] vars [nodes-length #:mutable]))

(define (progs->batch exprs #:timeline-push [timeline-push #f] #:vars [vars '()] #:taylor [taylor #f])
  (define icache (reverse vars))
  (define exprhash
    (make-hash (for/list ([var vars] [i (in-naturals)])
                 (cons var i))))
  ; Counts
  (define size 0)
  (define exprc 0)
  (define varc (length vars))

  ; Translates programs into an instruction sequence of operations
  (define (munge prog)
    (set! size (+ 1 size))
    (match prog ; approx nodes are ignored
      [(approx _ impl) (munge impl)]
      [_
       (define node ; This compiles to the register machine
         (match prog
           [(list op args ...) (cons op (map munge args))]
           [_ prog]))
       (hash-ref! exprhash
                  node
                  (lambda ()
                    (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                      (set! exprc (+ 1 exprc))
                      (set! icache (cons node icache)))))]))

  (define roots (list->vector (map munge exprs)))
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
      [(list op regs ...) (cons op (map unmunge regs))]
      [else node]))

  (define exprs
    (for/list ([root (in-vector roots)])
      (unmunge root)))
  exprs)

(define (expand-taylor batch)
  (define vars (batch-vars batch))
  (define icache (reverse vars))
  (define exprhash
    (make-hash (for/list ([var vars] [i (in-naturals)])
                 (cons var i))))
  ; Counts
  (define exprc 0)
  (define varc (length vars))

  ; Translates programs into an instruction sequence of operations
  (define (munge prog)
    (match prog
      [(approx _ impl) (munge impl)]
      [_
       (define node ; This compiles to the register machine
         (match prog
           [(list '- arg1 arg2) `(+ ,(munge arg1) ,(munge `(neg ,arg2)))]
           [(list 'pow base 1/2) `(sqrt ,(munge base))]
           [(list 'pow base 1/3) `(cbrt ,(munge base))]
           [(list 'pow base 2/3) `(cbrt ,(munge `(* ,base ,base)))]
           [(list 'pow base power)
            #:when (exact-integer? power)
            `(pow ,(munge base) ,(munge power))]
           [(list 'pow base power) `(exp ,(munge `(* ,power (log ,base))))]
           [(list 'tan args) `(/ ,(munge `(sin ,args)) ,(munge `(cos ,args)))]
           [(list 'cosh args) `(* ,(munge 1/2) ,(munge `(+ (exp ,args) (/ 1 (exp ,args)))))]
           [(list 'sinh args) `(* ,(munge 1/2) ,(munge `(+ (exp ,args) (neg (/ 1 (exp ,args))))))]
           [(list 'tanh args)
            `(/ ,(munge `(+ (exp ,args) (neg (/ 1 (exp ,args)))))
                ,(munge `(+ (exp ,args) (/ 1 (exp ,args)))))]
           [(list 'asinh args) `(log ,(munge `(+ ,args (sqrt (+ (* ,args ,args) 1)))))]
           [(list 'acosh args) `(log ,(munge `(+ ,args (sqrt (+ (* ,args ,args) -1)))))]
           [(list 'atanh args) `(* ,(munge 1/2) ,(munge `(log (/ (+ 1 ,args) (+ 1 (neg ,args))))))]
           [(list op args ...) (cons op (map munge args))]
           [_ prog]))
       (hash-ref! exprhash
                  node
                  (lambda ()
                    (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                      (set! exprc (+ 1 exprc))
                      (set! icache (cons node icache)))))]))

  (set-batch-roots! batch (list->vector (map munge (batch->progs batch))))
  (set-batch-nodes! batch (list->vector (reverse icache)))
  (set-batch-nodes-length! batch (vector-length (batch-nodes batch))))

(define (get-expr nodes reg)
  (define (unmunge reg)
    (define node (vector-ref nodes reg))
    (match node
      [(list op regs ...) (cons op (map unmunge regs))]
      [_ node]))
  (unmunge reg))

(module+ test
  (require rackunit)
  (define (test-expand-taylor expr)
    (define batch (progs->batch (list expr)))
    (expand-taylor batch)
    (car (batch->progs batch)))

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
  (check-equal? '(cbrt x) (test-expand-taylor '(pow x 1/3)))
  (check-equal? '(+ 100 (cbrt x)) (test-expand-taylor '(+ 100 (pow x 1/3)))))
