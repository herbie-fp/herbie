#lang racket

(require "../utils/timeline.rkt")

(provide progs->batch
         batch->progs
         (struct-out batch)
         get-expr)

(struct batch (nodes roots vars nodes-length))

(define (progs->batch exprs #:timeline-push [timeline-push #f] #:vars [vars '()])
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
    (define node ; This compiles to the register machine
      (match prog
        [(list op args ...) (cons op (map munge args))]
        [_ prog]))
    (hash-ref! exprhash
               node
               (lambda ()
                 (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                   (set! exprc (+ 1 exprc))
                   (set! icache (cons node icache))))))

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

(define (get-expr nodes reg)
  (define (unmunge reg)
    (define node (vector-ref nodes reg))
    (match node
      [(list op regs ...) (cons op (map unmunge regs))]
      [_ node]))
  (unmunge reg))
