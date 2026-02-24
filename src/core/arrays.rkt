#lang racket

(require racket/hash
         racket/list
         "../syntax/types.rkt")

(provide flatten-arrays-for-rival)

;; Flatten array inputs/outputs into scalar inputs/outputs for Rival.
;; Returns:
;;   - flattened specs
;;   - flattened contexts
;;   - flattened precondition
;;   - point assembler (original point -> flattened point)
;;   - output assembler (flattened outputs -> original outputs)
;;   - flattened output reprs
(define (flatten-arrays-for-rival specs ctxs pre)
  (define orig-vars (context-vars (first ctxs)))
  (define orig-reprs (map context-repr ctxs))
  (define orig-var-reprs (context-var-reprs (first ctxs)))
  (define taken (apply mutable-seteq orig-vars))
  (define (fresh base)
    (let loop ([i 0])
      (define candidate (string->symbol (format "~a_~a" base i)))
      (if (set-member? taken candidate)
          (loop (add1 i))
          (begin
            (set-add! taken candidate)
            candidate))))

  (define env (make-hasheq))
  (define new-vars '())
  (define new-var-reprs '())
  (for ([v orig-vars]
        [r orig-var-reprs])
    (cond
      [(array-representation? r)
       (define base (symbol->string v))
       (define len (array-representation-len r))
       (define vars
         (for/list ([_ (in-range len)])
           (fresh base)))
       (hash-set! env v `(array ,@vars))
       (set! new-vars (append new-vars vars))
       (set! new-var-reprs (append new-var-reprs (make-list len (array-representation-elem r))))]
      [else
       (hash-set! env v v)
       (set! new-vars (append new-vars (list v)))
       (set! new-var-reprs (append new-var-reprs (list r)))]))
  (define (lower-arr expr)
    (match expr
      [(? number?) expr]
      [(? symbol? s) (hash-ref env s s)]
      [`(,op ,args ...)
       (match `(,op ,@(map lower-arr args))
         [`(ref (array ,elems ...) ,idx) (list-ref elems idx)]
         [other other])]))

  (define new-specs '())
  (define new-reprs '())
  (define output-lens '())
  (for ([spec (in-list specs)]
        [repr (in-list orig-reprs)])
    (define lowered (lower-arr spec))
    (cond
      [(array-representation? repr)
       (define comps
         (match lowered
           [`(array ,elems ...) elems]))
       (define elem-repr (array-representation-elem repr))
       (define len (array-representation-len repr))
       (set! output-lens (append output-lens (list len)))
       (set! new-specs (append new-specs comps))
       (set! new-reprs (append new-reprs (make-list (length comps) elem-repr)))]
      [else
       (set! output-lens (append output-lens (list #f)))
       (set! new-specs (append new-specs (list lowered)))
       (set! new-reprs (append new-reprs (list repr)))]))

  (define new-pre (lower-arr pre))
  (define ctxs*
    (for/list ([ctx (in-list ctxs)])
      (define repr (context-repr ctx))
      (context new-vars
               (if (array-representation? repr)
                   (array-representation-elem repr)
                   repr)
               new-var-reprs)))

  (define (assemble-point pt)
    (define idx 0)
    (define (next)
      (begin0 (vector-ref pt idx)
        (set! idx (add1 idx))))
    (for/vector #:length (length orig-var-reprs)
                ([r (in-list orig-var-reprs)])
      (if (array-representation? r)
          (for/vector #:length (array-representation-len r)
                      ([_ (in-range (array-representation-len r))])
            (next))
          (next))))

  (define (assemble-output outs)
    (define outputs
      (if (vector? outs)
          (vector->list outs)
          outs))
    (define idx 0)
    (define (next)
      (begin0 (list-ref outputs idx)
        (set! idx (add1 idx))))
    (for/list ([len (in-list output-lens)])
      (if len
          (for/vector #:length len
                      ([_ (in-range len)])
            (next))
          (next))))

  (values new-specs ctxs* new-pre assemble-point assemble-output new-reprs))

(module+ test
  (require rackunit)

  (define vec2 (make-array-representation #:elem <binary64> #:len 2))
  (define ctx (context '(x) <binary64> (list vec2)))
  (let-values ([(specs* _ pre* _assemble-point _assemble-output _reprs*)
                (flatten-arrays-for-rival (list '(ref x 1)) (list ctx) '(< (ref x 0) (ref x 1)))])
    (check-equal? specs* '(x_1))
    (check-equal? pre* '(< x_0 x_1))))
