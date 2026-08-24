#lang racket

(require racket/hash
         racket/list
         "../syntax/types.rkt")

(provide flatten-tuples-for-rival)

;; Flatten tuple inputs/outputs into scalar inputs/outputs for Rival.
;; Returns:
;;   - flattened specs
;;   - flattened contexts
;;   - flattened precondition
;;   - point assembler (original point -> flattened point)
;;   - output assembler (flattened outputs -> original outputs)
;;   - flattened output reprs

;; Returns whether an expression contains a tuple literal or reference.
;; Tuple-valued intermediate expressions in LSpec are represented by tuple
;; literals, and references can hide them behind another expression.
(define (tuple-expression? expr)
  (match expr
    [(list 'tuple _ ...) #t]
    [(list 'ref _ ...) #t]
    [(list _ args ...) (ormap tuple-expression? args)]
    [_ #f]))

(define (flatten-tuples-for-rival/no-tuples? specs ctxs pre)
  (and (not (ormap tuple-representation? (map context-repr ctxs)))
       (not (ormap tuple-representation? (append* (map context-var-reprs ctxs))))
       (not (ormap tuple-expression? (cons pre specs)))))

(define (flatten-tuples-for-rival/no-tuples specs ctxs pre)
  (values specs ctxs pre identity identity (map context-repr ctxs)))

(define (flatten-tuples-for-rival specs ctxs pre)
  (if (flatten-tuples-for-rival/no-tuples? specs ctxs pre)
      (flatten-tuples-for-rival/no-tuples specs ctxs pre)
      (flatten-tuples-for-rival/with-tuples specs ctxs pre)))

(define (flatten-tuples-for-rival/with-tuples specs ctxs pre)
  (define orig-vars (context-vars (first ctxs)))
  (define orig-reprs (map context-repr ctxs))
  (define orig-var-reprs (context-var-reprs (first ctxs)))
  (define taken (apply mutable-seteq orig-vars))
  (define (fresh base)
    (let loop ([i 0])
      (define candidate (string->symbol (format "~a_~a" base i)))
      (cond
        [(set-member? taken candidate) (loop (add1 i))]
        [else
         (set-add! taken candidate)
         candidate])))
  (define (leaf-reprs repr)
    (if (tuple-representation? repr)
        (append* (map leaf-reprs (tuple-representation-slots repr)))
        (list repr)))
  (define (fresh-tree base repr)
    (cond
      [(tuple-representation? repr)
       (define-values (elems vars reprs)
         (for/lists (elems vars reprs)
                    ([slot (in-list (tuple-representation-slots repr))])
                    (fresh-tree base slot)))
       (values `(tuple ,@elems) (append* vars) (append* reprs))]
      [else
       (define v (fresh base))
       (values v (list v) (list repr))]))
  (define (flatten-by-repr expr repr)
    (cond
      [(tuple-representation? repr)
       (match-let ([`(tuple ,elems ...) expr])
         (append* (for/list ([elem (in-list elems)]
                             [slot (in-list (tuple-representation-slots repr))])
                    (flatten-by-repr elem slot))))]
      [else (list expr)]))
  (define (build-value next repr)
    (cond
      [(tuple-representation? repr)
       (for/vector #:length (length (tuple-representation-slots repr))
                   ([slot (in-list (tuple-representation-slots repr))])
         (build-value next slot))]
      [else (next)]))

  (define env (make-hasheq))
  (define new-vars '())
  (define new-var-reprs '())
  (for ([v orig-vars]
        [r orig-var-reprs])
    (cond
      [(tuple-representation? r)
       (define base (symbol->string v))
       (define-values (tree vars reprs) (fresh-tree base r))
       (hash-set! env v tree)
       (set! new-vars (append new-vars vars))
       (set! new-var-reprs (append new-var-reprs reprs))]
      [else
       (hash-set! env v v)
       (set! new-vars (append new-vars (list v)))
       (set! new-var-reprs (append new-var-reprs (list r)))]))
  (define (lower-tuples expr)
    (match expr
      [(? number?) expr]
      [(? symbol? s) (hash-ref env s s)]
      [`(,op ,args ...)
       (define lowered `(,op ,@(map lower-tuples args)))
       (match lowered
         [`(ref (tuple ,elems ...) ,idx) (list-ref elems idx)]
         [_ lowered])]))

  (define new-specs '())
  (define new-reprs '())
  (for ([spec (in-list specs)]
        [repr (in-list orig-reprs)])
    (define lowered (lower-tuples spec))
    (set! new-specs (append new-specs (flatten-by-repr lowered repr)))
    (set! new-reprs (append new-reprs (leaf-reprs repr))))

  (define new-pre (lower-tuples pre))
  (define ctxs*
    (for/list ([ctx (in-list ctxs)])
      (match-define (context _ repr _) ctx)
      (context new-vars repr new-var-reprs)))

  (define (assemble-point pt)
    (define idx 0)
    (define (next)
      (begin0 (vector-ref pt idx)
        (set! idx (add1 idx))))
    (for/vector #:length (length orig-var-reprs)
                ([repr (in-list orig-var-reprs)])
      (build-value next repr)))

  (define (assemble-output outs)
    (define outputs
      (if (vector? outs)
          (vector->list outs)
          outs))
    (define idx 0)
    (define (next)
      (begin0 (list-ref outputs idx)
        (set! idx (add1 idx))))
    (for/list ([repr (in-list orig-reprs)])
      (build-value next repr)))

  (values new-specs ctxs* new-pre assemble-point assemble-output new-reprs))

(module+ test
  (require rackunit)

  (define vec2 (make-tuple-representation #:slots (list <binary64> <binary64>)))
  (define ctx (context '(x) <binary64> (list vec2)))
  (let-values ([(specs* _ pre* _assemble-point _assemble-output _reprs*)
                (flatten-tuples-for-rival (list '(ref x 1)) (list ctx) '(< (ref x 0) (ref x 1)))])
    (check-equal? specs* '(x_1))
    (check-equal? pre* '(< x_0 x_1)))

  (define mat2 (make-tuple-representation #:slots (list vec2 vec2)))
  (define nested-ctx (context '(x) <binary64> (list mat2)))
  (let-values ([(specs* _ pre* assemble-point _assemble-output _reprs*)
                (flatten-tuples-for-rival (list '(ref (ref x 1) 0))
                                          (list nested-ctx)
                                          '(< (ref (ref x 0) 1) (ref (ref x 1) 0)))])
    (check-equal? specs* '(x_2))
    (check-equal? pre* '(< x_1 x_2))
    (check-equal? (assemble-point #(1 2 3 4)) #(#(#(1 2) #(3 4)))))

  (let-values ([(specs* _ctxs* _pre* _assemble-point assemble-output reprs*)
                (flatten-tuples-for-rival (list '(tuple (tuple 1 2) (tuple 3 4)))
                                          (list (context '() mat2 '()))
                                          'TRUE)])
    (check-equal? specs* '(1 2 3 4))
    (check-equal? reprs* (list <binary64> <binary64> <binary64> <binary64>))
    (check-equal? (assemble-output '(10 11 12 13)) (list #(#(10 11) #(12 13)))))

  (define scalar-ctxs (list (context '(x) <binary64> (list <binary64>))))
  (define scalar-specs (list '(+ x 1)))
  (let-values ([(specs* ctxs* pre* assemble-point assemble-output reprs*)
                (flatten-tuples-for-rival scalar-specs scalar-ctxs 'TRUE)])
    (check-eq? specs* scalar-specs)
    (check-eq? ctxs* scalar-ctxs)
    (check-eq? pre* 'TRUE)
    (check-eq? assemble-point identity)
    (check-eq? assemble-output identity)
    (check-equal? reprs* (list <binary64>)))

  (let-values ([(specs* _ctxs* _pre* _assemble-point _assemble-output _reprs*)
                (flatten-tuples-for-rival (list '(ref (tuple x x) 0)) scalar-ctxs 'TRUE)])
    (check-equal? specs* '(x)))

  (let-values ([(specs* _ctxs* pre* _assemble-point _assemble-output _reprs*)
                (flatten-tuples-for-rival (list '(+ x 1)) scalar-ctxs '(< (ref (tuple x x) 0) 2))])
    (check-equal? specs* '((+ x 1)))
    (check-equal? pre* '(< x 2)))

  (define mixed (make-tuple-representation #:slots (list <binary32> <binary64>)))
  (let-values ([(specs* _ctxs* _pre* _assemble-point assemble-output reprs*)
                (flatten-tuples-for-rival (list '(tuple (+ x 1) (* x 2)))
                                          (list (context '(x) mixed (list <binary64>)))
                                          'TRUE)])
    (check-equal? specs* '((+ x 1) (* x 2)))
    (check-equal? reprs* (list <binary32> <binary64>))
    (check-equal? (assemble-output '(10 11)) (list #(10 11)))))
