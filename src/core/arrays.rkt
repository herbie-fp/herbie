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
  (define (leaf-reprs repr)
    (if (array-representation? repr)
        (append* (for/list ([_ (in-range (array-representation-len repr))])
                   (leaf-reprs (array-representation-elem repr))))
        (list repr)))
  (define (fresh-tree base repr)
    (if (array-representation? repr)
        (let-values ([(elems vars reprs) (for/lists (elems vars reprs)
                                                    ([_ (in-range (array-representation-len repr))])
                                                    (fresh-tree base
                                                                (array-representation-elem repr)))])
          (values `(array ,@elems) (append* vars) (append* reprs)))
        (let ([v (fresh base)]) (values v (list v) (list repr)))))
  (define (flatten-by-repr expr repr)
    (if (array-representation? repr)
        (match-let ([`(array ,elems ...) expr])
          (append* (for/list ([elem (in-list elems)])
                     (flatten-by-repr elem (array-representation-elem repr)))))
        (list expr)))
  (define (build-value next repr)
    (if (array-representation? repr)
        (for/vector #:length (array-representation-len repr)
                    ([_ (in-range (array-representation-len repr))])
          (build-value next (array-representation-elem repr)))
        (next)))

  (define env (make-hasheq))
  (define new-vars '())
  (define new-var-reprs '())
  (for ([v orig-vars]
        [r orig-var-reprs])
    (cond
      [(array-representation? r)
       (define base (symbol->string v))
       (define-values (tree vars reprs) (fresh-tree base r))
       (hash-set! env v tree)
       (set! new-vars (append new-vars vars))
       (set! new-var-reprs (append new-var-reprs reprs))]
      [else
       (hash-set! env v v)
       (set! new-vars (append new-vars (list v)))
       (set! new-var-reprs (append new-var-reprs (list r)))]))
  (define (lower-arr expr)
    (match expr
      [(? number?) expr]
      [(? symbol? s) (hash-ref env s s)]
      [`(,op ,args ...)
       (define lowered `(,op ,@(map lower-arr args)))
       (match lowered
         [`(ref (array ,elems ...) ,idx) (list-ref elems idx)]
         [_ lowered])]))

  (define new-specs '())
  (define new-reprs '())
  (for ([spec (in-list specs)]
        [repr (in-list orig-reprs)])
    (define lowered (lower-arr spec))
    (cond
      [(array-representation? repr)
       (define comps (flatten-by-repr lowered repr))
       (define reprs (leaf-reprs repr))
       (set! new-specs (append new-specs comps))
       (set! new-reprs (append new-reprs reprs))]
      [else
       (set! new-specs (append new-specs (list lowered)))
       (set! new-reprs (append new-reprs (list repr)))]))

  (define new-pre (lower-arr pre))
  (define ctxs*
    (for/list ([ctx (in-list ctxs)])
      (match-define (context _ repr _) ctx)
      (context new-vars
               (if (array-representation? repr)
                   (array-representation-base repr)
                   repr)
               new-var-reprs)))

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
      (if (array-representation? repr)
          (build-value next repr)
          (next))))

  (values new-specs ctxs* new-pre assemble-point assemble-output new-reprs))

(module+ test
  (require rackunit)

  (define vec2 (make-array-representation #:elem <binary64> #:len 2))
  (define ctx (context '(x) <binary64> (list vec2)))
  (let-values ([(specs* _ pre* _assemble-point _assemble-output _reprs*)
                (flatten-arrays-for-rival (list '(ref x 1)) (list ctx) '(< (ref x 0) (ref x 1)))])
    (check-equal? specs* '(x_1))
    (check-equal? pre* '(< x_0 x_1)))

  (define mat2 (make-array-representation #:elem vec2 #:len 2))
  (define nested-ctx (context '(x) <binary64> (list mat2)))
  (let-values ([(specs* _ pre* assemble-point _assemble-output _reprs*)
                (flatten-arrays-for-rival (list '(ref (ref x 1) 0))
                                          (list nested-ctx)
                                          '(< (ref (ref x 0) 1) (ref (ref x 1) 0)))])
    (check-equal? specs* '(x_2))
    (check-equal? pre* '(< x_1 x_2))
    (check-equal? (assemble-point #(1 2 3 4)) #(#(#(1 2) #(3 4)))))

  (let-values ([(specs* _ctxs* _pre* _assemble-point assemble-output reprs*)
                (flatten-arrays-for-rival (list '(array (array 1 2) (array 3 4)))
                                          (list (context '() mat2 '()))
                                          'TRUE)])
    (check-equal? specs* '(1 2 3 4))
    (check-equal? reprs* (list <binary64> <binary64> <binary64> <binary64>))
    (check-equal? (assemble-output '(10 11 12 13)) (list #(#(10 11) #(12 13))))))
