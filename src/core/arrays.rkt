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
  (define (array-repr-len repr who)
    (unless (array-representation? repr)
      (error who "expected array representation, got ~a" repr))
    (when (array-representation? (array-representation-elem repr))
      (error who "expected one-dimensional array representation, got ~a" repr))
    (array-representation-len repr))

  (define (array-repr-elem repr who)
    (unless (array-representation? repr)
      (error who "expected array representation, got ~a" repr))
    (define elem (array-representation-elem repr))
    (when (array-representation? elem)
      (error who "expected one-dimensional array representation, got ~a" repr))
    elem)

  (define (build-array-expr len elems who)
    (define-values (vals rest) (split-at elems len))
    (unless (null? rest)
      (error who "internal error assembling array expression"))
    `(array ,@vals))

  (define (flatten-array-components arr who)
    (match arr
      [`(array ,elems ...)
       (for ([elem (in-list elems)])
         (match elem
           [`(array ,_ ...) (error who "expected one-dimensional array value, got ~a" arr)]
           [_ (void)]))
       elems]
      [_ (error who "expected an array value, got ~a" arr)]))

  (define (array-expr-len arr)
    (match arr
      [`(array ,elems ...) (length elems)]
      [_ #f]))

  (define (scalar-expr v who)
    (match v
      [`(scalar ,e) e]
      [`(array ,_ ...) (error who "expected scalar expression, got array: ~a" v)]))

  (define (select-component arr idx who)
    (unless (and (integer? idx) (<= 0 idx))
      (error who "array index must be a non-negative literal integer, got ~a" idx))
    (match arr
      [`(array ,elems ...)
       (unless (< idx (length elems))
         (error who "array index ~a out of bounds for length ~a" idx (length elems)))
       (list-ref elems idx)]
      [_ (error who "ref expects an array value, got ~a" arr)]))

  (define (lower-arr expr env)
    (match expr
      [(? number?) `(scalar ,expr)]
      [(? symbol? s) (hash-ref env s `(scalar ,s))]
      [`(array ,elems ...)
       `(array ,@(for/list ([elem (in-list elems)])
                   (match (lower-arr elem env)
                     [`(scalar ,v) v]
                     [`(array ,vs ...) `(array ,@vs)])))]
      [`(ref ,arr ,idx)
       (define idx*
         (if (syntax? idx)
             (syntax-e idx)
             idx))
       (unless (integer? idx*)
         (error 'ref "array index must be a literal integer, got ~a" idx))
       (define selected (select-component (lower-arr arr env) idx* 'ref))
       (match selected
         [`(array ,_ ...) selected]
         [_ `(scalar ,selected)])]
      [`(ref ,_ ,_ ...) (error 'ref "expected exactly one array index")]
      [`(let ([,ids ,vals] ...) ,body)
       ;; let evaluates rhs in outer env (simultaneous bindings)
       (define lowered-vals
         (for/list ([val (in-list vals)])
           (lower-arr val env)))
       (define env*
         (for/fold ([e env])
                   ([id (in-list ids)]
                    [val (in-list lowered-vals)])
           (hash-set e id val)))
       (lower-arr body env*)]
      [`(let* ([,ids ,vals] ...) ,body)
       ;; let* evaluates rhs in progressively-extended env
       (define env*
         (for/fold ([e env])
                   ([id (in-list ids)]
                    [val (in-list vals)])
           (hash-set e id (lower-arr val e))))
       (lower-arr body env*)]
      [`(if ,c ,t ,f)
       (let* ([c* (scalar-expr (lower-arr c env) 'if)]
              [t-expr (scalar-expr (lower-arr t env) 'if)]
              [f-expr (scalar-expr (lower-arr f env) 'if)])
         `(scalar (if ,c* ,t-expr ,f-expr)))]
      [`(! ,props ... ,body) `(scalar (! ,@props ,(scalar-expr (lower-arr body env) '!)))]
      [`(,op ,args ...)
       (define lowered-args (map (lambda (a) (scalar-expr (lower-arr a env) op)) args))
       `(scalar (,op ,@lowered-args))]
      [_ `(scalar ,expr)]))

  (define orig-vars (context-vars (first ctxs)))
  (define orig-reprs (map context-repr ctxs))
  (define orig-var-reprs (context-var-reprs (first ctxs)))
  (define taken (list->seteq orig-vars))
  (define (fresh base)
    (let loop ([i 0])
      (define candidate (string->symbol (format "~a_~a" base i)))
      (if (set-member? taken candidate)
          (loop (add1 i))
          candidate)))

  (define env (make-hasheq))
  (define new-vars '())
  (define new-var-reprs '())
  (for ([v orig-vars]
        [r orig-var-reprs])
    (cond
      [(array-representation? r)
       (define base (symbol->string v))
       (define len (array-repr-len r 'flatten-arrays-for-rival))
       (define vars
         (for/list ([_ (in-range len)])
           (define vi (fresh base))
           (set! taken (set-add taken vi))
           vi))
       (hash-set! env v (build-array-expr len vars 'flatten-arrays-for-rival))
       (set! new-vars (append new-vars vars))
       (set! new-var-reprs
             (append new-var-reprs (make-list len (array-repr-elem r 'flatten-arrays-for-rival))))]
      [else
       (hash-set! env v `(scalar ,v))
       (set! new-vars (append new-vars (list v)))
       (set! new-var-reprs (append new-var-reprs (list r)))]))

  (define env-immutable
    (for/fold ([e (hash)]) ([(k v) (in-hash env)])
      (hash-set e k v)))

  (define new-specs '())
  (define new-reprs '())
  (define output-lens '())
  (for ([spec (in-list specs)]
        [repr (in-list orig-reprs)])
    (define lowered (lower-arr spec env-immutable))
    (define lowered-array?
      (match lowered
        [`(array ,_ ...) #t]
        [_ #f]))
    (cond
      [(or (array-representation? repr) lowered-array?)
       (define comps (flatten-array-components lowered 'flatten-arrays-for-rival))
       (define elem-repr
         (if (array-representation? repr)
             (array-repr-elem repr 'flatten-arrays-for-rival)
             repr))
       (define len
         (if (array-representation? repr)
             (array-repr-len repr 'flatten-arrays-for-rival)
             (array-expr-len lowered)))
       (set! output-lens (append output-lens (list len)))
       (set! new-specs (append new-specs comps))
       (set! new-reprs (append new-reprs (make-list (length comps) elem-repr)))]
      [else
       (set! output-lens (append output-lens (list #f)))
       (set! new-specs (append new-specs (list (scalar-expr lowered 'program))))
       (set! new-reprs (append new-reprs (list repr)))]))

  (define new-pre (scalar-expr (lower-arr pre env-immutable) 'program))
  (define ctxs*
    (for/list ([ctx (in-list ctxs)])
      (define repr (context-repr ctx))
      (context new-vars
               (if (array-representation? repr)
                   (array-repr-elem repr 'flatten-arrays-for-rival)
                   repr)
               new-var-reprs)))

  (define (assemble-point pt)
    (define idx 0)
    (define (next)
      (begin0 (vector-ref pt idx)
        (set! idx (add1 idx))))
    (list->vector (for/list ([r (in-list orig-var-reprs)])
                    (if (array-representation? r)
                        (list->vector (for/list ([_ (in-range (array-repr-len r 'assemble-point))])
                                        (next)))
                        (next)))))

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
          (list->vector (for/list ([_ (in-range len)])
                          (next)))
          (next))))

  (values new-specs ctxs* new-pre assemble-point assemble-output new-reprs))

(module+ test
  (require rackunit)

  (define ctx (context '(x) <binary64> (list <binary64>)))

  ;; let rhs expressions must be evaluated in the outer environment.
  (let-values ([(specs* _ pre* _assemble-point _assemble-output _reprs*)
                (flatten-arrays-for-rival (list 'x)
                                          (list ctx)
                                          '(let ([x 1]
                                                 [y x])
                                             y))])
    (check-equal? specs* '(x))
    (check-equal? pre* 'x))

  ;; let* rhs expressions must be evaluated in the progressively-extended environment.
  (let-values ([(specs* _ pre* _assemble-point _assemble-output _reprs*)
                (flatten-arrays-for-rival (list 'x)
                                          (list ctx)
                                          '(let* ([x 1]
                                                  [y x])
                                             y))])
    (check-equal? specs* '(x))
    (check-equal? pre* 1)))
