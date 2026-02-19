#lang racket

(require racket/hash
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
  (define maybe-array-representation-elem
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (dynamic-require "../syntax/types.rkt" 'array-representation-elem)))
  (define maybe-array-representation-dims
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (dynamic-require "../syntax/types.rkt" 'array-representation-dims)))

  (define (array-repr? repr)
    (eq? (representation-type repr) 'array))
  (define (array-repr-elem repr who)
    (if maybe-array-representation-elem
        (maybe-array-representation-elem repr)
        (error who "array representations are unsupported in this build")))
  (define (array-repr-size repr who)
    (if maybe-array-representation-dims
        (apply * (maybe-array-representation-dims repr))
        (error who "array representations are unsupported in this build")))

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

  (define (normalize-index idx who)
    (define idx*
      (if (syntax? idx)
          (syntax-e idx)
          idx))
    (unless (integer? idx*)
      (error who "array index must be a literal integer, got ~a" idx))
    idx*)

  (define (lower-arr expr env)
    (match expr
      [(? number?) `(scalar ,expr)]
      [(? symbol? s) (hash-ref env s `(scalar ,s))]
      [`(array ,elems ...) `(array ,@(map (lambda (e) (scalar-expr (lower-arr e env) 'array)) elems))]
      [`(ref ,arr ,idxs ...)
       (unless (pair? idxs)
         (error 'ref "expected at least one index"))
       (define selected
         (for/fold ([current (lower-arr arr env)]) ([idx (in-list idxs)])
           (select-component current (normalize-index idx 'ref) 'ref)))
       `(scalar ,selected)]
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
              [t* (lower-arr t env)]
              [f* (lower-arr f env)]
              [t-expr (match t*
                        [`(scalar ,v) v]
                        [_ (error 'if "if branches must be scalars")])]
              [f-expr (match f*
                        [`(scalar ,v) v]
                        [_ (error 'if "if branches must be scalars")])])
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
      [(array-repr? r)
       (define base (symbol->string v))
       (define len (array-repr-size r 'flatten-arrays-for-rival))
       (define vars
         (for/list ([_ (in-range len)])
           (define vi (fresh base))
           (set! taken (set-add taken vi))
           vi))
       (hash-set! env v `(array ,@vars))
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
  (define (lower-scalar expr)
    (scalar-expr (lower-arr expr env-immutable) 'program))
  (define (lower-any expr)
    (lower-arr expr env-immutable))

  (define new-specs
    (append* (for/list ([spec (in-list specs)]
                        [repr (in-list orig-reprs)])
               (cond
                 [(array-repr? repr)
                  (define lowered (lower-any spec))
                  (for/list ([i (in-range (array-repr-size repr 'flatten-arrays-for-rival))])
                    (select-component lowered i 'flatten-arrays-for-rival))]
                 [else (list (lower-scalar spec))]))))
  (define new-reprs
    (append* (for/list ([repr (in-list orig-reprs)])
               (if (array-repr? repr)
                   (make-list (array-repr-size repr 'flatten-arrays-for-rival)
                              (array-repr-elem repr 'flatten-arrays-for-rival))
                   (list repr)))))
  (define new-pre (lower-scalar pre))
  (define ctxs*
    (for/list ([ctx (in-list ctxs)])
      (define repr (context-repr ctx))
      (context new-vars
               (if (array-repr? repr)
                   (array-repr-elem repr 'flatten-arrays-for-rival)
                   repr)
               new-var-reprs)))

  (define (assemble-point pt)
    (define idx 0)
    (list->vector (for/list ([r (in-list orig-var-reprs)])
                    (if (array-repr? r)
                        (let ([len (array-repr-size r 'assemble-point)])
                          (define elems
                            (for/list ([_ (in-range len)])
                              (begin0 (vector-ref pt idx)
                                (set! idx (add1 idx)))))
                          (list->vector elems))
                        (begin0 (vector-ref pt idx)
                          (set! idx (add1 idx)))))))

  (define (assemble-output outs)
    (define outputs
      (if (vector? outs)
          (vector->list outs)
          outs))
    (define idx 0)
    (for/list ([repr (in-list orig-reprs)])
      (if (array-repr? repr)
          (let ([len (array-repr-size repr 'assemble-output)])
            (list->vector (for/list ([_ (in-range len)])
                            (begin0 (list-ref outputs idx)
                              (set! idx (add1 idx))))))
          (begin0 (list-ref outputs idx)
            (set! idx (add1 idx))))))

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
