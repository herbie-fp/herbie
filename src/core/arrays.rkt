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
  (define (array-repr? repr)
    (match (representation-type repr)
      ['array #t]
      [`(array ,_ ,_) #t]
      [_ #f]))
  (define (array-repr-elem repr who)
    (array-representation-base repr))
  (define (array-repr-size repr who)
    (array-representation-size repr))
  (define (array-repr-shape repr who)
    (array-representation-shape repr))

  (define (build-array-expr dims elems who)
    (define (loop dims elems)
      (define d (first dims))
      (if (null? (rest dims))
          (values `(array ,@(take elems d)) (drop elems d))
          (let build ([n d]
                      [elems elems]
                      [acc '()])
            (if (zero? n)
                (values `(array ,@(reverse acc)) elems)
                (let-values ([(sub rest-elems) (loop (rest dims) elems)])
                  (build (sub1 n) rest-elems (cons sub acc)))))))
    (define-values (arr rest-elems) (loop dims elems))
    (unless (null? rest-elems)
      (error who "internal error assembling array expression"))
    arr)

  (define (flatten-array-components arr who)
    (match arr
      [`(array ,elems ...)
       (append* (for/list ([elem (in-list elems)])
                  (match elem
                    [`(array ,_ ...) (flatten-array-components elem who)]
                    [_ (list elem)])))]
      [_ (error who "expected an array value, got ~a" arr)]))
  (define (array-expr-shape arr)
    (match arr
      [`(array ,elems ...)
       (if (null? elems)
           '()
           (match (first elems)
             [`(array ,_ ...) (cons (length elems) (array-expr-shape (first elems)))]
             [_ (list (length elems))]))]
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
      [`(array ,elems ...)
       `(array ,@(for/list ([elem (in-list elems)])
                   (match (lower-arr elem env)
                     [`(scalar ,v) v]
                     [`(array ,vs ...) `(array ,@vs)])))]
      [`(ref ,arr ,idxs ...)
       (unless (pair? idxs)
         (error 'ref "expected at least one index"))
       (define selected
         (for/fold ([current (lower-arr arr env)]) ([idx (in-list idxs)])
           (select-component current (normalize-index idx 'ref) 'ref)))
       (match selected
         [`(array ,_ ...) selected]
         [_ `(scalar ,selected)])]
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
       (define shape (array-repr-shape r 'flatten-arrays-for-rival))
       (define vars
         (for/list ([_ (in-range len)])
           (define vi (fresh base))
           (set! taken (set-add taken vi))
           vi))
       (hash-set! env v (build-array-expr shape vars 'flatten-arrays-for-rival))
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

  (define new-specs '())
  (define new-reprs '())
  (define output-shapes '())
  (for ([spec (in-list specs)]
        [repr (in-list orig-reprs)])
    (define lowered (lower-any spec))
    (define lowered-array?
      (match lowered
        [`(array ,_ ...) #t]
        [_ #f]))
    (cond
      [(or (array-repr? repr) lowered-array?)
       (define comps (flatten-array-components lowered 'flatten-arrays-for-rival))
       (define elem-repr
         (if (array-repr? repr)
             (array-repr-elem repr 'flatten-arrays-for-rival)
             repr))
       (define shape
         (if (array-repr? repr)
             (array-repr-shape repr 'flatten-arrays-for-rival)
             (array-expr-shape lowered)))
       (set! output-shapes (append output-shapes (list shape)))
       (set! new-specs (append new-specs comps))
       (set! new-reprs (append new-reprs (make-list (length comps) elem-repr)))]
      [else
       (set! output-shapes (append output-shapes (list #f)))
       (set! new-specs (append new-specs (list (scalar-expr lowered 'program))))
       (set! new-reprs (append new-reprs (list repr)))]))
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
    (define (assemble-array dims)
      (define d (first dims))
      (if (null? (rest dims))
          (list->vector (for/list ([_ (in-range d)])
                          (begin0 (vector-ref pt idx)
                            (set! idx (add1 idx)))))
          (list->vector (for/list ([_ (in-range d)])
                          (assemble-array (rest dims))))))
    (list->vector (for/list ([r (in-list orig-var-reprs)])
                    (if (array-repr? r)
                        (assemble-array (array-repr-shape r 'assemble-point))
                        (begin0 (vector-ref pt idx)
                          (set! idx (add1 idx)))))))

  (define (assemble-output outs)
    (define outputs
      (if (vector? outs)
          (vector->list outs)
          outs))
    (define idx 0)
    (define (assemble-array dims)
      (define d (first dims))
      (if (null? (rest dims))
          (list->vector (for/list ([_ (in-range d)])
                          (begin0 (list-ref outputs idx)
                            (set! idx (add1 idx)))))
          (list->vector (for/list ([_ (in-range d)])
                          (assemble-array (rest dims))))))
    (for/list ([shape (in-list output-shapes)])
      (if shape
          (assemble-array shape)
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
