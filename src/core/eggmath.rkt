#lang racket

(require ffi/unsafe
         ffi/unsafe/define)
(require racket/runtime-path)
(require "../debug.rkt")
(require "../syntax/rules.rkt")
(module+ test (require rackunit))

(provide egraph-run egraph-add-exprs egraph-run-rules egraph-get-simplest egg-expr->expr egg-add-exn?)


(define-runtime-path libeggmath-path
  (build-path 'up 'up "egg-herbie" "target" "release"
              (case (system-type)
                [(windows) "egg_math"]
                [else "libegg_math"])))


(define-ffi-definer define-eggmath (ffi-lib libeggmath-path))

(define _egraph-pointer (_cpointer 'egraph))

(define-cstruct _EGraphAddResult
  ([id _uint]
   [successp _bool]))


;;  -> a pointer to an egraph
(define-eggmath egraph_create (_fun -> _egraph-pointer))

(define-eggmath egraph_destroy (_fun _egraph-pointer -> _void))

;; egraph pointer, s-expr string -> node number
(define-eggmath egraph_add_expr (_fun _egraph-pointer _string -> _EGraphAddResult-pointer))

(define-eggmath egraph_addresult_destroy (_fun _EGraphAddResult-pointer -> _void))

;; egraph pointer, number of iterations, limit on size of egraph
(define-eggmath egraph_run_rules (_fun _egraph-pointer _uint _uint -> _void))

;; node number -> s-expr string
(define-eggmath egraph_get_simplest (_fun _egraph-pointer _uint -> _string))

(struct egraph-data (egraph-pointer egg->herbie-dict herbie->egg-dict))


(define (egraph-get-simplest egraph-data node-id)
  (egraph_get_simplest (egraph-data-egraph-pointer egraph-data) node-id))

(define (egraph-run-rules egraph-data iters node-limit)
  (egraph_run_rules (egraph-data-egraph-pointer egraph-data) iters node-limit))

;; calls the function on a new egraph, and cleans up
(define (egraph-run egraph-function)
  (define egraph (egraph-data (egraph_create) (make-hash) (make-hash)))
  (define res (egraph-function egraph))
  (egraph_destroy (egraph-data-egraph-pointer egraph))
  res)

(define (egg-expr->expr expr eg-data)
  (define parsed (read (open-input-string expr)))
  (egg-parsed->expr parsed (egraph-data-egg->herbie-dict eg-data)))

(define (egg-parsed->expr parsed rename-dict)
  (cond
    [(list? parsed)
     (cons
      (first parsed)
      (for/list ([expr (rest parsed)])
        (egg-parsed->expr expr rename-dict)))]
    [(and (exact? parsed) (real? parsed))
     parsed]
    [else
     (if (set-member? fpconstants parsed)
         parsed
         (hash-ref rename-dict parsed))]))

;; returns a pair of the string representing an egg expr, and updates the hash tables in the egraph
;; the hash table maps all symbols and non-integer values to new names for egg
(define (expr->egg-expr expr egg-data)
  (define egg->herbie-dict (egraph-data-egg->herbie-dict egg-data))
  (define herbie->egg-dict (egraph-data-herbie->egg-dict egg-data))
  (expr->egg-expr-helper expr egg->herbie-dict herbie->egg-dict))

(define (expr->egg-expr-helper expr egg->herbie-dict herbie->egg-dict)
  (cond
    [(list? expr)
     (string-append
      "("
      (symbol->string (first expr))
      (foldr
       (lambda (sub-expr acc)
         (string-append " " (expr->egg-expr-helper sub-expr egg->herbie-dict herbie->egg-dict)
                        acc))
       "" (rest expr))
      ")")]
    [(integer? expr)
     (number->string expr)]
    [(rational? expr)
     (string-append "(/ " (number->string (numerator expr)) " "
                    (number->string (denominator expr)) ")")]
    [(hash-has-key? herbie->egg-dict expr)
     (symbol->string (hash-ref herbie->egg-dict expr))]
    [else
     (define new-key (string-append "h" (number->string (hash-count herbie->egg-dict))))
     (define new-key-symbol (string->symbol new-key))
         
     (hash-set! herbie->egg-dict
                expr
                new-key-symbol)
     (hash-set! egg->herbie-dict
                new-key-symbol
                expr)
      new-key]))


(define-struct (egg-add-exn exn:fail:user) ())

;; result function is a function that takes the ids of the nodes
;; egraph-add-exprs returns the result of result-function
(define (egraph-add-exprs eg-data exprs result-function)
  (define egg-exprs
    (for/list ([expr exprs])
      (expr->egg-expr expr eg-data)))
  (debug #:from 'simplify (format "Sending expressions to egg_math:\n ~a"
                                  (string-join (map ~a egg-exprs) "\n ")))
      
  (define expr-results
    (for/list ([expr egg-exprs])
      (egraph_add_expr (egraph-data-egraph-pointer eg-data) expr)))
  
  (define node-ids
    (for/list ([result expr-results])
      (if (EGraphAddResult-successp result)
          (EGraphAddResult-id result)
          (raise (egg-add-exn
               (string-append "Failed to add expr to egraph")
               (current-continuation-marks))))))

  (define res (result-function node-ids))

  (for/list ([result expr-results])
    (egraph_addresult_destroy result))

  res)


(module+ test
  (check-equal?
   (egraph-run
    (lambda (egg-graph)
      (egraph-add-exprs
       egg-graph
       (list '(+ x 2) '(+ x 1) '(+ x 0) '(+ x 23) 0 1)
       (lambda (node-ids)
         (egraph-run-rules egg-graph 4 9000)
         (for/list [(node-id node-ids)]
           (egg-expr->expr (egraph-get-simplest egg-graph node-id) egg-graph))))))
   (list '(+ x 2) '(+ x 1) 'x '(+ x 23) 0 1))

  (define expr-list
    (list
     '(+ x y)
     '(+ y x)
     '(- 2 (+ x y))
     '(- z (+ (+ y 2) x))))
  
  (check-equal?
   (egraph-run
    (lambda (egg-graph)
      (for/list ([expr
                  expr-list])
        (expr->egg-expr expr egg-graph))))
   (list
    "(+ h1 h0)"
    "(+ h0 h1)"
    "(- 2 (+ h1 h0))"
    "(- h2 (+ (+ h0 2) h1))"))

  (define extended-expr-list
    (append
     (list
      '(/ (- (exp x) (exp (- x))) 2)
      '(/ (+ (- b) (sqrt (- (* b b) (* (* 3 a) c)))) (* 3 a))
      '(/ (+ (- b) (sqrt (- (* b b) (* (* 3 a) c)))) (* 3 a)) ;; duplicated to make sure it would still work
      '(* r 30))
     expr-list))

  (check-equal?
   (egraph-run
    (lambda (egg-graph)
      (for/list ([expr
                  extended-expr-list])
        (egg-expr->expr (expr->egg-expr expr egg-graph) egg-graph) ;; call twice to make sure mutation works
        (egg-expr->expr (expr->egg-expr expr egg-graph) egg-graph))))
   extended-expr-list)
  )
