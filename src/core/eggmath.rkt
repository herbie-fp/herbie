#lang racket

(require ffi/unsafe
         ffi/unsafe/define)
(require "../syntax/rules.rkt")
(module+ test (require rackunit))

(provide egraph-run egraph-add-exprs egraph_run_rules egraph_get_simplest egg-expr->expr egg-add-exn?)

(define linux-path "../../../egg-herbie/target/debug/libegg_math.so")
(define windows-path "../../../egg-herbie/target/debug/egg_math.dll")

(define-ffi-definer define-eggmath (ffi-lib (if
                                             (file-exists? linux-path)
                                             linux-path
                                             windows-path)))

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



;; calls the function on a new egraph, and cleans up
(define (egraph-run egraph-function)
  (define egraph (egraph_create))
  (define res (egraph-function egraph))
  (egraph_destroy egraph)
  res)

(define (egg-expr->expr expr rename-dict)
  (define parsed (read (open-input-string expr)))
  (egg-parsed->expr parsed rename-dict))

(define (egg-parsed->expr parsed rename-dict)
  (cond
    [(list? parsed)
     (cons
      (first parsed)
      (for/list ([expr (rest parsed)])
        (egg-parsed->expr expr rename-dict)))]
    [(integer? parsed)
     parsed]
    [else
     (if (set-member? fpconstants parsed)
         parsed
         (hash-ref rename-dict parsed))]))

;; returns a pair of the string representing an egg expr, and a hash table for mapping symbols on the way back
;; the hash table maps all symbols and non-integer values to new names for egg
(define (expr->egg-expr expr)
  (expr->egg-expr-helper expr (make-hash) (make-hash)))

(define (expr->egg-expr-helper expr egg->herbie-dict herbie->egg-dict)
  (cond
    [(list? expr)
     (list
      (string-append
       "("
       (symbol->string (first expr))
       (foldr
        (lambda (sub-expr acc)
          (string-append " " (first (expr->egg-expr-helper sub-expr egg->herbie-dict herbie->egg-dict))
                          acc))
        "" (rest expr))
       ")")
      egg->herbie-dict)]
    [(integer? expr)
     (list (number->string expr) egg->herbie-dict herbie->egg-dict)]
    [(hash-has-key? herbie->egg-dict expr)
     (list (symbol->string (hash-ref herbie->egg-dict expr)) egg->herbie-dict)]
    [else
     (define new-key (string-append "h" (number->string (hash-count herbie->egg-dict))))
     (define new-key-symbol (string->symbol new-key))
         
     (hash-set! herbie->egg-dict
                expr
                new-key-symbol)
     (hash-set! egg->herbie-dict
                new-key-symbol
                expr)
     (list new-key egg->herbie-dict)]))


(define-struct (egg-add-exn exn:fail:user) ())

;; result function is a function that takes the ids of the nodes and the list of resulting rename dicts
;; egraph-add-exprs returns the result of result-function
(define (egraph-add-exprs egraph exprs result-function)
  (define expr-pairs
    (for/list ([expr exprs])
      (expr->egg-expr expr)))
      
  (define expr-results
    (for/list ([pair expr-pairs])
      (egraph_add_expr egraph (first pair))))
  
  (define node-ids
    (for/list ([result expr-results])
      (if (EGraphAddResult-successp result)
          (EGraphAddResult-id result)
          (raise egg-add-exn
               (string-append "Failed to add expr to egraph")
               (current-continuation-marks)))))

  (define expr-rename-dicts
    (for/list ([pair expr-pairs])
      (second pair)))
  
  (define res (result-function node-ids expr-rename-dicts))

  (for/list ([result expr-results])
    (egraph_addresult_destroy result))

  res)


(module+ test
  (check-equal?
   (egraph-run
    (lambda (egg-graph)
      (egraph-add-exprs
       egg-graph
       (list '(+ x 2) '(+ x 1) '(+ x 0) '(+ x 23))
       (lambda (node-ids rename-dicts)
         (egg-expr->expr (egraph_get_simplest egg-graph (first node-ids)) (first rename-dicts))))))
   '(+ x 2)))


