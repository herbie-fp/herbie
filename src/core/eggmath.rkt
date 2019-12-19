#lang racket

(require ffi/unsafe ffi/unsafe/define racket/runtime-path)

(require egg-herbie)

(require "../debug.rkt" "../common.rkt" "../syntax/rules.rkt"
         "../syntax/herbie-to-egg-pattern.rkt")

(module+ test (require rackunit))

(provide egraph-run egraph-add-exprs egraph-run-iter
         egraph-get-simplest egg-expr->expr egg-add-exn?
         make-ffi-rules egraph-get-cost egraph-get-size)


(struct egraph-data (egraph-pointer egg->herbie-dict herbie->egg-dict))


(define (egraph-get-size egraph-data)
  (egraph_get_size (egraph-data-egraph-pointer egraph-data)))

(define (egraph-get-cost egraph-data node-id)
  (egraph_get_cost (egraph-data-egraph-pointer egraph-data) node-id))

(define (egraph-get-simplest egraph-data node-id)
  (egraph_get_simplest (egraph-data-egraph-pointer egraph-data) node-id))

(define (make-ffi-rules rules)
  (for/list [(rule rules)]
      (make-FFIRule (symbol->string (rule-name rule))
                    (herbie-pattern->rust-pattern (rule-input rule))
                    (herbie-pattern->rust-pattern (rule-output rule)))))

(define (egraph-run-iter egraph-data node-limit ffi-rules precompute?)
  (egraph_run_iter (egraph-data-egraph-pointer egraph-data) node-limit ffi-rules precompute?))

(define (run-rules-recursive egraph-data node-limit ffi-rules precompute? last-count)
  (egraph_run_iter (egraph-data-egraph-pointer egraph-data) node-limit ffi-rules precompute?)
  (define cnt (egraph_get_size (egraph-data-egraph-pointer egraph-data)))
  (if (and (< cnt node-limit) (> cnt last-count))
      (run-rules-recursive egraph-data node-limit ffi-rules precompute? cnt)
      (void)))

(define (egraph-run-rules egraph-data node-limit rules precompute?)
  (run-rules-recursive egraph-data node-limit (make-ffi-rules rules) precompute? 0))


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
    [(and (number? parsed) (exact? parsed) (real? parsed))
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
    [(and (number? expr) (exact? expr) (real? expr))
     (number->string expr)]
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
  (require (submod "../syntax/rules.rkt" internals))
  
  (define all-simplify-rules
    (for/append ([rec (*rulesets*)])
      (match-define (list rules groups _) rec)
      (if 
       (set-member? groups 'simplify)
       rules
       '())))

  ;; check that no rules in simplify match on bare variables
  ;; this would be bad because we don't want to match type-specific operators on a value of a different type
  (for ([rule all-simplify-rules])
    (check-true
     (or
      (not (symbol? (rule-input rule)))
      (set-member? fpconstants (rule-input rule)))
     (string-append "Rule failed: " (symbol->string (rule-name rule)))))

  
  (check-equal?
   (egraph-run
    (lambda (egg-graph)
      (egraph-add-exprs
       egg-graph
       (list '(+ x 2) '(+ x 1) '(+ x 0) '(+ x 23) 0 1)
       (lambda (node-ids)
         (egraph-run-rules egg-graph 100 (*simplify-rules*) true)
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
      '(* r 30)
      '(* 23/54 r)
      '(+ 3/2 1.4))
     expr-list))

  (check-equal?
   (egraph-run
    (lambda (egg-graph)
      (for/list ([expr
                  extended-expr-list])
        (egg-expr->expr (expr->egg-expr expr egg-graph) egg-graph))))
   extended-expr-list)
  )
