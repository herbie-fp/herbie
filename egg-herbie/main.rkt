#lang racket

(require ffi/unsafe ffi/unsafe/define racket/runtime-path)

(require "./to-egg-pattern.rkt" "./egg-interface.rkt")

(module+ test (require rackunit))

(provide egraph-run egraph-add-expr with-egraph
         egraph-get-simplest egraph-get-variants
         egg-add-exn?
         free-ffi-rules egraph-get-cost
         egraph-stop-reason egraph-is-unsound-detected
         egraph-add-expr-egglog egglog-run
         egglog-get-simplest egglog-get-variants
         (struct-out egraph-data)
         (struct-out iteration-data))

;; the first hash table maps all symbols and non-integer values to new names for egg
;; the second hash is the reverse of the first
(struct egraph-data (egraph-pointer egg->herbie-dict herbie->egg-dict))
;; interface struct for accepting rules
(struct irule (name input output) #:prefab)
(struct iteration-data (num-nodes num-eclasses time))

(define (egraph-get-simplest egraph-data node-id iteration)
  (define ptr (egraph_get_simplest (egraph-data-egraph-pointer egraph-data) node-id iteration))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  str)

(define (egglog-get-simplest egraph-data node-id)
  (define ptr (egglog_get_simplest (egraph-data-egraph-pointer egraph-data) node-id))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  str)

(define (egglog-get-variants vartypes egraph-data node-id expr-str)
  (define ptr (egglog_get_variants (egraph-data-egraph-pointer egraph-data) node-id expr-str))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  str)

(define (egraph-get-variants vartypes egraph-data node-id expr-str)
  (define ptr (egraph_get_variants (egraph-data-egraph-pointer egraph-data) node-id expr-str))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  str)

(define (egraph-get-cost egraph-data node-id iteration)
  (egraph_get_cost (egraph-data-egraph-pointer egraph-data) node-id iteration))

(define (egraph-is-unsound-detected egraph-data)
  (egraph_is_unsound_detected (egraph-data-egraph-pointer egraph-data)))

(define (egraph-stop-reason egraph-data)
  (define sr (egraph_get_stop_reason (egraph-data-egraph-pointer egraph-data)))
  (match sr
   [0 'saturated]
   [1 'iter-limit]
   [2 'node-limit]
   [3 'unsound]
   [else (error 'egraph-stop-reason "unexpected stop reason ~a" sr)]))

(define (make-raw-string s)
  (define b (string->bytes/utf-8 s))
  (define n (bytes-length b))
  (define ptr (malloc 'raw (+ n 1)))
  (memcpy ptr b n)
  (ptr-set! ptr _byte n 0)
  ptr)

(define (egraph-get-times-applied egraph-data rule-name)
  (egraph_get_times_applied (egraph-data-egraph-pointer egraph-data)
                            (make-raw-string (symbol->string rule-name))))


(define (free-ffi-rules rules)
  (for [(rule rules)]
    (free (FFIRule-name rule))
    (free (FFIRule-left rule))
    (free (FFIRule-right rule))
    (free rule)))

(define (convert-iteration-data egraphiters size)
  (cond
    [(> size 0)
     (cons (iteration-data (EGraphIter-numnodes egraphiters) (EGraphIter-numeclasses egraphiters) (EGraphIter-time egraphiters))
           (convert-iteration-data (ptr-add egraphiters 1 _EGraphIter) (- size 1)))]
    [else empty]))

;; runs rules on an egraph
;; can optionally specify an iter limit
(define (egraph-run egraph-data node-limit ffi-rules precompute? [iter-limit #f])
  (define egraph-ptr (egraph-data-egraph-pointer egraph-data))
  (define-values (egraphiters res-len)
    (if iter-limit
        (egraph_run_with_iter_limit egraph-ptr iter-limit node-limit ffi-rules precompute?)
        (egraph_run egraph-ptr node-limit ffi-rules precompute?)))
  (define res (convert-iteration-data egraphiters res-len))
  (destroy_egraphiters res-len egraphiters)
  res)

(define (egglog-run egraph-data)
  (define egraph-ptr (egraph-data-egraph-pointer egraph-data))
  (define-values (egraphiters res-len)
    (egraph_run_egglog egraph-ptr))
  (define res (convert-iteration-data egraphiters res-len))
  (destroy_egraphiters res-len egraphiters)
  res)

;; calls the function on a new egraph, and cleans up
(define (with-egraph egraph-function)
  (define egraph (egraph-data (egraph_create) (make-hash) (make-hash)))
  (define res (egraph-function egraph))
  (egraph_destroy (egraph-data-egraph-pointer egraph))
  res)



(struct egg-add-exn exn:fail ())

;; result function is a function that takes the ids of the nodes
(define (egraph-add-expr eg-data expr-string)
  (define result (egraph_add_expr (egraph-data-egraph-pointer eg-data) expr-string))
  (when (= result 0)
    (raise (egg-add-exn
            "Failed to add expr to egraph"
            (current-continuation-marks))))
  (- result 1))

(define (egraph-add-expr-egglog vartypes eg-data expr-string)
  (define result (egraph_add_expr_egglog (egraph-data-egraph-pointer eg-data) expr-string))
  (when (= result 0)
    (raise (egg-add-exn
            "Failed to add expr to egraph"
            (current-continuation-marks))))
  result)

#;
(module+ test

  (define test-exprs
    (list (cons '(+ y x) "(+ real h0 h1)")
          (cons '(+ x y) "(+ real h1 h0)")
          (cons '(- 2 (+ x y)) "(- real 2 (+ real h1 h0))")
          (cons '(- z (+ (+ y 2) x)) "(- real h2 (+ real (+ real h0 2) h1))")
          (cons '(*.f64 x y) "(* f64 h1 h0)")
          (cons '(+.f32 (*.f32 x y) 2) "(+ f32 (* f32 h1 h0) 2)")
          (cons '(cos.f64 (PI.f64)) "(cos f64 (PI f64))")
          (cons '(if (TRUE) x y) "(if real (TRUE real) h1 h0)")))

  (define nil
    (with-egraph
     (lambda (egg-graph)
      (for/list ([(in expected-out) (in-dict test-exprs)])
        (let* ([out (expr->egg-expr in egg-graph)]
               [computed-in (egg-expr->expr out egg-graph)])
          (check-equal? out expected-out)
          (check-equal? computed-in in))))))

  (define extended-expr-list
    (list
     '(/ (- (exp x) (exp (- x))) 2)
     '(/ (+ (- b) (sqrt (- (* b b) (* (* 3 a) c)))) (* 3 a))
     '(/ (+ (- b) (sqrt (- (* b b) (* (* 3 a) c)))) (* 3 a)) ;; duplicated to make sure it would still work
     '(* r 30)
     '(* 23/54 r)
     '(+ 3/2 1.4)))

  (define extended-results
   (with-egraph
    (lambda (egg-graph)
      (for/list ([expr
                  extended-expr-list])
        (egg-expr->expr (expr->egg-expr expr egg-graph) egg-graph)))))
  (for ([res extended-results] [expected extended-expr-list])
    (check-equal? res expected))
  )
