#lang racket

(require ffi/unsafe ffi/unsafe/define racket/runtime-path)

(require "egg-interface.rkt")

(module+ test (require rackunit))

(provide egraph-run egraph-add-expr with-egraph
         egraph-get-simplest egraph-get-variants
         make-ffi-rules free-ffi-rules egraph-get-cost
         egraph-stop-reason egraph-is-unsound-detected
         egraph-get-times-applied egraph-get-proof
         egg-add-exn?
         (struct-out iteration-data))

;; the first hash table maps all symbols and non-integer values to new names for egg
;; the second hash is the reverse of the first
(struct egraph-data (egraph-pointer egg->herbie-dict herbie->egg-dict))
;; interface struct for accepting rules
(struct irule (name input output) #:prefab)
(struct iteration-data (num-nodes num-eclasses time))

(define (egg-parsed->expr expr rename-dict)
  (let loop ([expr expr])
    (match expr
      [(list 'Rewrite=> rule expr)
       (list 'Rewrite=> rule (loop expr))]
      [(list 'Rewrite<= rule expr)
       (list 'Rewrite<= rule (loop expr))]
      [(list op prec args ...)
       (cons op (cons prec (map loop args)))]
      [(? number?) expr]
      [else (hash-ref rename-dict expr)])))

;; Converts a string expression from egg into a Racket S-expr
(define (egg-expr->expr expr eg-data)
  (define parsed (read (open-input-string expr)))
  (egg-parsed->expr parsed (egraph-data-egg->herbie-dict eg-data)))

;; Like `egg-expr->expr` but expected the string to
;; parse into a list of S-exprs
(define (egg-exprs->exprs exprs eg-data)
  (define port (open-input-string exprs))
  (let loop ([parse (read port)] [exprs '()])
    (if (eof-object? parse)
        (reverse exprs)
        (let ([expr (egg-parsed->expr parse (egraph-data-egg->herbie-dict eg-data))])
          (loop (read port) (cons expr exprs))))))

(define (egraph-get-simplest egraph-data node-id iteration)
  (define ptr (egraph_get_simplest (egraph-data-egraph-pointer egraph-data) node-id iteration))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  (egg-expr->expr str egraph-data))

(define (egraph-get-variants egraph-data node-id orig-expr)
  (define expr-str (expr->egg-expr orig-expr egraph-data))
  (define ptr (egraph_get_variants (egraph-data-egraph-pointer egraph-data) node-id expr-str))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  (egg-exprs->exprs str egraph-data))

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

(define (make-ffi-rules rules)
  (for/list [(rule rules)]
    (define name (make-raw-string (symbol->string (irule-name rule))))
    (define left (make-raw-string (expr->egg-pattern (irule-input rule))))
    (define right (make-raw-string (expr->egg-pattern (irule-output rule))))
    (make-FFIRule name left right)))

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


;; calls the function on a new egraph, and cleans up
(define (with-egraph egraph-function)
  (define egraph (egraph-data (egraph_create) (make-hash) (make-hash)))
  (define res (egraph-function egraph))
  (egraph_destroy (egraph-data-egraph-pointer egraph))
  res)

;; takes a Racket expression and returns an egg pattern
(define (expr->egg-pattern expr)
  (match expr
    [(list op prec args ...)
     (string-join
      (cons (~a op) (cons (~a prec) (map expr->egg-pattern args)))
      " "
      #:before-first "("
      #:after-last ")")]
    [(? symbol?)
     (format "?~a" expr)]
    [(? number?)
     (number->string expr)]
    [_
     (error "expected list, number, or symbol: " expr)]))

;; returns a pair of the string representing an egg expr,
;; and updates the hash tables in the egraph
(define (expr->egg-expr expr egg-data)
  (define egg->herbie-dict (egraph-data-egg->herbie-dict egg-data))
  (define herbie->egg-dict (egraph-data-herbie->egg-dict egg-data))
  (expr->egg-expr-helper expr egg->herbie-dict herbie->egg-dict))

(define (expr->egg-expr-helper expr egg->herbie-dict herbie->egg-dict)
  (let loop ([expr expr])
    (match expr
      [(list op prec args ...)
       (string-join
         (cons (~a op) (cons (~a prec) (map loop args)))
         " "
         #:before-first "("
         #:after-last ")")]
      [(? number?)
       (number->string expr)]
      [(? (curry hash-has-key? herbie->egg-dict))
       (symbol->string (hash-ref herbie->egg-dict expr))]
      [else
       (define new-key (format "h~a" (hash-count herbie->egg-dict)))
       (define new-key-symbol (string->symbol new-key))
         
       (hash-set! herbie->egg-dict
                  expr
                  new-key-symbol)
       (hash-set! egg->herbie-dict
                  new-key-symbol
                  expr)

       new-key])))

(struct egg-add-exn exn:fail ())

(define (egraph-get-proof egraph-data expr goal)
  (define egg-expr (expr->egg-expr expr egraph-data))
  (define egg-goal (expr->egg-expr goal egraph-data))
  (define pointer (egraph_get_proof (egraph-data-egraph-pointer egraph-data) egg-expr egg-goal))
  (define proof (cast pointer _pointer _string/utf-8))
  (destroy_string pointer)
  (when (equal? proof "")
    (error 'egraph-get-proof "failed to produce proof for ~a to ~a" expr goal))
  (map (curryr egg-expr->expr egraph-data)
       (string-split proof "\n")))

;; result function is a function that takes the ids of the nodes
(define (egraph-add-expr eg-data expr)
  (define egg-expr (expr->egg-expr expr eg-data))
  (define result (egraph_add_expr (egraph-data-egraph-pointer eg-data) egg-expr))
  (when (= result 0)
    (raise (egg-add-exn
            "Failed to add expr to egraph"
            (current-continuation-marks))))
  (- result 1))

(module+ test

  (define test-exprs
    (list (cons '(+ p0 y x) "(+ p0 h0 h1)")
          (cons '(+ p0 x y) "(+ p0 h1 h0)")
          (cons '(- p0 2 (+ p0 x y)) "(- p0 2 (+ p0 h1 h0))")
          (cons '(- p0 z (+ p0 (+ p0 y 2) x)) "(- p0 h2 (+ p0 (+ p0 h0 2) h1))")
          (cons '(* p0 x y) "(* p0 h1 h0)")
          (cons '(+ p0 (* p0 x y) 2) "(+ p0 (* p0 h1 h0) 2)")
          (cons '(cos p0 (PI p0)) "(cos p0 (PI p0))")
          (cons '(if real (TRUE p0) x y) "(if real (TRUE p0) h1 h0)")))

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
