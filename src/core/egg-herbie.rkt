#lang racket

(require egg-herbie)
(require ffi/unsafe ffi/unsafe/define)
(require "../syntax/rules.rkt" "../syntax/sugar.rkt" "../syntax/syntax.rkt" "../syntax/types.rkt"
         "../common.rkt" "../errors.rkt" "../timeline.rkt")

(module+ test (require rackunit))

(provide with-egraph egraph-add-expr egraph-run-rules
         egraph-get-simplest egraph-get-variants
         egraph-get-proof egraph-is-unsound-detected)

;; Converts a string expression from egg into a Racket S-expr
(define (egg-expr->expr expr eg-data)
  (define parsed (read (open-input-string expr)))
  (egg-parsed->expr parsed (egraph-data-egg->herbie-dict eg-data)))

;; Like `egg-expr->expr` but expected the string to
;; parse into a list of S-exprs
(define (egg-exprs->exprs exprs eg-data)
  (for/list ([egg-expr (in-port read (open-input-string exprs))])
    (egg-parsed->expr egg-expr (egraph-data-egg->herbie-dict eg-data))))

(define (egg-parsed->expr expr rename-dict)
  (define-values (expr* _)
    (let loop ([expr expr])
     (match expr
      [(list 'Rewrite=> rule expr)
       (define-values (expr* otype) (loop expr))
       (values (list 'Rewrite=> rule expr*) otype)]
      [(list 'Rewrite<= rule expr)
       (define-values (expr* otype) (loop expr))
       (values (list 'Rewrite<= rule expr*) otype)]
      [(list op 'real args ...)
       (define args*
         (for/list ([arg (in-list args)])
           (let-values ([(arg* _) (loop arg)])
             arg*)))
       (values (cons op args*) 'real)]
      [(list (? constant-operator? op) prec)
       (define repr (get-representation prec))
       (let/ec k
           (for/list ([name (operator-all-impls op)])
             (define rtype (operator-info name 'otype))
             (when (or (equal? rtype repr) (equal? (representation-type rtype) 'bool))
               (k (list name) rtype)))
           (raise-herbie-missing-error "Could not find constant implementation for ~a at ~a"
                                        op (representation-name repr)))]
      [(list op prec args ...)
       (define-values (args* types*)
          (for/lists (l1 l2) ([arg (in-list args)])
            (loop arg)))
       (define op* (apply get-parametric-operator op types*))
       (values (cons op* args*) (get-representation prec))]
      [(? number?) (values expr (context-repr (*context*)))]
      [_
       (define renamed (hash-ref rename-dict expr))
       (values renamed (context-lookup (*context*) renamed))])))
  expr*)

(define (expr->egg-pattern+vars expr)
  (define ppatterns (make-hash))
  (define (get-pattern key)
    (hash-ref! ppatterns (real-operator-info key 'otype)
               (λ () (string->symbol (format "?p~a" (hash-count ppatterns))))))
  (define expr*
    (let loop ([expr expr])
      (match expr
        [(list 'if cond ift iff)
         (list 'if 'real (loop cond) (loop ift) (loop iff))]
        [(list (? operator-exists? op) args ...)
         (cons op (cons (get-pattern op) (map loop args)))]
        [(list op args ...)
         (define-values (op* prec) (expand-operator op))
         (cons op* (cons prec (map loop args)))]
        [(? number?) expr]
        [_ (format "?~a" expr)])))
  (values expr* (hash-keys ppatterns)))

;; Translates a Herbie rule LHS or RHS into a
;; pattern suitable for use in egg.
(define (expr->egg-pattern expr)
  (define-values (expr* _) (expr->egg-pattern+vars expr))
  expr*)

;; Translates a Herbie rule into possibly multiple rules
(define (rule->egg-rules r)
  (match-define (rule rname input output itypes otype) r)
  (define-values (input* input-pats) (expr->egg-pattern+vars input))
  (define-values (output* output-pats) (expr->egg-pattern+vars output))
  (cond
    [(andmap representation? (cons otype itypes))
     ;; Representation-specific rule
     ;; nothing special here: just return the 1 rule
     (list (cons input* output*))]
    [(andmap (curry set-member? input-pats) output-pats)
     ;; Non-expansive rules
     ;; also nothing special here: just return the 1 rule
     (list (cons input* output*))]
    [else
     ;; Expansive rules, i.e. x -> f(x) cannot be run in egg
     ;; since f(x) typically requires precision nodes
     ;; which cannot be inferred from just a node `x`
     ;; TODO: possible solution is to have special classes of rules
     ;;
     ;;  Rule(f, p) = { x -> f(x, p) }
     ;;  
     ;; where `p` is a representation.
     ;; This rule must be instantiated with a precision.
     ;;
     ;; While it may be possible to implement with a custom applier,
     ;; the hackier solution is just to duplicate the rule over
     ;; every possible representation before it enters the egraph.
     (reap [sow]
       (for ([repr (in-list (*needed-reprs*))])
         (define type (representation-type repr))
         (when (andmap (λ (p) (equal? type (cdr p))) itypes)
           (define itypes* (map (λ (p) (cons (car p) repr)) itypes))
           (define sugar-ctx (context (map car itypes) repr (map cdr itypes*)))
           (when (equal? otype type)
             (define rname* (sym-append rname '- (representation-name repr)))
             (define input* (desugar-program input sugar-ctx))
             (define output* (desugar-program output sugar-ctx))
             (sow (cons input* output*))))))]))
              

;; returns a pair of the string representing an egg expr, and updates the hash tables in the egraph
(define (expr->egg-expr expr egg-data)
  (define egg->herbie-dict (egraph-data-egg->herbie-dict egg-data))
  (define herbie->egg-dict (egraph-data-herbie->egg-dict egg-data))
  (expr->egg-expr-helper expr egg->herbie-dict herbie->egg-dict))

(define (expand-operator impl)
  (values (impl->operator impl)
          (representation-name (operator-info impl 'otype))))

(define (expr->egg-expr-helper expr egg->herbie-dict herbie->egg-dict)
  (let loop ([expr expr])
    (match expr
      [(list 'if cond ift iff)
       (list 'if 'real (loop cond) (loop ift) (loop iff))]
      [(list op args ...)
       (define-values (op* prec) (expand-operator op))
       (cons op* (cons prec (map loop args)))]
      [(? number?)
       expr]
      [(? (curry hash-has-key? herbie->egg-dict))
       (hash-ref herbie->egg-dict expr)]
      [else
       (define replacement (string->symbol (format "h~a" (hash-count herbie->egg-dict))))
       (hash-set! herbie->egg-dict expr replacement)
       (hash-set! egg->herbie-dict replacement expr)
       replacement])))

(module+ test
  (check-equal? (expr->egg-pattern `(+ a b)) '(+ real ?a ?b))
  (check-equal? (expr->egg-pattern `(/ c (- 2 a))) '(/ real ?c (- real 2 ?a)))
  (check-equal? (expr->egg-pattern `(cos.f64 (PI.f64))) '(cos f64 (PI f64)))
  (check-equal? (expr->egg-pattern `(if (TRUE) x y)) '(if real (TRUE real) ?x ?y))

  (define test-exprs
    (list (cons '(+ y x) "(+ real h0 h1)")
          (cons '(+ x y) "(+ real h1 h0)")
          (cons '(- 2 (+ x y)) "(- real 2 (+ real h1 h0))")
          (cons '(- z (+ (+ y 2) x)) "(- real h2 (+ real (+ real h0 2) h1))")
          (cons '(*.f64 x y) "(* f64 h1 h0)")
          (cons '(+.f32 (*.f32 x y) 2) "(+ f32 (* f32 h1 h0) 2)")
          (cons '(cos.f64 (PI.f64)) "(cos f64 (PI f64))")
          (cons '(if (TRUE) x y) "(if real (TRUE real) h1 h0)")))

  (with-egraph
   (lambda (egg-graph)
     (for ([(in expected-out) (in-dict test-exprs)])
       (let* ([out (~a (expr->egg-expr in egg-graph))]
              [computed-in (egg-expr->expr out egg-graph)])
         (check-equal? out expected-out)
         (check-equal? computed-in in)))))

  (define extended-expr-list
    (list
     '(/ (- (exp x) (exp (- x))) 2)
     '(/ (+ (- b) (sqrt (- (* b b) (* (* 3 a) c)))) (* 3 a))
     '(/ (+ (- b) (sqrt (- (* b b) (* (* 3 a) c)))) (* 3 a)) ;; duplicated to make sure it would still work
     '(* r 30)
     '(* 23/54 r)
     '(+ 3/2 1.4)))

  (with-egraph
   (lambda (egg-graph)
     (for ([expr extended-expr-list])
       (check-equal? 
        (egg-expr->expr (~a (expr->egg-expr expr egg-graph)) egg-graph)
        expr)))))

;; the first hash table maps all symbols and non-integer values to new names for egg
;; the second hash is the reverse of the first
(struct egraph-data (egraph-pointer egg->herbie-dict herbie->egg-dict))

(define (egraph-get-simplest egraph-data node-id iteration)
  (define ptr (egraph_get_simplest (egraph-data-egraph-pointer egraph-data) node-id iteration))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  (egg-expr->expr str egraph-data))

(define (egraph-get-variants egraph-data node-id orig-expr)
  (define expr-str (~a (expr->egg-expr orig-expr egraph-data)))
  (define ptr (egraph_get_variants (egraph-data-egraph-pointer egraph-data) node-id expr-str))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  (egg-exprs->exprs str egraph-data))

(define (egraph-is-unsound-detected egraph-data)
  (egraph_is_unsound_detected (egraph-data-egraph-pointer egraph-data)))

(define (egraph-get-cost egraph-data node-id iteration)
  (egraph_get_cost (egraph-data-egraph-pointer egraph-data) node-id iteration))

(define (egraph-get-times-applied egraph-data rule)
  (egraph_get_times_applied (egraph-data-egraph-pointer egraph-data) (FFIRule-name rule)))

(define (egraph-stop-reason egraph-data)
  (match (egraph_get_stop_reason (egraph-data-egraph-pointer egraph-data))
   [0 "saturated"]
   [1 "iter limit"]
   [2 "node limit"]
   [3 "unsound"]
   [sr (error 'egraph-stop-reason "unexpected stop reason ~a" sr)]))

(define (make-raw-string s)
  (define b (string->bytes/utf-8 s))
  (define n (bytes-length b))
  (define ptr (malloc 'raw (+ n 1)))
  (memcpy ptr b n)
  (ptr-set! ptr _byte n 0)
  ptr)

(define (make-ffi-rules rules)
  (for*/list ([rule (in-list rules)]
              [(lhs rhs) (in-dict (rule->egg-rules rule))])
    (define name (make-raw-string (symbol->string (rule-name rule))))
    (define left (make-raw-string (~a lhs)))
    (define right (make-raw-string (~a rhs)))
    (make-FFIRule name left right)))

(define (free-ffi-rules rules)
  (for ([rule (in-list rules)])
    (free (FFIRule-name rule))
    (free (FFIRule-left rule))
    (free (FFIRule-right rule))
    (free rule)))


;; calls the function on a new egraph, and cleans up
(define (with-egraph egraph-function)
  (define egraph (egraph-data (egraph_create) (make-hash) (make-hash)))
  (define res (egraph-function egraph))
  (egraph_destroy (egraph-data-egraph-pointer egraph))
  res)

(struct egg-add-exn exn:fail ())

(define (egraph-get-proof egraph-data expr goal)
  (define egg-expr (~a (expr->egg-expr expr egraph-data)))
  (define egg-goal (~a (expr->egg-expr goal egraph-data)))
  (define pointer (egraph_get_proof (egraph-data-egraph-pointer egraph-data) egg-expr egg-goal))
  (define res (cast pointer _pointer _string/utf-8))
  (destroy_string pointer)
  (for/list ([line (in-list (string-split res "\n"))])
    (egg-expr->expr line egraph-data)))

;; result function is a function that takes the ids of the nodes
(define (egraph-add-expr eg-data expr)
  (define egg-expr (~a (expr->egg-expr expr eg-data)))
  (define result (egraph_add_expr (egraph-data-egraph-pointer eg-data) egg-expr))
  (when (= result 0)
    (raise (egg-add-exn
            "Failed to add expr to egraph"
            (current-continuation-marks))))
  (- result 1))

(struct iteration-data (num-nodes num-eclasses time))

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

(define ffi-rules-cache #f)

(define (egraph-run-rules egg-graph node-limit rules node-ids precompute? #:limit [iter-limit #f])
  (unless (and ffi-rules-cache (equal? (car ffi-rules-cache) rules))
    (when ffi-rules-cache (free-ffi-rules (cdr ffi-rules-cache)))
    (set! ffi-rules-cache (cons rules (make-ffi-rules rules))))
  (define ffi-rules (cdr ffi-rules-cache))

  (define iteration-data (egraph-run egg-graph node-limit ffi-rules precompute? iter-limit))

  (let loop ([iter iteration-data] [counter 0] [time 0])
    (unless (null? iter)
      (define cnt (iteration-data-num-nodes (first iter)))
      (define cost (apply + (map (λ (node-id) (egraph-get-cost egg-graph node-id counter)) node-ids)))
      (define new-time (+ time (iteration-data-time (first iter))))
      (timeline-push! 'egraph counter cnt cost new-time)
      (loop (rest iter) (+ counter 1) new-time)))

  (timeline-push! 'stop (egraph-stop-reason egg-graph) 1)

  (for ([ffi-rule (in-list ffi-rules)] [rule (in-list rules)])
    (define count (egraph-get-times-applied egg-graph ffi-rule))
    (when (> count 0) (timeline-push! 'rules (~a (rule-name rule)) count)))

  iteration-data)
