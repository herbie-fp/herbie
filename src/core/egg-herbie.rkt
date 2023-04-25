#lang racket

(require egg-herbie)
(require ffi/unsafe ffi/unsafe/define)
(require "../syntax/rules.rkt" "../syntax/sugar.rkt" "../syntax/syntax.rkt" "../syntax/types.rkt"
         "../common.rkt" "../errors.rkt" "../timeline.rkt")

(module+ test (require rackunit))

(provide with-egraph egraph-add-expr egraph-run-rules
         egraph-get-simplest egraph-get-variants
         egraph-get-proof egraph-is-unsound-detected
         rule->egg-rules expand-rules get-canon-rule-name
         remove-rewrites)


(struct egraph-input (exprs rules terms-list num-variants iter-limit node-limit const-folding) #:transparent)

(define (make-egg-descriptor exprs rules terms-list num-variants [iter-limit #f] [node-limit #f] [const-folding #f])
  (egraph-input exprs rules terms-list num-variants iter-limit node-limit const-folding))

;; TODO : Main entry point return (cons (list (list variant)) (list proof))
(define (run-egraph input get-proof?)
  ;; TODO : Make simplify match

  ;; TODO : Make rr match
  #f
  )


(define (flatten-let term environment)
  (match term
    [`(let (,var ,term) ,body)
     (hash-set! environment var (flatten-let term environment))
     (flatten-let body environment)]
    [(? symbol?)
     (hash-ref environment term term)]
    [(? list?)
     (map (curryr flatten-let environment) term)]
    [(? number?)
     term]
    [else (error "Unknown term ~a" term)]))

;; Converts a string expression from egg into a Racket S-expr
(define (egg-expr->expr expr eg-data)
  (define parsed (read (open-input-string expr)))
  (egg-parsed->expr (flatten-let parsed (make-hash))
                    (egraph-data-egg->herbie-dict eg-data)))

;; Like `egg-expr->expr` but expected the string to
;; parse into a list of S-exprs
(define (egg-exprs->exprs exprs eg-data)
  (for/list ([egg-expr (in-port read (open-input-string exprs))])
    (egg-parsed->expr (flatten-let egg-expr (make-hash))
                      (egraph-data-egg->herbie-dict eg-data))))

;; Converts an S-expr from egg into one Herbie understands
(define (egg-parsed->expr expr rename-dict)
  (let loop ([expr expr])
    (match expr
      [`(Explanation ,body ...)
       `(Explanation ,@(map loop body))]
      [(list 'Rewrite=> rule expr)
       (list 'Rewrite=> rule (loop expr))]
      [(list 'Rewrite<= rule expr)
       (list 'Rewrite<= rule (loop expr))]
      [(list 'if 'real cond ift iff)
       (list 'if (loop cond) (loop ift) (loop iff))]
      [(list (? repr-conv? op) 'real arg)
       (list op (loop arg))]
      [(list op prec)
       (match-define (list '$Type otype) prec)
       (list (get-parametric-constant op (get-representation otype)))]
      [(list op prec args ...)
       (match-define (list '$Type otype itypes ...) prec)
       (define op* (apply get-parametric-operator op (map get-representation itypes)))
       (cons op* (map loop args))]
      [(? number?)
       expr]
      [_
       (hash-ref rename-dict expr)])))

;; Expands operators into (op, prec) so
;; that the egraph can happily constant fold
;; and so that we can recover the implementation
;; The ugly solution is to make prec = '(type otype itypes).
(define (expand-operator op-or-impl)
  (cond
    [(impl-exists? op-or-impl)
     (define op (impl->operator op-or-impl))
     (define itypes (map representation-name (operator-info op-or-impl 'itype)))
     (define otype (representation-name (operator-info op-or-impl 'otype)))
     (values op (cons '$Type (cons otype itypes)))]
    [else
     (values op-or-impl 'real)]))

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
        [(list (? repr-conv? op) arg)
         (list op 'real (loop arg))]
        [(list (? impl-exists? op) args ...)
         (define-values (op* prec) (expand-operator op))
         (cons op* (cons prec (map loop args)))]
        [(list (? operator-exists? op) args ...)
         (cons op (cons (get-pattern op) (map loop args)))]
        [(? number?) expr]
        [_ (string->symbol (format "?~a" expr))])))
  (values expr* (hash-keys ppatterns)))

;; Translates a Herbie rule LHS or RHS into a
;; pattern suitable for use in egg.
(define (expr->egg-pattern expr)
  (define-values (expr* _) (expr->egg-pattern+vars expr))
  expr*)

;; returns a pair of the string representing an egg expr, and updates the hash tables in the egraph
(define (expr->egg-expr expr egg-data)
  (define egg->herbie-dict (egraph-data-egg->herbie-dict egg-data))
  (define herbie->egg-dict (egraph-data-herbie->egg-dict egg-data))
  (expr->egg-expr-helper expr egg->herbie-dict herbie->egg-dict))

(define (expr->egg-expr-helper expr egg->herbie-dict herbie->egg-dict)
  (let loop ([expr expr])
    (match expr
      [(list 'if cond ift iff)
       (list 'if 'real (loop cond) (loop ift) (loop iff))]
      [(list (? repr-conv? op) arg)
       (list op 'real (loop arg))]
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
  (require "../load-plugin.rkt")
  (*context* (make-debug-context '()))

  (check-equal? (expr->egg-pattern `(+ a b)) '(+ ?p0 ?a ?b))
  (check-equal? (expr->egg-pattern `(/ c (- 2 a))) '(/ ?p0 ?c (- ?p0 2 ?a)))
  (check-equal? (expr->egg-pattern `(cos.f64 (PI.f64)))
                '(cos ($Type binary64 binary64) (PI ($Type binary64))))
  (check-equal? (expr->egg-pattern `(if (TRUE) x y))
                '(if real (TRUE ($Type bool)) ?x ?y))

  (define test-exprs
    (list (cons '(+.f64 y x)
                "(+ ($Type binary64 binary64 binary64) h0 h1)")
          (cons '(+.f64 x y)
                "(+ ($Type binary64 binary64 binary64) h1 h0)")
          (cons '(-.f64 2 (+.f64 x y))
                (~a '(- ($Type binary64 binary64 binary64) 2
                        (+ ($Type binary64 binary64 binary64) h1 h0))))
          (cons '(-.f64 z (+.f64 (+.f64 y 2) x))
                (~a '(- ($Type binary64 binary64 binary64) h2
                        (+ ($Type binary64 binary64 binary64)
                            (+ ($Type binary64 binary64 binary64) h0 2) h1))))
          (cons '(*.f64 x y)
                "(* ($Type binary64 binary64 binary64) h1 h0)")
          (cons '(+.f64 (*.f64 x y) 2)
                (~a '(+ ($Type binary64 binary64 binary64)
                        (* ($Type binary64 binary64 binary64) h1 h0) 2)))
          (cons '(cos.f32 (PI.f32))
                "(cos ($Type binary32 binary32) (PI ($Type binary32)))")
          (cons '(if (TRUE) x y)
                "(if real (TRUE ($Type bool)) h1 h0)")))

  (with-egraph
   (lambda (egg-graph)
     (for ([(in expected-out) (in-dict test-exprs)])
       (let* ([out (~a (expr->egg-expr in egg-graph))]
              [computed-in (egg-expr->expr out egg-graph)])
         (check-equal? out expected-out)
         (check-equal? computed-in in)))))

  (*context* (make-debug-context '(x a b c r)))
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
       (define expr* (desugar-program expr (*context*) #:full #f))
       (check-equal? 
        (egg-expr->expr (~a (expr->egg-expr expr* egg-graph)) egg-graph)
        expr*)))))


;; Given a list of types, computes the product of all possible
;; representation assignments where each element
;; is a dictionary mapping type to representation
(define (type-combinations types [reprs (*needed-reprs*)])
  (reap [sow]
    (let loop ([types types] [assigns '()])
      (match types
        [(list) (sow assigns)]
        [(list type rest ...)
         (for ([repr (in-list reprs)]
               #:when (equal? (representation-type repr) type))
           (loop rest (cons (cons type repr) assigns)))]))))

(define (reprs-in-expr expr)
  (define reprs (mutable-set))
  (let loop ([expr expr])
    (match expr
     [(list 'if cond ift iff)
      (loop cond) (loop ift) (loop iff)]
     [(list op args ...)
      (set-add! reprs (operator-info op 'otype))
      (for ([itype (operator-info op 'itype)])
        (set-add! reprs itype))
      (for-each loop args)]
     [_ (void)]))
  (set->list reprs))

;; Translates a Herbie rule into possibly multiple rules
(define (rule->egg-rules r)
  (match-define (rule name input output itypes otype) r)
  (cond
    [(andmap representation? (cons otype (map cdr itypes)))
     ;; rules over representations
     ;; nothing special here: just return the 1 rule
     ;; validate that we support the operators
     (if (andmap (curry set-member? (*needed-reprs*))
                 (append (reprs-in-expr input) (reprs-in-expr output)))
         (list r)
         (list))]
    [else
     ;; rules over types
     ;; must instantiate the rule over all possible
     ;; representation combinations, keeping in mind that
     ;; representations may not support certain operators
     (reap [sow]
       (define types (remove-duplicates (cons otype (map cdr itypes))))
       (for ([type-ctx (in-list (type-combinations types))])
         ;; Strange corner case:
         ;; Rules containing comparators cause desugaring to misbehave.
         ;; The reported output type is bool but then desugaring
         ;; thinks there will be a cast somewhere
         (define otype* (dict-ref type-ctx otype))
         (define sugar-otype
           (if (equal? otype 'bool)
               (dict-ref type-ctx 'real (get-representation 'bool))
               otype*))

         (define itypes* (map (λ (p) (cons (car p) (dict-ref type-ctx (cdr p)))) itypes))
         (define sugar-ctx (context (map car itypes) sugar-otype (map cdr itypes*)))
         (with-handlers ([exn:fail:user:herbie:missing? (const (void))])
           ;; The easier way to tell if every operator is supported
           ;; in a given representation is to just try to desguar
           ;; the expression and catch any errors.
           (define name* (sym-append name '_ (representation-name sugar-otype)))
           (define input* (desugar-program input sugar-ctx #:full #f))
           (define output* (desugar-program output sugar-ctx #:full #f))
           (when (andmap (curry set-member? (*needed-reprs*))
                         (append (reprs-in-expr input*) (reprs-in-expr output*)))
             (sow (rule name* input* output* itypes* otype*))))))]))

(module+ test
  ;; Make sure all built-in rules are valid in some
  ;; configuration of representations
  (*needed-reprs* (map get-representation '(binary64 binary32 bool)))
  (for ([rule (in-list (*rules*))])
    (test-case (~a (rule-name rule))
               (check-true (> (length (rule->egg-rules rule)) 0))))
)

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
  (for/list ([rule (in-list rules)])
    (define name (make-raw-string (symbol->string (rule-name rule))))
    (define left (make-raw-string (~a (expr->egg-pattern (rule-input rule)))))
    (define right (make-raw-string (~a (expr->egg-pattern (rule-output rule)))))
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

(define (remove-rewrites proof)
  (match proof
    [`(Rewrite=> ,rule ,something)
     (remove-rewrites something)]
    [`(Rewrite<= ,rule ,something)
     (remove-rewrites something)]
    [(list _ ...)
     (map remove-rewrites proof)]
    [else proof]))

;; Performs a product, but traverses the elements in order
;; This is the core logic of flattening a proof given flattened proofs for each child of a node
(define (sequential-product elements)
  (cond
    [(empty? elements) (list empty)]
    [else
     (define without-rewrites (remove-rewrites (last (first elements))))
     (append
      (for/list ([head (first elements)])
        (cons head (map first (rest elements))))
      (for/list ([other (in-list (rest (sequential-product (rest elements))))])
        (cons without-rewrites other)))]))

(module+ test
  (check-equal?
   (sequential-product `((1 2) (3 4 5) (6)))
   `((1 3 6) (2 3 6) (2 4 6) (2 5 6)))

  (expand-proof-term '(Explanation (+ x y) (+ y x)) (box 10)))


;; returns a flattened list of terms
;; The first term has no rewrite- the rest have exactly one rewrite
(define (expand-proof-term term budget)
  (match term
    [(? (lambda (x) (<= (unbox budget) 0)))
     (list #f)]
    [`(Explanation ,body ...)
     (expand-proof body budget)]
    [(? symbol?)
     (list term)]
    [(? number?)
     (list term)]
    [(? list?)
     (define children (map (curryr expand-proof-term budget) term))
     (cond 
       [(member (list #f) children)
        (list #f)]
       [else
        (define res (sequential-product children))
        (set-box! budget (- (unbox budget) (length res)))
        res])]
    [else (error "Unknown proof term ~a" term)]))


;; Remove the front term if it doesn't have any rewrites
(define (remove-front-term proof)
  (if (equal? (remove-rewrites (first proof)) (first proof))
      (rest proof)
      proof))

;; converts a let-bound tree explanation
;; into a flattened proof for use by Herbie
(define (expand-proof proof budget)
  (define expanded
          (map (curryr expand-proof-term budget) proof))
  ;; get rid of any unnecessary terms
  (define contiguous
    (cons (first expanded) (map remove-front-term (rest expanded))))
  ;; append together the proofs
  (define res
    (apply append contiguous))

  (set-box! budget (- (unbox budget) (length proof)))
  (if (member #f res)
      (list #f)
      res))

;; returns a flattened list of terms or #f if it failed to expand the proof due to budget
(define (egraph-get-proof egraph-data expr goal)
  (define egg-expr (~a (expr->egg-expr expr egraph-data)))
  (define egg-goal (~a (expr->egg-expr goal egraph-data)))
  (define pointer (egraph_get_proof (egraph-data-egraph-pointer egraph-data) egg-expr egg-goal))
  (define res (cast pointer _pointer _string/utf-8))
  (destroy_string pointer)
  (define env (make-hash))
  (define converted
    (for/list ([line (in-list (string-split res "\n"))])
      (egg-expr->expr line egraph-data)))
  (define expanded
    (expand-proof
     converted
     (box (*proof-max-length*))))

  (if (member #f expanded)
      #f
      expanded))

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
     (cons (iteration-data (EGraphIter-numnodes egraphiters)
                           (EGraphIter-numeclasses egraphiters)
                           (EGraphIter-time egraphiters))
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

;; (rules, reprs) -> (egg-rules, ffi-rules, name-map)
(define ffi-rules-cache #f)

;; Tries to look up the canonical name of a rule using the cache.
;; Obviously dangerous if the cache is invalid.
(define (get-canon-rule-name name [failure #f])
  (cond
    [ffi-rules-cache
     (match-define (list _ _ canon-names) (cdr ffi-rules-cache))
     (hash-ref canon-names name failure)]
    [else
     failure]))

;; expand the rules first due to some bad but currently
;; necessary reasons (see `rule->egg-rules` for details).
;; checks the cache in case we used them previously
(define (expand-rules rules)
  (define key (cons rules (*needed-reprs*)))
  (unless (and ffi-rules-cache (equal? (car ffi-rules-cache) key))
    ; free any rules in the cache
    (when ffi-rules-cache
      (match-define (list _ ffi-rules _) (cdr ffi-rules-cache))
      (free-ffi-rules ffi-rules))
    ; instantiate rules
    (define-values (egg-rules canon-names)
      (for/fold ([rules* '()] [canon-names (hash)] #:result (values (reverse rules*) canon-names))
                ([rule (in-list rules)])
        (define expanded (rule->egg-rules rule))
        (define orig-name (rule-name rule))
        (values (append expanded rules*)
                (for/fold ([canon-names* canon-names])
                          ([exp-rule (in-list expanded)])
                  (hash-set canon-names* (rule-name exp-rule) orig-name)))))
    ; update the cache
    (set! ffi-rules-cache (cons key (list egg-rules (make-ffi-rules egg-rules) canon-names))))
  (cdr ffi-rules-cache))

(define (egraph-run-rules egg-graph node-limit rules node-ids precompute? #:limit [iter-limit #f])
  ;; expand rules (will also check cache)
  (match-define (list egg-rules ffi-rules canon-names) (expand-rules rules))

  ;; run the rules
  (define iteration-data (egraph-run egg-graph node-limit ffi-rules precompute? iter-limit))

  ;; get cost statistics
  (let loop ([iter iteration-data] [counter 0] [time 0])
    (unless (null? iter)
      (define cnt (iteration-data-num-nodes (first iter)))
      (define cost (apply + (map (λ (node-id) (egraph-get-cost egg-graph node-id counter)) node-ids)))
      (define new-time (+ time (iteration-data-time (first iter))))
      (timeline-push! 'egraph counter cnt cost new-time)
      (loop (rest iter) (+ counter 1) new-time)))
  (timeline-push! 'stop (egraph-stop-reason egg-graph) 1)

  ;; get rule statistics
  (define rule-apps (make-hash))
  (for ([ffi-rule (in-list ffi-rules)] [rule (in-list egg-rules)])
    (define count (egraph-get-times-applied egg-graph ffi-rule))
    (define canon-name (hash-ref canon-names (rule-name rule)))
    (hash-update! rule-apps canon-name (curry + count) count))

  (for ([(name count) (in-hash rule-apps)])
    (when (> count 0) (timeline-push! 'rules (~a name) count)))

  iteration-data)
