#lang racket

(require egg-herbie
         (only-in ffi/vector
                  make-u32vector
                  u32vector-length
                  u32vector-set!
                  u32vector-ref
                  list->u32vector
                  u32vector->list))

(require racket/set
         "../utils/common.rkt"
         "../utils/errors.rkt"
         "../utils/timeline.rkt"
         "../syntax/platform.rkt"
         "../syntax/platform-state.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/block.rkt"
         "programs.rkt"
         "rules.rkt")

(provide (struct-out egg-runner)
         make-egraph
         egraph-equal?
         egraph-roots-equal?
         egraph-prove
         egraph-best
         egraph-variations
         deduplicate-exprs
         egraph-analyze-rewrite-impact)

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FFI utils

(define (in-u32vector vec)
  (make-do-sequence
   (lambda ()
     (define len (u32vector-length vec))
     (values (lambda (i) (u32vector-ref vec i)) add1 0 (lambda (i) (< i len)) #f #f))))

(define (repr-token repr)
  (match (representation-name repr)
    [(? symbol? name) (~a name)]
    [`(array ,slots ...) (format "array_~a" (string-join (map ~a slots) "_"))]))

(define do-lower-prefix "$do-lower.")
(define do-lower-leaf-prefix "$do-lower-leaf.")

(define (do-lower-op repr)
  (string->symbol (format "~a~a" do-lower-prefix (repr-token repr))))

(define (do-lower-op? op)
  (and (symbol? op)
       (string-prefix? (symbol->string op) do-lower-prefix)
       (not (do-lower-leaf-op? op))))

(define (do-lower-leaf-op? op)
  (and (symbol? op) (string-prefix? (symbol->string op) do-lower-leaf-prefix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; egg FFI shim
;;
;; egg-herbie requires a bit of nice wrapping
;; - FFIRule: struct defined in egg-herbie
;; - EgraphIter: struct defined in egg-herbie

; Adds expressions returning the root ids
(define (egraph-add-exprs ptr block vs ctx)

  ; pre-allocated id vectors for all the common cases
  (define 0-vec (make-u32vector 0))
  (define 1-vec (make-u32vector 1))
  (define 2-vec (make-u32vector 2))
  (define 3-vec (make-u32vector 3))

  (define (list->u32vec xs)
    (match xs
      [(list) 0-vec]
      [(list x)
       (u32vector-set! 1-vec 0 x)
       1-vec]
      [(list x y)
       (u32vector-set! 2-vec 0 x)
       (u32vector-set! 2-vec 1 y)
       2-vec]
      [(list x y z)
       (u32vector-set! 3-vec 0 x)
       (u32vector-set! 3-vec 1 y)
       (u32vector-set! 3-vec 2 z)
       3-vec]
      [_ (list->u32vector xs)]))

  ; node -> natural
  ; inserts an expression into the e-graph, returning its e-class id.

  (define (insert-node! node)
    (match node
      [(list op ids ...) (egraph_add_node ptr (~s op) (list->u32vec ids))]
      [(? (disjoin symbol? number?) x) (egraph_add_node ptr (~s x) 0-vec)]))

  (define add-to-egraph
    (block-recurse
     block
     (λ (v recurse)
       (define node (val-def v))
       (match node
         [(literal v _) (insert-node! v)]
         [(? number?) (insert-node! node)]
         [(? symbol?) (insert-node! (var->egg-var node ctx))]
         [(approx spec impl) (insert-node! (list '$approx (recurse spec) (recurse impl)))]
         [(list op (app recurse args) ...) (insert-node! (cons op args))]))))

  (for/list ([v (in-list vs)])
    (define v-id (add-to-egraph v)) ; remapping of v
    (egraph_add_root ptr v-id)
    v-id))

(define do-lower-leaf-counter 0)

(define (fresh-do-lower-leaf-op!)
  (define op (string->symbol (format "~a~a" do-lower-leaf-prefix do-lower-leaf-counter)))
  (set! do-lower-leaf-counter (add1 do-lower-leaf-counter))
  op)

(define (seed-do-lower-eclasses! ptr ctx root-ids)
  (define reprs (platform-reprs (*active-platform*)))
  (define real-reprs (filter (lambda (repr) (equal? (representation-type repr) 'real)) reprs))
  (define bool-reprs (filter (lambda (repr) (equal? (representation-type repr) 'bool)) reprs))
  (define array-reprs (filter array-representation? reprs))
  (define (type-reprs type)
    (cond
      [(representation? type) (list type)]
      [(equal? type 'real) real-reprs]
      [(equal? type 'bool) bool-reprs]
      [(equal? type 'array) array-reprs]
      [else '()]))
  (define (enode-reprs enode)
    (match enode
      [(? number?) real-reprs]
      [(? symbol? var) (list (context-lookup ctx (egg-var->var var ctx)))]
      [(cons f _)
       (cond
         [(eq? f 'array) array-reprs]
         [(eq? f 'if) (append real-reprs array-reprs)]
         [(eq? f '$approx) reprs]
         [(string-prefix? (symbol->string f) "sound-") real-reprs]
         [(impl-exists? f) (list (impl-info f 'otype))]
         [(operator-exists? f) (type-reprs (operator-info f 'otype))]
         [else '()])]))
  (define repr->ids (make-hash))
  (define id->enodes (make-hash))
  (let loop ([pending (remove-duplicates (map (curry egraph_find ptr) root-ids))])
    (unless (empty? pending)
      (define id (first pending))
      (if (hash-has-key? id->enodes id)
          (loop (rest pending))
          (let ([enodes (egraph-get-eclass ptr id)])
            (hash-set! id->enodes id enodes)
            (loop (append (rest pending)
                          (for*/list ([enode (in-vector enodes)]
                                      #:when (pair? enode)
                                      [child-id (in-u32vector (cdr enode))])
                            (egraph_find ptr child-id))))))))
  (for ([(id enodes) (in-hash id->enodes)])
    (define reprs-for-id (remove-duplicates (append-map enode-reprs (vector->list enodes))))
    (for ([repr (in-list reprs-for-id)])
      (hash-update! repr->ids repr (lambda (ids) (cons id ids)) '())))

  (define root-lower-ids
    (for/list ([_ (in-list root-ids)])
      (make-hash)))
  (define root-id->lower-ids (make-hash))
  (for ([root-id (in-list root-ids)]
        [lower-ids (in-list root-lower-ids)])
    (hash-update! root-id->lower-ids
                  (egraph_find ptr root-id)
                  (lambda (lower-idss) (cons lower-ids lower-idss))
                  '()))
  (define leaf-ops (make-hash))
  (for ([(repr ids) (in-hash repr->ids)])
    (define marker-ids (egraph_seed_do_lower ptr (~s (do-lower-op repr)) (list->u32vector ids)))
    (for ([id (in-list ids)]
          [marker-id (in-u32vector marker-ids)])
      (define root-lower-id (hash-ref root-id->lower-ids id #f))
      (when root-lower-id
        (for ([lower-ids (in-list root-lower-id)])
          (hash-set! lower-ids repr marker-id)))
      (define leaf
        (for/first ([enode (in-vector (hash-ref id->enodes id))]
                    #:when (or (number? enode)
                               (and (symbol? enode) (string-prefix? (symbol->string enode) "$var"))))
          enode))
      (when leaf
        (define leaf-op (fresh-do-lower-leaf-op!))
        (hash-set! leaf-ops leaf-op leaf)
        (egraph_add_node_to_eclass ptr marker-id (~s leaf-op) empty-u32vec))))
  (values root-lower-ids leaf-ops))

;; runs rules on an egraph (optional iteration limit)
(define (egraph-run ptr ffi-rules node-limit iter-limit scheduler)
  (define u32_max 4294967295) ; since we can't send option types
  (define node_limit (if node-limit node-limit u32_max))
  (define iter_limit (if iter-limit iter-limit u32_max))
  (define simple_scheduler?
    (match scheduler
      ['backoff #f]
      ['simple #t]
      [_ (error 'egraph-run "unknown scheduler: `~a`" scheduler)]))
  (egraph_run ptr ffi-rules iter_limit node_limit simple_scheduler?))

(define empty-u32vec (make-u32vector 0))

;; Extracts the nodes of an e-class as a vector
;; where each enode is either a symbol, number, or list
(define (egraph-get-eclass ptr id)
  (define eclass (egraph_get_eclass ptr id))
  ; need to fix up any constant operators
  (for ([enode (in-vector eclass)]
        [i (in-naturals)]
        #:when (and (symbol? enode) (not (string-prefix? (symbol->string enode) "$var"))))
    (vector-set! eclass i (cons enode empty-u32vec)))
  eclass)

(define (egraph-expr-equal? ptr expr goal ctx)
  (define-values (block vs) (progs->block (list expr goal) #:ctx ctx))
  (match-define (list id1 id2) (egraph-add-exprs ptr block vs ctx))
  (= id1 id2))

;; returns a flattened list of terms or #f if it failed to expand the proof due to budget
(define (egraph-get-proof ptr expr goal ctx)
  (define egg-expr (expr->egg-expr expr ctx))
  (define egg-goal (expr->egg-expr goal ctx))
  (define str (egraph_get_proof ptr egg-expr egg-goal))
  (cond
    [(<= (string-length str) (*proof-max-string-length*))
     (define converted
       (for/list ([expr (in-port read (open-input-string str))])
         (egg-expr->expr expr ctx)))
     (define expanded (expand-proof converted (box (*proof-max-length*))))
     (if (member #f expanded) #f expanded)]
    [else #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eggIR
;;
;; eggIR is an S-expr language nearly identical to Herbie's various IRs
;; consisting of two variants:
;;  - patterns: all variables are prefixed by '?'
;;  - expressions: all variables are normalized into `h<n>` where <n> is an integer
;;

;; Translates a Herbie rule LHS or RHS into a pattern usable by egg.
;; Rules can be over specs or impls.
(define (expr->egg-pattern expr)
  (let loop ([expr expr])
    (match expr
      [(? number?) expr]
      [(? literal?) (literal-value expr)]
      [(? symbol?) (string->symbol (format "?~a" expr))]
      [(approx spec impl) (list '$approx (loop spec) (loop impl))]
      [(list op args ...) (cons op (map loop args))])))

(define (var->egg-var var ctx)
  (define idx (index-of (context-vars ctx) var))
  (string->symbol (format "$var~a" idx)))

(define (egg-var->var egg-var ctx)
  (define idx (string->number (substring (symbol->string egg-var) 4)))
  (list-ref (context-vars ctx) idx))

;; Translates a Herbie expression into an expression usable by egg.
;; Updates translation dictionary upon encountering variables.
;; Result is the expression.
(define (expr->egg-expr expr ctx)
  (let loop ([expr expr])
    (match expr
      [(? number?) expr]
      [(? literal?) (literal-value expr)]
      [(? symbol? x) (var->egg-var x ctx)]
      [(approx spec impl) (list '$approx (loop spec) (loop impl))]
      [(list op args ...) (cons op (map loop args))])))

(define (flatten-let expr)
  (let loop ([expr expr]
             [env (hash)])
    (match expr
      [(? number?) expr]
      [(? symbol?) (hash-ref env expr expr)]
      [`(let (,var
              ,term)
          ,body)
       (loop body (hash-set env var (loop term env)))]
      [`(,op ,args ...) (cons op (map (curryr loop env) args))])))

(define (spec-arg-types op arity)
  (match op
    ['array (make-list arity 'real)]
    [_ (operator-info op 'itype)]))

;; Converts an S-expr from egg into one Herbie understands
;; TODO: typing information is confusing since proofs mean
;; we may process mixed spec/impl expressions;
;; only need `type` to correctly interpret numbers
(define (egg-parsed->expr expr ctx type)
  (let loop ([expr expr]
             [type type])
    (match expr
      [(? number?)
       (if (representation? type)
           (literal expr (representation-name type))
           expr)]
      [(? symbol?)
       (if (string-prefix? (symbol->string expr) "$var")
           (egg-var->var expr ctx)
           (list expr))]
      [(list '$approx spec impl) ; approx
       (define spec-type
         (if (representation? type)
             (representation-type type)
             type))
       (approx (loop spec spec-type) (loop impl type))]
      [`(Explanation ,body ...) `(Explanation ,@(map (lambda (e) (loop e type)) body))]
      [(list 'Rewrite=> rule expr) (list 'Rewrite=> rule (loop expr type))]
      [(list 'Rewrite<= rule expr) (list 'Rewrite<= rule (loop expr type))]
      [(list op args ...)
       #:when (string-prefix? (symbol->string op) "sound-")
       (define op* (string->symbol (substring (symbol->string op) (string-length "sound-"))))
       (define args* (drop-right args 1))
       (cons op* (map loop args* (map (const 'real) args*)))]
      [(list op args ...)
       ;; Unfortunately the type parameter doesn't tell us much because mixed exprs exist
       ;; so if we see something like (and a b) we literally don't know which "and" it is
       (cons op
             (map loop
                  args
                  (cond
                    [(and (operator-exists? op) (impl-exists? op))
                     (if (representation? type)
                         (impl-info op 'itype)
                         (spec-arg-types op (length args)))]
                    [(impl-exists? op) (impl-info op 'itype)]
                    [(operator-exists? op) (spec-arg-types op (length args))])))])))

;; Parses a string from egg into a single S-expr.
(define (egg-expr->expr egg-expr ctx)
  (egg-parsed->expr (flatten-let egg-expr) ctx (context-repr ctx)))

(module+ test
  (require "../syntax/float.rkt"
           "../syntax/load-platform.rkt")
  (activate-platform! (*platform-name*))
  (define ctx (context '(x y z) <binary64> (make-list 3 <binary64>)))

  (define test-exprs
    (list (cons '(+ y x) '(+ $var1 $var0))
          (cons '(+ x y) '(+ $var0 $var1))
          (cons '(- 2 (+ x y)) '(- 2 (+ $var0 $var1)))
          (cons '(- z (+ (+ y 2) x)) '(- $var2 (+ (+ $var1 2) $var0)))
          (cons '(* x y) '(* $var0 $var1))
          (cons '(+ (* x y) 2) '(+ (* $var0 $var1) 2))))

  (let ([egg-graph (egraph_create)])
    (for ([(in expected-out) (in-dict test-exprs)])
      (define out (expr->egg-expr in ctx))
      (define computed-in (egg-expr->expr out ctx))
      (check-equal? out expected-out)
      (check-equal? computed-in in)))

  (check-equal? (egg-expr->expr '(sound-sqrt $var0 $var1) ctx) '(sqrt x))

  (check-equal? (egg-expr->expr '(array $var0 $var1) ctx) '(array x y))
  (check-equal? (egg-expr->expr '(array $var0 $var1 $var2) ctx) '(array x y z))

  (set! ctx (context '(x a b c r) <binary64> (make-list 5 <binary64>)))
  (define extended-expr-list
    (list '(/ (- (exp x) (exp (neg x))) 2)
          '(/ (+ (neg b) (sqrt (- (* b b) (* (* 3 a) c)))) (* 3 a))
          '(/ (+ (neg b) (sqrt (- (* b b) (* (* 3 a) c)))) (* 3 a))
          '(* r 30)
          '(* 23/54 r)
          '(+ 3/2 14/10)))

  (let ([egg-graph (egraph_create)])
    (for ([expr extended-expr-list])
      (define egg-expr (expr->egg-expr expr ctx))
      (check-equal? (egg-expr->expr egg-expr ctx) expr)))

  (define dedup-ctx1 (context '(x y) <binary64> (list <binary64> <binary64>)))
  (define dedup-ctx2 (context '(y x) <binary64> (list <binary64> <binary64>)))
  (define deduped (deduplicate-exprs (list '(+ x y) '(+ y x)) (list dedup-ctx1 dedup-ctx2)))
  (check-equal? (length deduped) 2)
  (check-equal? (first deduped) (second deduped)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proofs
;;
;; Proofs from egg contain let expressions (not Scheme like) as
;; well as other information about rewrites; proof extraction requires
;; some flattening and translation

(define (remove-rewrites proof)
  (match proof
    [`(Rewrite=> ,_ ,something) (remove-rewrites something)]
    [`(Rewrite<= ,_ ,something) (remove-rewrites something)]
    [(list _ ...) (map remove-rewrites proof)]
    [_ proof]))

;; Performs a product, but traverses the elements in order
;; This is the core logic of flattening a proof given flattened proofs for each child of a node
(define (sequential-product elements)
  (cond
    [(empty? elements) (list empty)]
    [else
     (define without-rewrites (remove-rewrites (last (first elements))))
     (append (for/list ([head (first elements)])
               (cons head (map first (rest elements))))
             (for/list ([other (in-list (rest (sequential-product (rest elements))))])
               (cons without-rewrites other)))]))

;; returns a flattened list of terms
;; The first term has no rewrite- the rest have exactly one rewrite
(define (expand-proof-term term budget)
  (let loop ([term term])
    (cond
      [(<= (unbox budget) 0) (list #f)]
      [else
       (match term
         [(? symbol?) (list term)]
         [(? literal?) (list term)]
         [(? number?) (list term)]
         [(approx spec impl)
          (define children (list (loop spec) (loop impl)))
          (cond
            [(member (list #f) children) (list #f)]
            [else
             (define res (sequential-product children))
             (set-box! budget (- (unbox budget) (length res)))
             (map (curry apply approx) res)])]
         [`(Explanation ,body ...) (expand-proof body budget)]
         [(? list?)
          (define children (map loop term))
          (cond
            [(member (list #f) children) (list #f)]
            [else
             (define res (sequential-product children))
             (set-box! budget (- (unbox budget) (length res)))
             res])]
         [_ (error "Unknown proof term ~a" term)])])))

;; Remove the front term if it doesn't have any rewrites
(define (remove-front-term proof)
  (if (equal? (remove-rewrites (first proof)) (first proof))
      (rest proof)
      proof))

;; converts a let-bound tree explanation
;; into a flattened proof for use by Herbie
(define (expand-proof proof budget)
  (define expanded (map (curryr expand-proof-term budget) proof))
  ;; get rid of any unnecessary terms
  (define contiguous (cons (first expanded) (map remove-front-term (rest expanded))))
  ;; append together the proofs
  (define res (apply append contiguous))
  (set-box! budget (- (unbox budget) (length proof)))
  (if (member #f res)
      (list #f)
      res))

(module+ test
  (check-equal? (sequential-product `((1 2) (3 4 5) (6))) `((1 3 6) (2 3 6) (2 4 6) (2 5 6)))

  (check-equal? (expand-proof-term '(Explanation (+ x y) (+ y x)) (box 10)) '((+ x y))))

;; egg rule cache: rule -> FFI-rule
(define/reset *egg-rule-cache* (make-hasheq))

;; Expand and convert the rules for egg.
;; Uses a cache to only expand each rule once.
(define (convert-rules rules)
  (for/list ([ru (in-list rules)])
    (hash-ref! (*egg-rule-cache*)
               ru
               (lambda ()
                 (define input (expr->egg-pattern (rule-input ru)))
                 (define output (expr->egg-pattern (rule-output ru)))
                 (make-ffi-rule (rule-name ru) input output)))))

;; Rules from impl to spec (fixed for a particular platform)
(define/reset *lifting-rules* (make-hash))

;; Synthesizes the LHS and RHS of lifting/lowering rules.
(define (impl->rule-parts impl)
  (define vars (impl-info impl 'vars))
  (define spec (impl-info impl 'spec))
  (values vars spec (cons impl vars)))

;; Synthesizes lifting rules for a platform platform.
(define (platform-lifting-rules [pform (*active-platform*)])
  (define impls (platform-impls pform))
  (for/list ([impl (in-list impls)])
    (hash-ref! (*lifting-rules*)
               (cons impl pform)
               (lambda ()
                 (define name (sym-append 'lift- impl))
                 (define-values (vars spec-expr impl-expr) (impl->rule-parts impl))
                 (rule name impl-expr spec-expr '(lifting))))))

;; Lowering rules using explicit do-lower terms. The e-graph is seeded with
;; do-lower terms for matching existing e-classes
;; before these rules run.
(define (platform-do-lowering-rules [pform (*active-platform*)])
  (define helper-impls
    (for/seteq ([extension (in-list (*platform-extensions*))])
      (fpcore-extension-name extension)))
  (define normal-rules
    (append* (for/list ([impl (in-list (platform-impls pform))]
                        #:unless (set-member? helper-impls impl))
               (define vars (impl-info impl 'vars))
               (define var-reprs (map cons vars (impl-info impl 'itype)))
               (define spec (impl-info impl 'spec))
               (define otype (impl-info impl 'otype))
               (define lower-name (sym-append 'do-lower- impl '-impl))
               (list (rule lower-name
                           (list (do-lower-op otype) spec)
                           (cons impl
                                 (for/list ([var (in-list vars)])
                                   (list (do-lower-op (dict-ref var-reprs var)) var)))
                           '(lowering))))))
  (append normal-rules (array-lowering-rules pform)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduler
;;
;; A mini-interpreter for egraph "schedules" including running egg,
;; pruning certain kinds of nodes, extracting expressions, etc.

;; Runs rules over the egraph with the given egg parameters.
(define (egraph-run-rules egg-graph0
                          egg-rules
                          #:node-limit [node-limit #f]
                          #:iter-limit [iter-limit #f]
                          #:scheduler [scheduler 'backoff])
  ;; run the rules
  (define egg-graph (egraph_copy egg-graph0))
  (define iteration-data (egraph-run egg-graph egg-rules node-limit iter-limit scheduler))

  (when (egraph_is_unsound_detected egg-graph)
    (warn 'unsound-egraph #:url "faq.html#unsound-egraph" "unsoundness detected in the egraph"))
  (timeline-push! 'stop (~a (egraph_get_stop_reason egg-graph)) 1)
  (values egg-graph iteration-data))

(define (egraph-analyze-rewrite-impact block vs ctx iter)
  (define egg-graph (egraph_create))
  (egraph-add-exprs egg-graph block vs ctx)
  (define-values (egg-graph0 _0) (egraph-run-rules egg-graph '()))
  (define-values (egg-graph1 _1)
    (if (> iter 0)
        (egraph-run-rules egg-graph0 (convert-rules (*rules*)) #:iter-limit iter)
        (values egg-graph0 _0)))
  (define-values (egg-graph3 iter-data3) (egraph-run-rules egg-graph1 '()))
  (define initial-size (iteration-data-num-nodes (last iter-data3)))
  (define results
    (for/list ([rule (in-list (*rules*))])
      (define-values (egg-graph5 iter-data5)
        (egraph-run-rules egg-graph3 (convert-rules (list rule)) #:iter-limit 2))
      (define size (iteration-data-num-nodes (last (if (empty? iter-data5) iter-data3 iter-data5))))
      (cons rule (- size initial-size))))
  (define final-size
    (let-values ([(egg-graph6 iter-data6)
                  (egraph-run-rules egg-graph3 (convert-rules (*rules*)) #:iter-limit 2)])
      (iteration-data-num-nodes (last (if (empty? iter-data6) iter-data3 iter-data6)))))
  (values initial-size final-size results))

(define (egraph-run-schedule block vs schedule ctx)
  ; allocate the e-graph
  (define egg-graph (egraph_create))

  ; insert expressions into the e-graph
  (define root-ids (egraph-add-exprs egg-graph block vs ctx))
  (define-values (egg-graph0 rebuild-data) (egraph-run-rules egg-graph '()))

  (define (rewrite-node-limit initial-size)
    (if initial-size
        (max 0 (- (*node-limit*) initial-size))
        (*node-limit*)))

  ; run the schedule
  (define lower-roots #f)
  (define lower-leaf-ops (make-hash))
  (define egg-graph*
    (for/fold ([egg-graph egg-graph0]
               [rewrite-initial-size (iteration-data-num-nodes (last rebuild-data))]
               #:result egg-graph)
              ([step (in-list schedule)])
      (define-values (egg-graph* iteration-data)
        (match step
          ['lift
           (define rules (convert-rules (platform-lifting-rules)))
           (egraph-run-rules egg-graph rules #:iter-limit 1 #:scheduler 'simple)]
          ['lower
           (define-values (lower-roots* leaf-ops) (seed-do-lower-eclasses! egg-graph ctx root-ids))
           (set! lower-roots lower-roots*)
           (for ([(op enode) (in-hash leaf-ops)])
             (hash-set! lower-leaf-ops op enode))
           (define rules (convert-rules (platform-do-lowering-rules)))
           (egraph-run-rules egg-graph rules #:iter-limit 1 #:scheduler 'simple)]
          ['unsound
           (define rules (convert-rules (*sound-removal-rules*)))
           (egraph-run-rules egg-graph rules #:iter-limit 1 #:scheduler 'simple)]
          ['rewrite
           (define rules (convert-rules (*rules*)))
           (egraph-run-rules egg-graph
                             rules
                             #:node-limit (rewrite-node-limit rewrite-initial-size))]))

      ; get cost statistics
      (for ([iter (in-list iteration-data)]
            [i (in-naturals)])
        (define cnt (iteration-data-num-nodes iter))
        (define cost (for/sum ([id (in-list root-ids)]) (egraph_get_cost egg-graph* id i)))
        (timeline-push! 'egraph i cnt cost (iteration-data-time iter)))

      (define rewrite-initial-size*
        (if (empty? iteration-data)
            rewrite-initial-size
            (iteration-data-num-nodes (last iteration-data))))
      (values egg-graph* rewrite-initial-size*)))

  ; root eclasses may have changed
  (define root-ids* (map (lambda (id) (egraph_find egg-graph* id)) root-ids))
  (define lower-roots*
    (and lower-roots
         (for/list ([root-lower-ids (in-list lower-roots)])
           (for/hash ([(repr id) (in-hash root-lower-ids)])
             (values repr (egraph_find egg-graph* id))))))
  ; return what we need
  (values root-ids* lower-roots* lower-leaf-ops egg-graph*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;
;; Most calls to egg should be done through this interface.
;;  - `make-egraph`: constructs an egraph and runs rules on it
;;  - `egraph-equal?`: test if two expressions are equal
;;  - `egraph-prove`: return a proof that two expressions are equal
;;  - `egraph-best`: return a block with the best versions of another block
;;  - `egraph-variations`: return a block with all versions of another block

;; Herbie's version of an egg runner.
;; Defines parameters for running rewrite rules with egg
(struct egg-runner (block schedule ctx new-roots lower-roots lower-leaf-ops egg-graph)
  #:transparent ; for equality
  #:methods gen:custom-write ; for abbreviated printing
  [(define (write-proc alt port mode)
     (fprintf port "#<egg-runner>"))])

;; Constructs an egg runner.
;;
;; The schedule is a list of step symbols:
;;  - `lift`: run lifting rules for 1 iteration with simple scheduler
;;  - `rewrite`: run rewrite rules up to node limit with backoff scheduler
;;  - `unsound`: run sound-removal rules for 1 iteration with simple scheduler
;;  - `lower`: seed and run do-lower rules for 1 iteration
(define (make-egraph block vs schedule ctx)
  (define (oops! fmt . args)
    (apply error 'verify-schedule! fmt args))
  ; verify the schedule
  (for ([step (in-list schedule)])
    (unless (memq step '(lift lower unsound rewrite))
      (oops! "unknown schedule step `~a`" step)))

  (define-values (root-ids lower-roots lower-leaf-ops egg-graph)
    (egraph-run-schedule block vs schedule ctx))

  ; make the runner
  (egg-runner block schedule ctx root-ids lower-roots lower-leaf-ops egg-graph))

(module+ test
  (require "../syntax/load-platform.rkt")
  (activate-platform! "c")
  (test-case "do-lower terms produce implementations"
    (define ctx (context '(x) <binary64> (list <binary64>)))
    (define-values (block vs) (progs->block (list '(+ x 1)) #:ctx ctx))
    (define runner (make-egraph block vs '(lower) ctx))
    (define vals (egraph-best runner block (list <binary64>)))
    (check-equal? (car ((block-exprs block) (first (first vals)))) '+.f64)
    (check-not-equal? (first (egg-runner-new-roots runner))
                      (hash-ref (first (egg-runner-lower-roots runner)) <binary64>)))

  (test-case "do-lower terms handle nullary implementations"
    (define ctx (context '() <binary64> '()))
    (define-values (block vs) (progs->block (list '(PI)) #:ctx ctx))
    (define runner (make-egraph block vs '(lower) ctx))
    (define vals (egraph-best runner block (list <binary64>)))
    (check-equal? ((block-exprs block) (first (first vals))) '(PI.f64))))

(module+ test
  (require "../syntax/load-platform.rkt")
  (test-case "initial rebuild canonicalizes exact division literals"
    (activate-platform! "c")
    (define rebuild-ctx (context '(x y) <binary64> (list <binary64> <binary64>)))
    (define expr '(+ (/ 1 2) (* x y)))
    (define-values (block vs) (progs->block (list expr) #:ctx rebuild-ctx))
    (define runner (make-egraph block vs '() rebuild-ctx))
    (define egg-graph (egg-runner-egg-graph runner))
    (define eclasses (u32vector->list (egraph_get_eclasses egg-graph)))

    (check-false (for*/or ([id (in-list eclasses)]
                           [enode (in-vector (egraph-get-eclass egg-graph id))])
                   (match enode
                     [(list '/ _ ...) #t]
                     [_ #f])))))

(define (egraph-equal? runner start end)
  (define ctx (egg-runner-ctx runner))
  (define egg-graph (egg-runner-egg-graph runner))
  (egraph-expr-equal? egg-graph start end ctx))

(define (egraph-roots-equal? runner idx1 idx2)
  (define root-ids (egg-runner-new-roots runner))
  (= (list-ref root-ids idx1) (list-ref root-ids idx2)))

(define (egraph-prove runner start-v end-v)
  (define ctx (egg-runner-ctx runner))
  (define egg-graph (egg-runner-egg-graph runner))
  (define block (egg-runner-block runner))
  (define exprs (block-exprs block))
  (define start (exprs start-v))
  (define end (exprs end-v))

  (unless (egraph-expr-equal? egg-graph start end ctx)
    (error 'egraph-prove "cannot prove ~a is equal to ~a; not equal" start end))
  (define proof (egraph-get-proof egg-graph start end ctx))
  (when (null? proof)
    (error 'egraph-prove "proof extraction failed between`~a` and `~a`" start end))
  proof)

(define max-extraction-cost (sub1 (expt 2 64)))

(define (egg-leaf->block block leaf ctx type leaf-ops)
  (define leaf*
    (if (symbol? leaf)
        (hash-ref leaf-ops leaf leaf)
        leaf))
  (block-push!
   block
   (cond
     [(number? leaf*)
      (if (representation? type)
          (literal leaf* (representation-name type))
          leaf*)]
     [(and (symbol? leaf*) (string-prefix? (symbol->string leaf*) "$var")) (egg-var->var leaf* ctx)]
     [else (list leaf*)])))

(define (egg-batch-nodes->block nodes results block ctx repr leaf-ops)
  (define vals (make-vector (length nodes) #f))

  (define (add-node idx type)
    (define vals-by-type
      (or (vector-ref vals idx)
          (let ([new-vals (make-hash)])
            (vector-set! vals idx new-vals)
            new-vals)))
    (hash-ref! vals-by-type
               type
               (lambda ()
                 (match (list-ref nodes idx)
                   [(? number? n)
                    (block-push! block
                                 (if (representation? type)
                                     (literal n (representation-name type))
                                     n))]
                   [(? symbol? op) (egg-leaf->block block op ctx type leaf-ops)]
                   [(list '$approx spec impl)
                    (block-push! block
                                 (approx (val-idx (add-node spec
                                                            (if (representation? type)
                                                                (representation-type type)
                                                                type)))
                                         (val-idx (add-node impl type))))]
                   [(list impl args ...)
                    (define arg-types
                      (if (representation? type)
                          (impl-info impl 'itype)
                          (spec-arg-types impl (length args))))
                    (define arg-idxs
                      (for/list ([arg (in-list args)]
                                 [arg-type (in-list arg-types)])
                        (val-idx (add-node arg arg-type))))
                    (block-push! block (cons impl arg-idxs))]))))

  (for/list ([result (in-list results)])
    (match result
      [(list cost idx)
       #:when (< cost max-extraction-cost)
       (add-node idx repr)]
      [_ #f])))

(define (egg-best-expressions runner block ids reprs)
  (if (empty? ids)
      '()
      (let ([out (make-vector (length ids) #f)])
        (for ([repr (in-list (remove-duplicates reprs))])
          (define positions
            (for/list ([id (in-list ids)]
                       [i (in-naturals)]
                       [repr* (in-list reprs)]
                       #:when (equal? repr repr*))
              i))
          (define results
            (egraph_extract_best_batch (egg-runner-egg-graph runner)
                                       (list->u32vector (map (curry list-ref ids) positions))))
          (match results
            [(list batch-results nodes)
             (define batch-vals
               (egg-batch-nodes->block nodes
                                       batch-results
                                       block
                                       (egg-runner-ctx runner)
                                       repr
                                       (egg-runner-lower-leaf-ops runner)))
             (for ([i (in-list positions)]
                   [val (in-list batch-vals)])
               (vector-set! out i val))]))
        (vector->list out))))

(define (egg-best-expression runner block id repr)
  (first (egg-best-expressions runner block (list id) (list repr))))

(define (lower-enode-requests runner enode)
  (match enode
    [(cons op ids)
     (cond
       [(or (do-lower-op? op) (do-lower-leaf-op? op)) '()]
       [else
        (for/list ([id (in-u32vector ids)]
                   [arg-repr (in-list (impl-info op 'itype))])
          (cons (egraph_find (egg-runner-egg-graph runner) id) arg-repr))])]
    [_ '()]))

(define (lower-enode->block runner block enode repr best-exprs)
  (define ctx (egg-runner-ctx runner))
  (define leaf-ops (egg-runner-lower-leaf-ops runner))
  (define egg-graph (egg-runner-egg-graph runner))
  (match enode
    [(? number? n) (egg-leaf->block block n ctx repr leaf-ops)]
    [(? symbol? op) (egg-leaf->block block op ctx repr leaf-ops)]
    [(cons op ids)
     (cond
       [(do-lower-op? op) #f]
       [(do-lower-leaf-op? op) (egg-leaf->block block op ctx repr leaf-ops)]
       [else
        (define arg-vals
          (for/list ([id (in-u32vector ids)]
                     [arg-repr (in-list (impl-info op 'itype))])
            (hash-ref best-exprs (cons (egraph_find egg-graph id) arg-repr) #f)))
        (and (andmap values arg-vals) (block-push! block (cons op (map val-idx arg-vals))))])]))

(define (egraph-best-from-lower-root runner block root-lower-ids repr)
  (define lower-id (hash-ref root-lower-ids repr #f))
  (if lower-id
      (match (egg-best-expression runner block lower-id repr)
        [#f '()]
        [val (list val)])
      '()))

(define (egraph-variations-from-lower-root runner block root-lower-ids repr)
  (define lower-id (hash-ref root-lower-ids repr #f))
  (if lower-id
      (let* ([enodes (egraph-get-eclass (egg-runner-egg-graph runner) lower-id)]
             [requests (remove-duplicates (append* (for/list ([enode (in-vector enodes)])
                                                     (lower-enode-requests runner enode))))]
             [best-exprs (for/hash ([request (in-list requests)]
                                    [expr (in-list (egg-best-expressions runner
                                                                         block
                                                                         (map car requests)
                                                                         (map cdr requests)))])
                           (values request expr))]
             [vals (remove-duplicates
                    (for/list ([enode (in-vector enodes)]
                               #:do [(define val
                                       (lower-enode->block runner block enode repr best-exprs))]
                               #:when val)
                      val)
                    #:key val-idx)])
        vals)
      '()))

(define (egraph-best runner block reprs)
  (define egg-graph (egg-runner-egg-graph runner))

  ; Return empty results if unsound
  (cond
    [(egraph_is_unsound_detected egg-graph) (map (const empty) (egg-runner-new-roots runner))]
    [else
     (define lower-roots (egg-runner-lower-roots runner))
     (if lower-roots
         (for/list ([root-lower-ids (in-list lower-roots)]
                    [repr (in-list reprs)])
           (egraph-best-from-lower-root runner block root-lower-ids repr))
         (for/list ([id (in-list (egg-runner-new-roots runner))]
                    [repr (in-list reprs)])
           (match (egg-best-expression runner block id repr)
             [#f '()]
             [val (list val)])))]))

(define (egraph-variations runner block reprs)
  (define egg-graph (egg-runner-egg-graph runner))

  ; Return empty results if unsound
  (cond
    [(egraph_is_unsound_detected egg-graph) (map (const empty) (egg-runner-new-roots runner))]
    [else
     (define lower-roots (egg-runner-lower-roots runner))
     (if lower-roots
         (for/list ([root-lower-ids (in-list lower-roots)]
                    [repr (in-list reprs)])
           (egraph-variations-from-lower-root runner block root-lower-ids repr))
         (for/list ([id (in-list (egg-runner-new-roots runner))]
                    [repr (in-list reprs)])
           (match (egg-best-expression runner block id repr)
             [#f '()]
             [val (list val)])))]))

(define (deduplicate-exprs exprs ctxs)
  (define ctx (contexts-union ctxs))
  (define-values (block vs) (progs->block exprs #:ctx ctx))
  (define reprs (make-list (length vs) (context-repr ctx)))
  (define runner (make-egraph block vs '(rewrite lower) ctx))
  (define valss (egraph-best runner block reprs))
  (define block-pull (block-exprs block))
  (for/list ([orig-expr (in-list exprs)]
             [refs (in-list valss)])
    (if (empty? refs)
        orig-expr
        (block-pull (first refs)))))
