#lang racket

(require egg-herbie
        (only-in ffi/unsafe
          malloc memcpy free cast ptr-set! ptr-add
          _byte _pointer _string/utf-8
          register-finalizer))

(require "../syntax/rules.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../accelerator.rkt"
         "../common.rkt"
         "../errors.rkt"
         "../platform.rkt"
         "../programs.rkt"
         "../timeline.rkt" )

(provide (struct-out egraph-query)
         (struct-out regraph)
         untyped-egg-extractor
         typed-egg-extractor
         default-untyped-egg-cost-proc
         platform-egg-cost-proc
         make-egg-query
         run-egg
         get-canon-rule-name
         remove-rewrites
         real-rules
         platform-lowering-rules
         platform-impl-rules
         rule->impl-rules)

(module+ test
  (require rackunit)
  (require "../load-plugin.rkt")
  (load-herbie-builtins))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; egg FFI shim
;;
;; egg-herbie requires a bit of nice wrapping
;; - FFIRule: struct defined in egg-herbie
;; - EgraphIter: struct defined in egg-herbie

(define (make-raw-string s)
  (define b (string->bytes/utf-8 s))
  (define n (bytes-length b))
  (define ptr (malloc 'raw (+ n 1)))
  (memcpy ptr b n)
  (ptr-set! ptr _byte n 0)
  ptr)

(define (make-ffi-rule rule)
  (define name (make-raw-string (~a (rule-name rule))))
  (define lhs (make-raw-string (~a (rule-input rule))))
  (define rhs (make-raw-string (~a (rule-output rule))))
  (define p (make-FFIRule name lhs rhs))
  (register-finalizer p free-ffi-rule)
  p)

(define (free-ffi-rule rule)
  (free (FFIRule-name rule))
  (free (FFIRule-left rule))
  (free (FFIRule-right rule))
  (free rule))

;; the first hash table maps all symbols and non-integer values
;; to new names for egg; the second hash is the reverse of the first
(struct egraph-data (egraph-pointer egg->herbie-dict herbie->egg-dict))

; Makes a new egraph that is managed by Racket's GC
(define (make-egraph)
  (egraph-data (egraph_create) (make-hash) (make-hash)))

; Creates a new runner using an existing egraph.
; Useful for multi-phased rule application
(define (egraph-copy eg-data)
  (struct-copy egraph-data eg-data
    [egraph-pointer (egraph_copy (egraph-data-egraph-pointer eg-data))]))

;; result function is a function that takes the ids of the nodes
(define (egraph-add-expr eg-data expr ctx)
  (define egg-expr (~a (expr->egg-expr expr eg-data ctx)))
  (egraph_add_expr (egraph-data-egraph-pointer eg-data) egg-expr))

;; runs rules on an egraph (optional iteration limit)
(define (egraph-run egraph-data ffi-rules node-limit iter-limit scheduler const-folding?)
  (define u32_max 4294967295) ; since we can't send option types
  (define node_limit (if node-limit node-limit u32_max))
  (define iter_limit (if iter-limit iter-limit u32_max))
  (define simple_scheduler?
    (match scheduler
      ['backoff #f]
      ['simple #t]
      [_ (error 'egraph-run "unknown scheduler: `~a`" scheduler)]))
  (define-values (iterations length ptr)
    (egraph_run (egraph-data-egraph-pointer egraph-data)
                ffi-rules
                iter_limit
                node_limit
                simple_scheduler?
                const-folding?))
  (define iteration-data (convert-iteration-data iterations length))
  (destroy_egraphiters ptr)
  iteration-data)

(define (egraph-get-simplest egraph-data node-id iteration ctx)
  (define ptr (egraph_get_simplest (egraph-data-egraph-pointer egraph-data) node-id iteration))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  (egg-expr->expr str egraph-data (context-repr ctx)))

(define (egraph-get-variants egraph-data node-id orig-expr ctx)
  (define expr-str (~a (expr->egg-expr orig-expr egraph-data ctx)))
  (define ptr (egraph_get_variants (egraph-data-egraph-pointer egraph-data) node-id expr-str))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  (egg-exprs->exprs str egraph-data (context-repr ctx)))

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

;; An egraph is just a S-expr of the form
;; 
;;  egraph ::= (<eclass> ...)
;;  eclass ::= (<id> <enode> ..+)
;;  enode  ::= (<op> <id> ...)
;;
(define (egraph-serialize egraph-data)
  (egraph_serialize (egraph-data-egraph-pointer egraph-data)))

(define (egraph-find egraph-data id)
  (egraph_find (egraph-data-egraph-pointer egraph-data) id))

(define (egraph-is-equal egraph-data expr goal ctx)
  (define egg-expr (~a (expr->egg-expr expr egraph-data ctx)))
  (define egg-goal (~a (expr->egg-expr goal egraph-data ctx)))
  (egraph_is_equal (egraph-data-egraph-pointer egraph-data) egg-expr egg-goal))

;; returns a flattened list of terms or #f if it failed to expand the proof due to budget
(define (egraph-get-proof egraph-data expr goal ctx)
  (define egg-expr (~a (expr->egg-expr expr egraph-data ctx)))
  (define egg-goal (~a (expr->egg-expr goal egraph-data ctx)))
  (define pointer (egraph_get_proof (egraph-data-egraph-pointer egraph-data) egg-expr egg-goal))
  (define res (cast pointer _pointer _string/utf-8))
  (destroy_string pointer)
  (cond
   [(< (string-length res) 10000)
    (define converted (egg-exprs->exprs res egraph-data (context-repr ctx)))
    (define expanded (expand-proof converted (box (*proof-max-length*))))
    (if (member #f expanded)
        #f
        expanded)]
   [else
    #f]))

;; Racket representation of per-iteration runner data
(struct iteration-data (num-nodes num-eclasses time))

(define (convert-iteration-data egraphiters size)
  (for/list ([i (in-range size)])
    (define ptr (ptr-add egraphiters i _EGraphIter))
    (iteration-data (EGraphIter-numnodes ptr)
                    (EGraphIter-numeclasses ptr)
                    (EGraphIter-time ptr))))

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
      [(list op args ...) (cons op (map loop args))])))

;; Translates a Herbie expression into an expression usable by egg.
;; Updates translation dictionary upon encountering variables.
;; Result is the expression.
(define (expr->egg-expr expr egg-data ctx)
  (define egg->herbie-dict (egraph-data-egg->herbie-dict egg-data))
  (define herbie->egg-dict (egraph-data-herbie->egg-dict egg-data))
  (let loop ([expr expr])
    (match expr
      [(? number?) expr]
      [(? literal?) (literal-value expr)]
      [(? (curry hash-has-key? herbie->egg-dict))
       (hash-ref herbie->egg-dict expr)]
      [(? symbol?)
       (define replacement (string->symbol (format "$h~a" (hash-count herbie->egg-dict))))
       (hash-set! herbie->egg-dict expr replacement)
       (hash-set! egg->herbie-dict replacement (cons expr (context-lookup ctx expr)))
       replacement]
      [(list op args ...) (cons op (map loop args))])))

(define (flatten-let expr)
  (let loop ([expr expr] [env (hash)])
    (match expr
      [`(let (,var ,term) ,body)
       (loop body (hash-set env var (loop term env)))]
      [(? symbol?)
       (hash-ref env expr expr)]
      [(? list?)
       (map (curryr loop env) expr)]
      [(? number?)
       expr]
      [_
       (error "Unknown term ~a" expr)])))

;; Converts an S-expr from egg into one Herbie understands
(define (egg-parsed->expr expr rename-dict type)
  (let loop ([expr expr] [type type])
    (match expr
      [(? number?)
       (if (representation? type)
           (literal expr (representation-name type))
           expr)]
      [(? symbol?)
       (if (hash-has-key? rename-dict expr)
           (car (hash-ref rename-dict expr)) ; variable (extract uncanonical name)
           (list expr))] ; constant function
      [`(Explanation ,body ...) `(Explanation ,@(map (lambda (e) (loop e type)) body))]
      [(list 'Rewrite=> rule expr) (list 'Rewrite=> rule (loop expr type))]
      [(list 'Rewrite<= rule expr) (list 'Rewrite<= rule (loop expr type))]
      [(list 'if cond ift iff)
       (if (representation? type)
          (list 'if (loop cond (get-representation 'bool)) (loop ift type) (loop iff type))
          (list 'if (loop cond 'bool) (loop ift type) (loop iff type)))]
      [(list (? impl-exists? impl) args ...) (cons impl (map loop args (impl-info impl 'itype)))]
      [(list op args ...) (cons op (map loop args (operator-info op 'itype)))])))

;; Parses a string from egg into a list of S-exprs.
(define (egg-exprs->exprs s egraph-data type)
  (define egg->herbie (egraph-data-egg->herbie-dict egraph-data))
  (for/list ([egg-expr (in-port read (open-input-string s))])
    (egg-parsed->expr (flatten-let egg-expr) egg->herbie type)))

;; Parses a string from egg into a single S-expr.
(define (egg-expr->expr s egraph-data type)
  (first (egg-exprs->exprs s egraph-data type)))

(module+ test
  (define repr (get-representation 'binary64))
  (*context* (make-debug-context '()))
  (*context* (context-extend (*context*) 'x repr))
  (*context* (context-extend (*context*) 'y repr))
  (*context* (context-extend (*context*) 'z repr))

  (define test-exprs
    (list (cons '(+.f64 y x)
                (~a '(+.f64 $h0 $h1)))
          (cons '(+.f64 x y)
                (~a '(+.f64 $h1 $h0)))
          (cons '(-.f64 #s(literal 2 binary64) (+.f64 x y))
                (~a '(-.f64 2 (+.f64 $h1 $h0))))
          (cons '(-.f64 z (+.f64 (+.f64 y #s(literal 2 binary64)) x))
                (~a '(-.f64 $h2 (+.f64 (+.f64 $h0 2) $h1))))
          (cons '(*.f64 x y)
                (~a '(*.f64 $h1 $h0)))
          (cons '(+.f64 (*.f64 x y) #s(literal 2 binary64))
                (~a '(+.f64 (*.f64 $h1 $h0) 2)))
          (cons '(cos.f32 (PI.f32))
                (~a '(cos.f32 (PI.f32))))
          (cons '(if (TRUE) x y)
                (~a '(if (TRUE) $h1 $h0)))))

  (let ([egg-graph (make-egraph)])
    (for ([(in expected-out) (in-dict test-exprs)])
      (define out (~a (expr->egg-expr in egg-graph (*context*))))
      (define computed-in (egg-expr->expr out egg-graph (context-repr (*context*))))
      (check-equal? out expected-out)
      (check-equal? computed-in in)))

  (*context* (make-debug-context '(x a b c r)))
  (define extended-expr-list
    (list
     '(/ (- (exp x) (exp (- x))) 2)
     '(/ (+ (- b) (sqrt (- (* b b) (* (* 3 a) c)))) (* 3 a))
     '(/ (+ (- b) (sqrt (- (* b b) (* (* 3 a) c)))) (* 3 a)) ;; duplicated to make sure it would still work
     '(* r 30)
     '(* 23/54 r)
     '(+ 3/2 1.4)))

  (let ([egg-graph (make-egraph)])
    (for ([expr extended-expr-list])
      (define expr* (spec->prog expr (*context*)))
      (define egg-expr (expr->egg-expr expr* egg-graph (*context*)))
      (check-equal? (egg-expr->expr (~a egg-expr) egg-graph (context-repr (*context*))) expr*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proofs
;;
;; Proofs from egg contain let expressions (not Scheme like) as
;; well as other information about rewrites; proof extraction requires
;; some flattening and translation

(define (remove-rewrites proof)
  (match proof
    [`(Rewrite=> ,_ ,something)
     (remove-rewrites something)]
    [`(Rewrite<= ,_ ,something)
     (remove-rewrites something)]
    [(list _ ...)
     (map remove-rewrites proof)]
    [_ proof]))

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

;; returns a flattened list of terms
;; The first term has no rewrite- the rest have exactly one rewrite
(define (expand-proof-term term budget)
  (let loop ([term term])
    (cond
      [(<= (unbox budget) 0)
       (list #f)]
      [else
       (match term
         [(? symbol?) (list term)]
         [(? literal?) (list term)]
         [(? number?) (list term)]
         [`(Explanation ,body ...) (expand-proof body budget)]
         [(? list?)
          (define children (map loop term))
          (cond 
            [(member (list #f) children)
             (list #f)]
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
  (check-equal?
   (sequential-product `((1 2) (3 4 5) (6)))
   `((1 3 6) (2 3 6) (2 4 6) (2 5 6)))

  (expand-proof-term '(Explanation (+ x y) (+ y x)) (box 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule expansion
;;
;; Expansive rules are the only problematic rules.
;; We only support expansive rules where the LHS is a spec.

;; Translates a Herbie rule into an egg rule
(define (rule->egg-rule ru)
  (struct-copy rule ru
     [input (expr->egg-pattern (rule-input ru))]
     [output (expr->egg-pattern (rule-output ru))]))

(define (rule->egg-rules ru)
  (define input (rule-input ru))
  (cond
    [(symbol? input)
     ; expansive rules
     (define itype (dict-ref (rule-itypes ru) input))
     (unless (type-name? itype)
       (error 'rule->egg-rules "expansive rules over impls is unsound ~a" input))
     (for/list ([op (all-operators)] #:when (eq? (operator-info op 'otype) itype))
       (define itypes (operator-info op 'itype))
       (define vars (map (lambda (_) (gensym)) itypes))
       (rule (sym-append (rule-name ru) '-expand- op)
             (cons op vars)
             (replace-expression (rule-output ru) input (cons op vars))
             (map cons vars itypes)
             (rule-otype ru)))]
    [else
     ; non-expansive rule
     (list (rule->egg-rule ru))]))

;; egg rule cache
(define-resetter *egg-rule-cache*
  (λ () (make-hash))
  (λ () (make-hash)))

;; Cache mapping name to its canonical rule name
;; See `*egg-rules*` for details
(define-resetter *canon-names*
  (λ () (make-hash))
  (λ () (make-hash)))

;; Tries to look up the canonical name of a rule using the cache.
;; Obviously dangerous if the cache is invalid.
(define (get-canon-rule-name name [failure #f])
  (hash-ref (*canon-names*) name failure))

;; Expand and convert the rules for egg.
;; Uses a cache to only expand each rule once.
(define (expand-rules rules)
  (reap [sow]
    (for ([rule (in-list rules)])
      (define egg&ffi-rules
        (hash-ref! (*egg-rule-cache*)
                    (cons (*active-platform*) rule)
                    (lambda ()
                      (for/list ([egg-rule (in-list (rule->egg-rules rule))])
                        (define name (rule-name egg-rule))
                        (hash-set! (*canon-names*) name (rule-name rule))
                        (cons egg-rule (make-ffi-rule egg-rule))))))
      (for-each sow egg&ffi-rules))))

;; Spec contains no accelerators
(define (spec-has-accelerator? spec)
  (match spec
    [(list (? accelerator?) _ ...) #t]
    [(list _ args ...) (ormap spec-has-accelerator? args)]
    [_ #f]))

(define (real-rules rules)
  (filter-not
    (lambda (rule)
      (or (representation? (rule-otype rule))
          (spec-has-accelerator? (rule-input rule))
          (spec-has-accelerator? (rule-output rule))))
    rules))

;; Rules from spec to impl
;; These are fixed for a a particular platform
(define-resetter *lowering-rules*
  (λ () (make-hash))
  (λ () (make-hash)))

(define (platform-lowering-rules)
  (define impls (platform-impls (*active-platform*)))
  (for/list ([impl (in-list impls)])
    (hash-ref! (*lowering-rules*)
               (cons impl (*active-platform*))
               (lambda ()
                 (define op (impl->operator impl))
                 (define itypes (operator-info op 'itype))
                 (define otype (operator-info op 'otype))
                 (cond
                   [(accelerator? op)
                    ; accelerator lowering
                    (define vars (accelerator-info op 'vars))
                    (rule (sym-append 'accelerator-lowering- impl)
                          (accelerator-info op 'body)
                          (cons impl (accelerator-info op 'vars))
                          (map cons vars itypes)
                          otype)]
                   [else
                    ; direct lowering
                    (define vars (map (lambda (_) (gensym)) itypes))
                    (rule (sym-append op '-lowering- impl)
                          (cons op vars)
                          (cons impl vars)
                          (map cons vars itypes)
                          otype)])))))

;; Computes the product of all possible representation assignments to types.
(define (type-combinations types reprs)
  (reap [sow]
    (let loop ([types types] [assigns '()])
      (match types
        [(? null?) (sow assigns)]
        [(list type rest ...)
         (for ([repr (in-list reprs)])
           (when (equal? (representation-type repr) type)
             (loop rest (cons (cons type repr) assigns))))]))))

;; Representation name sanitizer (also in <herbie>/platform.rkt)
(define (repr->symbol repr)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (define repr-name (representation-name repr))
  (string->symbol (string-replace* (~a repr-name) replace-table)))

;; Instantiates rules from implementation to implementation in the platform.
;; If a rule is over implementations, filters by supported implementations.
;; If a rule is over real operators, instantiates for every possible output type.
;; By default, expansive rules will be ignored (causes issues in egg)
(define (platform-impl-rules rules #:expansive? [expansive? #f])
  (define reprs (platform-reprs (*active-platform*)))
  (define impls (list->set (platform-impls (*active-platform*))))
  (reap [sow]
    (for ([ru (in-list rules)]
          #:when (or expansive?
                     (not (symbol? (rule-input ru)))))
      (match-define (rule name input output itypes otype) ru)
      (cond
        [(representation? otype) ; rule over representation
         (when (andmap
                 (curry set-member? impls)
                 (filter-not
                   (curry eq? 'if)
                   (append (ops-in-expr input) (ops-in-expr output))))
           (sow ru))]
        [else
         ; rule over types need to be instantiated for every representation
         ; some operator implementations may not exist
         (define types (remove-duplicates (cons otype (map cdr itypes))))
         (for ([tsubst (in-list (type-combinations types reprs))])
            ;; Strange corner case:
            ;; Rules containing comparators cause desugaring to misbehave.
            ;; The reported output type is bool but then desugaring
            ;; thinks there will be a cast somewhere
            (define otype* (dict-ref tsubst otype))
            (define sugar-otype
              (if (equal? otype 'bool)
                  (dict-ref tsubst 'real (get-representation 'bool))
                  otype*))

           (define itypes* (map (λ (p) (cons (car p) (dict-ref tsubst (cdr p)))) itypes))
           (define sugar-ctx (context (map car itypes) sugar-otype (map cdr itypes*)))

           ;; The easier way to tell if every operator is supported
           ;; in a given representation is to just try to desguar
           ;; the expression and catch any errors.
           (with-handlers ([exn:fail:user:herbie:missing? (const (void))])
             (define name* (sym-append name '_ (repr->symbol sugar-otype)))
             (define input* (spec->prog input sugar-ctx))
             (define output* (spec->prog output sugar-ctx))
             (when (andmap
                     (curry set-member? impls)
                     (filter-not
                       (curry eq? 'if)
                       (append (ops-in-expr input*) (ops-in-expr output*))))
               (sow (rule name* input* output* itypes* otype*)))))]))))

;; For backwards compatability in unit tests
(define (rule->impl-rules rule)
  (platform-impl-rules (list rule) #:expansive? #t))

(module+ test
  ; Check that all builtin rules are instantiated at least once
  ; in the default platform
  (for ([rule (in-list (*rules*))])
    (> (length (rule->impl-rules rule)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket egraph
;;
;; Racket representation of an egraph; just a hashcons data structure
;; We can think of this as a read-only copy of an egraph for things like
;; platform-aware extraction and possibly ground-truth evaluation.
;; This is regraph reborn!

;; - eclasses: vector of enodes
;; - canon: map from egraph id to vector index
;; - has-leaf?: map from vector index to if the eclass contains a leaf
;; - constants: map from vector index to if the eclass contains a constant
;; - egg->herbie: data to translate egg IR to herbie IR
(struct regraph (eclasses canon has-leaf? constants egg->herbie))

;; Constructs a Racket egraph from an S-expr representation.
(define (sexpr->regraph egraph egg->herbie)
  ; reserve the next eclass
  (define canon (make-hash))
  (define counter 0)
  ; lookup the canonical eclass or reserve the next new eclass
  (define (resolve-id id)
    (hash-ref! canon id
                (lambda ()
                  (define id* counter)
                  (hash-set! canon id id*)
                  (set! counter (add1 counter))
                  id*)))
  ; iterate through eclasses and fill data
  (define n (length egraph))
  (define eclasses (make-vector n '()))
  (define has-leaf? (make-vector n #f))
  (define constants (make-vector n #f))
  (for ([eclass (in-list egraph)])
    (match-define (cons egg-id egg-nodes) eclass)
    (define id (resolve-id egg-id))
    (define node
      (for/vector #:length (length egg-nodes)
                  ([egg-node (in-list egg-nodes)])
        (match egg-node
          [(? number?) ; number
           (vector-set! has-leaf? id #t)
           (vector-set! constants id egg-node)
           egg-node]
          [(? symbol?) ; variable or constant
           (vector-set! has-leaf? id #t)
           (if (hash-has-key? egg->herbie egg-node)
               egg-node             ; variable
               (list egg-node))]    ; constant
          [(list op child-ids ...)
           (when (null? child-ids)
             (vector-set! has-leaf? id #t))
           (cons op (map resolve-id child-ids))]
          [_ (error 'sexpr->regraph "malformed enode: ~a" egg-node)])))
    (vector-set! eclasses id node))

  ; collect with wrapper
  (regraph eclasses canon has-leaf? constants egg->herbie))

;; Constructs a Racket egraph from an S-expr representation of
;; an egraph and data to translate egg IR to herbie IR.
(define (make-regraph egraph-data)
  (define egg->herbie (egraph-data-egg->herbie-dict egraph-data))
  (define egraph-str (egraph-serialize egraph-data))
  (sexpr->regraph (read (open-input-string egraph-str)) egg->herbie))

;; Egraph node has children.
;; Nullary operators have no children!
(define (node-has-children? node)
  (and (pair? node) (pair? (cdr node))))

;; Computes the parent relation of each eclass.
(define (regraph-parents regraph)
  (define eclasses (regraph-eclasses regraph))
  (define n (vector-length eclasses))
  (define parents (make-vector n '()))

  ; updates `parents` based on the eclass `id`
  (define (build-parents! id)
    (define eclass (vector-ref eclasses id))
    (for ([node (in-vector eclass)])
      (when (node-has-children? node) ; only care about nodes with children
        (for ([child-id (in-list (cdr node))])
          (vector-set! parents
                       child-id
                       (cons id (vector-ref parents child-id)))))))

  ; compute parent-child relation
  (for ([id (in-range n)]) (build-parents! id))
  ; dedup the result
  (for/vector #:length n ([id (in-range n)])
    (list->vector (remove-duplicates (vector-ref parents id)))))

;; Computes an analysis for each eclass.
;; Takes a regraph and an procedure taking the analysis, an eclass, and
;; its eclass id producing a non-`#f` result when the parents of the eclass
;; need to be revisited. Result is a vector where each entry is
;; the eclass's analysis.
(define (regraph-analyze regraph eclass-proc #:analysis [analysis #f])
  (define eclasses (regraph-eclasses regraph))
  (define has-leaf? (regraph-has-leaf? regraph))
  (define parents (regraph-parents regraph))
  (define n (vector-length eclasses))

  ; set analysis if not provided
  (unless analysis (set! analysis (make-vector n #f)))
  (define dirty?-vec (vector-copy has-leaf?)) ; visit eclass on next pass?
  (define changed?-vec (make-vector n #f)) ; eclass was changed last iteration

  ; run the analysis
  (let sweep ([iter 0])
    (define dirty?-vec* (make-vector n #f))
    (define changed?-vec* (make-vector n #f))
    (for ([id (in-range n)] #:when (vector-ref dirty?-vec id))
      (define eclass (vector-ref eclasses id))
      (when (eclass-proc analysis changed?-vec iter eclass id)
        ; eclass analysis was updated: need to revisit the parents
        (vector-set! changed?-vec* id #t)
        (for ([parent-id (in-vector (vector-ref parents id))])
          (vector-set! dirty?-vec* parent-id #t))))
    ; check if we need to sweep again
    (cond
      [(for/or ([dirty? (in-vector dirty?-vec*)]) dirty?)
       ; analysis has not converged so loop
       (set! dirty?-vec dirty?-vec*) ; update eclasses that require visiting
       (set! changed?-vec changed?-vec*) ; update eclasses that have changed
       (sweep (add1 iter))]
      [else (void)]))
  
  ; Invariant: all eclasses have an associated cost!  
  (unless (for/and ([eclass-analysis (in-vector analysis)]) eclass-analysis)
    (error 'regraph-analyze
           "analysis not run on all eclasses: ~a ~a"
           eclass-proc
           (for/vector #:length n ([id (in-range n)])
             (define eclass (vector-ref eclasses id))
             (define eclass-analysis (vector-ref analysis id))
             (list id eclass eclass-analysis))))

  analysis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regraph untyped extraction
;;
;; Untyped extraction is ideal for extracting specifications from an egraph.
;; This style of extractor associates to each eclass a best (cost, node) pair.
;; The extractor procedure simply takes an eclass id.
;; 
;; Untyped cost functions take:
;;  - the regraph we are extracting from
;;  - a mutable cache (to possibly stash per-node data)
;;  - the node we are computing cost for
;;  - unary procedure to get the eclass of an id
;;

;; The untyped extraction algorithm.
(define ((untyped-egg-extractor cost-proc) regraph)
  (define eclasses (regraph-eclasses regraph))
  (define n (vector-length eclasses))

  ; costs: mapping id to (cost, best node)
  (define costs (make-vector n #f))

  ; Checks if eclass has a cost
  (define (eclass-has-cost? id)
    (vector-ref costs id))

  ; Unsafe lookup of eclass cost
  (define (unsafe-eclass-cost id)
    (car (vector-ref costs id)))

  ; Computes the current cost of a node if its children have a cost
  ; Cost function has access to a mutable value through `cache`
  (define cache (box #f))
  (define (node-cost node changed?-vec)
    (if (node-has-children? node)
        (let ([child-ids (cdr node)]) ; (op child ...)
          ; compute the cost if at least one child eclass has a new analysis
          ; ... and an analysis exists for all the child eclasses
          (and (ormap (lambda (id) (vector-ref changed?-vec id)) child-ids)
               (andmap eclass-has-cost? child-ids)
               (cost-proc regraph cache node unsafe-eclass-cost)))
        (cost-proc regraph cache node unsafe-eclass-cost)))

  ; Updates the cost of the current eclass.
  ; Returns #t if the cost of the current eclass has improved.
  (define (eclass-set-cost! _ changed?-vec iter eclass id)
    ; Optimization: we only need to update node cost as needed.
    ;  (i) terminals, nullary operators: only compute once
    ;  (ii) non-nullary operators: compute when any of its child eclasses
    ;       have their analysis updated
    (define (node-requires-update? node)
      (if (node-has-children? node)
          (for/or ([id (in-list (cdr node))])
            (vector-ref changed?-vec id))
          (= iter 0)))

    (define new-cost
      (for/fold ([best #f]) ([node (in-vector eclass)])
        (cond
          [(node-requires-update? node)
           (define cost (node-cost node changed?-vec))
           (match* (best cost)
             [(_ #f) best]
             [(#f _) (cons cost node)]
             [(_ _) #:when (< cost (car best)) (cons cost node)]
             [(_ _) best])]
          [else best])))

    (cond
      [new-cost
       (define prev-cost (vector-ref costs id))
       (cond
         [(or (not prev-cost) ; first time
              (< (car new-cost) (car prev-cost)))
          (vector-set! costs id new-cost)
          #t]
         [else #f])]
      [else #f]))

  ; run the analysis
  (set! costs
    (regraph-analyze regraph
                     eclass-set-cost!
                     #:analysis costs))

  ; reconstructs the best expression at a node
  (define (build-expr id)
    (match (cdr (vector-ref costs id))
      [(? number? n) n] ; number
      [(? symbol? s) s] ; variable
      [(list op ids ...) (cons op (map build-expr ids))]
      [e (error 'untyped-extraction-proc "unexpected node" e)]))

  ; the actual extraction procedure
  (lambda (id _)
    (define cost (car (vector-ref costs id)))
    (cons cost (build-expr id))))

;; Is fractional with odd denominator.
(define (fraction-with-odd-denominator? frac)
  (and (rational? frac)
       (let ([denom (denominator frac)])
         (and (> denom 1) (odd? denom)))))

;; The default per-node cost function
(define (default-untyped-egg-cost-proc regraph cache node rec)
  (define constants (regraph-constants regraph))
  (match node
    [(? number?) 1]
    [(? symbol?) 1]
    [(list 'pow _ b e) ; special case for fractional pow
     (define n (vector-ref constants e))
     (if (and n (fraction-with-odd-denominator? n))
         +inf.0
         (+ 1 (rec b) (rec e)))]
    [(list _ args ...) (apply + 1 (map rec args))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regraph typed extraction
;;
;; Typed extraction is ideal for extracting specifications from an egraph.
;; By "typed", we refer to the output "representation" of a given operator.
;; This style of extractor associates to each eclass the best
;; (cost, node) pair for each possible output type in the eclass.
;; The extractor procedure takes an eclass id and an output type.
;;
;; Typed cost functions take:
;;  - the regraph we are extracting from
;;  - a mutable cache (to possibly stash per-node data)
;;  - the node we are computing cost for
;;  - 3 argument procedure taking:
;;       - an eclass id
;;       - an output type
;;       - a default failure value
;;
;; Types are represented as one of the following:
;;  - #t: top type (type of numbers)
;;  - (or <type> ..+) union of types
;;  - <representation>: representation type
;;  - <type-name>: type-name type
;;  - #f: inconclusive (possible result of `if` type during analysis)

;; Is `ty2` in `ty1`?
;; Restricting `ty1` to be non-`#f` and not a union type.
(define (has-type? ty1 ty2)
  (match* (ty1 ty2)
    [(_ #f) #f]
    [(#t _) #t]
    [((list 'or tys ...) _) (set-member? tys ty2)]
    [(_ _) (equal? ty1 ty2)]))

;; Applying the union operation over types
(define (type/union ty0 . tys)
  (match tys
    [(? null?) ty0]
    [(list ty1 tys ...)
     (apply type/union
            (match* (ty0 ty1)
              [(#t _) #t]
              [(_ #t) #t]
              [(#f _) ty1]
              [(_ #f) ty0]
              [((list 'or tys1 ...) (list 'or tys2 ...))
               (cons 'or (set-intersect tys1 tys2))]
              [((list 'or tys1 ...) ty)
               (if (set-member? tys1 ty) (cons 'or tys1) (list* 'or ty tys1))]
              [(ty (list 'or tys2 ...))
               (if (set-member? tys2 ty) (cons 'or tys2) (list* 'or ty tys2))]
              [(_ _)
               (if (equal? ty0 ty1) ty0 (list 'or ty0 ty1))])
            tys)]))

;; Applying the intersection operation over types
(define (type/intersect ty1 ty2)
  (match* (ty1 ty2)
    [(_ #t) ty1]
    [(#t _) ty2]
    [((list 'or tys1 ...) (list 'or tys2 ...))
     (match (set-intersect tys1 tys2)
       [(? null?) #f]
       [(list ty) ty]
       [(list tys ...) (cons 'or tys)])]
    [((list 'or tys1 ...) _)
     (and (set-member? tys1 ty2) ty2)]
    [(_ (list 'or tys2 ...))
     (and (set-member? tys2 ty1) ty1)]
    [(_ _)
     (and (equal? ty1 ty2) ty1)]))

;; Computes the set of extractable types for each eclass.
(define (regraph-eclass-types egraph)
  (define egg->herbie (regraph-egg->herbie egraph))

  (define (node-type analysis node)
    (match node
      [(? number?) #t]
      [(? symbol?)
       (define repr (cdr (hash-ref egg->herbie node)))
       (list 'or repr (representation-type repr))]
      [(list 'if _ ift iff)
       (define ift-types (vector-ref analysis ift))
       (define iff-types (vector-ref analysis iff))
       (and ift-types iff-types (type/intersect ift-types iff-types))]
      [(list (? impl-exists? impl) ids ...)
       (and (andmap has-type?
                    (map (curry vector-ref analysis) ids) 
                    (impl-info impl 'itype))
            (impl-info impl 'otype))]
      [(list op ids ...)
       (and (andmap has-type?
                    (map (curry vector-ref analysis) ids) 
                    (operator-info op 'itype))
            (operator-info op 'otype))]))

  (define (eclass-set-type! analysis changed?-vec _ eclass id)
    (define ty (vector-ref analysis id))
    (define changed? #f)
    (cond
      [ty ; revisiting an eclass
       (define ty*
         (apply type/union ty
                (filter-map ; only nodes (with children) with an updated child analysis
                  (lambda (node)
                    (and (node-has-children? node)
                         (ormap (curry vector-ref changed?-vec) (cdr node))
                         (andmap (curry vector-ref analysis) (cdr node))
                         (node-type analysis node)))
                  (vector->list eclass))))
       (unless (equal? ty ty*)
         (vector-set! analysis id ty*)
         (set! changed? #t))]
      [else ; first visit to eclass
       (define ty*
         (apply type/union #f
                (filter-map ; only leaves or nodes whose children have analyses
                  (lambda (node)
                    (and (or (not (node-has-children? node))
                             (andmap (curry vector-ref analysis) (cdr node)))
                         (node-type analysis node)))
                  (vector->list eclass))))
       (when ty*
         (vector-set! analysis id ty*)
         (set! changed? #t))])

    changed?)

  (regraph-analyze egraph eclass-set-type!))

;; Type checks the egraph, returning a copy of the egraph that only
;; contains the well-typed nodes.
(define (regraph-type-check egraph)
  (define eclasses (regraph-eclasses egraph))
  (define egg->herbie (regraph-egg->herbie egraph))
  (define n (vector-length eclasses))

  ; Compute valid types for each eclass
  (define eclass-types (regraph-eclass-types egraph))

  ; The return type of a node
  (define (node-type node)
    (match node
      [(? number?) #t]
      [(? symbol?) (cdr (hash-ref egg->herbie node))]
      [(list 'if _ ift iff)
       (define ift-types (vector-ref eclass-types ift))
       (define iff-types (vector-ref eclass-types iff))
       (and ift-types iff-types (type/intersect ift-types iff-types))]
      [(list (? impl-exists? op) _ ...) (impl-info op 'otype)]
      [(list op _ ...) (operator-info op 'otype)]))

  ; Prune any nodes that are not well-typed
  (define eclasses*
    (for/vector #:length n ([id (in-range n)])
      (define eclass (vector-ref eclasses id))
      (define ty (vector-ref eclass-types id))
      (list->vector
        (filter
          (lambda (node) (has-type? ty (node-type node)))
          (vector->list eclass)))))
  
  (struct-copy regraph egraph [eclasses eclasses*]))

;; Computes the return type of every node.
;; Assumes type checking has already pruned invalid nodes.
;; The result is a vector of vectors (one for each node) of types.
(define (regraph-node-types egraph)
  (define eclasses (regraph-eclasses egraph))
  (define egg->herbie (regraph-egg->herbie egraph))
  (define n (vector-length eclasses))

  ; Compute valid types for each eclass
  (define eclass-types (regraph-eclass-types egraph))

  ; The return type of a node
  (define (node-type node)
    (match node
      [(? number?) #t]
      [(? symbol?)
       (define repr (cdr (hash-ref egg->herbie node)))
       (list 'or repr (representation-type repr))]
      [(list 'if _ ift iff)
       (define ift-types (vector-ref eclass-types ift))
       (define iff-types (vector-ref eclass-types iff))
       (and ift-types iff-types (type/intersect ift-types iff-types))]
      [(list (? impl-exists? op) _ ...) (impl-info op 'otype)]
      [(list op _ ...) (operator-info op 'otype)]))

  ; compute node types
  (for/vector #:length n ([id (in-range n)])
    (define eclass (vector-ref eclasses id))
    (for/vector #:length (vector-length eclass) ([node (in-vector eclass)])
      (node-type node))))

;; The typed extraction algorithm.
;; Extraction is partial, that is, the result of the extraction
;; procedure if `#f` if extraction finds no well-typed program
;; at a particular id with a particular output type.
(define ((typed-egg-extractor cost-proc) regraph)
  ; first: prune unwanted nodes that will make the analysis crash
  (set! regraph (regraph-type-check regraph))

  ; some important regraph fields
  (define eclasses (regraph-eclasses regraph))
  (define n (vector-length eclasses))

  ; costs: mapping eclass id to a table from type to (cost, node) pair
  (define costs (make-vector n #f))

  ; Checks if eclass has a cost
  (define (eclass-has-cost? id type)
    (define c (vector-ref costs id))
    (cond
      [(hash-has-key? c type) (hash-ref c type)] ; typed choice has cost
      [(hash-has-key? c #t) (hash-ref c #t)] ; untyped choice has cost (constants)
      [else #t])) ; no choice but we can compute cost with failure value

  ; Unsafe lookup of eclass cost
  (define (unsafe-eclass-cost id type failure)
    (define cost (vector-ref costs id))
    (cond
      [(hash-has-key? cost type) 
       (define cost* (hash-ref cost type)) ; (cost . node) or #f
       (and cost* (car cost*))]
      [(hash-has-key? cost #t) ; type `#t` is always a valid choice
       (define cost* (hash-ref cost #t))  ; (cost . node) or #f
       (and cost* (car cost*))]
      [else failure]))

  ; Unsafe lookup of best eclass node.
  ; Returns `#f` if no best eclass exists.
  (define (unsafe-best-node id type)
    (define cost (vector-ref costs id))
    (cond
      [(hash-has-key? cost type) 
       (define cost* (hash-ref cost type)) ; (cost . node) or #f
       (and cost* (cdr cost*))]
      [(hash-has-key? cost #t) ; type `#t` is always a valid choice
       (define cost* (hash-ref cost #t))  ; (cost . node) or #f
       (and cost* (cdr cost*))]
      [else #f]))

  ; Create tables for nodes based on node types.
  ; Need to be careful since `regraph-analyze` will think the analysis
  ; is done even if we exit prematurely
  (define eclass-types (regraph-node-types regraph))
  (for ([id (in-range n)] #:unless (vector-ref costs id))
    (define table (make-hash))
    (define node-types (vector-ref eclass-types id))
    (for ([type (in-vector node-types)])
     (match type
       [(list 'or tys ...)
        (for ([ty (in-list tys)])
          (hash-set! table ty #f))]
       [_ (hash-set! table type #f)]))
     (vector-set! costs id table))

  ; We cache whether it is safe to apply the cost function on a given node
  ; for a particular type; once `#t` we need not check the `cost` vector
  ; to know if it is safe.
  (define ready?-vec
    (for/vector #:length n ([id (in-range n)])
      (define eclass (vector-ref eclasses id))
      (define node-types (vector-ref eclass-types id))
      (for/vector #:length (vector-length eclass) ([ty (in-vector node-types)])
        (match ty
          [(list 'or tys ...) (map (lambda (_) (box #f)) tys)]
          [_ (box #f)]))))

  (define (slow-node-ready? node type)
    (match node
      [(list 'if cond ift iff)
       (and (eclass-has-cost? cond (get-representation 'bool))
            (eclass-has-cost? ift type)
            (eclass-has-cost? iff type))]
      [(list (? impl-exists? op) args ...)
       (andmap eclass-has-cost? args (impl-info op 'itype))]
      [(list op args ...)
       (andmap eclass-has-cost? args (operator-info op 'itype))]))

  ; Computes the current cost of a node if its children have a cost
  ; Cost function has access to a mutable value through `cache`
  (define cache (box #f))
  (define (node-cost node type ready?)
    (and (or (not (node-has-children? node))
             (unbox ready?)
             (let ([v (slow-node-ready? node type)])
               (set-box! ready? v)
               v))
         (cost-proc regraph cache node type unsafe-eclass-cost)))

  ; Updates the cost of the current eclass.
  ; Returns #t if the cost of the current eclass has improved.
  (define (eclass-set-cost! _ changed?-vec iter eclass id)
    (define node-types (vector-ref eclass-types id))
    (define eclass-costs (vector-ref costs id))
    (define ready?/node (vector-ref ready?-vec id))
    (define updated? #f)

    ; Update cost information
    (define (update-cost! type new-cost node)
      (when new-cost
        (define prev-cost/node (hash-ref eclass-costs type #f))
        (when (or (not prev-cost/node) ; first cost
                  (< new-cost (car prev-cost/node))) ; better cost
          (hash-set! eclass-costs type (cons new-cost node))
          (set! updated? #t))))

    ; Optimization: we only need to update node cost as needed.
    ;  (i) terminals, nullary operators: only compute once
    ;  (ii) non-nullary operators: compute when any of its child eclasses
    ;       have their analysis updated
    (define (node-requires-update? node)
      (if (node-has-children? node)
          (for/or ([id (in-list (cdr node))])
            (vector-ref changed?-vec id))
          (= iter 0)))

    ; Iterate over the nodes
    (for ([node (in-vector eclass)]
          [ty (in-vector node-types)]
          [ready? (in-vector ready?/node)])
      (match ty
        [(list 'or tys ...) ; node is a union type (only for some `if` nodes)
         (for ([ty (in-list tys)] [ready? (in-list ready?)])
           (when (node-requires-update? node)
             (define new-cost (node-cost node ty ready?))
             (update-cost! ty new-cost node)))]
        [#t ; node is untyped (constant)
         (when (node-requires-update? node)
           (define new-cost (node-cost node ty ready?))
           (for ([ty (in-list (hash-keys eclass-costs))])
             (update-cost! ty new-cost node)))]
        [_ ; node has a specific type
         (when (node-requires-update? node)
           (define new-cost (node-cost node ty ready?))
           (update-cost! ty new-cost node))]))

    updated?)

  ; run the analysis
  (set! costs
    (regraph-analyze regraph
                     eclass-set-cost!
                     #:analysis costs))

  ; invariant: all eclasses have a cost for all types
  (for ([cost (in-vector costs)])
    (unless (andmap identity (hash-values cost))
      (error 'typed-egg-extractor
             "costs not computed for all eclasses ~a"
             costs)))

  ; rebuilds the extracted procedure
  (define (build-expr id type)
    (let/ec return
      (let loop ([id id] [type type])
        (match (unsafe-best-node id type)
          [(? number? n) n] ; number
          [(? symbol? s) s] ; variable
          [(list 'if cond ift iff) ; if expression
           (list 'if
                 (loop cond (get-representation 'bool))
                 (loop ift type)
                 (loop iff type))]
          [(list (? impl-exists? impl) ids ...) ; expression of impls
           (cons impl (map loop ids (impl-info impl 'itype)))]
          [(list (? operator-exists? op) ids ...) ; expression of operators
           (cons op (map loop ids (operator-info op 'itype)))]
          [_ (return #f)]))))

  ; the actual extraction procedure
  (lambda (id type)
    (cons (unsafe-eclass-cost id type +inf.0)
          (build-expr id type))))

;; Per-node cost function according to the platform
;; `rec` takes an id, type, and failure value
(define (platform-egg-cost-proc regraph cache node type rec)
  (define egg->herbie (regraph-egg->herbie regraph))
  (match node
    [(? number?) 0] ; numbers are free I guess
    [(? symbol?)
     (define repr (cdr (hash-ref egg->herbie node)))
     (platform-repr-cost (*active-platform*) repr)]
    [(list 'if cond ift iff) ; if expression
     (+ (platform-impl-cost (*active-platform*) 'if)
        (rec cond (get-representation 'bool) +inf.0)
        (rec ift type +inf.0)
        (rec iff type +inf.0))]
    [(list (? impl-exists? impl) args ...) ; impls
     (define itypes (impl-info impl 'itype))
     (apply +
            (platform-impl-cost (*active-platform*) impl)
            (for/list ([arg (in-list args)] [itype (in-list itypes)])
              (rec arg itype +inf.0)))]
    [(list _ ...) +inf.0])) ; specs

;; Extracts the best expression according to the extractor.
;; Result is a single element list.
(define (regraph-extract-best regraph extract id type)
  ; extract expr (extraction is partial)
  (define id* (hash-ref (regraph-canon regraph) id))
  (match-define (cons _ egg-expr) (extract id* type))
  ; translate egg IR to Herbie IR
  (cond
    [egg-expr
     (define egg->herbie (regraph-egg->herbie regraph))
     (list (egg-parsed->expr (flatten-let egg-expr) egg->herbie type))]
    [else
     (list)]))

;; Extracts multiple expressions according to the extractor
(define (regraph-extract-variants regraph extract id type)
  ; extract expressions
  (define eclasses (regraph-eclasses regraph))
  (define id* (hash-ref (regraph-canon regraph) id))
  (define egg-exprs
    (reap [sow]
      (for ([enode (vector-ref eclasses id*)])
        (match enode
          [(? number?) (sow enode)]
          [(? symbol?) (sow enode)]
          [(list 'if cond ift iff)
           (match-define (cons _ cond*)
             (extract cond (if (representation? type)
                               (get-representation 'bool)
                               'bool)))
           (match-define (cons _ ift*) (extract ift type))
           (match-define (cons _ iff*) (extract iff type))
           (when (and cond* ift* iff*) ; guard against failed extraction
             (sow (list 'if cond* ift* iff*)))]
          [(list (? impl-exists? impl) ids ...)
           (when (equal? (impl-info impl 'otype) type)
             (define args
               (for/list ([id (in-list ids)] [itype (in-list (impl-info impl 'itype))])
                 (match-define (cons _ expr) (extract id itype))
                 expr))
             (when (andmap identity args) ; guard against failed extraction
               (sow (cons impl args))))]
          [(list (? operator-exists? op) ids ...)
           (when (equal? (operator-info op 'otype) type)
             (define args
               (for/list ([id (in-list ids)] [itype (in-list (operator-info op 'itype))])
                   (match-define (cons _ expr) (extract id itype))
                   expr))
             (when (andmap identity args) ; guard against failed extraction
               (sow (cons op args))))]))))
  ; translate egg IR to Herbie IR
  (define egg->herbie (regraph-egg->herbie regraph))
  (for/list ([egg-expr (in-list egg-exprs)])
    (egg-parsed->expr (flatten-let egg-expr) egg->herbie type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduler
;;
;; A mini-interpreter for egraph "schedules" including running egg,
;; pruning certain kinds of nodes, extracting expressions, etc.

(define (verify-schedule! schedule)
  (define (oops! fmt . args)
    (apply error 'verify-schedule! fmt args))
  (for ([instr (in-list schedule)])
    (match instr
      [(list 'run rules params)
       ;; `run` instruction
       (unless (and (list? rules) (andmap rule? rules))
         (oops! "expected list of rules: `~a`" rules))
       (for ([param (in-list params)])
         (match param
           [(cons 'node (? nonnegative-integer?)) (void)]
           [(cons 'iteration (? nonnegative-integer?)) (void)]
           [(cons 'const-fold? (? boolean?)) (void)]
           [(cons 'scheduler mode)
            (unless (set-member? '(simple backoff) mode)
              (oops! "in instruction `~a`, unknown scheduler `~a`" instr mode))]
           [_ (oops! "in instruction `~a`, unknown parameter `~a`" instr param)]))]
      [(list 'type-check) (void)]
      [(list 'convert) (void)]
      [_ (oops! "unknown instruction `~a`" instr)])))

;; Runs rules over the egraph with the given egg parameters.
;; Invariant: the returned egraph is never unsound
(define (egraph-run-rules egg-graph0 egg-rules params)  
  (define node-limit (dict-ref params 'node #f))
  (define iter-limit (dict-ref params 'iteration #f))
  (define scheduler (dict-ref params 'scheduler 'backoff))
  (define const-folding? (dict-ref params 'const-fold? #t))

  ;; run the rules
  (let loop ([iter-limit iter-limit])
    (define ffi-rules (map cdr egg-rules))
    (define egg-graph (egraph-copy egg-graph0))
    (define iteration-data
      (egraph-run egg-graph
                  ffi-rules
                  node-limit
                  iter-limit
                  scheduler
                  const-folding?))

    (timeline-push! 'stop (egraph-stop-reason egg-graph) 1)
    (cond
      [(egraph-is-unsound-detected egg-graph)
       ; unsoundness means run again with less iterations
       (define num-iters (length iteration-data))
       (if (<= num-iters 1) ; nothing to fall back on
           (values egg-graph0 (list))
           (loop (sub1 num-iters)))]
      [else
       (values egg-graph iteration-data)])))

(define (egraph-run-schedule exprs schedule ctx)
  (define (oops! command why)
    (error 'egraph-run-schedule "~a: ~a" command why))

  ; prepare the egraph
  (define egg-graph0 (make-egraph))
  (define node-ids
    (for/list ([expr (in-list exprs)])
      (egraph-add-expr egg-graph0 expr ctx)))
  
  ; run the schedule
  (define rule-apps (make-hash))
  (define-values (egg-graph regraph)
    (for/fold ([egg-graph egg-graph0] [regraph #f]) ([instr (in-list schedule)])
      (match instr
        [(list 'run rules params)
         ;; `run` instruction
         (when regraph
           (oops! 'run "requires a Rust egraph (was this run after a `convert` instruction?)"))

         ; run rules in the egraph
         (define egg-rules (expand-rules rules))
         (define-values (egg-graph* iteration-data) (egraph-run-rules egg-graph egg-rules params))
 
         ; get cost statistics
         (for/fold ([time 0]) ([iter (in-list iteration-data)] [i (in-naturals)])
           (define cnt (iteration-data-num-nodes iter))
           (define cost (apply + (map (λ (node-id) (egraph-get-cost egg-graph* node-id i)) node-ids)))
           (define new-time (+ time (iteration-data-time iter)))
           (timeline-push! 'egraph i cnt cost new-time)
           new-time)
       
         ;; get rule statistics
         (for ([(egg-rule ffi-rule) (in-dict egg-rules)])
           (define count (egraph-get-times-applied egg-graph* ffi-rule))
           (define canon-name (hash-ref (*canon-names*) (rule-name egg-rule)))
           (hash-update! rule-apps canon-name (curry + count) count))

         (values egg-graph* regraph)]
        [(list 'convert)
         ; `convert` instruction
         (when regraph
           (oops! 'convert "requires a Rust egraph (was this run after a `convert` instruction?)"))
         (values egg-graph (make-regraph egg-graph))]
        [(list 'type-check)
         ; `type-check` instruction
         (unless regraph
           (oops! 'type-check "can only execute after a `convert` instruction"))
         (values egg-graph (regraph-type-check regraph))]
        [_ (error 'egraph-run-schedule "unimplemented: ~a" instr)])))

  ; report rule statistics
  (for ([(name count) (in-hash rule-apps)])
    (when (> count 0)
      (timeline-push! 'rules (~a name) count)))
  ; root eclasses may have changed
  (define node-ids* 
    (for/list ([id (in-list node-ids)])
      (egraph-find egg-graph id)))
  ; return what we need
  (values node-ids* egg-graph (or regraph (make-regraph egg-graph))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;
;; Most calls to egg should be done through this interface.
;;  - `make-egg-query` creates a struct that describes a _reproducible_ egg instance
;;  - `run-egg` actually runs egg and extracts expressions and possibly proofs

;; Herbie's version of an egg runner
;; Defines parameters for running rewrite rules with egg
(struct egraph-query (exprs reprs schedule ctx extractor)
                     #:transparent ; for equality
                     #:methods gen:custom-write ; for abbreviated printing
                     [(define (write-proc alt port mode)
                        (fprintf port "#<egraph-query>"))])

;; Fallback extractor if none is specified
(define default-egg-extractor
  (untyped-egg-extractor default-untyped-egg-cost-proc))

;; Constructs an egg runner.
(define (make-egg-query exprs
                        reprs
                        schedule
                        #:context [ctx (*context*)]
                        #:extractor [extractor #f])
  (verify-schedule! schedule)
  (egraph-query exprs
                reprs
                schedule
                ctx
                (or extractor default-egg-extractor)))

;; Runs egg using an egg runner.
(define (run-egg input variants? #:proof-inputs [proof-inputs '()])
  ;; Run egg and extract iteration data
  (define ctx (egraph-query-ctx input))
  (define-values (node-ids egg-graph regraph)
    (egraph-run-schedule (egraph-query-exprs input)
                         (egraph-query-schedule input)
                         ctx))
  ;; Compute eclass/enode cost in the graph
  (define extractor (egraph-query-extractor input))
  (define extract-id (extractor regraph))
  (define reprs (egraph-query-reprs input))
  ;; Extract the expressions
  (define extract-proc
    (if variants?
        regraph-extract-variants
        regraph-extract-best))
  (define rewritten
    (for/list ([id node-ids] [repr (in-list reprs)])
      (extract-proc regraph extract-id id repr)))
  ;; Extract the proof based on a pair (start, end) expressions.
  (define proofs
    (for/list ([proof-input (in-list proof-inputs)])
      (match-define (cons start end) proof-input)
      (unless (egraph-is-equal egg-graph start end ctx)
        (error "Cannot get proof: start and end are not equal.\n start: ~a \n end: ~a" start end))

      (define proof (egraph-get-proof egg-graph start end ctx))
      (when (null? proof)
        (error (format "Failed to produce proof for ~a to ~a" start end)))
      proof))
  ;; Return extracted expressions and the proof
  (cons rewritten proofs))


