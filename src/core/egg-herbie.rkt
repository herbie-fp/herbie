#lang racket

(require egg-herbie
        (only-in ffi/unsafe
          malloc memcpy free cast ptr-set! ptr-add
          _byte _pointer _string/utf-8))

(require "../syntax/rules.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../common.rkt"
         "../errors.rkt"
         "../float.rkt"
         "../ground-truth.rkt"
         "../platform.rkt"
         "../points.rkt"
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
         rule->impl-rules
         get-canon-rule-name
         remove-rewrites)

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
  (make-FFIRule name lhs rhs))

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
;; Expressions use a slightly different encoding than in Racket.
;; The IR is similar to Herbie's various Racket IRs.
;;
;; <expr> ::= (<op> <sig> <expr> ...)
;;        ::= ($Var <prec> <ident>)
;;        ::= <number>
;; 
;; <sig> ::= ($Type <prec> <prec> ...)
;;         | real
;;
;; The main difference is the use of type signatures to differentiate
;; operator implementations for different representations.

;; Expands operators into `(op, sig)` so that we can
;; recover the exact operator implementation when extracting.
;; The ugly solution is to make `sig` = `($Type otype itypes ...)`.
(define (expand-operator op-or-impl)
  (cond
    [(impl-exists? op-or-impl)
     (define op (impl->operator op-or-impl))
     (define itypes (map representation-name (impl-info op-or-impl 'itype)))
     (define otype (representation-name (impl-info op-or-impl 'otype)))
     (values op (cons '$Type (cons otype itypes)))]
    [else
     (values op-or-impl 'real)]))

;; Translates a Herbie rule LHS or RHS into a pattern usable by egg
(define (expr->egg-pattern expr)
  (let loop ([expr expr])
    (match expr
      [(list 'if cond ift iff)
       (list 'if 'real (loop cond) (loop ift) (loop iff))]
      [(list (? repr-conv? op) arg)
       (list op 'real (loop arg))]
      [(list (? operator-exists? op) args ...)
       (list* op 'real (map loop args))]
      [(list (? impl-exists? op) args ...)
       (define-values (op* prec) (expand-operator op))
       (cons op* (cons prec (map loop args)))]
      [(? literal?) (literal-value expr)]
      [(? number?) expr] ; TODO: we should have gotten a literal here
      [_ (string->symbol (format "?~a" expr))])))

;; Translates a Herbie expression into an expression usable by egg
;; Returns the string representing the expression and updates
;; the translation dictionary
(define (expr->egg-expr expr egg-data ctx)
  (define egg->herbie-dict (egraph-data-egg->herbie-dict egg-data))
  (define herbie->egg-dict (egraph-data-herbie->egg-dict egg-data))
  (let loop ([expr expr])
    (match expr
      [(list 'if cond ift iff)
       (list 'if 'real (loop cond) (loop ift) (loop iff))]
      [(list (? repr-conv? op) arg)
       (list op 'real (loop arg))]
      [(list (? operator-exists? op) args ...)
       (cons op (cons 'real (map loop args)))]
      [(list op args ...)
       (define-values (op* prec) (expand-operator op))
       (cons op* (cons prec (map loop args)))]
      [(? literal?)
       (literal-value expr)]
      [(? number?) expr] ; TODO: literals
      [(? (curry hash-has-key? herbie->egg-dict))
       (define prec (list '$Type (representation-name (context-lookup ctx expr))))
       (define replacement (hash-ref herbie->egg-dict expr))
       (list '$Var prec replacement)]
      [_
       (define prec (list '$Type (representation-name (context-lookup ctx expr))))
       (define replacement (string->symbol (format "h~a" (hash-count herbie->egg-dict))))
       (hash-set! herbie->egg-dict expr replacement)
       (hash-set! egg->herbie-dict replacement expr)
       (list '$Var prec replacement)])))

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
(define (egg-parsed->expr expr rename-dict prec)
  (let loop ([expr expr] [prec prec])
    (match expr
      [`(Explanation ,body ...)
       `(Explanation ,@(map (curryr loop prec) body))]
      [(list 'Rewrite=> rule expr)
       (list 'Rewrite=> rule (loop expr prec))]
      [(list 'Rewrite<= rule expr)
       (list 'Rewrite<= rule (loop expr prec))]
      [(list 'if 'real cond ift iff)
       (list 'if (loop cond 'body) (loop ift prec) (loop iff prec))]
      [(list '$Var _ name)
       (hash-ref rename-dict name)]
      [(list (? repr-conv? op) 'real arg)
       (list op (loop arg prec))] ; ???
      [(list op 'real args ...) ; TODO what the heack
       (cons op (map (curryr loop 'real) args))]
      [(list op prec)
       (match-define (list '$Type otype) prec)
       (list (get-parametric-constant op (get-representation otype)))]
      [(list op prec args ...)
       (match-define (list '$Type _ itypes ...) prec)
       (define op* (apply get-parametric-operator op (map get-representation itypes)))
       (cons op* (map loop args itypes))]
      [(? number?)
       (literal expr prec)])))

;; Parses a string from egg into a list of S-exprs.
(define (egg-exprs->exprs s egraph-data repr)
  (define egg->herbie (egraph-data-egg->herbie-dict egraph-data))
  (for/list ([egg-expr (in-port read (open-input-string s))])
    (egg-parsed->expr (flatten-let egg-expr) egg->herbie (representation-name repr))))

;; Parses a string from egg into a single S-expr.
(define (egg-expr->expr s egraph-data repr)
  (first (egg-exprs->exprs s egraph-data repr)))

(module+ test
  (define repr (get-representation 'binary64))
  (*context* (make-debug-context '()))
  (*context* (context-extend (*context*) 'x repr))
  (*context* (context-extend (*context*) 'y repr))
  (*context* (context-extend (*context*) 'z repr))

  (define test-exprs
    (list (cons '(+.f64 y x)
                (~a `(+ ($Type binary64 binary64 binary64)
                        ($Var ($Type binary64) h0)
                        ($Var ($Type binary64) h1))))
          (cons '(+.f64 x y)
                (~a `(+ ($Type binary64 binary64 binary64)
                        ($Var ($Type binary64) h1)
                        ($Var ($Type binary64) h0))))
          (cons '(-.f64 #s(literal 2 binary64) (+.f64 x y))
                (~a '(- ($Type binary64 binary64 binary64) 2
                        (+ ($Type binary64 binary64 binary64)
                           ($Var ($Type binary64) h1)
                           ($Var ($Type binary64) h0)))))
          (cons '(-.f64 z (+.f64 (+.f64 y #s(literal 2 binary64)) x))
                (~a '(- ($Type binary64 binary64 binary64)
                        ($Var ($Type binary64) h2)
                        (+ ($Type binary64 binary64 binary64)
                           (+ ($Type binary64 binary64 binary64)
                              ($Var ($Type binary64) h0)
                              2)
                           ($Var ($Type binary64) h1)))))
          (cons '(*.f64 x y)
                (~a `(* ($Type binary64 binary64 binary64)
                        ($Var ($Type binary64) h1)
                        ($Var ($Type binary64) h0))))
          (cons '(+.f64 (*.f64 x y) #s(literal 2 binary64))
                (~a '(+ ($Type binary64 binary64 binary64)
                        (* ($Type binary64 binary64 binary64)
                           ($Var ($Type binary64) h1)
                           ($Var ($Type binary64) h0))
                        2)))
          (cons '(cos.f32 (PI.f32))
                (~a '(cos ($Type binary32 binary32)
                          (PI ($Type binary32)))))
          (cons '(if (TRUE) x y)
                (~a '(if real
                         (TRUE ($Type bool))
                         ($Var ($Type binary64) h1)
                         ($Var ($Type binary64) h0))))))

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
         [`(Explanation ,body ...)
          (expand-proof body budget)]
         [(? symbol?)
          (list term)]
         [(? literal?)
          (list term)]
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

(define (rule->impl-rules rule)
  (error 'rule->impl-rules "unimplemented"))

;; Cache mapping name to its canonical rule name
;; See `*egg-rules*` for details
(define-resetter *canon-names*
  (λ () (make-hash))
  (λ () (make-hash)))

;; Tries to look up the canonical name of a rule using the cache.
;; Obviously dangerous if the cache is invalid.
(define (get-canon-rule-name name [failure #f])
  (hash-ref (*canon-names*) name failure))

;; expand the rules first due to some bad but currently necessary reasons
;; (see `rule->egg-rules` for details); checks the cache in case we used
; them previously
(define (expand-rules rules)
  (for/fold ([rules* '()] #:result (reverse rules*))
            ([rule (in-list rules)])
   (define orig-name (rule-name rule))
   (define egg&ffi
     (for/list ([egg-rule (in-list (rule->egg-rules rule))])
       (define name (rule-name egg-rule))
       (hash-set! (*canon-names*) name orig-name)
       (cons egg-rule (make-ffi-rule egg-rule))))
    (append (reverse egg&ffi) rules*)))

(module+ test
  ;; Make sure all built-in rules are valid in
  ;; some configuration of representations
  (for ([rule (in-list (*rules*))])
    (test-case (~a (rule-name rule))
               (check-true (> (length (rule->egg-rules rule)) 0))))
)

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
;; - types: map from vector index to a egg IR type or #f
;; - egg->herbie: data to translate egg IR to herbie IR
(struct regraph (eclasses canon has-leaf? constants types egg->herbie))

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
  (define type? (make-vector n #f))
  (for ([eclass egraph])
    (match-define (list egg-id egg-nodes ...) eclass)
    (define id (resolve-id egg-id))
    (define node
      (for/vector #:length (length egg-nodes)
                  ([egg-node (in-list egg-nodes)])
        (match egg-node
          [(? number?) ; number
           (vector-set! has-leaf? id #t)
           (vector-set! constants id egg-node)
           egg-node]
          [(? symbol?) ; egg IR typename or variable ID
           (vector-set! type? id 'typename)
           (vector-set! has-leaf? id #t)
           egg-node]
          [(list op child-ids ...)
           (define egg-node* (cons op (map resolve-id child-ids)))
           (match op
             ['$Type (vector-set! type? id 'type)] ; egg IR type signature
             ['$Var (vector-set! has-leaf? id #t)] ; variable
             [_ (void)])
           egg-node*]
          [_ (error 'sexpr->regraph "malformed enode: ~a" egg-node)])))
    (vector-set! eclasses id node))
  ; extract egg IR typenames or #f
  (define (extract-egg-typename id)
    (define eclass (vector-ref eclasses id))
    (for/or ([node (in-vector eclass)])
      (and (symbol? node) node)))
  ; extract egg IR type signatures or #f
  (define (extract-egg-type id)
    (define eclass (vector-ref eclasses id))
    (for/or ([node (in-vector eclass)])
      (match node
        [(list '$Type ids ...) (cons '$Type (map extract-egg-typename ids))]
        [_ #f])))
  ; extract egg IR types
  (define types (make-vector n #f))
  (for ([id (in-range n)])
    (match (vector-ref type? id)
      [#f (void)]
      ['typename (vector-set! types id (extract-egg-typename id))]
      ['type (vector-set! types id (extract-egg-type id))]
      [type (error 'sexpr->regraph "unexpected type eclass type ~a" type)]))
  ; collect with wrapper
  (regraph eclasses canon has-leaf? constants types egg->herbie))

;; Constructs a Racket egraph from an S-expr representation of
;; an egraph and data to translate egg IR to herbie IR.
(define (make-regraph egraph-data)
  (define egg->herbie (egraph-data-egg->herbie-dict egraph-data))
  (define egraph-str (egraph-serialize egraph-data))
  (sexpr->regraph (read (open-input-string egraph-str)) egg->herbie))

;; Computes the parent relation of each eclass.
(define (regraph-parents regraph)
  (define eclasses (regraph-eclasses regraph))
  (define n (vector-length eclasses))
  (define parents (make-vector n '()))

  ; updates `parents` based on the eclass `id`
  (define (build-parents! id)
    (define eclass (vector-ref eclasses id))
    (for ([node (in-vector eclass)])
      (when (pair? node) ; only care about nodes with children
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
(define (regraph-analyze regraph eclass-proc
                         #:analysis [analysis #f]
                         #:dirty?-vec [dirty?-vec #f])
  (define eclasses (regraph-eclasses regraph))
  (define has-leaf? (regraph-has-leaf? regraph))
  (define parents (regraph-parents regraph))
  (define n (vector-length eclasses))
  ; set analysis if not provided
  (unless analysis (set! analysis (make-vector n #f)))
  (unless dirty?-vec (set! dirty?-vec (vector-copy has-leaf?)))
  ; run the analysis
  (let sweep ()
    (for ([id (in-range n)] #:when (vector-ref dirty?-vec id))
      (vector-set! dirty?-vec id #f)
      (define eclass (vector-ref eclasses id))
      (when (eclass-proc analysis eclass id)
        ; need to revisit the parents
        (for ([parent-id (in-vector (vector-ref parents id))])
            (vector-set! dirty?-vec parent-id #t))))
    ; check if we need to sweep again
    (when (for/or ([dirty? (in-vector dirty?-vec)]) dirty?)
      (sweep)))
  ; Invariant: all eclasses have an associated cost!  
  (unless (for/and ([eclass-analysis (in-vector analysis)]) eclass-analysis)
    (error 'regraph-analyze
           "analysis not run on all eclasses: ~a ~a"
           eclass-proc analysis))

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

(define (cost-merger best cost node)
    (cond [(not cost) best]
          [(not best) (cons cost node)]
          [(< cost (car best)) (cons cost node)]
          [else best]))

;; The untyped extraction algorithm.
(define ((untyped-egg-extractor cost-proc) regraph)
  (define eclasses (regraph-eclasses regraph))
  (define has-leaf? (regraph-has-leaf? regraph))
  (define types (regraph-types regraph))
  (define n (vector-length eclasses))

  ; costs: mapping id to (cost, best node)
  (define costs (make-vector n #f))

  ; Checks if eclass has a cost
  (define (eclass-has-cost? id)
    (vector-ref costs id))

  ; Unsafe lookup of eclass cost
  (define (unsafe-eclass-cost id)
    (car (vector-ref costs id)))

  ; Precompute the cost for some eclasses and mark certain dirty eclasses
  ; Setting cost of all type eclasses to 0.
  (define dirty?-vec (vector-copy has-leaf?))
  (define parents (regraph-parents regraph))
  (for ([id (in-range n)])
    (define type (vector-ref types id))
    (when type ; any type eclass has cost 0
      (vector-set! costs id (cons 0 type))
      (vector-set! dirty?-vec id #f)
      (for ([parent (in-vector (vector-ref parents id))])
        (unless (vector-ref types parent) ; set non-type parents as dirty
          (vector-set! dirty?-vec parent #t)))))

  ; Computes the current cost of a node if its children have a cost
  ; Cost function has access to a mutable value through `cache`
  (define cache (box #f))
  (define (node-cost node)
    (if (pair? node)
        (let ([child-ids (cdr node)]) ; (op child ...)
          (and (andmap eclass-has-cost? child-ids)
               (cost-proc regraph cache node unsafe-eclass-cost)))
        (cost-proc regraph cache node unsafe-eclass-cost)))

  ; Updates the cost of the current eclass.
  ; Returns #t if the cost of the current eclass has improved.
  (define (eclass-set-cost! _ eclass id)
    (define prev-cost (vector-ref costs id))
    (define new-cost
      (for/fold ([best #f]) ([node (in-vector eclass)])
        (define cost (node-cost node))
        (cost-merger best cost node)))
    (cond
      [(not new-cost) #f]
      [(or (not prev-cost) (< (car new-cost) (car prev-cost)))
       (vector-set! costs id new-cost)
       #t]
      [else #f]))

  ; run the analysis
  (set! costs
    (regraph-analyze regraph
                     eclass-set-cost!
                     #:analysis costs
                     #:dirty?-vec dirty?-vec))

  ; reconstructs the best expression at a node
  (define (build-expr id)
    (match (cdr (vector-ref costs id))
      [(? number? n) n] ; number
      [(? symbol? s) s] ; typename or variable name
      [(cons '$Type types) (cons '$Type types)] ; type
      [(cons op ids) (cons op (map build-expr ids))] ; operator
      [e (error 'default-extraction-proc "could not reconstruct ~a" e)]))
  
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
    [(list '$Var _ _) 1] ; variable
    [(list 'pow _ b e) ; special case for fractional pow
     (define n (vector-ref constants e))
     (if (and n (fraction-with-odd-denominator? n))
         +inf.0
         (+ 1 (rec b) (rec e)))]
    [(list _ _ args ...) (apply + 1 (map rec args))]
    [_ 1]))

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

;; Computes the type signature of every eclass.
;; The result is a vector of vectors (one for each node) of the form
;;  - <name> : constant or variable type
;;  - (<name> <name> ...) : a function type
;; where <name> is one of:
;;  - #t: type of numbers
;;  - (or <name> ..+): union of types
;;  - 'real: real type
;;  - 'type: type of egg IR type nodes
;;  - <id>: representation
(define (regraph-node-types regraph)
  (define types (regraph-types regraph))

  (define (eclass-set-types! analysis eclass id)
    (cond
      [(vector-ref analysis id) #f]
      [(vector-ref types id)
       (vector-set! analysis id (make-vector (vector-length eclass) 'type))
       #t]
      [else
       (define node-types
         (for/list ([node (in-vector eclass)])
           (match node
             [(? number?) #t]
             [(list '$Var ty-id _)
              (match (vector-ref types ty-id)
                ['real 'real]
                [(list '$Type type) type])]
             [(list 'if _ _ ift iff)
              (define ift-types (vector-ref analysis ift))
              (define iff-types (vector-ref analysis iff))
              (error 'regraph-node-types "unimplemented: ~a" node)]
             [(list _ ty-id args ...)
              (match (vector-ref types ty-id)
                ['real (cons 'real (make-list (length args) 'real))]
                [(list '$Type otype itypes ...) (cons otype itypes)])])))
      (cond
        [(andmap identity node-types)
         (vector-set! analysis id (list->vector node-types))
         #t]
        [else #f])]))   

  (regraph-analyze regraph eclass-set-types!))

;; Prunes enodes that do not type check.
;; The rules:
;;  - an eclass with a variable can only have the same type as that variable.
(define (regraph-prune-types egraph)
  (define eclasses (regraph-eclasses egraph))
  (define types (regraph-types egraph))
  (define n (vector-length eclasses))

  ;; invariant: every eclass has at most one variable
  (when (for/or ([eclass (in-vector eclasses)])
          (> (for/count ([node (in-vector eclass)])
               (match node
                 [(list '$Var _ _) #t]
                 [_ #f]))
             1))
    (error 'regraph-prune-types "invariant violated"))

  ;; compute node types
  (define eclass-types (regraph-node-types egraph))

  ;; prune nodes  
  (define eclasses*
    (for/vector #:length n ([id (in-range n)])
      ; compute variable type (if it exists)
      (define eclass (vector-ref eclasses id))
      (define node-types (vector-ref eclass-types id))
      (define ty
        (for/or ([node (in-vector eclass)])
          (match node
            [(list '$Var ty-id _)
             (match-define (list '$Type ty) (vector-ref types ty-id))
             ty]
            [_ #f])))
      ; eliminate nodes not matching the type
      (if ty
          (list->vector
            (for/fold ([nodes* '()] #:result (reverse nodes*))
                      ([node (in-vector eclass)] [type (in-vector node-types)])
              (cond
                [(eq? type #t) (cons node nodes*)]
                [(eq? type ty) (cons node nodes*)]
                [(and (pair? type) (eq? (car type) ty)) (cons node nodes*)]
                [else nodes*])))
          eclass)))

  (when (for/or ([eclass (in-vector eclasses*)]) (vector-empty? eclass))
    (error 'regraph-prune-specs "invariant violated: eclass contains no enodes"))
  ; the pruned regraph
  (struct-copy regraph egraph [eclasses eclasses*]))


;; The typed extraction algorithm.
(define ((typed-egg-extractor cost-proc) regraph)
  ; first: prune unwanted nodes that will make the analysis crash
  (set! regraph (regraph-prune-types regraph))

  ; some important regraph fields
  (define eclasses (regraph-eclasses regraph))
  (define has-leaf? (regraph-has-leaf? regraph))
  (define types (regraph-types regraph))
  (define n (vector-length eclasses))

  ; costs: mapping eclass id to either:
  ; - a map from type to (cost, node) pair
  ; - (cost, node) pair 
  (define costs (make-vector n #f))

  ; Checks if eclass has a cost
  (define (eclass-has-cost? id type)
    (define c (vector-ref costs id))
    (cond [(pair? c) #t]
          [(hash-has-key? c type) (hash-ref c type)]
          [(hash-has-key? c #t) (hash-ref c #t)]
          [else #t])) ; there is no choice for the eclass for the type

  ; Unsafe lookup of eclass cost
  (define (unsafe-eclass-cost id type failure)
    (define cost (vector-ref costs id))
    (cond [(pair? cost) (car cost)] ; (cost . node)
          [(hash-has-key? cost type) 
           (define cost* (hash-ref cost type)) ; (cost . node) or #f
           (and cost* (car cost*))]
          [(hash-has-key? cost #t) ; type `#t` is always a valid choice
           (define cost* (hash-ref cost #t))  ; (cost . node) or #f
           (and cost* (car cost*))]
          [else failure]))

  ; Unsafe lookup of best eclass node
  (define (unsafe-best-node id type)
    (define cost (vector-ref costs id))
    (cond
      [(pair? cost) (cdr cost)] ; (cost . node)
      [(hash-has-key? cost type) 
       (define cost* (hash-ref cost type)) ; (cost . node) or #f
       (and cost* (cdr cost*))]
      [(hash-has-key? cost #t) ; type `#t` is always a valid choice
       (define cost* (hash-ref cost #t))  ; (cost . node) or #f
       (and cost* (cdr cost*))]
      [else (error 'unsafe-best-cost "unclear what to extract: ~a" id)]))

  ; Dirty vector
  (define dirty?-vec (vector-copy has-leaf?))
  (define parents (regraph-parents regraph))

  ; Precompute costs of type eclasses (all have cost 0)
  (for ([id (in-range n)])
    (define type (vector-ref types id))
    (when type ; any type eclass has cost 0
      (vector-set! costs id (cons 0 type))
      (vector-set! dirty?-vec id #f)
      (for ([parent (in-vector (vector-ref parents id))])
        (unless (vector-ref types parent) ; set non-type parents as dirty
          (vector-set! dirty?-vec parent #t)))))

  ; Create tables for all other nodes
  ; Need to be careful since `regraph-analyze` will think the analysis
  ; is done even if we exit prematurely
  (define eclass-types (regraph-node-types regraph))
  (for ([id (in-range n)] #:unless (vector-ref costs id))
    (define table (make-hash))
    (define node-types (vector-ref eclass-types id))
    (for ([type (in-vector node-types)])
      (cond [(eq? type #t) (hash-set! table type #f)]
            [(list? type) (hash-set! table (car type) #f)]
            [else (hash-set! table type #f)]))
     (vector-set! costs id table))

  ; Computes the current cost of a node if its children have a cost
  ; Cost function has access to a mutable value through `cache`
  (define cache (box #f))
  (define (node-cost node type)
    (and (match node
           [(? number?) #t]
           [(list '$Var _ _) #t]
           [(list 'if _ cond ift iff)
            (error 'node-cost "unimplemented `~a`" node)]
           [(list _ _ args ...)
            (match-define (list _ itypes ...) type)
            (andmap eclass-has-cost? args itypes)])
         (cost-proc regraph cache node unsafe-eclass-cost)))

  ; Updates the cost of the current eclass.
  ; Returns #t if the cost of the current eclass has improved.
  (define (eclass-set-cost! _ eclass id)
    ; compute type and untyped cost=
    (define node-types (vector-ref eclass-types id))
    (define prev-costs (vector-ref costs id))
    (define new-costs (hash-copy prev-costs))
    ; iterate over the nodes
    (for ([node (in-vector eclass)] [type (in-vector node-types)])
      (define new-cost (node-cost node type))
      (cond
        [(not new-cost) (void)]
        [(eq? type #t) ; node is untyped (constant) => merge with all types
         (for ([(type prev) (in-hash new-costs)])
           (hash-set! new-costs type
                      (cost-merger prev new-cost node)))
         (hash-update! new-costs
                       #t
                       (lambda (prev) (cost-merger prev new-cost node))
                       #f)]
        [(pair? type) ; node is an operator => merge with output type
         (hash-update! new-costs
                       (car type)
                       (lambda (prev) (cost-merger prev new-cost node)))]
        [else ; node is a variable => merge with type
         (hash-update! new-costs
                       type
                       (lambda (prev) (cost-merger prev new-cost node)))]))
    ; merge the new version
    (define updated?
      (for/or ([type (in-list (hash-keys prev-costs))])
        (define prev-cost (hash-ref prev-costs type))
        (define new-cost (hash-ref new-costs type))
        (cond [(not new-cost) #f]
              [(not prev-cost) #t]
              [(< (car new-cost) (car prev-cost)) #t]
              [else #f])))
    ; updated costs
    (vector-set! costs id new-costs)
    updated?)

  ; run the analysis
  (set! costs
    (regraph-analyze regraph
                     eclass-set-cost!
                     #:analysis costs
                     #:dirty?-vec dirty?-vec))

  ; invariant: all eclasses have a cost for all types
  (unless (for/and ([cost (in-vector costs)])
            (cond [(pair? cost) #t]
                  [(hash? cost) (andmap identity (hash-values cost))]
                  [else #f]))
    (error 'typed-egg-extractor "costs not computed for all eclasses ~a" costs))

  ; rebuilds the extracted procedure
  (define (build-expr id type)
    (match (unsafe-best-node id type)
      [(? number? n) n] ; number
      [(? symbol? s) s] ; typename or variable name
      [(list '$Var ty-id name-id)
       (define ty (build-expr ty-id #f))
       (define name (build-expr name-id #f))
       (list '$Var ty name)]
      [(list '$Type types ...) (cons '$Type types)] ; type
      [(list op ty-id ids ...)
       (define ty (build-expr ty-id type))
       (match-define (list '$Type _ itypes ...) ty)
       (list* op ty (map build-expr ids itypes))] ; operator
      [e (error 'default-extraction-proc "could not reconstruct ~a" e)]))

  ; the actual extraction procedure
  (lambda (id type)
    (cons (unsafe-eclass-cost id type +inf.0)
          (build-expr id type))))

;; Per-node cost function according to the platform
;; `rec` takes an id, type, and failure value
(define (platform-egg-cost-proc regraph cache node rec)
  (define constants (regraph-constants regraph))
  (define types (regraph-types regraph))
  (match node
    [(? number?) 0] ; numbers are free I guess
    [(list '$Var ty-id _) ; variable
     (match (vector-ref types ty-id)
       [(list '$Type prec)
        (platform-repr-cost
          (*active-platform*)
          (get-representation prec))]
       [_ +inf.0])] ; real or malformed
    [(list 'if _ cond ift iff) ; if expression
     (error 'platform-egg-cost-proc "unimplemented: ~a" node)]
    [(list 'pow ty-id b e) ; pow operator
     (define n (vector-ref constants e))
     (if (and n (fraction-with-odd-denominator? n))
         +inf.0
         (match (vector-ref types ty-id)
           [(list '$Type _ b-ty e-ty)
            (define impl
              (get-parametric-operator 'pow
                                       (get-representation b-ty)
                                       (get-representation e-ty)))
            (+ (platform-impl-cost (*active-platform*) impl)
               (rec b b-ty +inf.0)
               (rec e e-ty +inf.0))]
           [_ +inf.0]))] ; real or malformed
    [(list op ty-id) ; constant
     (match (vector-ref types ty-id)
       [(list '$Type otype)
        (define impl (get-parametric-constant op (get-representation otype)))
        (platform-impl-cost (*active-platform*) impl)]
       [_ +inf.0])] ; real or malformed
    [(list op ty-id args ...) ; operator
     (match (vector-ref types ty-id)
       [(list '$Type _ itypes ...)
        (define impl (apply get-parametric-operator op (map get-representation itypes)))
        (apply
          +
          (platform-impl-cost (*active-platform*) impl)
          (for/list ([arg (in-list args)] [itype (in-list itypes)])
            (rec arg itype +inf.0)))]
      [_ +inf.0])] ; real or malformed
    [(list _ ...) +inf.0])) ; malformed operator

;; Prunes any real operator nodes from a regraph.
(define (regraph-prune-specs re)
  (define eclasses (regraph-eclasses re))
  (define types (regraph-types re))
  (define n (vector-length eclasses))

  (define eclasses*
    (for/vector #:length n ([id (in-range n)])
      (define eclass (vector-ref eclasses id))
      (cond
        [(vector-ref types id) eclass]
        [else
         (define eclass*
           (reap [sow]
             (for ([node (in-vector eclass)])
               (match node
                 [(? number?) (sow node)] ; number
                 [(list 'Var _ _) (sow node)] ; variable
                 [(list 'if _ ...) (sow node)] ; if expression
                 [(list _ ty-id _ ...)  ; operator
                  (unless (eq? (vector-ref types ty-id) 'real) ; real operator
                    (sow node))]))))
         (if (null? eclass*)
             (vector (first eclass))
             (list->vector eclass*))])))

  ; invariant: all eclasses need at least one enode
  (when (for/or ([eclass (in-vector eclasses*)]) (vector-empty? eclass))
    (error 'regraph-prune-specs "invariant violated: eclass contains no enodes"))
  (struct-copy regraph re [eclasses eclasses*]))


;; Extracts the best expression according to the extractor.
;; Result is a single element list.
(define (regraph-extract-best regraph ctx extract id repr)
  ; extract expr
  (define id* (hash-ref (regraph-canon regraph) id))
  (match-define (cons _ egg-expr) (extract id* (representation-name repr)))
  ; translate egg IR to Herbie IR
  (define egg->herbie (regraph-egg->herbie regraph))
  (define repr-name (representation-name (context-repr ctx)))
  (list (egg-parsed->expr (flatten-let egg-expr) egg->herbie repr-name)))

;; Extracts multiple expressions according to the extractor
(define (regraph-extract-variants regraph ctx extract id repr)
  ; extract expressions
  (define eclasses (regraph-eclasses regraph))
  (define id* (hash-ref (regraph-canon regraph) id))
  (define egg-exprs
    (reap [sow]
      (for ([enode (vector-ref eclasses id*)])
        (match enode
          [(? number?) (sow enode)]
          [(list '$Var ty-id name-id)
           (match-define (cons _ ty) (extract ty-id #f))
           (match-define (cons _ name) (extract name-id #f))
           (sow (list '$Var ty name))]
          [(list 'if _ cond ift iff)
           (error 'regraph-extract-variants "unimplemented ~a" enode)]
          [(list op ty-id ids ...)
           (match-define (cons _ ty) (extract ty-id #f))
           (define-values (otype itypes)
             (match ty
               [(list '$Type otype itypes ...) (values otype itypes)]
               ['real (values 'real (make-list (length ids) 'real))]))
           (when (equal? otype (representation-name repr))
             (sow
               (list* op
                      ty
                      (for/list ([id (in-list ids)] [itype (in-list itypes)])
                        (define expr* (extract id itype))
                        (match-define (cons _ expr) expr*)
                        expr))))]))))
  ; translate egg IR to Herbie IR
  (define egg->herbie (regraph-egg->herbie regraph))
  (define repr-name (representation-name (context-repr ctx)))
  (for/list ([egg-expr (in-list egg-exprs)])
    (egg-parsed->expr (flatten-let egg-expr) egg->herbie repr-name)))

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
           [(cons 'scheduler mode)
            (unless (set-member? '(simple backoff) mode)
              (oops! "in instruction `~a`, unknown scheduler `~a`" instr mode))]
           [_ (oops! "in instruction `~a`, unknown parameter `~a`" instr param)]))]
      [(list 'prune-spec) (void)]
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
        [(list 'prune-spec)
         ; `prune-spec` instruction
         (unless regraph
           (oops! 'prune-spec "can only execute after a `convert` instruction"))
         (values egg-graph (regraph-prune-specs regraph))]
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
(struct egraph-query (exprs reprs schedule ctx extractor) #:transparent)

(define (make-egg-query exprs
                        reprs
                        schedule
                        #:context [ctx (*context*)]
                        #:extractor [extractor #f])
  (verify-schedule! schedule)
  (define default-extractor (untyped-egg-extractor default-untyped-egg-cost-proc))
  (egraph-query exprs
                reprs
                schedule
                ctx
                (or extractor default-extractor)))

(define (run-egg input
                 variants?
                 #:proof-inputs [proof-inputs '()]
                 #:proof-ignore-when-unsound? [proof-ignore-when-unsound? #f])
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
      (extract-proc regraph ctx extract-id id repr)))
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


