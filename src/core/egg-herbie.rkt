#lang racket

(require egg-herbie
        (only-in ffi/unsafe
          malloc memcpy free cast ptr-set! ptr-add
          _byte _pointer _string/utf-8))
(require "../syntax/rules.rkt" "../syntax/sugar.rkt" "../syntax/syntax.rkt"
         "../syntax/types.rkt" "../common.rkt" "../errors.rkt"
         "../programs.rkt" "../timeline.rkt")

(module+ test (require rackunit))

(provide (struct-out egraph-query) make-egg-query run-egg
         expand-rules get-canon-rule-name remove-rewrites)

;; Imported by `<herbie>/syntax/test-rules.rkt`
(module+ internals
  (provide rule->impl-rules))

;; Unfortunately Herbie expressions can't be placed directly into egg,
;; so we need an IR to represent both expressions and patterns.
;; The egg IR is similar to Herbie's IR:
;;
;; <expr> ::= (<op> <sig> <expr> ...)
;;        ::= ($Var <sig> <ident>)
;;        ::= <number>
;;
;; <sig> ::= ($Type <otype> <itype> ...)
;;
;; The main difference is the use of type signatures to differentiate
;; operator implementations for different representations.

;; Flatterns proof directly from egg (NOT FPCore format!)
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

;; Parses a string from egg into a list of S-exprs.
(define (egg-exprs->exprs s egraph-data)
  (define egg->herbie (egraph-data-egg->herbie-dict egraph-data))
  (for/list ([egg-expr (in-port read (open-input-string s))])
    (egg-parsed->expr (flatten-let egg-expr) egg->herbie)))

;; Parses a string from egg into a single S-expr.
(define (egg-expr->expr s egraph-data)
  (first (egg-exprs->exprs s egraph-data)))

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
      [(list '$Var _ name)
       (hash-ref rename-dict name)]
      [(list (? repr-conv? op) 'real arg)
       (list op (loop arg))]
      [(list op prec)
       (match-define (list '$Type otype) prec)
       (list (get-parametric-constant op (get-representation otype)))]
      [(list op prec args ...)
       (match-define (list '$Type _ itypes ...) prec)
       (define op* (apply get-parametric-operator op (map get-representation itypes)))
       (cons op* (map loop args))]
      [(? number?)
       expr])))

;; Expands operators into `(op, sig)` so that we can
;; recover the exact operator implementation when extracting.
;; The ugly solution is to make `sig` = `($Type otype itypes ...)`.
(define (expand-operator op-or-impl)
  (cond
    [(impl-exists? op-or-impl)
     (define op (impl->operator op-or-impl))
     (define itypes (map representation-name (operator-info op-or-impl 'itype)))
     (define otype (representation-name (operator-info op-or-impl 'otype)))
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
      [(list (? impl-exists? op) args ...)
       (define-values (op* prec) (expand-operator op))
       (cons op* (cons prec (map loop args)))]
      [(? number?) expr]
      [_ (string->symbol (format "?~a" expr))])))

;; Translates a Herbie expression into an expression usable by egg
;; Returns the string representing the expression and updates
;; the translation dictionary
(define (expr->egg-expr expr egg-data ctx)
  (define egg->herbie-dict (egraph-data-egg->herbie-dict egg-data))
  (define herbie->egg-dict (egraph-data-herbie->egg-dict egg-data))
  (define expr* (expr->egg-expr-helper expr egg->herbie-dict herbie->egg-dict ctx))
  expr*)

(define (expr->egg-expr-helper expr egg->herbie-dict herbie->egg-dict ctx)
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
       (define prec (list '$Type (representation-name (context-lookup ctx expr))))
       (define replacement (hash-ref herbie->egg-dict expr))
       (list '$Var prec replacement)]
      [_
       (define prec (list '$Type (representation-name (context-lookup ctx expr))))
       (define replacement (string->symbol (format "h~a" (hash-count herbie->egg-dict))))
       (hash-set! herbie->egg-dict expr replacement)
       (hash-set! egg->herbie-dict replacement expr)
       (list '$Var prec replacement)])))

(module+ test
  (require "../load-plugin.rkt")
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
          (cons '(-.f64 2 (+.f64 x y))
                (~a '(- ($Type binary64 binary64 binary64) 2
                        (+ ($Type binary64 binary64 binary64)
                           ($Var ($Type binary64) h1)
                           ($Var ($Type binary64) h0)))))
          (cons '(-.f64 z (+.f64 (+.f64 y 2) x))
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
          (cons '(+.f64 (*.f64 x y) 2)
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
      (define computed-in (egg-expr->expr out egg-graph))
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
      (define expr* (desugar-program expr (*context*) #:full #f))
      (define egg-expr (expr->egg-expr expr* egg-graph (*context*)))
      (check-equal? (egg-expr->expr (~a egg-expr) egg-graph) expr*))))


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

;; Translates a rewrite rule into potentially many rules.
;; If the rule is over types, the rule is duplicated for every
;; valid assignment of representations.
(define (rule->impl-rules r)
  (match-define (rule name input output itypes otype) r)
  (define supported? (curry set-member? (*needed-reprs*)))
  (cond
    [(andmap representation? (cons otype (map cdr itypes)))
     ; rule over representation: just return the rule
     ; make sure to validate that the operators are supported
     (if (and (andmap supported? (reprs-in-expr input))
              (andmap supported? (reprs-in-expr output)))
         (list r)
         (list))]
    [else
     ; rule over types: must instanstiate over all possible
     ; assignments of representations keeping in mind that
     ; some operator implementations may not exist
     (define types (remove-duplicates (cons otype (map cdr itypes))))
     (reap [sow]
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
           (when (and (andmap supported? (reprs-in-expr input*))
                      (andmap supported? (reprs-in-expr output*)))
             (sow (rule name* input* output* itypes* otype*))))))]))

;; Like `rule->impl-rule` except rules are consumable by egg.
;; Special pass to handle expansive rules `?x => f(?x)`
(define (rule->egg-rules r)
  (match-define (rule name input output _ _) r)
  (cond
    [(variable? input)
     ; expansive rule: `?x => f(?x)`
     ; special care needs to be taken here
     (when (variable? output)
       (error 'rule->egg-rules "rewriting variable to variable ~a" r))
     ; for each real operator `app` instantiate a rule of
     ; the form: `(app e ...) => f((app e ...))`
     (define op-rules
       (for/fold ([rules '()]) ([op (all-operators)] #:unless (eq? op 'cast))
         (define itypes (real-operator-info op 'itype))
         (define otype (real-operator-info op 'otype))
         (define vars (build-list (length itypes) (λ (i) (string->symbol (format "$T~a" i)))))
 
         (define name* (sym-append name '- op))
         (define input* (cons op vars))
         (define output* (replace-expression output input input*))
         (define itypes* (map cons vars itypes))

         (append (rule->egg-rules (rule name* input* output* itypes* otype)) rules)))
     ; for each implementation matching the output type/representation,
     ; make a rule for a variable under the same implementation
     (define var-rules
       (for/list ([impl-rule (rule->impl-rules r)])
         ; replace the LHS with a variable node
         (match-define (rule name input output itypes otype)
           (struct-copy rule impl-rule
                        [input (expr->egg-pattern (rule-input impl-rule))]
                        [output (expr->egg-pattern (rule-output impl-rule))]))
         (define repr (dict-ref itypes (rule-input impl-rule)))
         (define prec (list '$Type (representation-name repr)))
         (define input* (list '$Var prec input))
         (define output* (replace-expression output input input*))
         (rule name input* output* itypes otype)))
     ; both over ops and over a variable
     (append op-rules var-rules)]
    [else
     ;; all other rules should be replicated across representations
     ;; and then translated into a format usable by egg
     (for/list ([impl-rule (rule->impl-rules r)])
       (struct-copy rule impl-rule
                    [input (expr->egg-pattern (rule-input impl-rule))]
                    [output (expr->egg-pattern (rule-output impl-rule))]))]))
    

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

;; Herbie's version of an egg runner
;; Defines parameters for running rewrite rules with egg
(struct egraph-query (exprs rules ctx iter-limit node-limit const-folding?) #:transparent)

(define (make-egg-query exprs rules
                        #:context [ctx (*context*)]
                        #:iter-limit [iter-limit #f]
                        #:node-limit [node-limit (*node-limit*)]
                        #:const-folding? [const-folding? #t])
  (egraph-query exprs rules ctx iter-limit node-limit const-folding?))

(define (run-egg input variants?
                 #:proof-input [proof-input '()]
                 #:proof-ignore-when-unsound? [proof-ignore-when-unsound? #f])
  (define egg-graph (make-egraph))
  (define ctx (egraph-query-ctx input))
  (define node-ids
    (for/list ([expr (egraph-query-exprs input)])
      (egraph-add-expr egg-graph expr ctx)))
  (define iter-data (egraph-run-rules egg-graph
                                      (egraph-query-node-limit input)
                                      (egraph-query-rules input)
                                      node-ids
                                      (egraph-query-const-folding? input)
                                      #:limit (egraph-query-iter-limit input)))
  
  (define variants
    (if variants?
        (for/list ([id node-ids] [expr (egraph-query-exprs input)])
          (egraph-get-variants egg-graph id expr ctx))
        (for/list ([id node-ids])
          (for/list ([iter (in-range (length iter-data))])
            (egraph-get-simplest egg-graph id iter)))))
  
  (match proof-input
    [(cons start end)
     #:when (not (and (egraph-is-unsound-detected egg-graph) proof-ignore-when-unsound?))
     (when (not (egraph-is-equal egg-graph start end ctx))
        (error "Cannot get proof: start and end are not equal.\n start: ~a \n end: ~a" start end))

     (define proof (egraph-get-proof egg-graph start end ctx))
     (when (null? proof)
       (error (format "Failed to produce proof for ~a to ~a" start end)))
     (cons variants proof)]
    [_ (cons variants #f)]))

(define (egraph-get-simplest egraph-data node-id iteration)
  (define ptr (egraph_get_simplest (egraph-data-egraph-pointer egraph-data) node-id iteration))
  (define str (cast ptr _pointer _string/utf-8))
  (destroy_string ptr)
  (egg-expr->expr str egraph-data))

(define (egraph-get-variants egraph-data node-id orig-expr ctx)
  (define expr-str (~a (expr->egg-expr orig-expr egraph-data ctx)))
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

; Makes a new egraph that is managed by Racket's GC
(define (make-egraph)
  (egraph-data (egraph_create) (make-hash) (make-hash)))

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

(module+ test
  (check-equal?
   (sequential-product `((1 2) (3 4 5) (6)))
   `((1 3 6) (2 3 6) (2 4 6) (2 5 6)))

  (expand-proof-term '(Explanation (+ x y) (+ y x)) (box 10)))


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
         [(? number?)
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
   ;; TODO: sometimes the proof is *super* long and it takes us too long just string-split
   ;; Ideally we would skip the string-splitting
   [(< (string-length res) 10000)
    (define converted
      (for/list ([line (in-list (string-split res "\n"))])
        (egg-expr->expr line egraph-data)))
    (define expanded (expand-proof converted (box (*proof-max-length*))))
    (if (member #f expanded)
        #f
        expanded)]
   [else
    #f]))

;; result function is a function that takes the ids of the nodes
(define (egraph-add-expr eg-data expr ctx)
  (define egg-expr (~a (expr->egg-expr expr eg-data ctx)))
  (egraph_add_expr (egraph-data-egraph-pointer eg-data) egg-expr))

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
(define (egraph-run egraph-data node-limit ffi-rules const-folding? [iter-limit #f])
  (define egraph-ptr (egraph-data-egraph-pointer egraph-data))
  (define-values (iterations length ptr)
    (if iter-limit
        (egraph_run_with_iter_limit egraph-ptr ffi-rules iter-limit node-limit const-folding?)
        (egraph_run egraph-ptr ffi-rules node-limit const-folding?)))
  (define iteration-data (convert-iteration-data iterations length))
  (destroy_egraphiters ptr)
  iteration-data)

;; Cache mapping (name, representations) -> (listof ffi-rules)
;; Rule expansion takes a significant amount of time, so we cache
;; Assumes the set of rules, representations, and operator implementations
;; are fixed throughout the improvement loop; rules are added and never
;; removed or mutated
(define-resetter *ffi-rules*
  (λ () (make-hash))
  (λ () (make-hash))
  (λ (cache)
    (define ffi-rules/rule (hash-values cache))
    (for ([ffi-rules (in-list ffi-rules/rule)])
      (for-each free-ffi-rule ffi-rules))))

;; Cache mapping name to its canonical rule name
;; See `*egg-rules*` for details
(define-resetter *canon-names*
  (λ () (make-hash))
  (λ () (make-hash)))

;; Tries to look up the canonical name of a rule using the cache.
;; Obviously dangerous if the cache is invalid.
(define (get-canon-rule-name name [failure #f])
  (hash-ref (*canon-names*) name failure))

;; expand the rules first due to some bad but currently
;; necessary reasons (see `rule->egg-rules` for details).
;; checks the cache in case we used them previously
(define (expand-rules rules)
  (for/fold ([rules* '()]) ([rule (in-list rules)])
    (append
      (hash-ref! (*ffi-rules*) (cons rule (*needed-reprs*))
                 (λ ()
                   (define orig-name (rule-name rule))
                   (define egg-rules (rule->egg-rules rule))
                   (for/list ([egg-rule (in-list egg-rules)])
                     (define name (rule-name egg-rule))
                     (hash-set! (*canon-names*) name orig-name)
                     (cons name (make-ffi-rule egg-rule)))))
      rules*)))

(define (egraph-run-rules egg-graph node-limit rules node-ids const-folding? #:limit [iter-limit #f])
  ;; expand rules (may possibly be cached)
  (define egg-rules (expand-rules rules))

  ;; run the rules
  (define ffi-rules (map cdr egg-rules))
  (define iteration-data (egraph-run egg-graph node-limit ffi-rules const-folding? iter-limit))

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
  (for ([(name ffi-rule) (in-dict egg-rules)])
    (define count (egraph-get-times-applied egg-graph ffi-rule))
    (define canon-name (hash-ref (*canon-names*) name))
    (hash-update! rule-apps canon-name (curry + count) count))

  (for ([(name count) (in-hash rule-apps)])
    (when (> count 0) (timeline-push! 'rules (~a name) count)))

  iteration-data)
