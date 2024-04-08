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
         default-egg-extractor
         default-egg-cost-proc
         platform-egg-cost-proc
         local-error-egg-cost-proc
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

;; result function is a function that takes the ids of the nodes
(define (egraph-add-expr eg-data expr ctx)
  (define egg-expr (~a (expr->egg-expr expr eg-data ctx)))
  (egraph_add_expr (egraph-data-egraph-pointer eg-data) egg-expr))

;; runs rules on an egraph (optional iteration limit)
(define (egraph-run egraph-data node-limit ffi-rules const-folding? [iter-limit #f])
  (define egraph-ptr (egraph-data-egraph-pointer egraph-data))
  (define-values (iterations length ptr)
    (if iter-limit
        (egraph_run_with_iter_limit egraph-ptr ffi-rules iter-limit node-limit const-folding?)
        (egraph_run egraph-ptr ffi-rules node-limit const-folding?)))
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
      [(list (? impl-exists? op) args ...)
       (define-values (op* prec) (expand-operator op))
       (cons op* (cons prec (map loop args)))]
      [(? literal?) (literal-value expr)]
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
      [(list op args ...)
       (define-values (op* prec) (expand-operator op))
       (cons op* (cons prec (map loop args)))]
      [(? literal?)
       (literal-value expr)]
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
;; Rules in the egraph must be over representations.
;; We need to instatiate rules over types at particular representations
;; and translate them into egg's expression/pattern format.
;; This is the most annoying and bug-filled part of this library by far!

;; Given a list of types, computes the product of all possible
;; representation assignments where each element
;; is a dictionary mapping type to representation
(define (type-combinations types [reprs (platform-reprs (*active-platform*))])
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
      (set-add! reprs (impl-info op 'otype))
      (for ([itype (impl-info op 'itype)])
        (set-add! reprs itype))
      (for-each loop args)]
     [_ (void)]))
  (set->list reprs))

;; Representation name sanitizer (also in <herbie>/platform.rkt)
(define (repr->symbol repr)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (define repr-name (representation-name repr))
  (string->symbol (string-replace* (~a repr-name) replace-table)))

;; Translates a rewrite rule into potentially many rules.
;; If the rule is over types, the rule is duplicated for every
;; valid assignment of representations.
(define (rule->impl-rules r)
  (match-define (rule name input output itypes otype) r)
  (define active-reprs (platform-reprs (*active-platform*)))
  (define supported? (curry set-member? active-reprs))
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
           (define name* (sym-append name '_ (repr->symbol sugar-otype)))
           (define input* (spec->prog input sugar-ctx))
           (define output* (spec->prog output sugar-ctx))
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
         (define itypes (operator-info op 'itype))
         (define otype (operator-info op 'otype))
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
    
;; Cache mapping (rule, platform) -> (listof expanded-rule)
;; where expanded-rule is (pairof egg-rule ffi-rule)))
;; Rule expansion takes a significant amount of time, so we cache
;; Assumes the set of rules, representations, and operator implementations
;; are fixed throughout the improvement loop; rules are added and never
;; removed or mutated
(define-resetter *ffi-rules*
  (λ () (make-hash))
  (λ () (make-hash))
  (λ (cache)
    (for ([(_ entry) (in-hash cache)])
      (define ffi-rules (map cdr entry))
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

;; expand the rules first due to some bad but currently necessary reasons
;; (see `rule->egg-rules` for details); checks the cache in case we used
; them previously
(define (expand-rules rules)
  (define pform (*active-platform*))
  (for/fold ([rules* '()] #:result (reverse rules*))
            ([rule (in-list rules)])
    (define egg&ffi
      (hash-ref! (*ffi-rules*) ; cache
                 (cons rule (platform-name pform)) ; key
                 (λ () ; generate
                   (define orig-name (rule-name rule))
                   (for/list ([egg-rule (in-list (rule->egg-rules rule))])
                     (define name (rule-name egg-rule))
                     (hash-set! (*canon-names*) name orig-name)
                     (cons egg-rule (make-ffi-rule egg-rule))))))
    (append (reverse egg&ffi) rules*)))

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
  (for ([(egg-rule ffi-rule) (in-dict egg-rules)])
    (define count (egraph-get-times-applied egg-graph ffi-rule))
    (define canon-name (hash-ref (*canon-names*) (rule-name egg-rule)))
    (hash-update! rule-apps canon-name (curry + count) count))

  (for ([(name count) (in-hash rule-apps)])
    (when (> count 0) (timeline-push! 'rules (~a name) count)))
  iteration-data)

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

;; Immutable egraph
;; - eclasses: vector of enodes
;; - canon: map from egraph id to vector index
;; - has-leaf?: map from vector index to if the eclass contains a leaf
;; - constants: map from vector index to if the eclass contains a constant
;; - types: map from vector index to a egg IR type or #f
;; - egg->herbie: data to translate egg IR to herbie IR
;; - node->id: mapping node to eclass id (using eq? for comparison)
(struct regraph (eclasses canon has-leaf? constants types node->id egg->herbie))

;; Constructs a Racket egraph from an S-expr representation of
;; an egraph and data to translate egg IR to herbie IR.
(define (make-regraph egraph egg->herbie)
  ; reserve the next eclass
  (define canon (make-hash))
  (define new-eclass
    (let ([counter 0])
      (lambda (id)
        (define id* counter)
        (hash-set! canon id id*)
        (set! counter (add1 counter))
        id*)))
  ; lookup the canonical eclass or the next new eclass
  (define (resolve-id id)
    (define canon-id (hash-ref canon id #f))
    (if canon-id canon-id (new-eclass id)))
  ; iterate through eclasses and fill data
  (define n (length egraph))
  (define eclasses (make-vector n '()))
  (define has-leaf? (make-vector n #f))
  (define constants (make-vector n #f))
  (define type? (make-vector n #f))
  (define node->id (make-hasheq))
  (for ([eclass egraph])
    (match-define (list egg-id egg-nodes ...) eclass)
    (define id (resolve-id egg-id))
    (vector-set! eclasses id
                 (for/vector #:length (length egg-nodes)
                            ([egg-node (in-list egg-nodes)])
                   (match egg-node
                     [(? number?) ; number
                      (hash-set! node->id egg-node id)
                      (vector-set! has-leaf? id #t)
                      (vector-set! constants id egg-node)
                      egg-node]
                     [(? symbol?) ; egg IR typename
                      (hash-set! node->id egg-node id)
                      (vector-set! type? id 'typename)
                      egg-node]
                     [(list op child-ids ...)
                      (define egg-node* (cons op (map resolve-id child-ids)))
                      (hash-set! node->id egg-node* id)
                      (match op
                        ['$Type (vector-set! type? id 'type)] ; egg IR type signature
                        ['$Var (vector-set! has-leaf? id #t)] ; variable
                        [_ (void)])
                      egg-node*]
                     [_ (error 'make-regraph "malformed enode: ~a" egg-node)]))))
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
      [type (error 'make-regraph "unexpected type eclass type ~a" type)]))
  ; construct with wrapper
  (regraph eclasses canon has-leaf? constants types node->id egg->herbie))

;; Computes the parent relation of each eclass.
(define (regraph-parents regraph)
  (define eclasses (regraph-eclasses regraph))
  (define n (vector-length eclasses))
  (define parents (make-vector n '()))

  ; updates `parents` based on the eclass `id`
  (define (build-parents id)
    (define eclass (vector-ref eclasses id))
    (for ([node (in-vector eclass)])
      (when (pair? node) ; only care about nodes with children
        (for ([child-id (in-list (cdr node))])
          (vector-set! parents
                       child-id
                       (cons id (vector-ref parents child-id)))))))

  ; compute parent-child relation
  ; (ensure the result uses only vectors)
  (for ([id (in-range n)]) (build-parents id))
  (vector-map list->vector parents))

;; Creates an extraction procedure based on (eclass cost, cheapest node)
(define (egg-extractor-proc cost-vec)
  (define n (vector-length cost-vec))
  (define costs (vector-map car cost-vec))
  (define nodes (make-vector n #f))
  
  ; reconstructs the best expression at a node
  (define (build-expr id)
    (define node (vector-ref nodes id)) ; check if it is cached
    (cond
      [node node]
      [else
       (define node 
         (match (cdr (vector-ref cost-vec id))
           [(? number? n) n] ; number
           [(? symbol? s) s] ; typename or variable name
           [(cons '$Type types) (cons '$Type types)] ; type
           [(cons op ids) (cons op (map build-expr ids))] ; operator
           [e (error 'default-extraction-proc "could not reconstruct ~a" e)]))
       (vector-set! nodes id node)
       node]))

  ; the actual procedure
  (lambda (id) (cons (vector-ref costs id) (build-expr id))))

;; Default extraction algorithm
;; Implements the extraction function found in egg with some improvements
(define (default-egg-extractor cost-proc regraph)
  (define eclasses (regraph-eclasses regraph))
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

  ; maintain a dirty set of eclasses that need costs recomputed
  ; if an eclass cost is updated, all parents are dirty
  (define parents (regraph-parents regraph))
  (define dirty?-vec (vector-copy (regraph-has-leaf? regraph)))

  ; set the cost of all type eclasses to 0
  (for ([id (in-range n)])
    (define type (vector-ref types id))
    (when type ; any type eclass has cost 0
      (vector-set! costs id (cons 0 type))
      (for ([parent (in-vector (vector-ref parents id))])
        ; important: need to update the dirty set with non-type eclasses
        (unless (vector-ref types parent)
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

  ; Computes eclass cost returning (cost, node) or #f
  (define (eclass-cost id)
    (define eclass (vector-ref eclasses id))
    (for/fold ([best #f]) ([node (in-vector eclass)])
      (define cost (node-cost node))
      (cond [(not cost) best]
            [(not best) (cons cost node)]
            [(< cost (car best)) (cons cost node)]
            [else best])))

  ; Computes costs for all eclasses
  (let build-costs ()
    (for ([id (in-range n)] #:when (vector-ref dirty?-vec id))
      (vector-set! dirty?-vec id #f)
      (define cost (eclass-cost id))
      (when cost ; computed a cost
        (define prev-cost (vector-ref costs id))
        (when (or (not prev-cost) (< (car cost) (car prev-cost))) ; cost is better
          (vector-set! costs id cost)
          (for ([j (in-vector (vector-ref parents id))])
            (vector-set! dirty?-vec j #t)))))
    (when (for/or ([dirty? (in-vector dirty?-vec)]) dirty?)
      (build-costs)))

  ; invariant: all eclasses have an associated cost!
  (unless (for/and ([cost (in-vector costs)]) cost)
    (error 'default-extraction-proc
           "did not compute cost for all eclasses: ~a"
           costs))
  
  ; construct the actual extraction procedure
  (egg-extractor-proc costs))

;; Is fractional with odd denominator.
(define (fraction-with-odd-denominator? frac)
  (and (rational? frac)
       (let ([denom (denominator frac)])
         (and (> denom 1) (odd? denom)))))

;; The default per-node cost function
(define (default-egg-cost-proc regraph cache node rec)
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

;; Per-node cost function according to the platform
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
       [_ +inf.0])] ; malformed (maybe unsound?)
    [(list 'if _ cond ift iff) ; if expression
     (+ (platform-impl-cost (*active-platform*) 'if)
        (rec cond)
        (rec ift)
        (rec iff))]
    [(list 'pow ty-id b e) ; pow operator
     (define n (vector-ref constants e))
     (if (and n (fraction-with-odd-denominator? n))
         +inf.0
         (match (vector-ref types ty-id)
           [(list '$Type _ itypes ...)
            (+ (platform-impl-cost
                 (*active-platform*)
                 (apply get-parametric-operator 'pow (map get-representation itypes)))
               (rec b)
               (rec e))]
           [_ +inf.0]))]  ; malformed (maybe unsound?)       
    [(list op ty-id args ...) ; operator
     (match (vector-ref types ty-id)
       [(list '$Type otype itypes ...)
        (apply
          +
          (platform-impl-cost
            (*active-platform*)
            (if (null? itypes)
                (get-parametric-constant op (get-representation otype))
                (apply get-parametric-operator op (map get-representation itypes))))
          (map rec args))]
      [_ +inf.0])]  ; malformed (maybe unsound?)
    [(list _ ...) +inf.0])) ; malformed operator

;; Computes the ground truth of every expression eclass.
(define (regraph-compute-ground-truth! regraph ctx pcontext cache)
  (define eclasses (regraph-eclasses regraph))
  (define n (vector-length eclasses))

  (define egg->herbie (regraph-egg->herbie regraph))
  (define extract-id (default-egg-extractor default-egg-cost-proc regraph))
  (define repr-name (representation-name (context-repr ctx)))
  (define num-points (min (*local-error-egg-num-points*) (pcontext-length pcontext)))

  ;; Extract a representative from each expression eclass
  (define id&exprs
    (reap [sow]
      (for ([id (in-range n)])
        (match-define (cons _ expr) (extract-id id))
        (match expr
          [(? symbol?) (void)] ; precision name
          [(list '$Type _ ...) (void)] ; type annotation
          [egg-expr
           (define expr* (egg-parsed->expr (flatten-let egg-expr) egg->herbie repr-name))
           (sow (cons id expr*))]))))

  ;; Build ground-truth interpreter
  (define fn
    (let ([exprs (map cdr id&exprs)])
      (eval-progs-real
        (map prog->spec exprs)
        (for/list ([expr (in-list exprs)])
          (struct-copy context ctx
            [repr (repr-of expr ctx)])))))

  ;; Compute exacts
  (define exacts
    (for/vector #:length n ([_ (in-range n)])
      (make-vector num-points #f)))
  (for ([(pt _) (in-pcontext (*pcontext*))] [i (in-range num-points)])
    (for ([(id _) (in-dict id&exprs)] [exact (in-list (apply fn pt))])
      (vector-set! (vector-ref exacts id) i exact)))

  ;; Set cache
  (set-box! cache (cons exacts (make-hasheq))))

;; Per-node cost function by local error
(define ((local-error-egg-cost-proc context pcontext) regraph cache node rec)
  (unless (unbox cache)
    (regraph-compute-ground-truth! regraph context pcontext cache))
  (match-define (cons exacts node->error) (unbox cache))
  (define node->id (regraph-node->id regraph))
  (define types (regraph-types regraph))
  (match node
    [(? number?) 0] ; numbers are free I guess
    [(list '$Var _ _) 0] ; variable
    [(list op ty-id arg-ids ...)
     (hash-ref! node->error
                node
                (lambda ()
                  (match (vector-ref types ty-id)
                    [#f +inf.0]
                    [ty-sig
                     (match-define (list '$Type otype itypes ...) ty-sig)
                     (define impl
                       (if (null? itypes)
                           (get-parametric-constant op (get-representation otype))
                           (apply get-parametric-operator op (map get-representation itypes))))
                     (define exact (vector-ref exacts (hash-ref node->id node)))
                     (define approx (apply (impl-info impl 'fl) (map (curry vector-ref exacts) arg-ids)))
                     (ulps->bits (ulp-difference approx exact (impl-info impl 'otype)))])))]))

;; Extracts the best expression according to the extractor
(define (regraph-extract-best regraph ctx extract id)
  (define egg->herbie (regraph-egg->herbie regraph))
  (define canon (regraph-canon regraph))
  (define repr (context-repr ctx))
  (match-define (cons _ egg-expr) (extract (hash-ref canon id)))
  (egg-parsed->expr (flatten-let egg-expr) egg->herbie (representation-name repr)))

;; Extracts multiple expressions according to the extractor
(define (regraph-extract-variants regraph ctx extract id)
  (define egg->herbie (regraph-egg->herbie regraph))
  (define canon (regraph-canon regraph))
  (define repr (context-repr ctx))
  ; for each eclass, iterate through the enodes and extract the best child
  (define eclasses (regraph-eclasses regraph))
  (for/list ([enode (vector-ref eclasses (hash-ref canon id))])
    (define egg-expr
      (match enode
        [(list op ids ...)
         (cons op (for/list ([id (in-list ids)])
                    (match-define (cons _ expr) (extract id))
                    expr))]
        [_ enode]))
    (egg-parsed->expr (flatten-let egg-expr)
                      egg->herbie
                      (representation-name repr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;
;; Most calls to egg should be done through this interface.
;;  - `make-egg-query` creates a struct that describes a _reproducible_ egg instance
;;  - `run-egg` actually runs egg and extracts expressions and possibly proofs

;; Herbie's version of an egg runner
;; Defines parameters for running rewrite rules with egg
(struct egraph-query (exprs rules ctx iter-limit node-limit extractor cost-proc const-folding?) #:transparent)

(define (make-egg-query exprs
                        rules
                        #:context [ctx (*context*)]
                        #:iter-limit [iter-limit #f]
                        #:node-limit [node-limit (*node-limit*)]
                        #:extractor [extractor default-egg-extractor]
                        #:cost-proc [cost-proc default-egg-cost-proc]
                        #:const-folding? [const-folding? #t])
  (egraph-query exprs rules ctx iter-limit node-limit extractor cost-proc const-folding?))

(define (run-egg input
                 variants?
                 #:proof-inputs [proof-inputs '()]
                 #:proof-ignore-when-unsound? [proof-ignore-when-unsound? #f])
  ;; Create the egraph (and runner)
  (define egg-graph (make-egraph))
  ;; Insert expressions
  (define ctx (egraph-query-ctx input))
  (define node-ids
    (for/list ([expr (egraph-query-exprs input)])
      (egraph-add-expr egg-graph expr ctx)))
  ;; Run egg and extract iteration data
  (egraph-run-rules egg-graph
                    (egraph-query-node-limit input)
                    (egraph-query-rules input)
                    node-ids
                    (egraph-query-const-folding? input)
                    #:limit (egraph-query-iter-limit input))
  ;; Read egraph via serialization
  (define racket-egraph
    (let ([serial-egraph (egraph-serialize egg-graph)])
      (make-regraph (read (open-input-string serial-egraph))
                    (egraph-data-egg->herbie-dict egg-graph))))
  ;; Compute eclass/enode cost in the graph
  (define extractor (egraph-query-extractor input))
  (define cost-proc (egraph-query-cost-proc input))
  (define extract-id (extractor cost-proc racket-egraph))
  ;; Extract the expressions
  (define variants
    (cond
      [(egraph-is-unsound-detected egg-graph)
       (map (lambda (_) (list)) node-ids)]
      [variants?
       (for/list ([id node-ids])
         (define id* (egraph-find egg-graph id))
         (regraph-extract-variants racket-egraph ctx extract-id id*))]
      [else
       (for/list ([id node-ids])
         (define id* (egraph-find egg-graph id))
         (list (regraph-extract-best racket-egraph ctx extract-id id*)))]))
  ;; Extract the proof based on a pair (start, end) expressions.
  (define proofs
    (for/list ([proof-input (in-list proof-inputs)])
      (cond
        [(not (and (egraph-is-unsound-detected egg-graph) proof-ignore-when-unsound?))
         (match-define (cons start end) proof-input)
         (unless (egraph-is-equal egg-graph start end ctx)
           (error "Cannot get proof: start and end are not equal.\n start: ~a \n end: ~a" start end))

         (define proof (egraph-get-proof egg-graph start end ctx))
         (when (null? proof)
           (error (format "Failed to produce proof for ~a to ~a" start end)))
         proof]
        [else #f])))
  ;; Return extracted expressions and the proof
  (cons variants proofs))


