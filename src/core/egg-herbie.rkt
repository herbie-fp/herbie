#lang racket

(require egg-herbie
        (only-in ffi/unsafe
          malloc memcpy free cast ptr-set! ptr-add
          _byte _pointer _string/utf-8))
(require "../syntax/rules.rkt" "../syntax/sugar.rkt" "../syntax/syntax.rkt"
         "../syntax/types.rkt" "../common.rkt" "../errors.rkt"
         "../programs.rkt" "../timeline.rkt" "../platform.rkt")

(provide (struct-out egraph-query) make-egg-query run-egg
         rule->impl-rules get-canon-rule-name remove-rewrites)

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

;; Racket egraph
(struct regraph (canon eclasses))

;; Constructs a Racket egraph from an S-expr representation of an egraph.
(define (make-regraph egraph)
  (define n (length egraph))
  (define canon (make-hash))
  (define eclasses (make-vector n '()))
  ; reserve the next eclass
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
  ; iterate through eclasses
  (for ([eclass egraph])
    (match-define (list egg-id egg-nodes ...) eclass)
    (define id (resolve-id egg-id))
    (vector-set! eclasses id
                 (for/vector #:length (length egg-nodes)
                            ([egg-node (in-list egg-nodes)])
                   (match egg-node
                     [(list op child-ids ...) (cons op (map resolve-id child-ids))]
                     [(? symbol?) egg-node]
                     [(? number?) egg-node]))))
  ; construct with wrapper
  (regraph canon eclasses))

;; Default extraction algorithm (taken directly from egg)
(define (default-extraction-proc cost-proc regraph)
  (define eclasses (regraph-eclasses regraph))
  (define n (vector-length eclasses))
  (define costs (make-vector n #f))

  ; Unsafe call to get cost
  (define (unsafe-get-cost id)
    (car (vector-ref costs id)))

  ; Computes the current cost of a node if its children
  (define (node-cost node)
    (match node
      [(cons _ child-ids)
       (and (andmap (curry vector-ref costs) child-ids)
            (cost-proc regraph node unsafe-get-cost))]
      [_
       (cost-proc regraph node unsafe-get-cost)]))

  ; Computes eclass cost
  (define (eclass-cost eclass)
    (define costs
      (filter identity
        (for/list ([i (in-range (vector-length eclass))])
          (define node (vector-ref eclass i))
          (define cost (node-cost node))
          (and cost (cons cost node)))))
    (if (null? costs)
        #f
        (argmin car costs)))

  ; Computes costs for all eclasses
  (let build-costs ()
    ; make a pass on all eclasses
    (define changed?
      (for/fold ([changed? #f]) ([i (in-range n)])
        (define prev-cost (vector-ref costs i))
        (define cost (eclass-cost (vector-ref eclasses i)))
        (cond
          [(not prev-cost)
            (vector-set! costs i cost)
            #t]
          [(< (car cost) (car prev-cost))
            (vector-set! costs i cost)
            #t]
          [else
            changed?])))
    ; loop if we computed a new cost somewhere
    (when changed? (build-costs)))

  ; Just some sanity checking
  (unless (andmap identity (vector->list costs))
    (error 'default-extraction-proc "did not compute cost for all eclasses"))
      
  ; The actual extraction procedure
  ; Lookup the cost and reconstruct the best expression
  (lambda (id)
    (match-define (cons cost _) (vector-ref costs id))
    (cons cost
          (let loop ([id id])
            (match (cdr (vector-ref costs id))
              [(cons op ids) (cons op (map loop ids))]
              [expr expr])))))


;; The default per-node cost function
(define (default-cost-proc regraph node rec)
  (match node
    [(list 'pow _ b e) (+ 1 (rec b) (rec e))]
    [(list _ _ args ...) (apply + 1 (map rec args))]
    [_ 1]))

;; Extracts the best expression according to the extractor
(define (regraph-extract-best egraph-data regraph ctx extract id)
  (define egg->herbie (egraph-data-egg->herbie-dict egraph-data))
  (define canon (regraph-canon regraph))
  (define repr (context-repr ctx))
  (match-define (cons _ egg-expr) (extract (hash-ref canon id)))
  (egg-parsed->expr (flatten-let egg-expr) egg->herbie (representation-name repr)))

;; Extracts multiple expressions according to the extractor
(define (regraph-extract-variants egraph-data regraph ctx extract id)
  (define egg->herbie (egraph-data-egg->herbie-dict egraph-data))
  (define canon (regraph-canon regraph))
  (define repr (context-repr ctx))
  ; for each eclass, iterate through the enodes and extract the best child
  (define eclasses (regraph-eclasses regraph))
  (for/list ([enode (vector-ref eclasses (hash-ref canon id))])
    (define egg-expr
      (match enode
        [(list op ids ...) (cons op (map (lambda (id) (cdr (extract id))) ids))]
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
(struct egraph-query (exprs rules ctx iter-limit node-limit const-folding?) #:transparent)

(define (make-egg-query exprs
                        rules
                        #:context [ctx (*context*)]
                        #:iter-limit [iter-limit #f]
                        #:node-limit [node-limit (*node-limit*)]
                        #:const-folding? [const-folding? #t])
  (egraph-query exprs rules ctx iter-limit node-limit const-folding?))

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
      (make-regraph (read (open-input-string serial-egraph)))))
  ;; Extract the expressions using iteration data
  (define extract (default-extraction-proc default-cost-proc racket-egraph))
  (define variants
    (cond
      [(egraph-is-unsound-detected egg-graph) '()]
      [variants?
       (for/list ([id node-ids])
         (define id* (egraph-find egg-graph id))
         (regraph-extract-variants egg-graph racket-egraph ctx extract id*))]
      [else
       (for/list ([id node-ids])
         (define id* (egraph-find egg-graph id))
         (list (regraph-extract-best egg-graph racket-egraph ctx extract id*)))]))
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


