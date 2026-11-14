;; Expression conversions
;;
;; Herbie uses three expression languages.
;; All formats are S-expressions with variables, numbers, and applications.
;;
;; FPCore: the input/output language
;;
;; - standardized interchange format with other tools
;; - using a loopless subset with array literals
;; - operators denote real computations while rounding contexts decide format
;;
;;  <FPCore> ::= (FPCore (<var> ...) <props> ... <expr>)
;;             | (FPCore <id> (<var> ...) <props> ... <expr>)
;;
;;  <expr>   ::= (let ([<id> <expr>] ...) <expr>)
;;             | (let* ([<id> <expr>] ...) <expr>)
;;             | (if <expr> <expr> <expr>
;;             | (cast <expr>)
;;             | (! <props> ... <expr>)
;;             | (<op> <expr> ...)
;;             | <number>
;;             | <id>
;;
;;  <var>    ::= <id>
;;             | (! <props> ... <expr>)
;;
;; LImpl: the language of floating-point expressions
;;
;; - internal language, describes floating-point expressions
;; - unary minus represented by `neg`
;; - every operator is rounded
;; - let expressions are inlined
;; - numbers are rounded to a particular format
;;
;; <expr> ::= (<impl> <expr> ...)
;;          | (literal <number> <repr>)
;;          | <id>
;;
;; Every operator has a type signature where types are representations.
;; In practice, most operator implemenetations have uniform representations but
;; operations like casts are multi-format.
;;
;; LSpec: the language of mathematical formula
;;
;; - internal language, describes real-number formula
;; - unary minus represented by `neg`
;; - every operator is a real-number operation
;; - every operator is supported by Rival
;; - let expressions are inlined
;; - casts are mapped to the identity operation
;; - numbers are formatless
;;
;; <expr> ::= (<op> <expr> ...)
;;          | <number>
;;          | <id>
;;

#lang racket

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "platform.rkt"
         "matcher.rkt"
         "syntax.rkt"
         "types.rkt")

(provide fpcore->spec
         fpcore->prog
         spec->prog
         prog->fpcore)

;; Local copies to avoid depending on core/programs.rkt.
(define (repr-of expr ctx)
  (match expr
    [(literal _ precision) (get-representation precision)]
    [(? symbol?) (context-lookup ctx expr)]
    [(approx _ impl) (repr-of impl ctx)]
    [(list op _ ...) (impl-info op 'otype)]))

(define (replace-vars dict expr)
  (let loop ([expr expr])
    (match expr
      [(? literal?) expr]
      [(? number?) expr]
      [(? symbol?) (dict-ref dict expr expr)]
      [(approx impl spec) (approx (loop impl) (loop spec))]
      [(list op args ...) (cons op (map loop args))])))

(define (fpcore-extension-spec expr)
  ;; Pattern matching requires equal root operators, so other bodies cannot match.
  (define specs
    (remove-duplicates (filter identity
                               (for/list ([impl (in-list (platform-fpcore-impls (*active-platform*)
                                                                                (car expr)))])
                                 (define-values (_ body) (impl->fpcore impl))
                                 (define subst (pattern-match body expr))
                                 (and subst (pattern-substitute (impl-info impl 'spec) subst))))))
  (match specs
    ['() #f]
    [(list spec) spec]
    [_ (error 'fpcore->spec "ambiguous platform specs for `~a`: ~a" expr specs)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FPCore -> LSpec

(define (fpcore->spec prog)
  (define (lower-app op args)
    (define expr* `(,op ,@args))
    (match (fpcore-extension-spec expr*)
      [#f expr*]
      [(== expr*) expr*]
      [spec (loop spec '())]))

  (define (fold-app op args)
    (for/fold ([out (first args)]) ([arg (in-list (rest args))])
      (lower-app op (list out arg))))

  (define (conjoin args)
    (match args
      ['() (lower-app 'TRUE '())]
      [(list arg) arg]
      [_ (fold-app 'and args)]))

  (define (loop expr env)
    (match expr
      [(? number?)
       (match expr
         [(or +inf.0 -inf.0 +nan.0) expr]
         [(? exact?) expr]
         [_ (inexact->exact expr)])]
      [`(let ([,vars ,vals] ...) ,body)
       ;; Lower each parallel binding once, then reuse its result in the body.
       (define env*
         (for/fold ([env* env])
                   ([var (in-list vars)]
                    [val (in-list vals)])
           (dict-set env* var (loop val env))))
       (loop body env*)]
      [`(let* ([,vars ,vals] ...) ,body)
       ;; Each sequential binding sees and extends the preceding environment.
       (define env*
         (for/fold ([env* env])
                   ([var (in-list vars)]
                    [val (in-list vals)])
           (dict-set env* var (loop val env*))))
       (loop body env*)]
      [`(,(and (or 'and 'or) op)) (lower-app (if (eq? op 'and) 'TRUE 'FALSE) '())]
      [`(,(or '+ '-)) 0]
      [`(,(or '* '/)) 1]
      [`(,(or '+ '* 'and 'or) ,arg) (loop arg env)]
      [`(- ,arg) (lower-app 'neg (list (loop arg env)))]
      [`(/ ,arg) (lower-app '/ (list 1 (loop arg env)))]
      [`(,(and (or '+ '- '* '/ 'and 'or) op) ,first ,second ,rest ...)
       (fold-app op (map (curryr loop env) (list* first second rest)))]
      [`(,(and (or '< '<= '> '>= '=) op) ,args ...)
       (define args* (map (curryr loop env) args))
       (conjoin (for/list ([term (in-list args*)]
                           [next (in-list (if (null? args*)
                                              '()
                                              (cdr args*)))])
                  (lower-app op (list term next))))]
      [`(!= ,args ...)
       (define args* (map (curryr loop env) args))
       (conjoin (for*/list ([i (in-range (length args*))]
                            [j (in-range (add1 i) (length args*))])
                  (lower-app '!= (list (list-ref args* i) (list-ref args* j)))))]
      [(list '! _ ... body) (loop body env)]
      [(list 'cast arg) (loop arg env)]
      [(list op args ...) (lower-app op (map (curryr loop env) args))]
      [(? operator-exists? op) (lower-app op '())]
      [(? symbol? var) (dict-ref env var var)]
      [_ expr]))

  (loop prog '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FPCore -> LImpl

(define (assert-fpcore-impl op prop-dict ireprs)
  (or (get-fpcore-impl op prop-dict ireprs)
      (raise-herbie-missing-error
       "No implementation for `~a` under rounding context `~a` with types `~a`"
       op
       prop-dict
       (string-join (map (λ (r) (format "<~a>" (representation-name r))) ireprs) " "))))

(define (array-accessor-impl impl)
  (match (impl-info impl 'fpcore)
    [(list 'ref (? symbol?) (? exact-nonnegative-integer?))
     (and (array-representation? (first (impl-info impl 'itype))) impl)]
    [_ #f]))

;; Translates an FPCore operator application into
;; an LImpl operator application.
(define (impl-app op prop-dict args ctx)
  (define ireprs (map (lambda (arg) (repr-of arg ctx)) args))
  (define impl (assert-fpcore-impl op prop-dict ireprs))
  (define vars (impl-info impl 'vars))
  (define-values (_ body) (impl->fpcore impl))
  (define subst (pattern-match body (cons op args)))
  (pattern-substitute (cons impl vars) subst))

;; Translates from FPCore to an LImpl.
(define (fpcore->prog prog ctx)
  (define (fold-app op args prop-dict)
    (for/fold ([out (first args)]) ([arg (in-list (rest args))])
      (impl-app op prop-dict (list out arg) ctx)))

  (define (conjoin args prop-dict)
    (match args
      ['() (loop 'TRUE prop-dict '())]
      [(list arg) arg]
      [_ (fold-app 'and args prop-dict)]))

  (define (loop expr prop-dict env)
    (match expr
      [(? number? n)
       (literal (match n
                  [(or +inf.0 -inf.0 +nan.0) n]
                  [(? exact?) n]
                  [_ (inexact->exact n)])
                (dict-ref prop-dict ':precision))]
      [`(let ([,vars ,vals] ...) ,body)
       ;; Convert each parallel binding once, in the context where it is bound.
       (define env*
         (for/fold ([env* env])
                   ([var (in-list vars)]
                    [val (in-list vals)])
           (dict-set env* var (loop val prop-dict env))))
       (loop body prop-dict env*)]
      [`(let* ([,vars ,vals] ...) ,body)
       ;; Sequential bindings are converted once and extend the environment.
       (define env*
         (for/fold ([env* env])
                   ([var (in-list vars)]
                    [val (in-list vals)])
           (dict-set env* var (loop val prop-dict env*))))
       (loop body prop-dict env*)]
      [`(,(or 'and 'or)) (loop (if (eq? (first expr) 'and) 'TRUE 'FALSE) prop-dict env)]
      [`(,(or '+ '-)) (loop 0 prop-dict env)]
      [`(,(or '* '/)) (loop 1 prop-dict env)]
      [`(,(or '+ '* 'and 'or) ,arg) (loop arg prop-dict env)]
      [`(- ,arg) (impl-app '- prop-dict (list (loop arg prop-dict env)) ctx)]
      [`(/ ,arg) (impl-app '/ prop-dict (list (loop 1 prop-dict env) (loop arg prop-dict env)) ctx)]
      [`(,(and (or '+ '- '* '/ 'and 'or) op) ,first ,second ,rest ...)
       (fold-app op
                 (map (lambda (arg) (loop arg prop-dict env)) (list* first second rest))
                 prop-dict)]
      [`(,(and (or '< '<= '> '>= '=) op) ,args ...)
       (define args* (map (lambda (arg) (loop arg prop-dict env)) args))
       (conjoin (for/list ([term (in-list args*)]
                           [next (in-list (if (null? args*)
                                              '()
                                              (cdr args*)))])
                  (impl-app op prop-dict (list term next) ctx))
                prop-dict)]
      [`(!= ,args ...)
       (define args* (map (lambda (arg) (loop arg prop-dict env)) args))
       (conjoin (for*/list ([i (in-range (length args*))]
                            [j (in-range (add1 i) (length args*))])
                  (impl-app '!= prop-dict (list (list-ref args* i) (list-ref args* j)) ctx))
                prop-dict)]
      [(? operator-exists? op) (impl-app op prop-dict '() ctx)]
      [(? symbol? var) (dict-ref env var var)]
      [(list '! props ... body)
       (loop body
             (if (not (null? props))
                 (apply dict-set* prop-dict props)
                 prop-dict)
             env)]
      [(list 'neg arg) ; non-standard but useful [TODO: remove]
       (define arg* (loop arg prop-dict env))
       (impl-app '- prop-dict (list arg*) ctx)]
      [(list 'cast arg) ; special case: unnecessary casts
       (define arg* (loop arg prop-dict env))
       (define repr (get-representation (dict-ref prop-dict ':precision)))
       (if (equal? (repr-of arg* ctx) repr)
           arg*
           (impl-app 'cast prop-dict (list arg*) ctx))]
      [(list 'ref arr idx)
       (define arr* (loop arr prop-dict env))
       (define arr-repr (repr-of arr* ctx))
       (unless (array-representation? arr-repr)
         (raise-herbie-missing-error "No implementation for `ref` over `~a`"
                                     (representation-name arr-repr)))
       (list (ensure-array-ref-impl! arr-repr idx) arr*)]
      [(list 'array args ...)
       (define args* (map (lambda (arg) (loop arg prop-dict env)) args))
       (define slots (map (lambda (arg) (repr-of arg ctx)) args*))
       (define repr (apply make-array-representation slots))
       (cons (ensure-array-impls! repr) args*)]
      [(list op args ...)
       (define args* (map (lambda (arg) (loop arg prop-dict env)) args))
       (impl-app op prop-dict args* ctx)]))

  (loop prog (repr->prop (context-repr ctx)) '()))

;; Translates from LSpec to an LImpl.
(define (spec->prog prog ctx)
  (define prop-dict (repr->prop (context-repr ctx)))
  (let loop ([expr prog])
    (match expr
      [(? number?)
       (literal (match expr
                  [(or +inf.0 -inf.0 +nan.0) expr]
                  [(? exact?) expr]
                  [_ (inexact->exact expr)])
                (dict-ref prop-dict ':precision))]
      [(? symbol?) expr]
      [(list 'neg arg) (impl-app '- prop-dict (list (loop arg)) ctx)]
      [(list op args ...) (impl-app op prop-dict (map loop args) ctx)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LImpl -> FPCore
;; Translates from LImpl to an FPCore

;; TODO: this process uses a block-like data structure
;; but _without_ deduplication since different use sites
;; of a particular subexpression may have different
;; parent rounding contexts. Would be nice to explore
;; if the block data structure can be used.

;; Instruction vector index
(struct index (v) #:prefab)

;; Translates a literal (LImpl) to an FPCore expr
(define (literal->fpcore x)
  (match x
    [(literal -inf.0 _) '(- INFINITY)]
    [(literal +inf.0 _) 'INFINITY]
    [(literal v (or 'binary64 'binary32))
     (define v* (exact->inexact v))
     (cond
       [(equal? v* +inf.0) 'INFINITY]
       [(equal? v* -inf.0) '(- INFINITY)]
       [else v*])]
    [(literal v _) v]))

;; Step 1.
;; Translates from LImpl to a series of let bindings such that each
;; local variable is bound once and used at most once. The result is an
;; instruction vector, representing the let bindings; the operator
;; implementation for each instruction, and the final "root" operation/literal.
;; Except for let-bound variables, the subexpressions are in FPCore.

(define (prog->let-exprs expr)
  (define instrs '())
  (define (push! impl node)
    (define id (length instrs))
    (set! instrs (cons (cons node impl) instrs))
    (index id))

  (define (munge expr #:root? [root? #f])
    (match expr
      [(? literal?) (literal->fpcore expr)]
      [(? number?) expr]
      [(? symbol?) expr]
      [(approx _ impl) (munge impl)]
      [(list (? impl-exists? impl) args ...)
       (define args* (map munge args))
       (define vars (impl-info impl 'vars))
       (define node (replace-vars (map cons vars args*) (impl-info impl 'fpcore)))
       (if root?
           node
           (push! impl node))]
      [(list spec-op) spec-op]
      [(list spec-op args ...) (list* spec-op (map munge args))]))

  (define root (munge expr #:root? #t))
  (cons (list->vector (reverse instrs)) root))

;; Step 2.
;; Inlines let bindings; let-inlining is generally unsound with
;; rounding properties (the parent context may change),
;; so we only inline those that result in the same operator
;; implementation when converting back from FPCore to LImpl.

(define (inline! root ivec ctx)
  (define global-prop-dict (repr->prop (context-repr ctx)))
  (let loop ([node root]
             [prop-dict global-prop-dict])
    (match node
      [(? number?) node] ; number
      [(? symbol?) node] ; variable
      [(index idx) ; let-bound variable
       ; we check what happens if we inline
       (match-define (cons expr impl) (vector-ref ivec idx))
       (define impl*
         (or (array-accessor-impl impl)
             (match expr
               [(list '! props ... (or (? symbol? op) (list op _ ...)))
                ; rounding context updated parent context
                (define prop-dict*
                  (if (not (null? props))
                      (apply dict-set prop-dict props)
                      prop-dict))
                (assert-fpcore-impl op prop-dict* (impl-info impl 'itype))]
               ; rounding context inherited from parent context
               [(or (? symbol? op) (list op _ ...))
                (assert-fpcore-impl op prop-dict (impl-info impl 'itype))])))
       (cond
         [(equal? impl impl*) ; inlining is safe
          (define expr* (loop expr prop-dict))
          (vector-set! ivec idx #f)
          expr*]
         [else ; inlining is not safe
          (define expr* (loop expr global-prop-dict))
          (vector-set! ivec idx expr*)
          node])]
      [(list '! props ... body) ; explicit rounding context
       (define prop-dict* (props->dict props))
       (define body* (loop body prop-dict*))
       (define new-props
         (for/list ([(k v) (in-dict prop-dict*)]
                    #:unless (and (dict-has-key? prop-dict k) (equal? (dict-ref prop-dict k) v)))
           (list k v)))
       (if (null? new-props)
           body*
           `(! ,@(append* new-props) ,body*))]
      [(list op args ...) ; operator application
       (define args* (map (lambda (e) (loop e prop-dict)) args))
       `(,op ,@args*)])))

;; Step 3.
;; Construct the final FPCore expression using remaining let-bindings
;; and the let-free body from the previous step.

(define (reachable-indices ivec expr)
  (define reachable (mutable-set))
  (let loop ([expr expr])
    (match expr
      [(? number?) (void)]
      [(? symbol?) (void)]
      [(index idx)
       (set-add! reachable idx)
       (loop (vector-ref ivec idx))]
      [(list _ args ...) (for-each loop args)]))
  reachable)

(define (remove-indices id->name expr)
  (let loop ([expr expr])
    (match expr
      [(? number?) expr]
      [(? symbol?) expr]
      [(index idx) (hash-ref id->name idx)]
      [(list '! props ... body) `(! ,@props ,(loop body))]
      [(list op args ...) `(,op ,@(map loop args))])))

(define (build-expr expr ivec ctx)
  ; variable generation
  (define vars (list->mutable-seteq (context-vars ctx)))
  (define counter 0)
  (define (gensym)
    (set! counter (add1 counter))
    (match (string->symbol (format "t~a" counter))
      [(? (curry set-member? vars)) (gensym)]
      [x
       (set-add! vars x)
       x]))

  ; need fresh variables for reachable, non-inlined subexpressions
  (define reachable (reachable-indices ivec expr))
  (define id->name (make-hash))
  (for ([expr (in-vector ivec)]
        [idx (in-naturals)]
        #:when (and expr (set-member? reachable idx)))
    (hash-set! id->name idx (gensym)))

  (for/fold ([body (remove-indices id->name expr)]) ([idx (in-list (sort (hash-keys id->name) >))])
    (define var (hash-ref id->name idx))
    (define val (remove-indices id->name (vector-ref ivec idx)))
    `(let ([,var ,val]) ,body)))

;; Translates from LImpl to an FPCore.
;; The implementation of this procedure is complicated since
;;  (1) every operator implementation requires certain (FPCore) rounding properties
;;  (2) rounding contexts have lexical scoping
(define (prog->fpcore prog ctx)
  ; step 1: convert to an instruction vector where
  ; each expression is evaluated under explicit rounding contexts
  (match-define (cons ivec root) (prog->let-exprs prog))

  ; step 2: inline nodes
  (define body (inline! root ivec ctx))

  ; step 3: construct the actual FPCore expression from
  ; the remaining let-bindings and body
  (build-expr body ivec ctx))
