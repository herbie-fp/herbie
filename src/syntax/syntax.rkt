#lang racket

(require math/bigfloat)

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "../core/rival.rkt"
         "types.rkt")

(provide (rename-out [operator-or-impl? operator?])
         (struct-out literal)
         variable?
         constant-operator?
         operator-exists?
         operator-deprecated?
         operator-accelerator?
         operator-info
         all-operators
         all-constants
         all-accelerators
         expand-accelerators
         impl-exists?
         impl-info
         impl->operator
         operator-all-impls
         operator-active-impls
         activate-operator-impl!
         clear-active-operator-impls!
         *functions*
         register-function!
         get-parametric-operator
         get-parametric-constant
         get-cast-impl
         generate-cast-impl
         cast-impl?)

(module+ internals
  (provide define-operator-impl
           register-operator-impl!
           define-operator
           register-operator!
           register-conversion-generator!
           variable?))

(module+ test
  (require rackunit
           rival))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Real operators
;; Pure mathematical operations

;; TODO: specs should really be associated with impls
;; unfortunately Herbie still mandates that every impl
;; has an associated operator so the spec is here

;; A real operator requires
;;  - a (unique) name
;;  - input and output types
;;  - optionally a specification [#f by default]
;;  - optionally a deprecated? flag [#f by default]
;; Operator implementations _implement_ a real operator
;; for a particular set of input and output representations.
(struct operator (name itype otype spec deprecated))

;; All real operators
(define operators (make-hasheq))

;; Checks if an operator has been registered.
(define (operator-exists? op)
  (hash-has-key? operators op))

;; Checks if an operator has been registered as deprecated.
(define (operator-deprecated? op)
  (operator-deprecated (hash-ref operators op)))

;; Checks if an operator is an "accelerator".
(define (operator-accelerator? op)
  (and (hash-has-key? operators op) (operator-spec (hash-ref operators op))))

;; Returns all operators.
(define (all-operators)
  (sort (hash-keys operators) symbol<?))

;; Returns all constant operators (operators with no arguments).
(define (all-constants)
  (sort (for/list ([(name rec) (in-hash operators)] #:when (null? (operator-itype rec)))
          name)
        symbol<?))

;; Returns all "accelerator" operators
(define (all-accelerators)
  (sort (for/list ([(name rec) (in-hash operators)] #:when (operator-spec rec))
          name)
        symbol<?))

;; Looks up a property `field` of an real operator `op`.
;; Panics if the operator is not found.
(define/contract (operator-info op field)
  (-> symbol? (or/c 'itype 'otype 'spec) any/c)
  (unless (hash-has-key? operators op)
    (error 'operator-info "Unknown operator ~a" op))
  (define info (hash-ref operators op))
  (case field
    [(itype) (operator-itype info)]
    [(otype) (operator-otype info)]
    [(spec) (operator-spec info)]))

;; Map from operator to its implementations
(define operators-to-impls (make-hasheq))

;; All implementations of an operator `op`.
;; Panics if the operator is not found.
(define (operator-all-impls op)
  (unless (hash-has-key? operators op)
    (error 'operator-info "Unknown operator ~a" op))
  (hash-ref operators-to-impls op))

;; Checks an "accelerator" specification
(define (check-accelerator-spec! name itypes otype spec)
  (define (bad! fmt . args)
    (error name "~a in `~a`" (apply format fmt args) spec))

  (define (type-error! expr actual-ty expect-ty)
    (bad! "expression `~a` has type `~a`, expected `~a`" expr actual-ty expect-ty))

  (define-values (vars body)
    (match spec
      [`(,(or 'lambda 'λ) (,vars ...) ,spec)
       (for ([var (in-list vars)])
         (unless (symbol? var)
           (bad! "expected symbol `~a` in `~a`" var spec)))
       (values vars spec)]
      [_ (bad! "malformed specification, expected `(lambda <vars> <expr>)`")]))

  (unless (= (length itypes) (length vars))
    (bad! "arity mismatch; expected ~a, got ~a" (length itypes) (length vars)))

  (define env (map cons vars itypes))
  (define actual-ty
    (let type-of ([expr body])
      (match expr
        [(? number?) 'real]
        [(? symbol?)
         (cond
           [(assq expr env)
            =>
            cdr]
           [else (bad! "unbound variable `~a`" expr)])]
        [`(if ,cond ,ift ,iff)
         (define cond-ty (type-of cond))
         (unless (equal? cond-ty 'bool)
           (type-error! cond cond-ty 'bool))
         (define ift-ty (type-of ift))
         (define iff-ty (type-of iff))
         (unless (equal? ift-ty iff-ty)
           (type-error! iff iff-ty ift-ty))
         ift-ty]
        [`(,op ,args ...)
         (unless (operator-exists? op)
           (bad! "expected operator at `~a`, got `~a` in `~a`" expr op))
         (define itypes (operator-info op 'itype))
         (for ([arg (in-list args)] [itype (in-list itypes)])
           (define arg-ty (type-of arg))
           (unless (equal? itype arg-ty)
             (type-error! arg arg-ty itype)))
         (operator-info op 'otype)]
        [_ (bad! "expected an expression, got `~a`" expr)])))

  (unless (equal? actual-ty otype)
    (type-error! body actual-ty otype)))

;; Applies a substitution.
;; Slightly different than `replace-vars` in `programs.rkt`.
(define (replace-vars expr env)
  (let loop ([expr expr])
    (match expr
      [(? number?) expr]
      [(? symbol?) (cdr (assq expr env))]
      [`(,op ,args ...) `(,op ,@(map loop args))])))

;; Expands an "accelerator" specification.
;; Any nested accelerator is unfolded into its definition.
(define (expand-accelerators spec)
  (let loop ([expr spec])
    (match expr
      [(? number?) expr]
      [(? symbol?) expr]
      [`(,(? operator-accelerator? op) ,args ...)
       (define spec (operator-info op 'spec))
       (match-define `(,(or 'lambda 'λ) (,vars ...) ,body) spec)
       (define env (map cons vars (map loop args)))
       (replace-vars body env)]
      [`(,op ,args ...) `(,op ,@(map loop args))])))

;; Registers an operator with an attribute mapping.
;; Panics if an operator with name `name` has already been registered.
;; By default, the input types are specified by `itypes`, the output type
;; is specified by `otype`, and the operator is not deprecated; but
;; `attrib-dict` can override these properties.
(define (register-operator! name itypes otype attrib-dict)
  (when (hash-has-key? operators name)
    (error 'register-operator! "operator already registered: ~a" name))
  ; extract relevant fields
  (define itypes* (dict-ref attrib-dict 'itype itypes))
  (define otype* (dict-ref attrib-dict 'otype otype))
  (define spec (dict-ref attrib-dict 'spec #f))
  (define deprecated? (dict-ref attrib-dict 'deprecated #f))
  ; check the spec if it is provided
  (when spec
    (check-accelerator-spec! name itypes otype spec)
    (set! spec (expand-accelerators spec)))
  ; update tables
  (define info (operator name itypes* otype* spec deprecated?))
  (hash-set! operators name info)
  (hash-set! operators-to-impls name '()))

;; Syntactic form for `register-operator!`.
;; Special translations for
(define-syntax (define-operator stx)
  (define (bad! why [what #f])
    (raise-syntax-error 'define-operator why stx what))

  (define (attribute-val key val)
    (syntax-case key (spec)
      [spec
       (with-syntax ([val val])
         (syntax 'val))]
      [_ val]))

  (syntax-case stx ()
    [(_ (id itype ...) otype [key val] ...)
     (let ([id #'id] [keys (syntax->list #'(key ...))] [vals (syntax->list #'(val ...))])
       (unless (identifier? id)
         (bad! "expected identifier" id))
       (with-syntax ([id id] [(val ...) (map attribute-val keys vals)])
         #'(register-operator! 'id '(itype ...) 'otype (list (cons 'key val) ...))))]))

(define-syntax define-operators
  (syntax-rules (: ->)
    [(_ [name : itype ... -> otype] ...)
     (begin
       (define-operator (name itype ...) otype) ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rival-supported operators

; real constants (encoded as nullary operators)
(define-operators
  [PI : -> real]
  [E : -> real]
  [INFINITY : -> real]
  [NAN : -> real])

; boolean constants (encoded as nullary operators)
(define-operators
  [TRUE : -> bool]
  [FALSE : -> bool])

; boolean operators
(define-operators
  [not : bool -> bool]
  [and : bool bool -> bool]
  [or : bool bool -> bool])

; real-boolean operators
(define-operators
  [== : real real -> bool]
  [!= : real real -> bool]
  [< : real real -> bool]
  [> : real real -> bool]
  [<= : real real -> bool]
  [>= : real real -> bool])

; real operators
(define-operators
  [acos : real -> real]
  [acosh : real -> real]
  [asin : real -> real]
  [asinh : real -> real]
  [atan : real -> real]
  [atanh : real -> real]
  [cbrt : real -> real]
  [ceil : real -> real]
  [cos : real -> real]
  [cosh : real -> real]
  [erf : real -> real]
  [exp : real -> real]
  [exp2 : real -> real]
  [fabs : real -> real]
  [floor : real -> real]
  [lgamma : real -> real]
  [log : real -> real]
  [log10 : real -> real]
  [log2 : real -> real]
  [logb : real -> real]
  [neg : real -> real]
  [rint : real -> real]
  [round : real -> real]
  [sin : real -> real]
  [sinh : real -> real]
  [sqrt : real -> real]
  [tan : real -> real]
  [tanh : real -> real]
  [tgamma : real -> real]
  [trunc : real -> real]
  [+ : real real -> real]
  [- : real real -> real]
  [* : real real -> real]
  [/ : real real -> real]
  [atan2 : real real -> real]
  [copysign : real real -> real]
  [fdim : real real -> real]
  [fmax : real real -> real]
  [fmin : real real -> real]
  [fmod : real real -> real]
  [pow : real real -> real]
  [remainder : real real -> real])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accelerator operators

(define-operator (cast real) real [spec (lambda (x) x)])

(define-operator (erfc real) real [spec (lambda (x) (- 1 (erf x)))])

(define-operator (expm1 real) real [spec (lambda (x) (- (exp x) 1))])

(define-operator (log1p real) real [spec (lambda (x) (log (+ 1 x)))])

(define-operator (hypot real real) real [spec (lambda (x y) (sqrt (+ (* x x) (* y y))))])

(define-operator (fma real real real) real [spec (lambda (x y z) (+ (* x y) z))])

(module+ test
  ; check expected number of operators
  (check-equal? (length (all-operators)) 63)

  ; check that Rival supports all non-accelerator operators
  (for ([op (in-list (all-operators))] #:unless (operator-accelerator? op))
    (define vars (map (lambda (_) (gensym)) (operator-info op 'itype)))
    (define disc (discretization 64 #f #f)) ; fake arguments
    (rival-compile (list `(,op ,@vars)) vars (list disc)))

  ; test accelerator operator
  ; log1pmd(x) = log1p(x) - log1p(-x)
  (define-operator (log1pmd real) real [spec (lambda (x) (- (log1p x) (log1p (neg x))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator implementations
;; Floating-point operations that approximate mathematical operations

;; Operator implementations
;; An "operator implementation" implements a mathematical operator for
;; a particular set of representations satisfying the types described
;; by the `itype` and `otype` properties of the operator.
(struct operator-impl (name op ctx spec fpcore fl))

;; Operator implementation table
;; Tracks implementations that are loaded into Racket's runtime
(define operator-impls (make-hasheq))

;; "Active" operator set
;; Tracks implementations that will be used by Herbie during the improvement loop.
;; Guaranteed to be a subset of the `operator-impls` table.
(define active-operator-impls (mutable-set))

;; Looks up a property `field` of an real operator `op`.
;; Panics if the operator is not found.
(define/contract (impl-info impl field)
  (-> symbol? (or/c 'itype 'otype 'fl) any/c)
  (unless (hash-has-key? operator-impls impl)
    (error 'impl-info "Unknown operator implementation ~a" impl))
  (define info (hash-ref operator-impls impl))
  (case field
    [(itype) (context-var-reprs (operator-impl-ctx info))]
    [(otype) (context-repr (operator-impl-ctx info))]
    [(spec) (operator-impl-spec info)]
    [(fpcore) (operator-impl-fpcore info)]
    [(fl) (operator-impl-fl info)]))

;; Like `operator-all-impls`, but filters for only active implementations.
(define (operator-active-impls name)
  (filter (curry set-member? active-operator-impls) (operator-all-impls name)))

;; Looks up the name of an operator corresponding to an implementation `name`.
;; Panics if the operator is not found.
(define (impl->operator name)
  (unless (hash-has-key? operator-impls name)
    (raise-herbie-missing-error "Unknown operator implementation ~a" name))
  (define impl (hash-ref operator-impls name))
  (operator-name (operator-impl-op impl)))

;; Activates an implementation.
;; Panics if the operator is not found.
(define (activate-operator-impl! name)
  (unless (hash-has-key? operator-impls name)
    (raise-herbie-missing-error "Unknown operator implementation ~a" name))
  (set-add! active-operator-impls name))

;; Clears the table of active implementations.
(define (clear-active-operator-impls!)
  (set-clear! active-operator-impls))

;; Registers an operator implementation `name`
;; fl, spec,, and fpcore can be synthesize from an operator
(define (register-operator-impl! op
                                  name
                                  args
                                  orepr
                                  #:fl [fl #f]
                                  #:spec [spec #f]
                                  #:fpcore [fpcore #f])
  ;; Check if spec is given (if not, infer it from the operator which is required)
  (define vars (map car args))
  (unless spec
    (unless op
      (raise-herbie-syntax-error "Missing required operator"))
    (set! spec `(,op ,@vars)))

  (define new-op op)
  (if op
      op
      (let loop ([expr spec] [operator #f])
        (match expr
          [`(,(? symbol? op) ,args ...)
           (if (null? args)
               (set! new-op op)
               (for ([a (in-list args)])
                 (if operator
                     (raise-herbie-syntax-error "Could not infer operator from ~a" spec)
                     (loop a op))))]
          [_ (set! new-op operator)])))

  (define bool-repr (get-representation 'bool))
  (if fpcore
      ;; Verify fpcore is well formed
      (match fpcore
        [`(! ,props ... (,operator ,args ...)) (void)]
        [`(,operator ,args ...) (void)]
        [_ (raise-herbie-syntax-error "Invalid fpcore given" fpcore)])
      (if (equal? orepr bool-repr)
          (set! fpcore `(,new-op ,@vars))
          (set! fpcore `(! :precision ,(representation-name orepr) (,new-op ,@vars)))))

  (define op-info
    (hash-ref
     operators
     new-op
     (lambda ()
       (raise-herbie-missing-error "Cannot register `~a`, operator `~a` does not exist" name op))))

  ; check arity and types
  (define ireprs (map cdr args))
  (define itypes (operator-itype op-info))
  (define otype (operator-otype op-info))
  (define expect-arity (length itypes))
  (define actual-arity (length ireprs))
  (unless (= expect-arity actual-arity)
    (raise-herbie-missing-error
     "Cannot register `~a` as an implementation of `~a`: expected ~a arguments, got ~a"
     name
     new-op
     expect-arity
     actual-arity))
  (for ([repr (in-list (cons orepr ireprs))] [type (in-list (cons otype itypes))])
    (unless (equal? (representation-type repr) type)
      "Cannot register `~a` as implementation of `~a`: ~a is not a representation of ~a"
      name
      new-op
      repr
      type))

  ;; Synthesizes a correctly-rounded floating-point implemenation
  (define (synth-fl-impl name vars spec)
    (define ctx (context vars orepr ireprs))
    (define compiler (make-real-compiler (list spec) (list ctx)))
    (define fail ((representation-bf->repr orepr) +nan.bf))
    (procedure-rename (lambda pt
                        (define-values (_ exs) (real-apply compiler pt))
                        (if exs (first exs) fail))
                      (sym-append 'synth: name)))

  ;; Get floating-point implementation
  (define fl-proc
    (cond
      [fl fl] ; user-provided implementation
      [(operator-accelerator? new-op) ; Rival-synthesized accelerator implementation
       (match-define `(,(or 'lambda 'λ) (,vars ...) ,body) (operator-spec op-info))
       (synth-fl-impl name vars body)]
      [else ; Rival-synthesized operator implementation
       (define vars (build-list (length ireprs) (lambda (i) (string->symbol (format "x~a" i)))))
       (synth-fl-impl name vars `(,new-op ,@vars))]))

  ; update tables
  (define impl (operator-impl name op-info (context vars orepr ireprs) spec fpcore fl-proc))
  (hash-set! operator-impls name impl)
  (hash-update! operators-to-impls new-op (curry cons name)))

(define-syntax (define-operator-impl stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'define-impl why stx sub-stx))
  (syntax-case stx ()
    [(_ (id [var : repr] ...) rtype fields ...)
     (let loop ([fields #'(fields ...)] [operator #f] [spec #f] [core #f] [fl-expr #f])
       (syntax-case fields ()
         [()
          (let ([impl-id #'id])
            (unless (identifier? impl-id)
              (oops! "impl id is not a valid identifier" impl-id))
            (with-syntax
                ([impl-id impl-id] [operator operator] [spec spec] [core core] [fl-expr fl-expr])
              #'(register-operator-impl! 'operator
                                          'impl-id
                                          (list (cons 'var (get-representation 'repr)) ...)
                                          (get-representation 'rtype)
                                          #:fl 'fl-expr
                                          #:spec 'spec
                                          #:fpcore 'core)))]
         [(#:spec expr rest ...) (loop #'(rest ...) operator #'expr core fl-expr)]
         [(#:fpcore expr rest ...) (loop #'(rest ...) operator spec #'expr fl-expr)]
         [(#:fl expr rest ...) (loop #'(rest ...) operator spec core #'expr)]
         [(#:op name rest ...) (loop #'(rest ...) #'name spec core fl-expr)]
         [_ (oops! "bad syntax" fields)]))]
    [_ (oops! "bad syntax")]))

;; Among active implementations, looks up an implementation with
;; the operator name `name` and argument representations `ireprs`.
(define (get-parametric-operator #:all? [all? #f] name . ireprs)
  (define get-impls (if all? operator-all-impls operator-active-impls))
  (let/ec k
          (for/first ([impl (get-impls name)] #:when (equal? (impl-info impl 'itype) ireprs))
            (k impl))
          (raise-herbie-missing-error
           "Could not find operator implementation for ~a with ~a"
           name
           (string-join (map (λ (r) (format "<~a>" (representation-name r))) ireprs) " "))))

;; Among active implementations, looks up an implementation of
;; a constant (nullary operator) with the operator name `name`
;; and representation `repr`.
(define (get-parametric-constant name repr #:all? [all? #f])
  (define get-impls (if all? operator-all-impls operator-active-impls))
  (let/ec k
          (for ([impl (get-impls name)])
            (define rtype (impl-info impl 'otype))
            (when (or (equal? rtype repr) (equal? (representation-type rtype) 'bool))
              (k impl)))
          (raise-herbie-missing-error "Could not find constant implementation for ~a with ~a"
                                      name
                                      (format "<~a>" (representation-name repr)))))

(module+ test
  (require math/flonum
           math/bigfloat
           (submod "types.rkt" internals))

  (define (shift bits fn)
    (define shift-val (expt 2 bits))
    (λ (x) (fn (- x shift-val))))

  (define (unshift bits fn)
    (define shift-val (expt 2 bits))
    (λ (x) (+ (fn x) shift-val)))

  ; for testing: also in <herbie>/reprs/binary64.rkt
  (define-representation (binary64 real flonum?)
                         bigfloat->flonum
                         bf
                         (shift 63 ordinal->flonum)
                         (unshift 63 flonum->ordinal)
                         64
                         (conjoin number? nan?))

  ; correctly-rounded log1pmd(x) for binary64
  (define-operator-impl (log1pmd.f64 [x : binary64])
                         binary64
                         #:spec (- (log1p x) (log1p (neg x)))
                         #:fpcore (! :precision binary64 (log1pmd x))
                         #:fl log1pmd)
  ; correctly-rounded sin(x) for binary64
  (define-operator-impl (sin.acc.f64 [x : binary64])
                         binary64
                         #:spec (sin x)
                         #:fpcore (! :precision binary64 (sin x))
                         #:fl sin)

  (define log1pmd-proc (impl-info 'log1pmd.f64 'fl))
  (define log1pmd-vals '((0.0 . 0.0) (0.5 . 1.0986122886681098) (-0.5 . -1.0986122886681098)))
  (for ([(pt out) (in-dict log1pmd-vals)])
    (check-equal? (log1pmd-proc pt) out (format "log1pmd(~a) = ~a" pt out)))

  (define sin-proc (impl-info 'sin.acc.f64 'fl))
  (define sin-vals '((0.0 . 0.0) (1.0 . 0.8414709848078965) (-1.0 . -0.8414709848078965)))
  (for ([(pt out) (in-dict sin-vals)])
    (check-equal? (sin-proc pt) out (format "sin(~a) = ~a" pt out)))

  (void))

;; Casts and precision changes

(define (cast-impl? x)
  (and (symbol? x) (set-member? (operator-all-impls 'cast) x)))

(define (get-cast-impl irepr orepr #:all? [all? #f])
  (define get-impls (if all? operator-all-impls operator-active-impls))
  (for/or ([name (get-impls 'cast)])
    (and (equal? (impl-info name 'otype) orepr) (equal? (first (impl-info name 'itype)) irepr) name)))

; Similar to representation generators, conversion generators
; allow Herbie to query plugins for optimized implementations
; of representation conversions, rather than the default
; bigfloat implementation
(define conversion-generators '())

(define/contract (register-conversion-generator! proc)
  (-> (-> any/c any/c boolean?) void?)
  (unless (set-member? conversion-generators proc)
    (set! conversion-generators (cons proc conversion-generators))))

(define (generate-cast-impl irepr orepr)
  (match (get-cast-impl irepr orepr)
    [#f
     (for/first ([gen (in-list conversion-generators)])
       (gen (representation-name irepr) (representation-name orepr)))]
    [impl impl]))

;; Expression predicates ;;

(define (impl-exists? op)
  (hash-has-key? operator-impls op))

(define (operator-or-impl? op)
  (and (symbol? op)
       (not (equal? op 'if))
       (or (hash-has-key? operators op) (hash-has-key? operator-impls op))))

(define (constant-operator? op)
  (and (symbol? op)
       (or (and (hash-has-key? operators op) (null? (operator-itype (hash-ref operators op))))
           (and (hash-has-key? operator-impls op)
                (null? (context-vars (operator-impl-ctx (hash-ref operator-impls op))))))))

(define (variable? var)
  (and (symbol? var)
       (or (not (hash-has-key? operators var))
           (not (null? (operator-itype (hash-ref operators var)))))
       (or (not (hash-has-key? operator-impls var))
           (not (null? (context-vars (operator-impl-ctx (hash-ref operator-impls var))))))))

;; Floating-point expressions require that number
;; be rounded to a particular precision.
(struct literal (value precision) #:prefab)

;; name -> (vars repr body)	;; name -> (vars prec body)
(define *functions* (make-parameter (make-hasheq)))

(define (register-function! name args repr body) ;; Adds a function definition.
  (hash-set! (*functions*) name (list args repr body)))
