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
         operator-info
         all-operators
         all-constants
         impl-exists?
         impl-info
         impl->operator
         all-operator-impls
         (rename-out [all-active-operator-impls active-operator-impls])
         operator-all-impls
         operator-active-impls
         activate-operator-impl!
         clear-active-operator-impls!
         *functions*
         register-function!
         get-fpcore-impl
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
;;  - optionally a deprecated? flag [#f by default]
(struct operator (name itype otype deprecated))

;; All real operators
(define operators (make-hasheq))

;; Checks if an operator has been registered.
(define (operator-exists? op)
  (hash-has-key? operators op))

;; Checks if an operator has been registered as deprecated.
(define (operator-deprecated? op)
  (operator-deprecated (hash-ref operators op)))

;; Returns all operators.
(define (all-operators)
  (sort (hash-keys operators) symbol<?))

;; Returns all constant operators (operators with no arguments).
(define (all-constants)
  (sort (for/list ([(name rec) (in-hash operators)] #:when (null? (operator-itype rec)))
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
    [(otype) (operator-otype info)]))

;; Map from operator to its implementations
(define operators-to-impls (make-hasheq))

;; All implementations of an operator `op`.
;; Panics if the operator is not found.
(define (operator-all-impls op)
  (unless (hash-has-key? operators op)
    (error 'operator-info "Unknown operator ~a" op))
  (hash-ref operators-to-impls op))

;; Checks a specification
(define (check-spec! name itypes otype spec)
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
  (define deprecated? (dict-ref attrib-dict 'deprecated #f))
  ; update tables
  (define info (operator name itypes* otype* deprecated?))
  (hash-set! operators name info)
  (hash-set! operators-to-impls name '()))

;; Syntactic form for `register-operator!`
(define-syntax (define-operator stx)
  (define (bad! why [what #f])
    (raise-syntax-error 'define-operator why stx what))
  (syntax-case stx ()
    [(_ (id itype ...) otype [key val] ...)
     (let ([id #'id])
       (unless (identifier? id)
         (bad! "expected identifier" id))
       (with-syntax ([id id])
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

(define-operators
  [cast : real -> real]
  [erfc : real -> real]
  [expm1 : real -> real]
  [log1p : real -> real]
  [hypot : real real -> real]
  [fma : real real real -> real])

(module+ test
  ; check expected number of operators
  (check-equal? (length (all-operators)) 63)

  ; check that Rival supports all non-accelerator operators
  (for ([op (in-list (all-operators))])
    (define vars (map (lambda (_) (gensym)) (operator-info op 'itype)))
    (define disc (discretization 64 #f #f)) ; fake arguments
    (rival-compile (list `(,op ,@vars)) vars (list disc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator implementations
;; Floating-point operations that approximate mathematical operations

;; An operator implementation requires
;;  - a (unique) name
;;  - input and output representations
;;  - a specification it approximates
;;  - its FPCore representation
;;  - an implementation
;; Operator implementations _approximate_ a program of
;; mathematical operators with fixed input and output representations.
(struct operator-impl (name op itype otype spec fpcore fl))

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
  (-> symbol? (or/c 'itype 'otype 'spec 'fpcore 'fl) any/c)
  (unless (hash-has-key? operator-impls impl)
    (error 'impl-info "Unknown operator implementation ~a" impl))
  (define info (hash-ref operator-impls impl))
  (case field
    [(itype) (operator-impl-itype info)]
    [(otype) (operator-impl-otype info)]
    [(spec) (operator-impl-spec info)]
    [(fpcore) (operator-impl-fpcore info)]
    [(fl) (operator-impl-fl info)]))

;; Returns all operator implementations.
(define (all-operator-impls)
  (sort (hash-keys operator-impls) symbol<?))

;; Returns all active operator implementations.
(define (all-active-operator-impls)
  (sort (set->list active-operator-impls) symbol<?))

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

;; Registers an operator implementation `name` or real operator `op`.
;; The input and output representations must satisfy the types
;; specified by the `itype` and `otype` fields for `op`.
(define/contract (register-operator-impl! op name ireprs orepr attrib-dict)
  (-> symbol? symbol? (listof representation?) representation? (listof pair?) void?)
  (define op-info
    (hash-ref
     operators
     op
     (lambda ()
       (raise-herbie-missing-error "Cannot register `~a`, operator `~a` does not exist" name op))))

  ; extract or generate the spec
  (define spec
    (match (dict-ref attrib-dict 'spec #f)
      ; not provided => need to generate it
      [#f
       (define vars (gen-vars (length ireprs)))
       `(lambda ,vars (,op ,@vars))]
      ; provided => check for syntax and types
      [spec
       (check-spec! name (map representation-type ireprs) (representation-type orepr) spec)
       spec]))

  ; extract or generate the fpcore translation
  (match-define `(,(or 'lambda 'λ) ,vars ,body) spec)
  (define fpcore
    (match (dict-ref attrib-dict 'fpcore #f)
      ; not provided => need to generate it
      [#f
       ; special case: boolean-valued operations do not
       ; need a precision annotation
       (if (equal? orepr (get-representation 'bool))
           `(,op ,@vars)
           `(! :precision ,(representation-name orepr) (,op ,@vars)))]
      ; provided -> TODO: check free variables
      [fpcore fpcore]))

  ; extract or generate floating-point implementation
  (define fl-proc
    (match (dict-ref attrib-dict 'fl #f)
      ; not provided => need to generate it
      [#f
       (define ctx (context vars orepr ireprs))
       (define compiler (make-real-compiler (list body) (list ctx)))
       (define fail ((representation-bf->repr orepr) +nan.bf))
       (procedure-rename (lambda pt
                           (define-values (_ exs) (real-apply compiler pt))
                           (if exs (first exs) fail))
                         (sym-append 'synth: name))]
      ; provided
      [(? procedure? proc)
       (define expect-arity (length ireprs))
       (unless (procedure-arity-includes? proc expect-arity #t)
         (error 'register-operator-impl!
                "~a: procedure does not accept ~a arguments"
                name
                expect-arity))
       proc]
      ; not a procedure
      [bad
       (error 'register-operator-impl! "~a: expected a procedure with attribute 'fl ~a" name bad)]))

  ; update tables
  (define impl (operator-impl name op-info ireprs orepr spec fpcore fl-proc))
  (hash-set! operator-impls name impl)
  (hash-update! operators-to-impls op (curry cons name)))

;; Syntactic form for `register-operator-impl!`
(define-syntax (define-operator-impl stx)
  (define (bad! why [what #f])
    (raise-syntax-error 'define-operator-impl why stx what))

  (define (attribute-val key val)
    (with-syntax ([val val])
      (syntax-case key (spec fpcore)
        [spec #''val]
        [fpcore #''val]
        [_ #'val])))

  (syntax-case stx ()
    [(_ (op id itype ...) otype [key val] ...)
     (let ([id #'id] [keys (syntax->list #'(key ...))] [vals (syntax->list #'(val ...))])
       (unless (identifier? id)
         (bad! "expected identifier" id))
       (with-syntax ([id id] [(val ...) (map attribute-val keys vals)])
         #'(register-operator-impl! 'op
                                    'id
                                    (list (get-representation 'itype) ...)
                                    (get-representation 'otype)
                                    (list (cons 'key val) ...))))]))

;; Unions two bindings. Returns #f if they disagree.
(define (merge-bindings binding1 binding2)
  (and binding1
       binding2
       (let/ec quit
               (for/fold ([binding binding1]) ([(k v) (in-dict binding2)])
                 (dict-update binding k (λ (x) (if (equal? x v) v (quit #f))) v)))))

;; Pattern matcher that returns a substitution or #f.
;; A substitution is an association list of symbols and expressions.
(define (pattern-match pattern expr)
  (match* (pattern expr)
    [((? number?) _) (and (equal? pattern expr) '())]
    [((? variable?) _) (list (cons pattern expr))]
    [((list phead prest ...) (list head rest ...))
     (and (equal? phead head)
          (= (length prest) (length rest))
          (for/fold ([bindings '()]) ([pat (in-list prest)] [term (in-list rest)])
            (merge-bindings bindings (pattern-match pat term))))]
    [(_ _) #f]))

;; Checks if two specs are syntactically equivalent modulo renaming.
;; This is just pattern matching.
(define (spec-equal? spec1 spec2)
  ; force result of `pattern-match` to be a boolean
  (and (pattern-match spec1 spec2) #t))

;; Returns the list of implementations that implement a given spec
;; and have the given input and output representations.
; (define (get-impls spec ireprs orepr #:impls [impls (all-active-operator-impls)])
;   (reap [sow]
;         (for ([impl (in-list impls)])
;           (when (and (equal? orepr (impl-info impl 'otype))
;                      (equal? ireprs (impl-info impl 'itype))
;                      (let ()
;                        (match-define (list _ _ spec*) (impl-info impl 'spec))
;                        (spec-equal? spec spec*)))
;             (sow impl)))))

;; Finds the best operator implemenation for a given
;; FPCore expression, input representations, and rounding properties.
;; Panics if none can be found.
(define (get-fpcore-impl expr ireprs prop-dict #:impls [all-impls (all-active-operator-impls)])
  ; ensure `':precision` is in the prop list
  (unless (dict-has-key? prop-dict ':precision)
    (error 'get-impl "expected key ':precision in properties `~a`" prop-dict))
  (define orepr (get-representation (dict-ref prop-dict ':precision)))
  ; gather all implementations that have the same spec,
  ; input and output representations, and its FPCore translation
  ; has properties that are found in `prop-dict`
  (define impls
    (reap [sow]
          (for ([impl (in-list all-impls)])
            (when (and (equal? orepr (impl-info impl 'otype))
                       (equal? ireprs (impl-info impl 'itype))
                       (let ()
                         (match-define (list '! props ... body) (impl-info impl 'fpcore))
                         (define prop-dict* (props->dict props))
                         (and (andmap (lambda (prop) (member prop prop-dict)) prop-dict*)
                              (spec-equal? expr body))))
              (sow impl)))))
  ; check that we have any matching impls
  (when (null? impls)
    (raise-herbie-missing-error
     "No implementation for `~a` with type `~a -> ~a`"
     expr
     (string-join (map (λ (r) (format "<~a>" (representation-name r))) ireprs) " ")
     (format "<~a>" (representation-name orepr))))
  ; ; we rank implementations and select the highest scoring one
  (define scores
    (for/list ([impl (in-list impls)])
      (match-define (list '! props ... _) (impl-info impl 'fpcore))
      (define prop-dict* (props->dict props))
      (define matching (filter (lambda (prop) (member prop prop-dict*)) prop-dict))
      (cons (length matching) (- (length prop-dict) (length matching)))))
  ; select the best implementation
  ; sort first by the number of matched properties,
  ; then tie break on the number of extraneous properties
  (match-define (list (cons _ best) _ ...)
    (sort (map cons scores impls)
          (lambda (x y)
            (cond
              [(> (car x) (car y)) #t]
              [(< (car x) (car y)) #f]
              [else (> (cdr x) (cdr y))]))
          #:key car))
  best)

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
                (null? (operator-impl-itype (hash-ref operator-impls op)))))))

(define (variable? var)
  (and (symbol? var)
       (or (not (hash-has-key? operators var))
           (not (null? (operator-itype (hash-ref operators var)))))
       (or (not (hash-has-key? operator-impls var))
           (not (null? (operator-impl-itype (hash-ref operator-impls var)))))))

;; Floating-point expressions require that number
;; be rounded to a particular precision.
(struct literal (value precision) #:prefab)

;; name -> (vars repr body)	;; name -> (vars prec body)
(define *functions* (make-parameter (make-hasheq)))

(define (register-function! name args repr body) ;; Adds a function definition.
  (hash-set! (*functions*) name (list args repr body)))
