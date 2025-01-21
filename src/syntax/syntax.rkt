#lang racket

(require math/bigfloat)

(require "../core/rival.rkt"
         "../utils/common.rkt"
         "../utils/errors.rkt"
         "matcher.rkt"
         "types.rkt")

(provide (struct-out literal)
         (struct-out approx)
         (struct-out hole)
         variable?
         constant-operator?
         operator-exists?
         operator-info
         all-operators

         prog->spec

         impl-exists?
         impl-info
         *functions*
         register-function!)

(module+ internals
  (provide define-operator-impl
           register-operator-impl!
           define-operator
           register-operator!
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
(struct operator (name itype otype))

;; All real operators
(define operators (make-hasheq))

;; Checks if an operator has been registered.
(define (operator-exists? op)
  (hash-has-key? operators op))

;; Returns all operators.
(define (all-operators)
  (sort (hash-keys operators) symbol<?))

;; Looks up a property `field` of an real operator `op`.
;; Panics if the operator is not found.
(define/contract (operator-info op field)
  (-> symbol? (or/c 'itype 'otype) any/c)
  (unless (hash-has-key? operators op)
    (error 'operator-info "Unknown operator ~a" op))
  (define info (hash-ref operators op))
  (case field
    [(itype) (operator-itype info)]
    [(otype) (operator-otype info)]))

;; Registers an operator. Panics if the operator already exists.
(define (register-operator! name itypes otype)
  (when (hash-has-key? operators name)
    (error 'register-operator! "operator already registered: ~a" name))
  (hash-set! operators name (operator name itypes otype)))

;; Syntactic form for `register-operator!`
(define-syntax (define-operator stx)
  (syntax-case stx ()
    [(_ (id itype ...) otype _ ...) ; The _ ... is for backwards-compatibility
     (unless (identifier? #'id)
       (raise-syntax-error 'define-operator stx "expected identifier" #'id))
     #'(register-operator! 'id '(itype ...) 'otype)]))

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

(module+ test
  ; check expected number of operators
  (check-equal? (length (all-operators)) 57)

  ; check that Rival supports all non-accelerator operators
  (for ([op (in-list (all-operators))])
    (define vars (map (lambda (_) (gensym)) (operator-info op 'itype)))
    (define disc (discretization 64 #f #f)) ; fake arguments
    (rival-compile (list `(,op ,@vars)) vars (list disc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator implementations
;; Floating-point operations that approximate mathematical operations

;; Operator implementations _approximate_ a program of
;; mathematical operators with fixed input and output representations.
;;
;; An operator implementation requires
;;  - a (unique) name
;;  - input variables/representations
;;  - output representation
;;  - a specification it approximates
;;  - its FPCore representation
;;  - a floating-point implementation
;;
(struct operator-impl (name ctx spec fpcore fl identities))

;; Operator implementation table
;; Tracks implementations that are loaded into Racket's runtime
(define operator-impls (make-hasheq))

;; Looks up a property `field` of an real operator `op`.
;; Panics if the operator is not found.
(define/contract (impl-info impl field)
  (-> symbol? (or/c 'vars 'itype 'otype 'spec 'fpcore 'fl 'identities) any/c)
  (unless (hash-has-key? operator-impls impl)
    (error 'impl-info "Unknown operator implementation ~a" impl))
  (define info (hash-ref operator-impls impl))
  (case field
    [(vars) (context-vars (operator-impl-ctx info))]
    [(itype) (context-var-reprs (operator-impl-ctx info))]
    [(otype) (context-repr (operator-impl-ctx info))]
    [(spec) (operator-impl-spec info)]
    [(fpcore) (operator-impl-fpcore info)]
    [(fl) (operator-impl-fl info)]
    [(identities) (operator-impl-identities info)]))

;; Checks a specification.
(define (check-spec! name ctx spec)
  (define (bad! fmt . args)
    (error name "~a in `~a`" (apply format fmt args) spec))

  (define (type-error! expr actual-ty expect-ty)
    (bad! "expression `~a` has type `~a`, expected `~a`" expr actual-ty expect-ty))

  (match-define (context vars repr var-reprs) ctx)
  (define itypes (map representation-type var-reprs))
  (define otype (representation-type repr))

  (unless (= (length itypes) (length vars))
    (bad! "arity mismatch; expected ~a, got ~a" (length itypes) (length vars)))

  (define env (map cons vars itypes))
  (define actual-ty
    (let type-of ([expr spec])
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
           (bad! "at `~a`, `~a` not an operator" expr op))
         (define itypes (operator-info op 'itype))
         (unless (= (length itypes) (length args))
           (bad! "arity mismatch at `~a`: expected `~a`, got `~a`"
                 expr
                 (length itypes)
                 (length args)))
         (for ([arg (in-list args)]
               [itype (in-list itypes)])
           (define arg-ty (type-of arg))
           (unless (equal? itype arg-ty)
             (type-error! arg arg-ty itype)))
         (operator-info op 'otype)]
        [_ (bad! "expected an expression, got `~a`" expr)])))

  (unless (equal? actual-ty otype)
    (type-error! spec actual-ty otype)))

; Registers an operator implementation `name` with context `ctx` and spec `spec.
; Can optionally specify a floating-point implementation and fpcore translation.
(define/contract (register-operator-impl! name
                                          ctx
                                          spec
                                          #:commutes? [commutes? #f]
                                          #:fl [fl-proc #f]
                                          #:fpcore [fpcore #f]
                                          #:identities [identities #f])
  (->* (symbol? context? any/c)
       (#:commutes? boolean? #:fl (or/c procedure? #f) #:fpcore any/c #:identities any/c)
       void?)
  ; check specification
  (check-spec! name ctx spec)
  (define vars (context-vars ctx))
  ; synthesize operator (if the spec contains exactly one operator)
  (define op
    (match spec
      [(list op (or (? number?) (? symbol?)) ...) op]
      [_ #f]))
  ; check or synthesize FPCore translatin
  (define fpcore*
    (cond
      [fpcore ; provided -> TODO: check free variables, props
       (match fpcore
         [`(! ,props ... (,op ,args ...))
          (unless (even? (length props))
            (error 'register-operator-impl! "~a: umatched property in ~a" name fpcore))
          (unless (symbol? op)
            (error 'register-operator-impl! "~a: expected symbol `~a`" name op))
          (for ([arg (in-list args)]
                #:unless (or (symbol? arg) (number? arg)))
            (error 'register-operator-impl! "~a: expected terminal `~a`" name arg))]
         [`(,op ,args ...)
          (unless (symbol? op)
            (error 'register-operator-impl! "~a: expected symbol `~a`" name op))
          (for ([arg (in-list args)]
                #:unless (or (symbol? arg) (number? arg)))
            (error 'register-operator-impl! "~a: expected terminal `~a`" name arg))]
         [_ (error 'register-operator-impl! "Invalid fpcore for ~a: ~a" name fpcore)])
       fpcore]
      [else ; not provided => need to generate it
       (define repr (context-repr ctx))
       (define bool-repr (get-representation 'bool))
       (if (equal? repr bool-repr)
           `(,op ,@vars) ; special case: boolean-valued operations do not need a precision annotation
           `(! :precision ,(representation-name repr) (,op ,@vars)))]))
  ; check or synthesize floating-point operation
  (define fl-proc*
    (cond
      [fl-proc ; provided => check arity
       (unless (procedure-arity-includes? fl-proc (length vars) #t)
         (error 'register-operator-impl!
                "~a: procedure does not accept ~a arguments"
                name
                (length vars)))
       fl-proc]
      [else ; need to generate
       (define compiler (make-real-compiler (list spec) (list ctx)))
       (define fail ((representation-bf->repr (context-repr ctx)) +nan.bf))
       (procedure-rename (lambda pt
                           (define-values (_ exs) (real-apply compiler pt))
                           (if exs
                               (first exs)
                               fail))
                         name)]))

  ; make hash table
  (define rules '())
  (define rule-names (make-hasheq))
  (define commutes? #f)
  (when identities
    (when commutes?
      (cons (list 'commutes) identities))
    (set! rules
          (for/list ([ident (in-list identities)]
                     [i (in-naturals)])
            (match ident
              [(list ident-name lhs-expr rhs-expr)
               (cond
                 [(hash-has-key? rule-names ident-name)
                  (raise-herbie-syntax-error "Duplicate identity ~a" ident-name)]
                 [(not (well-formed? lhs-expr))
                  (raise-herbie-syntax-error "Ill-formed identity expression ~a" lhs-expr)]
                 [(not (well-formed? rhs-expr))
                  (raise-herbie-syntax-error "Ill-formed identity expression ~a" rhs-expr)]
                 [else
                  (define rule-name (string->symbol (format "~a-~a" ident-name name)))
                  (hash-set! rule-names rule-name #f)
                  (list 'directed rule-name lhs-expr rhs-expr)])]
              [(list 'exact expr)
               (cond
                 [(not (well-formed? expr))
                  (raise-herbie-syntax-error "Ill-formed identity expression ~a" expr)]
                 [else
                  (define rule-name (gensym (string->symbol (format "~a-exact-~a" name i))))
                  (hash-set! rule-names rule-name #f)
                  (list 'exact rule-name expr)])]
              [(list 'commutes)
               (cond
                 [commutes? (error "Commutes identity already defined")]
                 [(hash-has-key? rule-names (string->symbol (format "~a-commutes" name)))
                  (error "Commutes identity already manually defined")]
                 [(not (equal? (length vars) 2))
                  (raise-herbie-syntax-error "Cannot commute a non 2-ary operator")]
                 [else
                  (set! commutes? #t)
                  (define rule-name (string->symbol (format "~a-commutes" name)))
                  (hash-set! rule-names rule-name #f)
                  (list 'commutes rule-name `(,name ,@vars) `(,name ,@(reverse vars)))])]))))

  ; update tables
  (define impl (operator-impl name ctx spec fpcore* fl-proc* rules))
  (hash-set! operator-impls name impl))

(define (well-formed? expr)
  (match expr
    [(? number?) #t]
    [(? variable?) #t]
    [`(,impl ,args ...) (andmap well-formed? args)]
    [_ #f]))

(define-syntax (define-operator-impl stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'define-operator-impl why stx sub-stx))
  (syntax-case stx (:)
    [(_ (id [var : repr] ...) rtype fields ...)
     (let ([id #'id]
           [vars (syntax->list #'(var ...))]
           [fields #'(fields ...)])
       (unless (identifier? id)
         (oops! "expected identifier" id))
       (for ([var (in-list vars)]
             #:unless (identifier? var))
         (oops! "expected identifier" var))
       (define commutes? #f)
       (define spec #f)
       (define core #f)
       (define fl-expr #f)
       (define identities #f)
       (let loop ([fields fields])
         (syntax-case fields ()
           [()
            (unless spec
              (oops! "missing `#:spec` keyword"))
            (with-syntax ([id id]
                          [spec spec]
                          [core core]
                          [commutes? commutes?]
                          [fl-expr fl-expr]
                          [identities identities])
              #'(register-operator-impl! 'id
                                         (context '(var ...)
                                                  (get-representation 'rtype)
                                                  (list (get-representation 'repr) ...))
                                         'spec
                                         #:commutes? 'commutes?
                                         #:fl fl-expr
                                         #:fpcore 'core
                                         #:identities 'identities))]
           [(#:spec expr rest ...)
            (cond
              [spec (oops! "multiple #:spec clauses" stx)]
              [else
               (set! spec #'expr)
               (loop #'(rest ...))])]
           [(#:spec) (oops! "expected value after keyword `#:spec`" stx)]
           [(#:fpcore expr rest ...)
            (cond
              [core (oops! "multiple #:fpcore clauses" stx)]
              [else
               (set! core #'expr)
               (loop #'(rest ...))])]
           [(#:fpcore) (oops! "expected value after keyword `#:fpcore`" stx)]
           [(#:fl expr rest ...)
            (cond
              [fl-expr (oops! "multiple #:fl clauses" stx)]
              [else
               (set! fl-expr #'expr)
               (loop #'(rest ...))])]
           [(#:fl) (oops! "expected value after keyword `#:fl`" stx)]
           [(#:commutes rest ...)
            (cond
              [commutes? (oops! "multiple #:commutes clauses" stx)]
              [else
               (set! commutes? #t)
               (loop #'(rest ...))])]
           [(#:identities (ident-exprs ...) rest ...)
            (cond
              [identities (oops! "multiple #:identities clauses" stx)]
              [else
               (set! identities
                     (let ident-loop ([ident-exprs #'(ident-exprs ...)])
                       (syntax-case ident-exprs ()
                         [() '()]
                         [([name lhs-expr rhs-expr] rem ...)
                          (cons (list #'name #'lhs-expr #'rhs-expr) (ident-loop #'(rem ...)))]
                         [(#:exact expr rem ...) (cons (list 'exact #'expr) (ident-loop #'(rem ...)))]
                         [_ (oops! "bad syntax" ident-exprs)])))
               (loop #'(rest ...))])]
           [(#:identities rest ...) (oops! "expected list of impl identities" stx)]
           [(#:identities) (oops! "expected value after keyword #:identities clause" stx)]
           ; bad
           [_ (oops! "bad syntax" fields)])))]
    [_ (oops! "bad syntax")]))

;; Expression predicates ;;

(define (impl-exists? op)
  (hash-has-key? operator-impls op))

(define (constant-operator? op)
  (and (symbol? op)
       (or (and (hash-has-key? operators op) (null? (operator-itype (hash-ref operators op))))
           (and (hash-has-key? operator-impls op) (null? (impl-info op 'vars))))))

(define (variable? var)
  (and (symbol? var)
       (or (not (hash-has-key? operators var))
           (not (null? (operator-itype (hash-ref operators var)))))
       (or (not (hash-has-key? operator-impls var)) (not (null? (impl-info var 'vars))))))

;; Floating-point expressions require that numbers
;; be rounded to a particular precision.
(struct literal (value precision) #:prefab)

;; An approximation of a specification by
;; a floating-point expression.
(struct approx (spec impl) #:prefab)

;; An unknown floating-point expression that implements a given spec
(struct hole (precision spec) #:prefab)

;; name -> (vars repr body)	;; name -> (vars prec body)
(define *functions* (make-parameter (make-hasheq)))

(define (register-function! name args repr body) ;; Adds a function definition.
  (hash-set! (*functions*) name (list args repr body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LImpl -> LSpec

;; Translates an LImpl to a LSpec.
(define (prog->spec expr)
  (match expr
    [(? literal?) (literal-value expr)]
    [(? variable?) expr]
    [(approx spec _) spec]
    [`(if ,cond ,ift ,iff)
     `(if ,(prog->spec cond)
          ,(prog->spec ift)
          ,(prog->spec iff))]
    [`(,impl ,args ...)
     (define vars (impl-info impl 'vars))
     (define spec (impl-info impl 'spec))
     (define env (map cons vars (map prog->spec args)))
     (pattern-substitute spec env)]))
