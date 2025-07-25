#lang racket

(require math/bigfloat
         racket/hash)

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "matcher.rkt"
         "types.rkt"
         "generators.rkt")

(provide (struct-out literal)
         (struct-out approx)
         (struct-out hole)
         operator-exists?
         operator-info
         all-operators ; return a list of operators names
         operators ; returns hash of operators
         *functions*
         register-function!
         make-operator-impl
         fpcore-context
         (struct-out operator-impl) ; required by platform.rkt
         (struct-out operator)) ; required by platform.rkt

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
    (rival-compile (list `(,op ,@vars)) vars (list flonum-discretization))))

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
(struct operator-impl (name ctx spec fpcore fl cost aggregate))

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

(define fpcore-context (make-parameter '()))

(define (fpcore-parameterize spec)
  (match (fpcore-context)
    ['() spec]
    [`(! ,ctx-props ...)
     (match spec
       [`(! ,props ... ,body)
        (define props1 (make-immutable-hash (props->dict ctx-props)))
        (define props2 (make-immutable-hash (props->dict props)))
        (define props* (dict->props (hash-union props1 props2 #:combine (lambda (x y) y))))
        `(! ,@props* ,body)]
       [body `(! ,@ctx-props ,body)])]
    [ctx (error 'fpcore-context "expected `(! ...)`, got `~a`" ctx)]))

; Registers an operator implementation `name` with context `ctx` and spec `spec`.
; Can optionally specify a floating-point implementation and fpcore translation.
(define/contract (create-operator-impl! name
                                        ctx
                                        spec
                                        #:impl [fl-proc #f]
                                        #:fpcore [fpcore #f]
                                        #:cost [cost #f]
                                        #:aggregate [aggregate +])
  (->* (symbol? context? any/c)
       (#:impl (or/c procedure? generator? #f)
               #:fpcore any/c
               #:cost (or/c #f real?)
               #:aggregate procedure?)
       operator-impl?)
  ; check specification
  (check-spec! name ctx spec)
  ; synthesize operator (if the spec contains exactly one operator)
  (define op
    (match spec
      [(list op (or (? number?) (? symbol?)) ...) op]
      [_ #f]))
  ; check FPCore translation
  (match (fpcore-parameterize (or fpcore spec))
    [`(! ,props ... (,op ,args ...))
     (unless (even? (length props))
       (error 'create-operator-impl! "~a: umatched property in ~a" name fpcore))
     (unless (symbol? op)
       (error 'create-operator-impl! "~a: expected symbol `~a`" name op))
     (for ([arg (in-list args)]
           #:unless (or (symbol? arg) (number? arg)))
       (error 'create-operator-impl! "~a: expected terminal `~a`" name arg))]
    [`(,op ,args ...)
     (unless (symbol? op)
       (error 'create-operator-impl! "~a: expected symbol `~a`" name op))
     (for ([arg (in-list args)]
           #:unless (or (symbol? arg) (number? arg)))
       (error 'create-operator-impl! "~a: expected terminal `~a`" name arg))]
    [(? symbol?) (void)]
    [_ (error 'create-operator-impl! "Invalid fpcore for ~a: ~a" name fpcore)])
  ; check or synthesize floating-point operation
  (define fl-proc*
    (match fl-proc
      [(? generator?) ((generator-gen fl-proc) spec ctx)]
      [(? procedure?) fl-proc]
      [#f (error 'create-operator-impl! "fl-proc is not provided for `~a` implementation" name)]))
  (unless (procedure-arity-includes? fl-proc* (length (context-vars ctx)) #t) ; check arity
    (error 'arity-check
           "Procedure `~a` accepts ~a arguments, but ~a is provided"
           name
           (procedure-arity fl-proc*)
           (length (context-vars ctx))))
  (operator-impl name ctx spec (fpcore-parameterize (or fpcore spec)) fl-proc* cost aggregate))

(define-syntax (make-operator-impl stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'make-operator-impl why stx sub-stx))
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
       (define spec #f)
       (define core #f)
       (define fl-expr #f)
       (define op-cost #f)
       (define aggregate-expr #f)

       (let loop ([fields fields])
         (syntax-case fields ()
           [()
            (unless spec
              (oops! "missing `#:spec` keyword"))
            (with-syntax ([id id]
                          [spec spec]
                          [core core]
                          [fl-expr fl-expr]
                          [op-cost op-cost]
                          [aggregate-expr aggregate-expr])
              #'(create-operator-impl! 'id
                                       (context '(var ...) rtype (list repr ...))
                                       'spec
                                       #:impl fl-expr
                                       #:fpcore 'core
                                       #:cost op-cost
                                       #:aggregate (or aggregate-expr +)))]
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
           [(#:impl expr rest ...)
            (cond
              [fl-expr (oops! "multiple #:fl clauses" stx)]
              [else
               (set! fl-expr #'expr)
               (loop #'(rest ...))])]
           [(#:impl) (oops! "expected value after keyword `#:fl`" stx)]
           [(#:cost cost rest ...)
            (cond
              [op-cost (oops! "multiple #:cost clauses" stx)]
              [else
               (set! op-cost #'cost)
               (loop #'(rest ...))])]
           [(#:cost) (oops! "expected value after keyword `#:cost`" stx)]
           [(#:aggregate expr rest ...)
            (cond
              [aggregate-expr (oops! "multiple #:aggregate clauses" stx)]
              [else
               (set! aggregate-expr #'expr)
               (loop #'(rest ...))])]
           [(#:aggregate) (oops! "expected value after keyword `#:aggregate`" stx)]

           ; bad
           [_ (oops! "bad syntax" fields)])))]
    [_ (oops! "bad syntax")]))

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
