#lang racket

(require "platform.rkt"
         "syntax.rkt"
         "types.rkt"
         "generators.rkt"
         "../utils/errors.rkt"
         "../config.rkt")

(provide define-representation
         define-operation
         define-operations
         fpcore-context
         if-impl
         if-cost
         create-operator-impl!
         platform-register-implementation!
         make-operator-impl
         (rename-out [platform-module-begin #%module-begin])
         (except-out (all-from-out racket) #%module-begin)
         (all-from-out "platform.rkt")
         (all-from-out "generators.rkt")
         (all-from-out "types.rkt"))

(define platform-being-defined (make-parameter #f))

;; Specification checking and operator implementation creation moved
;; from syntax.rkt
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
         #:when (assq expr env)
         (cdr (assq expr env))]
        [(? symbol?) (bad! "unbound variable `~a`" expr)]
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

(define fpcore-context (make-parameter '_))

(define (fpcore-parameterize spec)
  (let loop ([ctx (fpcore-context)])
    (match ctx
      ['_ spec]
      [(list arg ...) (map loop arg)]
      [_ ctx])))

(define/contract (create-operator-impl! name
                                        ctx
                                        spec
                                        #:impl [fl-proc #f]
                                        #:fpcore [fpcore #f]
                                        #:cost [cost #f])
  (->* (symbol? context? any/c)
       (#:impl (or/c procedure? generator? #f) #:fpcore any/c #:cost (or/c #f real? procedure?))
       operator-impl?)
  ;; check specification
  (check-spec! name ctx spec)
  ;; synthesize operator (if the spec contains exactly one operator)
  (define op
    (match spec
      [(list op (or (? number?) (? symbol?)) ...) op]
      [_ #f]))
  ;; check FPCore translation
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
  ;; check or synthesize floating-point operation
  (define fl-proc*
    (match fl-proc
      [(? generator?) ((generator-gen fl-proc) spec ctx)]
      [(? procedure?) fl-proc]
      [#f (error 'create-operator-impl! "fl-proc is not provided for `~a` implementation" name)]))
  (unless (procedure-arity-includes? fl-proc* (length (context-vars ctx)) #t)
    (error 'arity-check
           "Procedure `~a` accepts ~a arguments, but ~a is provided"
           name
           (procedure-arity fl-proc*)
           (length (context-vars ctx))))
  (define-values (cost* aggregate*)
    (cond
      [(number? cost) (values cost +)]
      [(procedure? cost) (values 0 cost)]
      [else (values cost +)]))
  (operator-impl name ctx spec (fpcore-parameterize (or fpcore spec)) fl-proc* cost* aggregate*))

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

       (let loop ([fields fields])
         (syntax-case fields ()
           [()
            (unless spec
              (oops! "missing `#:spec` keyword"))
            (with-syntax ([id id]
                          [spec spec]
                          [core core]
                          [fl-expr fl-expr]
                          [op-cost op-cost])
              #'(create-operator-impl! 'id
                                       (context '(var ...) rtype (list repr ...))
                                       'spec
                                       #:impl fl-expr
                                       #:fpcore 'core
                                       #:cost op-cost))]
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

           ; bad
           [_ (oops! "bad syntax" fields)])))]
    [_ (oops! "bad syntax")]))

;; Platform registration functions moved from platform.rkt
(define (platform-register-representation! platform #:repr repr #:cost cost)
  (define reprs (platform-representations platform))
  (define repr-costs (platform-representation-costs platform))
  ; Duplicate check
  (when (hash-has-key? reprs (representation-name repr))
    (raise-herbie-error "Duplicate representation ~a in platform ~a"
                        (representation-name repr)
                        (*platform-name*)))
  ; Update tables
  (hash-set! reprs (representation-name repr) repr)
  (hash-set! repr-costs (representation-name repr) cost))

(define (platform-register-implementation! platform impl)
  (unless impl
    (raise-herbie-error "Platform ~a missing implementation" (*platform-name*)))
  ; Reprs check
  (define reprs (platform-representations platform))
  (define otype (context-repr (operator-impl-ctx impl)))
  (define itype (context-var-reprs (operator-impl-ctx impl)))
  (define impl-reprs (map representation-name (remove-duplicates (cons otype itype))))
  (unless (andmap (curry hash-has-key? reprs) impl-reprs)
    (raise-herbie-error "Platform ~a missing representation of ~a implementation"
                        (*platform-name*)
                        (operator-impl-name impl)))
  ; Cost check
  (define impl-cost (operator-impl-cost impl))
  (unless impl-cost
    (raise-herbie-error "Missing cost for ~a" (operator-impl-name impl)))
  ; Duplicate check
  (define impls (platform-implementations platform))
  (when (hash-has-key? impls (operator-impl-name impl))
    (raise-herbie-error "Impl ~a is already registered in platform ~a"
                        (operator-impl-name impl)
                        (*platform-name*)))
  ; Update table
  (hash-set! impls (operator-impl-name impl) impl))

(define (validate-platform! platform)
  (when (empty? (platform-implementations platform))
    (raise-herbie-error "Platform contains no operations"))
  (for ([impl (in-hash-values (platform-implementations platform))])
    (define ctx (operator-impl-ctx impl))
    (for ([repr (in-list (cons (context-repr ctx) (context-var-reprs ctx)))]
          #:unless
          (equal? (hash-ref (platform-representations platform) (representation-name repr) #f) repr))
      (raise-herbie-error "Representation ~a not defined" (representation-name repr)))))

(define-syntax (platform-register-implementations! stx)
  (syntax-case stx ()
    [(_ platform ([name ([var : repr] ...) otype spec fl fpcore cost] ...))
     #'(begin
         (platform-register-implementation! platform
                                            (make-operator-impl (name [var : repr] ...)
                                                                otype
                                                                #:spec spec
                                                                #:impl fl
                                                                #:fpcore fpcore
                                                                #:cost cost)) ...)]))

(define-syntax-rule (define-representation repr #:cost cost)
  (platform-register-representation! (platform-being-defined) #:repr repr #:cost cost))

(define-syntax-rule (define-operation (name [arg irepr] ...) orepr flags ...)
  (let ([impl (make-operator-impl (name [arg : irepr] ...) orepr flags ...)])
    (platform-register-implementation! (platform-being-defined) impl)))

(define-syntax (define-operations stx)
  (syntax-case stx ()
    [(_ ([arg irepr] ...) orepr #:fpcore fc [name flags ...] ...)
     #'(parameterize ([fpcore-context 'fc])
         (begin
           (define-operation (name [arg irepr] ...) orepr flags ...) ...))]
    [(_ ([arg irepr] ...) orepr [name flags ...] ...)
     #'(begin
         (define-operation (name [arg irepr] ...) orepr flags ...) ...)]))

(define-syntax (platform-module-begin stx)
  (with-syntax ([local-platform (datum->syntax stx 'platform)])
    (syntax-case stx ()
      [(_ content ...)
       #'(#%module-begin (define local-platform (make-empty-platform))
                         (define old-platform-being-defined (platform-being-defined))
                         (platform-being-defined local-platform)
                         content ...
                         (platform-being-defined old-platform-being-defined)
                         (validate-platform! local-platform)
                         (provide local-platform)
                         (module+ main
                           (display-platform local-platform))
                         (module test racket/base
                           ))])))

(define (if-impl c t f)
  (if c t f))

(define ((if-cost base) c t f)
  (+ base c (max t f)))
