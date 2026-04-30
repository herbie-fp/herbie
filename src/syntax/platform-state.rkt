#lang racket

(require "../config.rkt"
         "../core/compiler.rkt"
         "../utils/errors.rkt"
         "../utils/timeline.rkt"
         "platform.rkt"
         (only-in "platform-language.rkt" create-operator-impl! platform-register-implementation!)
         "sugar.rkt"
         "syntax.rkt"
         "types.rkt")

(provide *platform-extensions*
         platform-serialize
         register-platform-extensions!
         register-fpcore-operator!
         (struct-out platform-state))

(struct fpcore-extension (name vars output-repr-data var-repr-data body spec) #:prefab)
(struct platform-state (name extensions) #:prefab)

(define *platform-extensions* (make-parameter '()))

(define (make-fpcore-extension name ctx body* spec*)
  (fpcore-extension name
                    (context-vars ctx)
                    (representation-name (context-repr ctx))
                    (map representation-name (context-var-reprs ctx))
                    body*
                    spec*))

(define (register-fpcore-operator! name ctx body* spec*)
  (define output-repr (context-repr ctx))
  (define spec-expr (prog->spec spec*))
  (cond
    [(impl-exists? name)
     (unless (and (equal? (impl-info name 'vars) (context-vars ctx))
                  (equal? (impl-info name 'itype) (context-var-reprs ctx))
                  (equal? (impl-info name 'otype) output-repr)
                  (equal? (impl-info name 'spec) spec-expr))
       (raise-herbie-error "Impl ~a is already registered in platform ~a with a different definition"
                           name
                           (*platform-name*)))]
    [else
     (define core-proc (compile-prog body* ctx))
     (define fl-proc
       (procedure-reduce-arity (lambda args (core-proc (list->vector args)))
                               (length (context-vars ctx))))
     (define cost ((platform-cost-proc (*active-platform*)) body* output-repr))
     (define fpcore-expr (cons name (context-vars ctx)))
     (define impl
       (create-operator-impl! name
                              ctx
                              #:spec spec-expr
                              #:impl fl-proc
                              #:fpcore fpcore-expr
                              #:cost cost))
     (platform-register-implementation! (*active-platform*) impl)
     (*platform-extensions* (append (*platform-extensions*)
                                    (list (make-fpcore-extension name ctx body* spec*))))]))

(define (register-platform-extension! extension)
  (match extension
    [(fpcore-extension name vars output-repr-data var-repr-data body spec)
     (unless (impl-exists? name)
       (define ctx
         (context vars (get-representation output-repr-data) (map get-representation var-repr-data)))
       (register-fpcore-operator! name ctx body spec))]))

(define (register-platform-extensions! extensions)
  (parameterize ([*timeline-disabled* #t])
    (for ([extension (in-list extensions)])
      (register-platform-extension! extension))))

(define (platform-serialize)
  (platform-state (*platform-name*) (*platform-extensions*)))
