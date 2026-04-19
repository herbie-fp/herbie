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

(provide *added-fpcore-operators*
         platform-serialize
         register-fpcore-operator-records!
         register-fpcore-operator!
         (struct-out platform-state))

(struct fpcore-operator-record (name vars output-repr-data var-repr-data body spec) #:prefab)
(struct platform-state (name added-fpcore-operators) #:prefab)

(define *added-fpcore-operators* (make-parameter '()))

(define (record-fpcore-operator name ctx body* spec*)
  (fpcore-operator-record name
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
     (*added-fpcore-operators* (append (*added-fpcore-operators*)
                                       (list (record-fpcore-operator name ctx body* spec*))))]))

(define (register-fpcore-operator-record! record)
  (unless (impl-exists? (fpcore-operator-record-name record))
    (define ctx
      (context (fpcore-operator-record-vars record)
               (get-representation (fpcore-operator-record-output-repr-data record))
               (map get-representation (fpcore-operator-record-var-repr-data record))))
    (register-fpcore-operator! (fpcore-operator-record-name record)
                               ctx
                               (fpcore-operator-record-body record)
                               (fpcore-operator-record-spec record))))

(define (register-fpcore-operator-records! records)
  (parameterize ([*timeline-disabled* #t])
    (for ([record (in-list records)])
      (register-fpcore-operator-record! record))))

(define (platform-serialize)
  (platform-state (*platform-name*) (*added-fpcore-operators*)))
