#lang racket

(require racket/set
         "../config.rkt"
         "../core/compiler.rkt"
         "../utils/timeline.rkt"
         "platform.rkt"
         (only-in "platform-language.rkt" create-operator-impl! platform-register-implementation!)
         "sugar.rkt"
         "syntax.rkt"
         "types.rkt")

(provide *added-fpcore-operators*
         dependent-fpcore-operators->fpcores
         platform-deserialize!
         platform-serialize
         register-fpcore-operator!
         replay-added-fpcore-operators!
         (struct-out platform-state))

(struct fpcore-operator-record (name vars output-repr-data var-repr-data body spec) #:prefab)
(struct platform-state (name added-fpcore-operators) #:prefab)

(define *added-fpcore-operators* (make-parameter '()))

(define (record-fpcore-operator name ctx body* spec*)
  (fpcore-operator-record name
                          (context-vars ctx)
                          (repr->datum (context-repr ctx))
                          (map repr->datum (context-var-reprs ctx))
                          body*
                          spec*))

(define (register-fpcore-operator! name ctx body* spec* #:remember? [remember? #t])
  (define output-repr (context-repr ctx))
  (define spec-expr (prog->spec spec*))
  (define core-proc (compile-prog body* ctx))
  (define fl-proc
    (procedure-reduce-arity (lambda args (core-proc (list->vector args)))
                            (length (context-vars ctx))))
  (define cost ((platform-cost-proc (*active-platform*)) body* output-repr))
  (define fpcore-expr (cons name (context-vars ctx)))
  (define impl
    (create-operator-impl! name ctx #:spec spec-expr #:impl fl-proc #:fpcore fpcore-expr #:cost cost))
  (platform-register-implementation! (*active-platform*) impl)
  (when remember?
    (*added-fpcore-operators* (append (*added-fpcore-operators*)
                                      (list (record-fpcore-operator name ctx body* spec*))))))

(define (replay-added-fpcore-operators!)
  (parameterize ([*timeline-disabled* #t])
    (for ([record (in-list (*added-fpcore-operators*))])
      (unless (impl-exists? (fpcore-operator-record-name record))
        (define ctx
          (context (fpcore-operator-record-vars record)
                   (datum->repr (fpcore-operator-record-output-repr-data record))
                   (map datum->repr (fpcore-operator-record-var-repr-data record))))
        (register-fpcore-operator! (fpcore-operator-record-name record)
                                   ctx
                                   (fpcore-operator-record-body record)
                                   (fpcore-operator-record-spec record)
                                   #:remember? #f)))))

(define (platform-serialize)
  (platform-state (*platform-name*) (*added-fpcore-operators*)))

(define (platform-deserialize! state)
  (match-define (platform-state name added-fpcore-operators) state)
  (unless (equal? name (*platform-name*))
    (error 'platform-deserialize!
           "platform mismatch, expected ~a but current platform is ~a"
           name
           (*platform-name*)))
  (*added-fpcore-operators* added-fpcore-operators)
  (replay-added-fpcore-operators!))

(define (render-fpcore-argument var repr-data output-precision)
  (define repr (datum->repr repr-data))
  (cond
    [(array-representation? repr)
     (define dims (array-representation-shape repr))
     (define elem-precision (representation-name (array-representation-base repr)))
     (if (equal? elem-precision output-precision)
         (append (list var) dims)
         (append (list '! ':precision elem-precision var) dims))]
    [else
     (define var-precision (representation-name repr))
     (if (equal? var-precision output-precision)
         var
         (list '! ':precision var-precision var))]))

(define (fpcore-record->fpcore record)
  (define output-repr (datum->repr (fpcore-operator-record-output-repr-data record)))
  (define output-precision (representation-name (array-representation-base output-repr)))
  (define ctx
    (context (fpcore-operator-record-vars record)
             output-repr
             (map datum->repr (fpcore-operator-record-var-repr-data record))))
  `(FPCore ,(fpcore-operator-record-name record)
           ,(for/list ([var (in-list (fpcore-operator-record-vars record))]
                       [repr-data (in-list (fpcore-operator-record-var-repr-data record))])
              (render-fpcore-argument var repr-data output-precision))
           :precision
           ,output-precision
           ,(prog->fpcore (fpcore-operator-record-body record) ctx)))

(define (prog-operators prog)
  (match prog
    [(? literal?) '()]
    [(? number?) '()]
    [(? symbol?) '()]
    [(approx spec impl) (append (prog-operators spec) (prog-operators impl))]
    [(list op args ...) (remove-duplicates (cons op (append-map prog-operators args)))]))

(define (dependent-fpcore-operators->fpcores prog #:exclude [exclude '()])
  (define records (*added-fpcore-operators*))
  (define record-table
    (for/hash ([record (in-list records)])
      (values (fpcore-operator-record-name record) record)))
  (define needed (mutable-set))

  (define (visit prog)
    (for ([op (in-list (prog-operators prog))]
          #:when (hash-has-key? record-table op))
      (unless (set-member? needed op)
        (set-add! needed op)
        (visit (fpcore-operator-record-body (hash-ref record-table op))))))

  (visit prog)

  (for/list ([record (in-list records)]
             #:when (set-member? needed (fpcore-operator-record-name record))
             #:unless (member (fpcore-operator-record-name record) exclude))
    (fpcore-record->fpcore record)))
