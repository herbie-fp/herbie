#lang racket

(require "syntax/types.rkt"
         "utils/errors.rkt"
         "syntax/platform.rkt"
         (submod "syntax/types.rkt" internals)
         (submod "syntax/syntax.rkt" internals)
         (submod "syntax/platform.rkt" internals))

(define-syntax-rule (define-ruleset _ ...)
  (void))
(define-syntax-rule (define-ruleset* _ ...)
  (void))
(define (register-ruleset! . args)
  (void))
(define (register-conversion-generator! proc)
  (void))

(provide define-type
         define-representation
         define-operator-impl
         define-operator
         define-ruleset*
         define-ruleset
         register-operator-impl!
         register-representation!
         register-representation-alias!
         register-conversion-generator!
         register-generator!
         register-operator!
         register-ruleset!
         (struct-out representation)
         get-representation
         warn
         define-platform
         register-platform!
         platform-union
         platform-intersect
         platform-subtract)

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
       (define spec #f)
       (define core #f)
       (define fl-expr #f)

       (let loop ([fields fields])
         (syntax-case fields ()
           [()
            (unless spec
              (oops! "missing `#:spec` keyword"))
            (with-syntax ([id id]
                          [spec spec]
                          [core core]
                          [fl-expr fl-expr])
              #'(register-operator-impl! 'id
                                         (context '(var ...)
                                                  (get-representation 'rtype)
                                                  (list (get-representation 'repr) ...))
                                         'spec
                                         #:fl fl-expr
                                         #:fpcore 'core))]
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

           ; bad
           [_ (oops! "bad syntax" fields)])))]
    [_ (oops! "bad syntax")]))
