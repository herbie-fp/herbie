#lang racket

(require "syntax/syntax.rkt"
         "syntax/types.rkt"
         (submod "syntax/syntax.rkt" internals)
         "accelerator.rkt"
         "sampling.rkt")

(provide register-accelerator-impl!
         define-accelerator-impl)

(define (register-accelerator-impl! op
                                    name
                                    itypes
                                    otype
                                    #:impl [impl #f])
  (unless (operator-accelerator? op)
    (error 'register-accelerator-impl "must be an accelerator ~a" op))
  (define spec (operator-info op 'spec))
  (match-define `(,(or 'lambda 'λ) (,vars ...) ,body) spec)
  (unless (= (length vars) (length itypes))
    (error 'register-accelerator-impl!
           "implementation does not have expected arity: ~a ~a"
           (length vars)
           (length itypes)))
  (define ctx (context vars otype itypes))
  (define impl-fn
    (or impl
        (let ([fn (eval-progs-real (list (expand-accelerators body)) (list ctx))])
          (λ args (first (apply fn args))))))
  (register-operator-impl! op
                           name
                           itypes
                           otype
                           (list (cons 'fl impl-fn))))

(define-syntax define-accelerator-impl
  (syntax-rules ()
    [(_ operator name (itypes ...) otype)
     (register-accelerator-impl! 'operator 'name
                                 (list (get-representation 'itypes) ...)
                                 (get-representation 'otype))]
    [(_ operator name (itypes ...) otype implementation)
     (register-accelerator-impl! 'operator 'name
                                 (list (get-representation 'itypes) ...)
                                 (get-representation 'otype)
                                 #:impl implementation)]))
