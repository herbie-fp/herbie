#lang racket

(require "platform.rkt"
         "syntax.rkt"
         "types.rkt"
         "generators.rkt")

(provide define-if
         define-representation
         define-operation
         define-operations
         fpcore-context
         (rename-out [platform-module-begin #%module-begin])
         (except-out (all-from-out racket) #%module-begin)
         (all-from-out "platform.rkt")
         (all-from-out "generators.rkt")
         (all-from-out "types.rkt"))

(define platform-being-defined (make-parameter #f))

(define-syntax-rule (define-if #:cost cost)
  (platform-register-if-cost! (platform-being-defined) #:cost cost))

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
