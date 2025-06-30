#lang racket

(require (for-syntax racket))
(require "syntax/platform.rkt"
         "syntax/types.rkt")
(provide (rename-out [platform-module-begin #%module-begin])
         (except-out (all-from-out racket) #%module-begin)
         (all-from-out "syntax/platform.rkt")
         (all-from-out "syntax/types.rkt"))

(define-syntax (platform-module-begin stx)
  (with-syntax ([local-platform (datum->syntax stx 'platform)])
    (syntax-case stx ()
      [(_ content ...)
       #'(#%module-begin (define local-platform (make-empty-platform 'platform))
                         content ...
                         (provide local-platform))])))
