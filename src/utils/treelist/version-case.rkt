#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     version/utils))

(provide meta-if-version-at-least)

(define-syntax (meta-if-version-at-least stx)
  (syntax-parse stx
    [(_ vers a b) (if (version<? (version) (syntax-e #'vers)) #'b #'a)]))
