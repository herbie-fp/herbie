#lang racket

(provide type-name?)
(module+ internals (provide define-type))

(define type-dict (make-hasheq))
(define (type-name? x) (hash-has-key? type-dict x))

(define-syntax-rule (define-type name _ ...)
  (hash-set! type-dict 'name #t))


(define-type real)
(define-type bool)
