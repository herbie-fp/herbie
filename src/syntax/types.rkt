#lang racket

(require math/bigfloat)

(provide type-name get-type type-name?)
(module+ internals (provide define-type))

(define type-name identity)
(define type-dict (make-hasheq))

(define-syntax-rule (define-type name (exact? inexact?) e->i i->e)
  (hash-set! type-dict 'name #t))

(define (type-name? x) (hash-has-key? type-dict x))
(define/contract (get-type x)
  (-> type-name? type-name?)
  x)

(define-type real (real? bigfloat?)
  bf bigfloat->flonum)

(define-type bool (boolean? boolean?)
  identity identity)
