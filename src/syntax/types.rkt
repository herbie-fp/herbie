#lang racket

(require math/bigfloat)

(provide (struct-out type) get-type type-name?)
(module+ internals (provide define-type))

(struct type (name))

(define type-dict (make-hasheq))

(define-syntax-rule (define-type name (exact? inexact?) e->i i->e)
  (hash-set! type-dict 'name (type 'name)))

(define (type-name? x) (hash-has-key? type-dict x))
(define (get-type x) (hash-ref type-dict x))

(define-type real (real? bigfloat?)
  bf bigfloat->flonum)

(define-type bool (boolean? boolean?)
  identity identity)
