#lang racket

(require math/bigfloat)
(require "../bigcomplex.rkt")

(provide type-dict type? value? bigvalue? value-of bigvalue-of)
(module+ internals (provide define-type))

(define type-dict (make-hash))
(define-syntax-rule (define-type name val? bigval?)
  (hash-set! type-dict 'name (cons val? bigval?)))

(define (type? x) (hash-has-key? type-dict x))

(define (value-of type) (car (hash-ref type-dict type)))
(define (bigvalue-of type) (cdr (hash-ref type-dict type)))

(define (value? x) (for/or ([(type rec) (in-hash type-dict)]) ((car rec) x)))
(define (bigvalue? x) (for/or ([(type rec) (in-hash type-dict)]) ((cdr rec) x)))

(define-type real real? bigfloat?)
(define-type bool boolean? boolean?)
(define-type complex (conjoin complex? (negate real?)) bigcomplex?)
