#lang racket

(require math/bigfloat)
(require "../common.rkt" "../bigcomplex.rkt" "softposit.rkt")

(provide types type? value? bigvalue? value-of bigvalue-of)

(define types '(bool real complex _posit8 _posit16 _posit32 _quire8 _quire16 _quire32))
(define (type? x) (set-member? types x))

(define (either-flonum? x)
  (if (flag-set? 'precision 'double)
      (double-flonum? x)
      (single-flonum? x)))

(define/match (value-of type) [('bool) boolean?] [('real) either-flonum?] [('complex) complex?]
  [('_posit8) posit8?] [('_posit16) posit16?] [('_posit32) posit32?]
  [('_quire8) quire8?] [('_quire16) quire16?]  [('_quire32) quire32?])
(define/match (bigvalue-of type) [('bool) boolean?] [('real) bigfloat?] [('complex) bigcomplex?]
  [('_posit8 ) big-posit8?] [('_posit16) big-posit16?] [('_posit32) big-posit32?]
  [('_quire8) big-quire8?] [('_quire16) big-quire16?] [('_quire32) big-quire32?])

(define value? (apply or/c (map value-of types)))
(define bigvalue? (apply or/c (map bigvalue-of types)))

