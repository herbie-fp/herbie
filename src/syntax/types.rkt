#lang racket

(require math/bigfloat)
(require "../common.rkt" "../bigcomplex.rkt" "softposit.rkt")

(provide types type? value? bigvalue? value-of bigvalue-of)

(define types '(bool real complex posit8 posit16 posit32 quire8 quire16 quire32))
(define (type? x) (set-member? types x))

(define/match (value-of type) [('bool) boolean?] [('real) real?] [('complex) complex?]
  [('posit8) posit8?] [('posit16) posit16?] [('posit32) posit32?]
  [('quire8) quire8?] [('quire16) quire16?]  [('quire32) quire32?])
(define/match (bigvalue-of type) [('bool) boolean?] [('real) bigfloat?] [('complex) bigcomplex?]
  [('posit8) big-posit8?] [('posit16) big-posit16?] [('posit32) big-posit32?]
  [('quire8) big-quire8?] [('quire16) big-quire16?] [('quire32) big-quire32?])

(define value? (apply or/c (map value-of types)))
(define bigvalue? (apply or/c (map bigvalue-of types)))

