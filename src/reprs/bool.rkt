#lang racket

;; Builtin boolean plugin

(require "../plugin.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (bool bool boolean?)
  identity
  identity
  (λ (x) (= x 0))
  (λ (x) (if x 1 0))
  1
  (const #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant-impl (TRUE TRUE) bool
  [fl (const true)])

(define-constant-impl (FALSE FALSE) bool
  [fl (const false)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

(define-operator-impl (not not bool) bool
  [fl not])

(define-operator-impl (and and . bool) bool
  [fl and-fn])

(define-operator-impl (or or . bool) bool
  [fl or-fn])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-ruleset bool-reduce (bools simplify fp-safe)
  #:type ([a bool] [b bool])
  [not-true     (not TRUE)       FALSE]
  [not-false    (not FALSE)      TRUE]
  [not-not      (not (not a))    a]
  [not-and      (not (and a b))  (or  (not a) (not b))]
  [not-or       (not (or  a b))  (and (not a) (not b))]
  [and-true-l   (and TRUE a)     a]
  [and-true-r   (and a TRUE)     a]
  [and-false-l  (and FALSE a)    FALSE]
  [and-false-r  (and a FALSE)    FALSE]
  [and-same     (and a a)        a]
  [or-true-l    (or TRUE a)      TRUE]
  [or-true-r    (or a TRUE)      TRUE]
  [or-false-l   (or FALSE a)     a]
  [or-false-r   (or a FALSE)     a]
  [or-same      (or a a)         a])
