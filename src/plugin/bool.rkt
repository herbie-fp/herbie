#lang racket

(require (submod "../syntax/syntax.rkt" internals)
         (submod "../syntax/types.rkt" internals))

(provide bool
         TRUE
         FALSE
         not
         and
         or)

;; shim to rename boolean procedures
(module bool-ops racket/base
  (provide not-proc
           and-proc
           or-proc)

  (define not-proc not)
  (define (and-proc x y)
    (and x y))
  (define (or-proc x y)
    (or x y)))

(require (submod "." bool-ops))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Representation

(define-representation bool
                       (bool bool boolean?)
                       identity
                       identity
                       (λ (x) (= x 0))
                       (λ (x) (if x 1 0))
                       1
                       (const #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementations

(define-operator-impl (TRUE) bool
  #:spec (TRUE)
  #:fl (const #t))

(define-operator-impl (FALSE) bool
  #:spec (FALSE)
  #:fl (const #t))

(define-operator-impl (not [x : bool]) bool
  #:spec (not x)
  #:fl not-proc)

(define-operator-impl (and [x : bool] [y : bool]) bool
  #:spec (and x y)
  #:fl and-proc)

(define-operator-impl (or [x : bool] [y : bool]) bool
  #:spec (or x y)
  #:fl or-proc)