#lang racket

;; Builtin boolean plugin

(require "runtime/utils.rkt")

;; Do not run this file with `raco test`
(module test racket/base
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (bool bool boolean?)
                       identity
                       identity
                       (λ (x) (= x 0))
                       (λ (x) (if x 1 0))
                       1
                       (const #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants bool [TRUE TRUE true] [FALSE FALSE false])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(define-operator-impl (not [x : bool]) bool #:spec (not x) #:fl not #:identities (#:exact (not a)))

(define-operator-impl (and [x : bool] [y : bool])
                      bool
                      #:spec (and x y)
                      #:fl and-fn
                      #:identities (#:exact (and a b)))

(define-operator-impl (or [x : bool] [y : bool])
                      bool
                      #:spec (or x y)
                      #:fl or-fn
                      #:identities (#:exact (or a b)))
