#lang racket

;; Common definitions for the builtin plugins

(require "../../plugin.rkt")
(provide (all-defined-out) (all-from-out "../../plugin.rkt"))

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(define-syntax-rule (define-constants repr [name impl-name value] ...)
  (begin
    (define-operator-impl (name impl-name) repr
      [fl (const value)])
    ...))

(define-syntax-rule (define-comparator-impls repr [name impl-name impl-fn] ...)
  (begin
    (define-operator-impl (name impl-name repr repr) bool
      [fl impl-fn])
    ...))