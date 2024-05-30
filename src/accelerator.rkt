#lang racket

(require "common.rkt" "syntax/types.rkt" "core/matcher.rkt")

(provide accelerator-exists? all-accelerators expand-accelerators)

(module+ internals (provide (struct-out accelerator) accelerators))

;;
;;  Accelerator operators
;;

;; An "accelerator" operator
;;
;; Each operator is just a composition of existing operators defined by
;;  - a (unique) name
;;  - input and output types
;;  - a definition as an S-expr
(struct accelerator (name itypes otype spec))

;; Accelerators known to Herbie at runtime
(define accelerators (make-hasheq))

;; Is the operator an accelerator?
(define (accelerator-exists? x)
  (hash-has-key? accelerators x))

;; The list of accelerators as a list.
(define (all-accelerators)
  (hash-keys accelerators))

;; LHS and RHS patterns of a rewrite rules to apply and undo
;; an accelerator definition.
(define (accelerator-patterns acc)
  (match-define (list (or 'lambda 'λ) (list vars ...) body) (accelerator-spec acc))
  (values (cons (accelerator-name acc) vars) body))

(define (expand-accelerators expr #:accelerators [ops (all-accelerators)])
  (define (expand expr)
    (for/fold ([expr expr]) ([op (in-list ops)])
      (define info (hash-ref accelerators op))
      (define-values (lhs rhs) (accelerator-patterns info))
      (define bindings (pattern-match lhs expr))
      (if bindings (pattern-substitute rhs bindings) expr)))
  (let loop ([expr expr])
    (match (expand expr)
      [(list 'if cond ift iff) `(if ,(loop cond) ,(loop ift) ,(loop iff))]
      [(list op args ...) `(,op ,@(map loop args))]
      [expr expr])))
