#lang racket

(require "syntax/syntax.rkt"
         "core/matcher.rkt")

(provide expand-accelerators)

;; LHS and RHS patterns of a rewrite rules to undo
;; an accelerator definition
(define (accelerator-patterns op)
  (define spec (operator-info op 'spec))
  (match-define `(,(or 'lambda 'λ) (,vars ...) ,body) spec)
  (values `(,op ,@vars) body))

(define (expand-accelerators expr #:accelerators [ops (all-accelerators)])
  (define (expand expr)
    (for/fold ([expr expr]) ([op (in-list ops)])
      (define-values (lhs rhs) (accelerator-patterns op))
      (define bindings (pattern-match lhs expr))
      (if bindings (pattern-substitute rhs bindings) expr)))
  (let loop ([expr expr])
    (match (expand expr)
      [(list 'if cond ift iff) `(if ,(loop cond) ,(loop ift) ,(loop iff))]
      [(list op args ...) `(,op ,@(map loop args))]
      [expr expr])))
