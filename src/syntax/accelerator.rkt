#lang racket

(require "../core/matcher.rkt")

(provide accelerator-exists?
         all-accelerators
         accelerator-info
         expand-accelerators)
  
(module+ internals
  (provide (struct-out accelerator)
           accelerators))

;; An "accelerator" operator
;; A composition of existing Herbie operators
(struct accelerator (name vars body itypes otype))

;; Table of accelerator operators
(define accelerators (make-hash))

;; Key in the accelerator table
(define (accelerator-exists? op)
  (hash-has-key? accelerators op))

;; All keys in the accelerator table
(define (all-accelerators)
  (hash-keys accelerators))

;; LHS and RHS patterns of a rewrite rules to apply and
;; undo an accelerator definition.
(define (accelerator-patterns op)
  (match-define (accelerator name vars body _ _) (hash-ref accelerators op))
  (values `(,name ,@vars) body))

;; Fields of an accelerator
(define/contract (accelerator-info op field)
  (-> symbol? (or/c 'itype 'otype 'body 'vars) any/c)
  (unless (hash-has-key? accelerators op)
    (error 'accelerator-info "Unknown accelerator ~a" op))
  (define accessor
    (match field
      ['vars accelerator-vars]
      ['body accelerator-body]
      ['itype accelerator-itypes]
      ['otype accelerator-otype]))
  (accessor (hash-ref accelerators op)))

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
