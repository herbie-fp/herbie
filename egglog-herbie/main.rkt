#lang racket/base

(require racket/contract
         racket/format
         racket/port)

(require "ffi.rkt")

(provide make-egraph
         egraph?
         (contract-out [egraph-run (-> egraph? any/c (listof any/c))]
                       [egraph-run! (-> egraph? any/c any)]
                       [egraph-extract
                        (->* (egraph? list?) (#:variants exact-positive-integer?) (listof any/c))]))

;; Racket wrapper around a Rust-allocated e-graph
(struct egraph (ptr))

;; Creates a new e-graph instance
(define (make-egraph)
  (egraph (egraph_create)))

;; Runs a program on an e-graph instance returning the output.
(define (egraph-run egraph program)
  (define program*
    (if (string? program)
        program
        (~a program)))
  (define output (egraph_run (egraph-ptr egraph) program*))
  (port->list read (open-input-string output)))

;; Runs a program on an e-graph instance without returning the output.
(define (egraph-run! egraph program)
  (define program*
    (if (string? program)
        program
        (~s program)))
  (egraph_run (egraph-ptr egraph) program*)
  (void))

;; Runs extraction for a list of expressions.
(define (egraph-extract egraph exprs #:variants [variants #f])
  (define exprvec
    (for/vector #:length (length exprs)
                ([expr (in-list exprs)])
      (if (string? expr)
          expr
          (~s expr))))
  (define output (egraph_extract (egraph-ptr egraph) exprvec (or variants 0)))
  (port->list read (open-input-string output)))
