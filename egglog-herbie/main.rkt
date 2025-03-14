#lang racket/base

(require racket/port
         racket/contract)

(require "ffi.rkt")

(provide make-egraph
         egraph?
         (contract-out
           [egraph-run (-> egraph? string? (listof any/c))]))

;; Racket wrapper around a Rust-allocated e-graph
(struct egraph (ptr))

;; Creates a new e-graph instance
(define (make-egraph)
  (egraph (egraph_create)))

;; Runs a program on an e-graph instance
(define (egraph-run egraph program)
  (define output (egraph_run (egraph-ptr egraph) program))
  (port->list read (open-input-string output)))
