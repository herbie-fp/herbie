#lang racket/base

(provide (struct-out discretization)
         (struct-out rival-machine)
         *rival-max-precision*
         *rival-min-precision*
         *rival-max-iterations*
         *rival-profile-executions*
         *ampl-tuning-bits*
         *sampling-iteration*
         *lower-bound-early-stopping*
         *base-tuning-precision*
         *bumps-activated*)

(define *rival-max-precision* (make-parameter 10000))
(define *rival-min-precision* (make-parameter 20))
(define *rival-max-iterations* (make-parameter 5))
(define *rival-profile-executions* (make-parameter 1000))
(define *lower-bound-early-stopping* (make-parameter #f))
(define *bumps-activated* (make-parameter #f))

(struct discretization (target convert distance))

(struct rival-machine
        (arguments instructions
                   outputs
                   discs
                   registers
                   repeats
                   initial-repeats
                   precisions
                   initial-precisions
                   best-known-precisions
                   output-distance
                   default-hint
                   constant-lookup
                   max-precision
                   [iteration #:mutable]
                   [bumps #:mutable]
                   [profile-ptr #:mutable]
                   profile-instruction
                   profile-number
                   profile-time
                   profile-memory
                   profile-precision
                   profile-iteration))

(define *ampl-tuning-bits* (make-parameter 2))
(define *sampling-iteration* (make-parameter 0))
(define *base-tuning-precision* (make-parameter 5))
