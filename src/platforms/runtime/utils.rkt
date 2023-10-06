#lang racket

;; Common definitions for floating-point runtimes

(require "../../plugin.rkt")
(provide (all-from-out "../../plugin.rkt")
         shift unshift)

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))
