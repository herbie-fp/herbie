#lang racket

;; Common definitions for the builtin plugins

(require "../../plugin.rkt")
(provide shift unshift (all-from-out "../../plugin.rkt"))

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))
