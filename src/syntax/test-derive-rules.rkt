#lang racket

(require "rules.rkt")
(require "../core/simplify.rkt")


(define (differentiate expr)
  (simplify-batch (list expr) #:rules (*derivative-rules*) #:precompute true))



(println (differentiate `(d a a)))