#lang racket

(require "./syntax/load-plugin.rkt"
         "plugin.rkt")

(load-herbie-builtins)

(define-operator-impl (+.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (+ x y)
                      #:fpcore (! :precision binary64 (+ x y))
                      #:fl +)

(define-platform math64
                 [+.f64 1]
                 [-.f64 1]
                 [*.f64 2]
                 [/.f64 2]
                 [neg.f64 2]
                 #:optional
                 #:literal [binary64 1]
                 #:if-cost 1)
