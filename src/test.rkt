#lang racket

(require "./syntax/load-plugin.rkt"
         "plugin.rkt")

(load-herbie-builtins)

(define-platform math64
                 [+.f64 1]
                 [-.f64 1]
                 [*.f64 2]
                 [/.f64 2]
                 [neg.f64 2]
                 #:optional
                 #:literals ([binary64 1] [binary64 1])
                 #:if-cost 1)