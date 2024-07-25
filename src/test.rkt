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
                 #:literal [binary64 1]
                 #:literal [binary32 1]
                 #:default-cost 1
                 #:if-cost 1)
