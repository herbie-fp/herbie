#lang racket

(require "./syntax/load-plugin.rkt" "plugin.rkt")

(load-herbie-builtins)

(define-platform math64
  [+.f64 1] [-.f64 1] [*.f64 2] [/.f64 2] [neg.f64 2] [sqrt.f64 4] )