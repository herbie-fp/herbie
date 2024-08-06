#lang racket

(require "./syntax/load-plugin.rkt"
         "plugin.rkt")

(load-herbie-builtins)

(define-operator-impl2 (+.f64 [x : binary64] [y : binary64]) binary64
  [spec (+ x y)]
  [fpcore (! :precision binary64 (+ x y))]
  [fl +])