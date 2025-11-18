#lang racket

(require "../../src/api/sandbox.rkt"
         "../../src/syntax/read.rkt"
         "../../src/syntax/types.rkt"
         "../../src/core/points.rkt"
         "../../src/api/sandbox.rkt"
         "../../src/syntax/load-platform.rkt"
         "../../src/core/points.rkt"
         "../../src/syntax/load-platform.rkt"
         "../../src/syntax/types.rkt")

(activate-platform! "growlibm")

(define test (load-tests (open-input-string (vector-ref (current-command-line-arguments) 0))))

(define (sample-test t)
  (*context* (test-context t))
  (define points (pcontext-points (get-sample t)))
  (for-each (lambda (x) (begin (for-each (lambda (y) (display (format "~a " y))) x)
                               (displayln ""))) (map vector->list (vector->list points))))

(for-each sample-test test)