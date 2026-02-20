#lang racket

(require "../../src/api/sandbox.rkt"
         "../../src/config.rkt"
         "../../src/syntax/read.rkt"
         "../../src/syntax/types.rkt"
         "../../src/core/points.rkt"
         "../../src/api/sandbox.rkt"
         "../../src/syntax/load-platform.rkt"
         "../../src/core/points.rkt"
         "../../src/syntax/load-platform.rkt"
         "../../src/syntax/types.rkt")

(activate-platform! "growlibm")

(define args (current-command-line-arguments))
(define test (load-tests (open-input-string (vector-ref args 0))))
(when (> (vector-length args) 1)
  (define seed (string->number (vector-ref args 1)))
  (when seed
    (set-seed! seed)))

(define (sample-test t)
  (*context* (test-context t))
  (define points (pcontext-points (get-sample t)))
  (for-each (lambda (x) (begin (for-each (lambda (y) (display (format "~a " y))) x)
                               (displayln ""))) (map vector->list (vector->list points))))

(for-each sample-test test)
