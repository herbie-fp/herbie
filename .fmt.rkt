#lang racket/base

(require fmt/conventions)

(provide the-formatter-map)

(define (the-formatter-map s)
  (case s
    [("define-operators") (standard-formatter-map "begin")]
    [else #f]))
