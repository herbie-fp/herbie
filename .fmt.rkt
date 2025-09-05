#lang racket/base

(require fmt/conventions)

(provide the-formatter-map)

(define (the-formatter-map s)
  (case s
    [("define-operators") (standard-formatter-map "begin")]
    [("define-operations") (standard-formatter-map "define")]
    [("define-rules") (standard-formatter-map "define")]
    [("define-generator") (standard-formatter-map "define")]
    [("define-api-endpoint") (standard-formatter-map "define")]
    [else #f]))
