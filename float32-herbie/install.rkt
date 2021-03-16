#lang racket/base

(require racket/system racket/list)
(provide pre-installer)

(define (call-and-wait! command)
  (define out (process command))
  (for ([i (in-naturals)]
       #:break (not (eq? ((fifth out) 'status) 'running)))
    (void)))

(define (pre-installer collections-top-path this-collection-path)
  (call-and-wait! "make"))