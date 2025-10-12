#lang racket/base

(require json)

(define count-list
  (call-with-input-file "reports/counts.rkt" read))

(define test-list (map (lambda (p) (cons (string->symbol (format "~a"(car p))) (cdr p))) count-list))
(define hash (make-hash test-list))
(call-with-output-file "reports/counts.json"
  (lambda (out) (write-json hash out))
  #:exists 'replace)