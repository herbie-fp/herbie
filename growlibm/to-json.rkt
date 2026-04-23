#lang racket/base

(require json)

(define file-name (vector-ref (current-command-line-arguments) 0))

(define l
  (call-with-input-file (string-append "reports/" file-name ".rkt") read))

(define test-list (map (lambda (p) (cons (string->symbol (format "~a"(car p))) (cdr p))) l))
(define hash (make-hash test-list))

(call-with-output-file (string-append "reports/" file-name ".json")
  (lambda (out) (write-json hash out))
  #:exists 'replace)