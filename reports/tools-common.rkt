#lang racket

(provide write-file write-string)

(define-syntax (write-file stx)
  (syntax-case stx ()
    [(_ filename . rest)
     #'(with-output-to-file filename (lambda () . rest) #:exists 'replace)]))

(define-syntax (write-string stx)
  (syntax-case stx ()
    [(_ . rest)
     #'(with-output-to-string (lambda () . rest))]))
