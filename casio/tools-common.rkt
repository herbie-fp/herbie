#lang racket

(provide (all-defined-out))

(define-syntax (qexpand stx)
  (syntax-case stx ()
    [(_ expr)
     #'(syntax->datum (expand-once 'expr))]))
;; Simple inline printing
(define (text . args)
  (for ([val args])
    (if (string? val)
	(display val)
	(write val))))

(define-syntax (write-file stx)
  (syntax-case stx ()
    [(_ filename . rest)
     #'(with-output-to-file filename (lambda () . rest) #:exists 'replace)]))
