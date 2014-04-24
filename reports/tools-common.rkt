#lang racket

(provide (all-defined-out))

(define-syntax (qexpand stx)
  (syntax-case stx ()
    [(_ expr)
     #'(syntax->datum (expand-once 'expr))]))

;; Simple inline printing
(define (text . args)
  (for ([val args])
    (cond [(string? val)
	   (display val)]
	  [(number? val)
	   (display (~a val #:max-width 7))]
	  [#t
	   (write val)])))

(define-syntax (write-file stx)
  (syntax-case stx ()
    [(_ filename . rest)
     #'(with-output-to-file filename (lambda () . rest) #:exists 'replace)]))

(define-syntax (write-string stx)
  (syntax-case stx ()
    [(_ . rest)
     #'(with-output-to-string (lambda () . rest))]))
