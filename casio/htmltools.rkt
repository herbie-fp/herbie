#lang racket

(require casio/common)

(provide (all-defined-out))

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

(define-syntax (tag stx)
  (syntax-case stx ()
    [(_ label . rest)
     #'(begin (text "<" label ">")
	      ((lambda () . rest))
	      (text "</" label ">"))]))

(define-syntax (make-tag stx)
  (syntax-case stx ()
    [(_ tagname)
     #'(define-syntax (tagname stx)
	 (syntax-case stx ()
	   [(_ . rest) #'(tag 'tagname . rest)]))]))

(define (make-table datum)
  (table (for/list ([row datum])
	   (tr (for/list ([item row])
		 (td (text item)))))))

(make-tag head)
(make-tag body)
(make-tag html)
(make-tag td)
(make-tag tr)
(make-tag table)
(make-tag bold)

(define-syntax (qexpand stx)
  (syntax-case stx ()
    [(_ expr)
     #'(syntax->datum (expand-once 'expr))]))
