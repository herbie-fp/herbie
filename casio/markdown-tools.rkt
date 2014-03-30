#lang racket

(require casio/tools-common)

(provide (all-defined-out))

(define-syntax (mode stx)
  (syntax-case stx ()
    [(_ delimitor . rest)
     #'(begin (text delimitor)
	      ((lambda () . rest))
	      (text delimitor))]))

(define-syntax (make-mode stx)
  (syntax-case stx ()
    [(_ delimitor name)
     #'(define-syntax (name stx)
	 (syntax-case stx ()
	   [(_ . rest)
	    #'(mode delimitor . rest)]))]))

(make-mode '** bold)

(define (make-table labels data)
  (text "|")
  (for/list ([label labels])
    (text label "\t\t|"))
  (newline)
  (text "|")
  (for/list ([label labels])
    (text ":------:|"))
  (for/list ([datum data])
    (newline)
    (text "|")
    (for/list ([cell datum])
      (text cell "|")))
  (void))
