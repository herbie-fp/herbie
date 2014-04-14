#lang racket

(provide (all-defined-out))

(require reports/tools-common)

;; args should be in the form of an alist
(define-syntax (tag stx)
  (syntax-case stx ()
    [(_ label #:args args)
     #'(begin (text "<" label)
	      (for ([arg args])
		(text " " (car arg) "=\"" (cdr arg) "\""))
	      (text ">")
	      (text "</" label ">"))]
    [(_ label #:args args . rest)
     #'(begin (text "<" label)
	      (for ([arg args])
		(text " " (car arg) "=\"" (cdr arg)"\""))
	      (text ">")
	      ((lambda () . rest))
	      (text "</" label ">"))]
    [(_ label . rest)
     #'(tag label #:args '() . rest)]))

(define-syntax (unitag stx)
  (syntax-case stx ()
    [(_ label #:args args)
     #'(begin (text "</" label)
	      (for ([arg args])
		(text " " (car arg) "=\"" (cdr arg) "\""))
	      (text ">"))]
    [(_ label)
     #'(begin (text "</" label ">"))]))

(define-syntax (make-tag stx)
  (syntax-case stx ()
    [(_ tagname)
     #'(define-syntax (tagname stx)
	 (syntax-case stx ()
	   [(_ #:args args . rest) #'(tag 'tagname #:args args . rest)]
	   [(_ . rest) #'(tag 'tagname . rest)]))]))

(define-syntax (make-unitag stx)
  (syntax-case stx ()
    [(_ tagname)
     #'(define-syntax (tagname stx)
	 (syntax-case stx ()
	   [(_ #:args args) #'(unitag 'tagname #:args args)]
	   [(_) #'(unitag 'tagname)]))]))

(define (make-table datum)
  (table #:args '((border . 1) (style . 'width:300px)) (newline)
	 (for/list ([row datum])
	   (tr (for/list ([item row])
		 (td (text item)))
	       (newline)))))

(make-tag head)
(make-tag body)
(make-tag html)
(make-tag script)
(make-tag td)
(make-tag tr)
(make-tag table)
(make-tag b)
(make-unitag br)

(define (heading)
  (text "<!DOCTYPE html>") (newline))
