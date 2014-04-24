#lang racket

(require reports/tools-common)

(provide (all-defined-out) (all-from-out reports/tools-common))

;; args should be in the form of an alist
(define-syntax (tag stx)
  (syntax-case stx ()
    [(_ label #:args args)
     #'(begin (text "<" label)
	      (for ([arg (reverse args)])
		(text " " (car arg) "=\"" (cdr arg) "\""))
	      (text ">")
	      (text "</" label ">"))]
    [(_ label #:args args . rest)
     #'(begin (text "<" label)
	      (for ([arg (reverse args)])
		(text " " (car arg) "=\"" (cdr arg)"\""))
	      (text ">")
	      ((lambda () . rest))
	      (text "</" label ">"))]
    [(_ label . rest)
     #'(tag label #:args '() . rest)]))

(define-syntax (unitag stx)
  (syntax-case stx ()
    [(_ label #:args args)
     #'(begin (text "<" label)
	      (for ([arg args])
		(text " " (car arg) "=\"" (cdr arg) "\""))
	      (text "/>"))]
    [(_ label)
     #'(unitag label #:args '())]))

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
