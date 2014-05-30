#lang racket

(provide (all-defined-out))

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

(define-syntax (xml-comment stx)
  (syntax-case stx ()
    [(_ . rest)
     #'(begin (text "<!--")
	      ((lambda () . rest))
	      (text "-->"))]))

;; Simple inline printing
(define (text . args)
  (for ([val args])
    (cond [(string? val)
	   (display val)]
	  [(number? val)
	   (display (~a val #:max-width 7))]
	  [#t
	   (write val)])))

(make-unitag line)
(make-unitag circle)
(make-unitag path)
(make-tag svg)

(define-syntax (text-tag stx)
  (syntax-case stx ()
    [(_ #:args args . rest) #'(tag 'text #:args args . rest)]
    [(_ . rest) #'(tag 'text . rest)]))

(define-syntax (draw-text stx)
  (syntax-case stx ()
    [(_ pos rot . rest) #'(text-tag #:args `((x . ,(car pos)) (y . ,(cdr pos))
					     (fill . "black")
					     (transform . ,(string-append "rotate(" (number->string rot)
									  " " (number->string (car pos))
									  "," (number->string (cdr pos))
									  ")")))
				    . rest)]))
