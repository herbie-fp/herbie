#lang racket

(require reports/xml-common)
(provide (all-defined-out) (all-from-out reports/xml-common))

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
