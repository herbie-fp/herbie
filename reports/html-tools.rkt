#lang racket

(require reports/xml-common)

(provide (all-defined-out) (all-from-out reports/xml-common))

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
(make-unitag link)
(make-unitag br)

(define (heading)
  (text "<!DOCTYPE html>") (newline))
