#lang racket

(require casio/alternative)
(require casio/points)
(require reports/html-tools)
(require reports/tools-common)
(require json)

(provide (all-defined-out))

;; Makes a graph of the error-performance of a run
;; with starting alt 'start' and ending alt 'end',
;; and writes it to a file at filename. dir should
;; be a string.
(define (make-graph start end points exacts dir path-to-graphscript)
  (copy path-to-graphscript (string-append dir "graph.js"))
  (make-graph-json start end (string-append dir "data.json") points exacts)
  (let-values ([(points exacts) (prepare-points (alt-program start))])
    (write-file (string-append dir "graph.html")
		(head (newline)
		      (script #:args '((src . "http://d3js.org/d3.v3.min.js") (charset . "utf-8")))
		      (newline))
		(body (newline)
		      (script #:args '((src . "graph.js") (charset . "utf-8")))
		      (newline))
		)))

(define (point-filter p) (and (reasonable-error? (cadr p)) (> (cadr p) 0)))

(define (make-graph-json start end filename points exacts)
  (let ([json-object `(,(filter point-filter (map list (map car points) (alt-errors start)))
		       ,(filter point-filter (map list (map car points) (alt-errors end)))
		       ,(filter point-filter (map list (map car points) exacts)))])
    (write-file filename
		(write-json json-object))))
