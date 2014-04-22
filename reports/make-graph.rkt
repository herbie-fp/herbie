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
(define (make-graph start end points exacts dir include-js include-css)
  ;; Returns the full local path of the canonical location for an included file with the given extension.
  (define (include-path index extension)
    (string-append dir (include-name index extension)))

  (define (include-name index extension)
    (string-append (number->string index) "include." extension))

  ;; Copy the js files to our graph directory
  (for/list ([js include-js] [i (build-list (length include-js) identity)])
    (copyr-file js (include-path i "js")))

  ;; Copy the css files to our graph directory 
  (for/list ([css include-css] [i (build-list (length include-css) identity)])
    (copyr-file css (include-path i "css")))

  ;; Generate the json for our data
  (make-graph-json start end (string-append dir "data.json") points exacts)

  ;; Generate the html for our graph page
  (let ([page-path (string-append dir "graph.html")])
    (write-file page-path
		(html (newline)
		      (head (newline)
			    ;; Include all our given css
			    (for/list ([i (build-list (length include-css) identity)])
			      (link #:args `((rel . "stylesheet") (type . "text/css") (href . ,(include-name i "css")) (media . "screen")))
			      (newline))
			(newline)
			(body (newline)
			      ;; Include all our given javascript
			      (for/list ([i (build-list (length include-js) identity)])
				(script #:args `((src . ,(include-name i "js")) (charset . "utf-8")))
				(newline))
			      (newline))
		  )))))

;; Copies a file, replacing the file at destination if it exists.
(define (copyr-file src dest)
  (when (file-exists? dest) (delete-file dest))
  (copy-file src dest))

(define (get-json-data points data-points)
  (filter (compose reasonable-error? cadr)
	  (map list (map car points) data-points)))

(define (make-graph-json start end filename points exacts)
  (let ([json-object `(,(get-json-data points (alt-errors start))
		       ,(get-json-data points (alt-errors end))
		       ,(get-json-data points (errors-difference (alt-errors start)
							  (alt-errors end)))
		       ,(get-json-data points exacts))])
    (write-file filename
		(write-json json-object))))
