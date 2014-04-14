#lang racket

(require casio/tools-common)
(require casio/markdown-tools)
(require racket/date)

(define (make-index-page foldernames)
  (let ([sorted-files (sort (map (lambda (filename) (list (parse-datestring filename) (substring filename 13) filename))
				 foldernames)
			    (lambda (f1 f2)
			      (> (string->number (substring (caddr f1) 0 12)) (string->number (substring (caddr f2) 0 12)))))])
    (write-file "index.md"
		(make-heading "Reports")
		(for/list ([file sorted-files])
		  (index-row (car file) (cadr file) (caddr file))
		  (newline)
		  (newline)))))

(define (parse-datestring filename)
  (let ([year-string (substring filename 0 2)]
	[month-string (substring filename 2 4)]
	[day-string (substring filename 4 6)]
	[hour-string (substring filename 6 8)]
	[minute-string (substring filename 8 10)]
	[second-string (substring filename 10 12)])
    (let ([year (+ 2000 (string->number year-string))]
	  [month (string->number month-string)]
	  [day (string->number day-string)]
	  [hour (string->number hour-string)]
	  [minute (string->number minute-string)]
	  [second (string->number second-string)])
      (date second minute hour day month year 0 0 #f 0))))

(define (index-row date host-branch-commit filename)
  (text "[" (date->string date) " on " host-branch-commit "](" filename "/report.html)"))

(make-index-page
 (command-line
  #:program "make-index"
  #:args foldernames
  foldernames))
