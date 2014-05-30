#lang racket

(require reports/tools-common)
(require racket/date)

(define (make-index-page foldernames)
  (let ([sorted-files (sort (filter identity (map (lambda (filename) (with-handlers ([(const #t) (const #f)])
								       (list (parse-datestring filename) (substring filename 13) filename)))
						  foldernames))
			    (lambda (f1 f2)
			      (> (string->number (substring (caddr f1) 0 12)) (string->number (substring (caddr f2) 0 12)))))])
    (write-file "index.html"
      (printf "<!doctype html>\n")
      (printf "<html>")
      (printf "<head><meta charset='utf-8' /><title>Casio Reports</title></head>\n")
      (printf "<body>\n")
      (printf "<h1>Reports</h1>\n")
      (printf "<ul id='reports'>\n")
      (for/list ([file sorted-files])
        (printf "<li><a href='~a/report.html'>Report at ~a on ~a</a></li>\n"
                (third file) (date->string (first file)) (second file)))
      (printf "</ul>\n")
      (printf "</body>\n"))))

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

(make-index-page
 (command-line
  #:program "make-index"
  #:args foldernames
  foldernames))
