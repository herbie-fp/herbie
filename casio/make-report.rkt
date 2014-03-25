#lang racket

(require casio/htmltools)
(require casio/load-bench)
(require casio/test)
(require casio/common)
(require casio/points)
(require casio/main)
(require casio/programs)
(require casio/alternative)
(require racket/date)

(define (get-table-data)
  (cons '("Test Name" "Errors Before Improvement" "Errors After Imrovement" "Total Improvement")
	(map (lambda (test) (let-values ([(end start) (improve (make-prog test) (*num-iterations*))])
			      (let ([start-error-score (errors-score (alt-errors start))]
				    [end-error-score (errors-score (alt-errors end))]
				    [errors-diff=score (errors-diff-score (alt-errors start) (alt-errors end))])
				(list (test-name test) start-error-score end-error-score errors-diff-score))))
	     (filter (lambda (test) (= 1 (length (test-vars test))))
		     (load-all)))))

(define (make-report)
  (let ([cur-date (current-date)]
	[commit (with-output-to-string (lambda () (system "git rev-parse HEAD")))]
	[branch (with-output-to-string (lambda () (system "git rev-parse --abbrev-ref HEAD")))]
	[results (get-table-data)])
    (write-file "report.html"
		(text "<!DOCTYPE html>") (newline)
		(html (newline)
		      (body (newline)
			    (b (text (date-year cur-date) " "
				     (date-month cur-date) " "
				     (date-day cur-date) " "
				     (date-hour cur-date) ":"
				     (date-minute cur-date) ":"
				     (date-second cur-date)))
			    (newline)
			    (text " Commit: " commit " on " branch))
		      (newline)
		      (make-table results)
		      (newline))
		(newline))))

(define (make-dummy-report)
  (write-file "test.html"
	      (text "<!DOCTYPE html>") (newline)
	      (html (newline)
		    (body (newline)
			  (make-table '((1 2 3) (4 5 6) (7 8 9)))
			  (newline))
		    (newline))))

(make-report)
