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

(define commit (with-output-to-string (λ() (system "git rev-parse HEAD"))))
(define branch (with-output-to-string (λ() (system "git rev-parse --abbrev-ref HEAD"))))

(define (get-table-data)
  (cons '("Test Name" "Errors Before Improvement" "Errors After Imrovement" "Total Improvement")
	(map (lambda (test) (let-values ([(end start) (improve (make-prog test) (*num-iterations*))])
			      (let ([start-error-score (errors-score (alt-errors start))]
				    [end-error-score (errors-score (alt-errors end))]
				    [errors-diff=score (errors-diff-score (alt-errors start) (alt-errors end))])
				(list (test-name test) start-error-score end-error-score errors-diff-score))))
	     (filter (lambda (test) (= 1 (length (test-vars test))))
		     (load-all)))))

(write-file "test.html"
	    (text "<!DOCTYPE html>") (newline)
	    (html (newline)
		  (body (newline)
			(bold (let ([cur-date (current-date)])
				(text (date-year cur-date) " "
				      (date-month cur-date) " "
				      (date-day cur-date) " "
				      (date-hour cur-date) ":"
				      (date-minute cur-date) ":"
				      (date-second cur-date)))
			      (newline)
			      (text " Commit: " commit " on " branch))
			(newline)
			(make-table (get-table-data))
			(newline))
		  (newline)))
