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

(define (table-row test)
  (let-values ([(end start) (improve (make-prog test) (*num-iterations*))])
    (let ([start-errors (alt-errors start)]
	  [end-errors (alt-errors end)])
      (let ([diff-score (errors-diff-score start-errors end-errors)])
	(list (test-name test) diff-score)))))

(define univariate-tests
  (filter (λ (test) (= 1 (length (test-vars test))))
	  (load-all)))

(define table-labels '("Test Name" "Error Improvement"))

(define (get-table-data)
  (cons table-labels
	(progress-map table-row univariate-tests
		      #:map-name 'execute-tests)))

(define (info-stamp cur-date cur-commit cur-branch)
  (b (text (date-year cur-date) " "
	   (date-month cur-date) " "
	   (date-day cur-date) ", "
	   (date-hour cur-date) ":"
	   (date-minute cur-date) ":"
	   (date-second cur-date))
     (br)(newline)
     (text "Commit: " cur-commit " on " cur-branch)(br)))

(define (make-report)
  (let ([cur-date (current-date)]
	[commit (with-output-to-string (lambda () (system "git rev-parse HEAD")))]
	[branch (with-output-to-string (lambda () (system "git rev-parse --abbrev-ref HEAD")))]
	[results (get-table-data)])
    (write-file "report.html"
		(heading)
		(html (newline)
		      (body (newline)
			    (info-stamp cur-date commit branch)
			    (newline)
			    (make-table results)
			    (newline))
		      (newline)))))

(define (make-dummy-report)
  (let ([cur-date (current-date)]
	[commit (with-output-to-string (lambda () (system "git rev-parse HEAD")))]
	[branch (with-output-to-string (lambda () (system "git rev-parse --abbrev-ref HEAD")))])
    (write-file "test.html"
		(heading)
		(html (newline)
		      (body (newline)
			    (info-stamp cur-date commit branch)
			    (newline)
			    (make-table (cons table-labels '((1 2 3) (4 5 6) (7 8 9))))
			    (newline))
		      (newline)))))

;;(make-report)

(define (progress-map f l #:map-name [name 'progress-map])
  (let ([total (length l)])
    (let loop ([rest l] [acc '()] [done 0])
      (println name ": "
	       (quotient (* 100 done) total)
	       "%")
      (if (null? rest)
	  (reverse acc)
	  (loop (cdr rest) (cons (f (car rest)) acc) (1+ done))))))
