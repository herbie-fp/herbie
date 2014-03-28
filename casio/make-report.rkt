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
  (with-handlers ([(const #t) (const `(,(test-name test) "N/A" #t))])
    (let-values ([(end start) (improve (make-prog test) (*num-iterations*))])
      (let ([start-errors (alt-errors start)]
	    [end-errors (alt-errors end)])
	(let ([diff-score (errors-diff-score start-errors end-errors)])
	  (list (test-name test) (/ 100 (truncate (* 100  (/ diff-score (length start-errors)) #f)))))))))

(define univariate-tests
  (filter (λ (test) (= 1 (length (test-vars test))))
	  (load-all #:bench-path-string "../bench/")))

(define table-labels '("Test Name" "Error Improvement" "Crashed?"))

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

(define (filename-stamp cur-date cur-commit cur-branch)
  (string-append (number->string (date-year cur-date)) "-"
		 (number->string (date-month cur-date)) "-"
		 (number->string (date-day cur-date)) "--"
		 (number->string (date-hour cur-date)) ":"
		 (number->string (date-minute cur-date)) ":"
		 (number->string (date-second cur-date)) "C"
		 cur-commit "B" cur-branch))

(define (make-report)
  (let ([cur-date (current-date)]
	[commit (with-output-to-string (lambda () (system "git rev-parse HEAD")))]
	[branch (with-output-to-string (lambda () (system "git rev-parse --abbrev-ref HEAD")))]
	[results (get-table-data)])
    (write-file (string-append (filename-stamp cur-date commit branch) "-report.html")
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

(define (progress-map f l #:map-name [name 'progress-map])
  (let ([total (length l)])
    (let loop ([rest l] [acc '()] [done 0])
      (if (null? rest)
	  (reverse acc)
	  (begin (println name ": "
			  (quotient (* 100 done) total)
			  "%")
		 (loop (cdr rest) (cons (f (car rest)) acc) (1+ done)))))))


(make-report)
