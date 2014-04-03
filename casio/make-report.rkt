#lang racket

(require casio/tools-common)
(require casio/markdown-tools)
(require casio/load-bench)
(require casio/test)
(require casio/common)
(require casio/points)
(require casio/main)
(require casio/programs)
(require casio/alternative)
(require racket/date)

(define (table-row test)
  (let-values ([(improvement-cols cpu-mil real-mil garbage-mil)
		(time-apply (lambda (test) (with-handlers ([(const #t) (const '("N/A" "N/A" "N/A" Yes))])
					     (let-values ([(end start) (improve (make-prog test) (*num-iterations*))])
					       (append (get-improvement-columns start end) (list 'No)))))
			    (list test))])
    (append (list (test-name test)) (car improvement-cols) (list real-mil))))

(define (get-improvement-columns start end)
  (let* ([start-errors (alt-errors start)]
	 [end-errors (alt-errors end)]
	 [diff (errors-difference start-errors end-errors)]
	 [annotated-diff (map list diff start-errors end-errors)]) ;; We use this annotated-diff because eventually we'll want to show in some way which points have inf improvement.
    (let*-values ([(reals infs) (partition (compose reasonable-error? car) annotated-diff)]
		  [(good bad) (partition (compose positive? car) infs)])
      (list (/ (apply + (map car reals)) (length reals))
	    (length good)
	    (length bad)))))

(define (univariate-tests bench-dir)
  (filter (λ (test) (= 1 (length (test-vars test))))
	  (load-all #:bench-path-string bench-dir)))

(define table-labels '("Test Name"
		       "Error Improvement"
		       "Points with Immeasurable Improvement"
		       "Points with Immeasurable Regression"
		       "Crashed?"
		       "Time Taken (Milliseconds)"))

(define (get-table-data bench-dir)
  (progress-map table-row (univariate-tests bench-dir)
		#:map-name 'execute-tests
		#:item-name-func test-name
		#:show-time #t))

(define (info-stamp cur-date cur-commit cur-branch)
  (bold (text (date-year cur-date) " "
	      (date-month cur-date) " "
	      (date-day cur-date) ", "
	      (date-hour cur-date) ":"
	      (date-minute cur-date) ":"
	      (date-second cur-date)))
  (newline)
  (newline)
  (bold (text "Commit: " cur-commit " on " cur-branch))
  (newline)
  (newline))

(define (strip-end string num-chars)
  (substring string 0 (- (string-length string) num-chars)))

(define (bad? row)
  (or (not (number? (list-ref row 5)))
      (< 10000 (list-ref row 5))
      (not (number? (list-ref row 1)))
      (> -1 (list-ref row 1))
      (eq? 'Yes (list-ref row 4))))

(define (good? row)
  (and (not bad? row)
       (< 5 (list-ref row 1))))

(define (make-report bench-dir)
  (let ([cur-date (current-date)]
	[commit (strip-end (with-output-to-string (lambda () (system "git rev-parse HEAD"))) 1)]
	[branch (strip-end (with-output-to-string (lambda () (system "git rev-parse --abbrev-ref HEAD"))) 1)]
	[results (get-table-data bench-dir)])
    (write-file "report.md"
		(info-stamp cur-date commit branch)
		(make-table table-labels results #:modifier-alist `((,bad? . red)
								    (,good? . green))))))

(define (make-dummy-report)
  (let ([cur-date (current-date)]
	[commit (with-output-to-string (lambda () (system "git rev-parse HEAD")))]
	[branch (with-output-to-string (lambda () (system "git rev-parse --abbrev-ref HEAD")))])
    (write-file "test.md"
		(info-stamp cur-date commit branch)
		(make-table '(A B C) '((1 2 3) (4 5 6) (7 8 9)) #:modifier-alist `((,(lambda (row) (> 3 (cadr row))) . green)
										   (,(lambda (row) (< 7 (cadr row))) . red))))))

(define (string-when test value)
  (if test
      value
      ""))

(define (progress-map f l #:map-name [name 'progress-map] #:item-name-func [item-name #f] #:show-time [show-time? #f])
  (let ([total (length l)])
    (let loop ([rest l] [acc '()] [done 1])
      (if (null? rest)
	  (reverse acc)
	  (let-values ([(results cpu-mil real-mil garbage-mill) (time-apply f (list (car rest)))])
	    (println name
		     ": "
		     (quotient (* 100 done) total)
		     "%\t["
		     (string-when show-time? (~a real-mil #:width 8))
		     (string-when show-time? " milliseconds]")
		     (string-when show-time? "\t\t")
		     (string-when item-name (item-name (car rest))))
	    (loop (cdr rest) (cons (car results) acc) (1+ done)))))))


(make-report
 (command-line
  #:program "make-report"
  #:multi [("-d") "Turn On Debug Messages (Warning: Very Verbose)"
	   (*debug* #t)]
  #:args (bench-dir)
  bench-dir))

