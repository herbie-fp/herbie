#lang racket

(require reports/tools-common)
(require reports/markdown-tools)
(require reports/make-graph)
(require casio/load-bench)
(require casio/test)
(require casio/common)
(require casio/points)
(require casio/main)
(require casio/programs)
(require casio/alternative)
(require racket/date)

(define *graph-folder-name-length* 8)

(define disallowed-strings '("/" " " "(" ")"))

(define (strip-string s)
  (pipe s (map (λ (p) (λ (s) (string-replace s p "")))
	       disallowed-strings)))

(define (graph-folder-path tname index)
  (let ([stripped-tname (strip-string tname)])
    (string-append "graphs/" (number->string index) (substring stripped-tname 0 (min (string-length stripped-tname) *graph-folder-name-length*)) "/")))

(define (test-result test)
  (let ([start-prog (make-prog test)])
    (let-values ([(start-end-points-exacts-list cpu-mil real-mil garbage-mill)
		  (time-apply (λ (orig)
				 (with-handlers ([(const #t) (const (make-list 4 '()))])
				   (let-values ([(points exacts) (prepare-points start-prog)])
				     (parameterize ([*points* points] [*exacts* exacts])
				       (let* ([start-alt (make-alt orig)]
					      [end-alt (improve-on-points start-alt (*num-iterations*))])
					 (list start-alt end-alt points exacts))))))
			      (list start-prog))])
      (append (car start-end-points-exacts-list) (list real-mil)))))

(define (table-row results test)
  (append (list (test-name test))
	  (if (null? (car results))
	      (append (make-list 5 "N/A") (list 'Yes))
	      (append (get-improvement-columns (car results)  (cadr results) (test-output test)) (list 'No)))
	  (list (fifth results))))

(define (get-improvement-columns start end expected-output)
  (let* ([start-errors (alt-errors start)]
	 [end-errors (alt-errors end)]
	 [diff (errors-difference start-errors end-errors)]
	 [annotated-diff (map list diff start-errors end-errors)]) ;; We use this annotated-diff because eventually we'll want to show in some way which points have inf improvement.
    (let*-values ([(reals infs) (partition (compose reasonable-error? car) annotated-diff)]
		  [(good bad) (partition (compose positive? car) infs)])
      (list (/ (apply + (map car reals)) (length reals))
	    (length good)
	    (length bad)
	    (alt-program end)
	    (if expected-output
		(if (equal? expected-output (program-body (alt-program end))) 'Yes 'No)
		"N/A")))))

(define (univariate-tests bench-dir)
  (filter (λ (test) (= 1 (length (test-vars test))))
	  (load-all #:bench-path-string bench-dir)))

(define table-labels '("Name"
		       "Error Improvement"
		       "Points with Immeasurable Improvement"
		       "Points with Immeasurable Regression"
		       "Resulting Program"
		       "Passed Test"
		       "Crashed?"
		       "Time Taken (Milliseconds)"))

(define (bad? row)
  (or (not (number? (list-ref row 7)))
      (< 10000 (list-ref row 7))
      (not (number? (list-ref row 1)))
      (> -1 (list-ref row 1))
      (eq? 'Yes (list-ref row 5))))

(define (good? row)
  (and (not (bad? row))
       (< 5 (list-ref row 1))))

(define (get-test-results tests)
  (progress-map test-result tests
		#:map-name 'execute-tests
		#:item-name-func test-name
		#:show-time #t))

(define (get-table-data results tests)
  (map table-row results tests))

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

(define (make-report bench-dir)
  (let ([cur-date (current-date)]
	[commit (strip-end (with-output-to-string (lambda () (system "git rev-parse HEAD"))) 1)]
	[branch (strip-end (with-output-to-string (lambda () (system "git rev-parse --abbrev-ref HEAD"))) 1)]
	[tests (univariate-tests bench-dir)])
    (let* ([results (get-test-results tests)]
	   [table-data (get-table-data results tests)])
      (when (not (directory-exists? "graphs")) (make-directory "graphs"))
      (map (λ (result tname index)
	      (let ([dir (graph-folder-path tname index)])
		(when (not (null? (first result)))
		  (when (not (directory-exists? dir)) (make-directory dir))
		  (make-graph (first result) (second result) (third result) (fourth result) dir "reports/graph.js"))))
	   results
	   (map test-name tests)
	   (build-list (length tests) identity))
      (write-file "report.md"
		  (info-stamp cur-date commit branch)
		  (make-table table-labels table-data
			      #:modifier-alist `((,bad? . red)
						 (,good? . green))
			      #:row-links (map (lambda (test index) (string-append (graph-folder-path (test-name test) index) "graph.html"))
					       tests
					       (build-list (length tests) identity)))))))

;; No longer maintained
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

