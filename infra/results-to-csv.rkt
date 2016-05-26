#lang racket

(require "../src/common.rkt")
(require "../src/reports/datafile.rkt")

(define (results-to-csv infile outfile)
  (let ([lines (read-datafile infile)])
    (write-file outfile
		(for ([line lines])
		  (display (car line))
		  (for ([item (cdr line)])
		    (display ",")
		    (display item))
		  (newline)))))

(command-line
 #:program "results-to-csv"
 #:args (infile outfile)
 (results-to-csv infile outfile))
