#lang racket

(require racket/date)
(require unstable/sequence)
(require "../common.rkt")
(require "datafile.rkt")

(provide read-report-info)

(define (parse-folder-name name)
  (let ([name (path->string name)])
    (if (= (length (string-split name ":")) 4)
        (string-split name ":")
        (string-split name "-"))))

(define name->timestamp (compose string->number first parse-folder-name))

(define (read-report-info folder)
  (let ([info-file (build-path report-output-path "reports" folder "results.json")])
    (if (file-exists? info-file)
        (read-datafile info-file)
        (match (parse-folder-name folder)
          [`(,timestamp ,hostname ... ,branch ,commit)
           (report-info (seconds->date (string->number timestamp)) commit branch
                        #f #f #f #f #f #f #f)]))))

(define (print-list infos)
  (printf "<ul id='reports'>\n")
  (for/list ([(folder info) (in-pairs infos)])
    (match info
      [(report-info date commit branch seed flags points iterations bit-width note tests)
       (define total-gained
	 (if tests
	     (for/sum ([row tests])
	       (or (table-row-result row) 0))
	     0))
       (define total-start
         (if tests
	     (for/sum ([row tests])
	       (or (table-row-start row) 0))
	     0))

       (define (round* x)
         (inexact->exact (round x)))

       (printf "<li>")
       (printf "<a href='./~a/report.html'>~a</a>, on <abbr title='~a'>~a</abbr> (improved ~a/~a)"
               folder (date->string date) commit branch
               (round* (- total-start total-gained)) (round* total-start))
       (when note (printf "<p>~a</p>" note))
       (printf "</li>\n")]))
  (printf "</ul>\n"))

(define (make-index-page)
  (define dirs (directory-list (build-path report-output-path "reports/")))

  (let* ([folders
          (map (λ (dir) (cons dir (read-report-info dir)))
               (remove-duplicates
                (sort (filter name->timestamp dirs) > #:key name->timestamp)
                #:key name->timestamp))])
    (write-file "index.html"
      (printf "<!doctype html>\n")
      (printf "<html>")
      (printf "<head>")
      (printf "<meta charset='utf-8' /><title>Herbie Reports</title>\n")
      (printf "<link rel='stylesheet' href='index.css' />\n")
      (printf "</head>\n")
      (printf "<body>\n")
      (printf "<h1>Herbie Reports</h1>\n")

      (print-list (take-up-to (filter (λ (x) (equal? (report-info-branch (cdr x)) "master")) folders) 5))
      
      (for/list ([branch
                  (filter (λ (x) (not (equal? (report-info-branch (cdar x)) "master")))
                          (sort
                           (multipartition folders (compose report-info-branch cdr)) >
                           #:key (λ (x) (date->seconds (report-info-date (cdar x))))))])
        (printf "<h2>Latest in <code>~a</code></h2>" (report-info-branch (cdar branch)))
        (print-list (take-up-to branch 5)))
      
      (printf "<h2>All reports</h2>\n")

      (print-list folders)

      (printf "</body>\n"))))

(make-index-page)
