#lang racket

(require racket/date)
(require unstable/sequence)
(require "../common.rkt")
(require "datafile.rkt")

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
       (printf "<li>")
       (printf "<a href='./~a/report.html'>~a</a>, on <abbr title='~a'>~a</abbr>"
               folder (date->string date) commit branch)
       (when note (printf "<p>~a</p>" note))
       (printf "</li>\n")]))
  (printf "</ul>\n"))

(define (make-index-page)
  (let* ([folders
          (map (λ (dir) (cons dir (read-report-info dir)))
               (remove-duplicates
                (sort (directory-list (build-path report-output-path "reports/")) > #:key name->timestamp)
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
