#lang racket

(require racket/date)
(require "../common.rkt")
(require "thread-pool.rkt")
(require json)

(define (parse-folder-name name)
  (let ([name (path->string name)])
    (if (= (length (string-split name ":")) 4)
        (string-split name ":")
        (string-split name "-"))))

(define name->timestamp (compose string->number first parse-folder-name))

(struct report-info (folder date commit branch seed flags points iterations master? note tests))

(define (read-report-info folder)
  (let ([info-file (build-path report-output-path "reports" folder "results.json")])
    (if (file-exists? info-file)
        (let* ([json (call-with-input-file info-file read-json)]
               [get (λ (field) (hash-ref json field))])
                                        ; TODO: Not getting the tests field
          (report-info folder (seconds->date (get 'date)) (get 'commit) (get 'branch) (get 'seed)
                       (get 'flags) (get 'points) (get 'iterations) (get 'master) (hash-ref json 'note #f)
                       (for/list ([test (get 'tests)])
                         (let ([get (λ (field) (hash-ref test field))])
                           ; TODO: ignoring the result-est
                           (table-row (get 'name) (get 'status) (get 'start) (get 'end) (get 'target)
                                      (get 'ninf) (get 'pinf) 0 (get 'vars) (get 'input) (get 'output)
                                      (get 'time) (get 'bits) (get 'link))))))
        (match (parse-folder-name folder)
          [`(,timestamp ,hostname ... ,branch ,commit)
           (report-info folder (seconds->date (string->number timestamp)) commit branch
                        #f #f #f #f #f #f #f)]))))

(define (print-list infos)
  (printf "<ul id='reports'>\n")
  (for/list ([info infos])
    (match info
      [(report-info folder date commit branch seed flags points iterations master? note tests)
       (printf "<li class='~a'>" (if master? "master" ""))
       (printf "<a href='./~a/report.html'>~a</a>, on <abbr title='~a'>~a</abbr>"
               folder (date->string date) commit branch)
       (when note (printf "<p>~a</p>" note))
       (printf "</li>\n")]))
  (printf "</ul>\n"))

(define (make-index-page)
  (let* ([folders
          (map read-report-info
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

      (print-list
       (take-up-to
        (filter (λ (e) (and (report-info-master? e) (equal? (report-info-branch e) "master"))) folders)
        5))
      
      (for/list ([branch (multipartition folders report-info-branch)])
        (printf "<h2>Latest in <code>~a</code></h2>" (report-info-branch (car branch)))
        (print-list (take-up-to (cdr branch) 5)))
      
      (printf "<h2>All reports</h2>\n")

      (print-list folders)

      (printf "</body>\n"))))

(make-index-page)
