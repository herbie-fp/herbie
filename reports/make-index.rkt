#lang racket

(require racket/date)
(require casio/common)

(define (make-index-page folders)
  (let* ([sorted-folders (sort folders > #:key (compose string->number first (curryr string-split "-")))])
    (write-file "index.html"
      (printf "<!doctype html>\n")
      (printf "<html>")
      (printf "<head><meta charset='utf-8' /><title>Casio Reports</title></head>\n")
      (printf "<body>\n")
      (printf "<h1>Reports</h1>\n")
      (printf "<ul id='reports'>\n")
      (for/list ([folder sorted-folders])
        (match (string-split folder "-")
          [`(,timestamp ,hostname ... ,branch ,commit)
           (printf "<li><a href='~a/report.html'>Report on ~a in ~a (<code>~a</code>) by <code>~a</code></a></li>\n"
                   folder
                   (date->string (seconds->date (string->number timestamp)))
                   branch
                   commit
                   (string-join hostname "-"))]))
      (printf "</ul>\n")
      (printf "</body>\n"))))

(make-index-page
 (command-line
  #:program "make-index"
  #:args foldernames
  foldernames))
