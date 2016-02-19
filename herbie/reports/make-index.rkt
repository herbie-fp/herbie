#lang racket

(require racket/date)
(require "../common.rkt")
(require "datafile.rkt")

(provide read-report-info name->timestamp)

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

(define (month->string i)
  (list-ref (string-split "Jan Feb Mar Apr May Jun Jul Aug Sept Oct Nov Dec") (- i 1)))

(define (date->string/short date)
  (format "~a ~a, ~a:~a"
          (month->string (date-month date))
          (~r (date-day date) #:min-width 2 #:pad-string "0")
          (~r (date-hour date) #:min-width 2 #:pad-string "0")
          (~r (date-minute date) #:min-width 2 #:pad-string "0")))

(define (print-rows infos #:name name)
  (printf "<thead id='reports-~a' data-branch='~a'><th>Date</th><th>Branch</th><th>Collection</th><th>Tests</th><th>Bits</th></thead>\n" name name)
  (printf "<tbody>\n")
  (for ([(folder info) (in-pairs infos)])
    (match-define (report-info date commit branch seed flags points iterations bit-width note tests) info)

    (define-values (total-start total-end)
      (for/fold ([start 0] [end 0]) ([row (or tests '())])
        (values
         (+ start (or (table-row-start row) 0))
         (+ end (or (table-row-result row) 0)))))

     (define total-passed
       (for/sum ([row (or tests '())])
         (if (member (table-row-status row) '("gt-target" "eq-target" "imp-start")) 1 0)))
     (define total-available
       (for/sum ([row (or tests '())])
         (if (not (equal? (table-row-status row) "ex-start")) 1 0)))

    (define (round* x)
      (cond
       [(>= (abs x) (expt 10 6))
        "?"]
       [(>= (abs x) 10)
        (~a (inexact->exact (round x)))]
       [else
        (~r x #:precision 2)]))

    (printf "<tr>")
    (printf "<td title='~a:~a on ~a'><time data-unix='~a'>~a</time></td>"
            (date-hour date) (~r (date-minute date) #:min-width 2 #:pad-string "0")
            ;; TODO: Best to output a datetime field in RFC3338 format,
            ;; but Racket doesn't make that easy.
            (date->string date) (date->seconds date) (date->string/short date))
    (printf "<td title='~a'>~a</td>" commit branch)
    (if note
        (printf "<td class='note'>~a</td>" note)
        (printf "<td>⭐</td>"))
    (if tests
        (printf "<td>~a/~a</td>" total-passed total-available)
        (printf "<td></td>"))
    (if (> total-start 0)
        (printf "<td>~a/~a</td>" (round* (- total-start total-end)) (round* total-start))
        (printf "<td></td>"))
    (printf "<td><a href='./~a/report.html'>»</a></td>" folder)
    (printf "</tr>\n"))
  (printf "</tbody>\n"))

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
      (printf "<script src='http://d3js.org/d3.v3.min.js' charset='utf-8'></script>\n")
      (printf "<script src='regression-chart.js'></script>\n")
      (printf "<script src='report.js'></script>\n")
      (printf "</head>\n")
      (printf "<body onload='index()'>\n")

      (define branch-infos*
        (sort
         (multipartition folders (compose report-info-branch cdr))
         > #:key (λ (x) (date->seconds (report-info-date (cdar x))))))

      (define-values (master-info* other-infos)
        (partition (λ (x) (equal? (report-info-branch (cdar x)) "master"))
                   branch-infos*))

      ;; This is a hack due to the use of partition to simultaneously
      ;; find the reports with branch master, and also to remove it
      ;; from the list.
      (define master-info (car master-info*))

      ; Big bold numbers
      (printf "<div id='large'>\n")
      (printf "<div>Reports: <span class='number'>~a</span></div>\n"
              (length folders))
      (printf "<div>Branches: <span class='number'>~a</span></div>\n"
              (length branch-infos*))
      (printf "<div>On Master: <span class='number'>~a</span></div>\n"
              (length master-info))
      (printf "</div>\n")

      (printf "<ul id='toc'>")
      (for ([rows (cons master-info other-infos)])
        (define branch (report-info-branch (cdar rows)))
        (printf "<li><a href='#reports-~a'>~a</a></li>" branch branch))
      (printf "</ul>")

      (printf "<figure><svg id='graph' width='800'></svg>\n")
      (printf "<ul id='suites'></ul>")
      (printf "<script>window.addEventListener('load', function(){draw_results(d3.select('#graph'))})</script>\n")
      (printf "</figure>\n")

      (printf "<table id='reports'>\n")
      (print-rows master-info #:name "master")
      (for ([rows other-infos])
        (print-rows rows #:name (report-info-branch (cdar rows))))
      (printf "</table>\n")

      (printf "</body>\n")
      (printf "</html>\n"))))

(module+ main
  (make-index-page))
