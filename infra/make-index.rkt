#lang racket
(require racket/runtime-path)
(require (only-in xml write-xexpr))
(define-runtime-path report-json-path "../graphs/reports/")


(require racket/date)
(require "../src/common.rkt")
(require "../src/formats/datafile.rkt")

(provide read-report-info name->timestamp)

(define (parse-folder-name name)
  (let ([name (path->string name)])
    (if (= (length (string-split name ":")) 4)
        (string-split name ":")
        (string-split name "-"))))

(define name->timestamp (compose string->number first parse-folder-name))

(define (read-report-info folder)
  (let ([info-file (build-path report-json-path folder "results.json")])
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

(define (get-options ri)
  (define flags
    (for*/list ([(cat flags) (in-dict (or (report-info-flags ri) '()))] [fl flags])
      (string->symbol (format "~a:~a" cat fl))))
  (if (equal? (report-info-iterations ri) 2)
      (cons 'fuel:2 flags)
      flags))

(define (print-rows infos #:name name)
  `((thead ((id ,(format "reports-~a" name)) (data-branch ,name))
           (th "Date") (th "Branch") (th "Collection") (th "Tests") (th "Bits"))
    (tbody
     ,@(for/list ([(folder info) (in-dict infos)])
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

         `(tr
           ;; TODO: Best to output a datetime field in RFC3338 format,
           ;; but Racket doesn't make that easy.
           (td ((title ,(format "~a:~a on ~a" (date-hour date) (~r (date-minute date) #:min-width 2 #:pad-string "0") (date->string date))))
               (time ((data-unix ,(format "~a" (date->seconds date))))
                     ,(date->string/short date)))
           (td ((title ,commit)) ,branch)
           (td ((title ,(string-join (map ~a (get-options info)) " "))
                (class ,(if note "note" "")))
               ,(if note note "⭐"))
           (td ,(if tests (format "~a/~a" total-passed total-available) ""))
           (td ,(if total-start (format "~a/~a" (round* (- total-start total-end))
                                        (round* total-start))
                    ""))
           (td (a ((href ,(format "./~a/report.html" folder))) "»")))))))

(define (make-index-page)
  (define dirs (directory-list report-json-path))

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
         (group-by (compose report-info-branch cdr) folders)
         > #:key (λ (x) (date->seconds (report-info-date (cdar x))))))

      (define-values (master-info* other-infos)
        (partition (λ (x) (equal? (report-info-branch (cdar x)) "master"))
                   branch-infos*))

      ;; This is a hack due to the use of partition to simultaneously
      ;; find the reports with branch master, and also to remove it
      ;; from the list.
      (define master-info (and (not (null? master-info*)) (car master-info*)))

      ; Big bold numbers
      (printf "<div id='large'>\n")
      (printf "<div>Reports: <span class='number'>~a</span></div>\n"
              (length folders))
      (printf "<div>Branches: <span class='number'>~a</span></div>\n"
              (length branch-infos*))
      (when master-info
        (printf "<div>On Master: <span class='number'>~a</span></div>\n"
                (length master-info)))
      (printf "</div>\n")

      (printf "<ul id='toc'>")
      (for ([rows (if master-info (cons master-info other-infos) other-infos)])
        (define branch (report-info-branch (cdar rows)))
        (printf "<li><a href='#reports-~a'>~a</a></li>" branch branch))
      (printf "</ul>")

      (printf "<figure>")
      (printf "<ul id='classes'></ul>")
      (printf "<svg id='graph' width='800'></svg>\n")
      (printf "<ul id='suites'></ul>")
      (printf "<script>window.addEventListener('load', function(){draw_results(d3.select('#graph'))})</script>\n")
      (printf "</figure>\n")

      (printf "<table id='reports'>\n")
      (when master-info
        (for-each (curryr write-xexpr (current-output-port))
                  (print-rows master-info #:name "master")))
      (for ([rows other-infos])
        (print-rows rows #:name (report-info-branch (cdar rows))))
      (printf "</table>\n")

      (printf "</body>\n")
      (printf "</html>\n"))))

(module+ main
  (make-index-page))
