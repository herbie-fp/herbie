#lang racket
(require racket/runtime-path)
(require (only-in xml write-xexpr) json)
(define-runtime-path report-json-path "../previous/")


(require racket/date)
(require "../src/common.rkt")
(require "../src/formats/datafile.rkt")

(provide directory-jsons name->timestamp)

(define (name->timestamp path)
  (define rpath (find-relative-path (simple-form-path report-json-path) path))
  (define folder (path-element->string (first (explode-path rpath))))
  (string->number
   (if (string-contains? folder ":")
       (first (string-split folder ":"))
       folder)))

(define (directory-jsons dir)
  (reap [sow]
    (let loop ([dir dir])
      (cond
       [(file-exists? (build-path dir "results.json"))
        (sow (find-relative-path (simple-form-path report-json-path) (simple-form-path dir)))]
       [(directory-exists? dir)
        (for-each loop (directory-list dir #:build? true))]))))

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

(define *cache* (make-parameter (make-hasheq)))

(define (compute-row folder)
  (eprintf "Reading ~a\n" folder)
  (define info (read-datafile (build-path report-json-path folder "results.json")))
  (match-define (report-info date commit branch hostname seed flags points iterations bit-width note tests) info)

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

  (hash 'date-full (format "~a:~a on ~a" (date-hour date) (~r (date-minute date) #:min-width 2 #:pad-string "0") (date->string date))
        'date-short (date->string/short date)
        'date-unix (date->seconds date)
        'folder (path->string folder)
        'commit commit
        'branch branch
        'options (map ~a (get-options info))
        'note note
        'tests-passed total-passed
        'tests-available total-available
        'bits-improved (- total-start total-end)
        'bits-available total-start))

(define (read-row folder)
  (dict-ref! (*cache*) (string->symbol (path->string folder)) (λ () (compute-row folder))))

(define (round* x)
  (cond
   [(>= (abs x) (expt 10 6)) "?"]
   [(>= (abs x) 10) (~a (inexact->exact (round x)))]
   [else (~r x #:precision 2)]))

(define (print-rows infos #:name name)
  `((thead ((id ,(format "reports-~a" name)) (data-branch ,name))
           (th "Date") (th "Branch") (th "Collection") (th "Tests") (th "Bits"))
    (tbody
     ,@(for/list ([info infos])
         (define field (curry dict-ref info))

         `(tr
           ;; TODO: Best to output a datetime field in RFC3338 format,
           ;; but Racket doesn't make that easy.
           (td ((title ,(field 'date-full)))
               (time ((data-unix ,(~a (field 'date-unix))))
                     ,(field 'date-short)))
           (td ((title ,(field 'commit))) ,(field 'branch))
           (td ((title ,(string-join (field 'options) " "))
                (class ,(if (field 'note) "note" ""))) ,(or (field 'note) "⭐"))
           (td ,(if (> (field 'tests-available) 0) (format "~a/~a" (field 'tests-passed) (field 'tests-available)) ""))
           (td ,(if (field 'bits-improved) (format "~a/~a" (round* (field 'bits-improved)) (round* (field 'bits-available))) ""))
           (td (a ((href ,(format "./~a/report.html" (field 'folder)))) "»")))))))

(define (make-index-page)
  (when (file-exists? (build-path report-json-path "index.cache"))
    (*cache* (hash-copy (call-with-input-file (build-path report-json-path "index.cache") read-json))))

  (define dirs (directory-jsons report-json-path))
  (define folders
    (map read-row (sort (filter name->timestamp dirs) > #:key name->timestamp)))

  (define branch-infos*
    (sort
     (group-by (curryr dict-ref 'branch) folders)
     > #:key (λ (x) (dict-ref (first x) 'date-unix))))

  (define-values (master-info* other-infos)
    (partition (λ (x) (equal? (dict-ref (first x) 'branch) "master"))
               branch-infos*))
  (define master-info (if (null? master-info*) '() (first master-info*)))

  (write-file "index.html"
    (printf "<!doctype html>\n")
    (write-xexpr
     `(html
       (head
        (meta ((charset "utf-8")))
        (title "Herbie Reports")
        (link ((rel "stylesheet") (href "index.css")))
        (script ((src "http://d3js.org/d3.v3.min.js") (charset "utf-8")))
        (script ((src "regression-chart.js")))
        (script ((src "report.js"))))
       (body
        ((onload "index()"))
        (div
         ((id "large"))
         (div "Reports: " (span ((class "number")) ,(~a (length folders))))
         (div "Branches: " (span ((class "number")) ,(~a (length branch-infos*))))
         ,(if master-info
              `(div "On Master: " (span ((class "number")) ,(~a (length master-info))))
              ""))
        (ul ((id "toc"))
            ,@(for/list ([rows (if master-info (cons master-info other-infos) other-infos)])
                (define branch (dict-ref (first rows) 'branch))
                `(li (a ((href ,(format "#reports-~a" branch))) ,branch))))
        (figure
         (ul ((id "classes")))
         (svg ((id "graph") (width "800")))
         (ul ((id "suites")))
         (script "window.addEventListener('load', function(){draw_results(d3.select('#graph'))})"))
        (table
         ((id "reports"))
         ,@(if master-info
               (print-rows master-info #:name "master")
               '())
         ,@(apply
            append
            (for/list ([rows other-infos])
              (print-rows rows #:name (dict-ref (first rows) 'branch)))))))))

  (call-with-output-file (build-path report-json-path "index.cache") #:exists 'replace (curry write-json (*cache*))))

(module+ main
  (make-index-page))
