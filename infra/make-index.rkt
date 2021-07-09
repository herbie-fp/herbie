#lang racket

(require racket/runtime-path)
(require (only-in xml write-xexpr) json)
(require racket/date "../src/common.rkt" "../src/datafile.rkt")
(provide directory-jsons name->timestamp)

(define-runtime-path report-json-path "../previous/")

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

(define key-contracts
  (hash string? '(date-full date-short folder commit branch hostname)
        (or/c string? false) '(note)
        exact-nonnegative-integer? '(date-unix tests-passed tests-available tests-crashed)
        (listof string?) '(options)
        (and/c real? (curryr >= 0)) '(bits-available)
        real? '(bits-improved)))

(define cache-row?
  (apply and/c hash?
         (for*/list ([(valid? keys) (in-hash key-contracts)] [key keys])
           (make-flat-contract
            #:name key
            #:first-order (λ (x) (and (hash-has-key? x key) (valid? (hash-ref x key))))))))

(define/contract (compute-row folder)
  (-> path? cache-row?)
  (eprintf "Reading ~a\n" folder)
  (define info (read-datafile (build-path report-json-path folder "results.json")))
  (match-define (report-info date commit branch hostname seed flags points iterations note tests) info)

  (define-values (total-start total-end)
    (for/fold ([start 0] [end 0]) ([row (or tests '())])
      (values
       (+ start (or (table-row-start row) 0))
       (+ end (or (table-row-result row) 0)))))

  (define statuses (map table-row-status (or tests '())))
  (define total-passed
    (count (curry set-member? '("gt-target" "eq-target" "imp-start")) statuses))
  (define total-available
    (count (negate (curry equal? "ex-start")) statuses))
  (define total-crashed
    (+ (count (curry equal? "crash") statuses) (if (= iterations -1) 1 0)))
  (define total-timeout
    (count (curry equal? "timeout") statuses))
  (define total-unimproved
    (count (curry set-member? '("lt-start" "uni-start")) statuses))

  (define speed (apply + (map table-row-time (or tests '()))))

  (hash 'date-full (format "~a:~a on ~a" (date-hour date) (~r (date-minute date) #:min-width 2 #:pad-string "0") (date->string date))
        'date-short (date->string/short date)
        'date-unix (date->seconds date)
        'speed speed
        'folder (path->string folder)
        'hostname hostname
        'commit commit
        'branch branch
        'options (map ~a (get-options info))
        'note note
        'tests-passed total-passed
        'tests-available total-available
        'tests-crashed total-crashed
        'tests-unimproved total-unimproved
        'tests-timeout total-timeout
        'bits-improved (- total-start total-end)
        'bits-available total-start))

(define (read-row folder)
  (dict-ref! (*cache*) (string->symbol (path->string folder)) (λ () (compute-row folder))))

(define (round* x)
  (cond
   [(>= (abs x) (expt 10 6)) "?"]
   [(>= (abs x) 10) (~a (inexact->exact (round x)))]
   [else (~r x #:precision 2)]))

(define (bad-result? info)
  (or (> (dict-ref info 'tests-crashed 0) 0)
      (> (dict-ref info 'tests-unimproved 0) 0)
      (> (dict-ref info 'tests-timeout 0) 0)))

(define (print-rows infos #:name name)
  `((thead ((id ,(format "reports-~a" name)) (data-branch ,name))
           (th "Date") (th "Speed") (th "Branch") (th "Collection") (th "Tests") (th "Bits"))
    (tbody
     ,@(for/list ([info infos])
         (define field (curry dict-ref info))

         `(tr ([class ,(if (bad-result? info) "crash" "")])
           ;; TODO: Best to output a datetime field in RFC3338 format,
           ;; but Racket doesn't make that easy.
           (td ([title ,(field 'date-full)])
               (time ([data-unix ,(~a (field 'date-unix))]) ,(field 'date-short)))
           (td (time ([data-ms ,(~a (field 'speed))]) ,(format-time (field 'speed))))
           (td ([title ,(field 'commit)]) ,(field 'branch))
           (td ([title ,(string-join (field 'options) " ")]
                [class ,(if (field 'note) "note" "")])
               ,(or (field 'note) "⭐"))
           (td ,(if (> (field 'tests-available) 0) (format "~a/~a" (field 'tests-passed) (field 'tests-available)) ""))
           (td ,(if (field 'bits-improved) (format "~a/~a" (round* (field 'bits-improved)) (round* (field 'bits-available))) ""))
           (td ([title ,(format "At ~a\nOn ~a\nFlags ~a" (field 'date-full) (field 'hostname) (string-join (field 'options) " "))])
               (a ([href ,(format "./~a/results.html" (field 'folder))]) "»")))))))

(define (make-index-page out)
  (when (file-exists? (build-path report-json-path "index.cache"))
    (define cached-info (hash-copy (call-with-input-file (build-path report-json-path "index.cache") read-json)))
    (if (for/and ([(k v) (in-hash cached-info)]) (cache-row? v))
        (*cache* cached-info)
        (eprintf "Ignoring cache; contains invalid values")))

  (define dirs (directory-jsons report-json-path))
  (define folders
     (map read-row (sort (filter name->timestamp dirs) > #:key name->timestamp)))
  (define recent-folders
    (filter (λ (x) (< (- (current-seconds) (dict-ref x 'date-unix)) (* 60 60 24 30)))
            folders))

  (define branch-infos*
    (sort
     (group-by (curryr dict-ref 'branch) recent-folders)
     > #:key (λ (x) (dict-ref (first x) 'date-unix))))

  (define-values (mainline-infos other-infos)
    (partition (λ (x) (set-member? '("master" "develop") (dict-ref (first x) 'branch)))
               branch-infos*))

  (when (null? mainline-infos)
    (set! mainline-infos
          (list
           (filter
            (curryr dict-ref 'note)
            (map first
                 (group-by
                  (curryr dict-ref 'note)
                  (sort
                   (filter (λ (x) (set-member? '("master" "develop") (dict-ref x 'branch))) folders)
                   > #:key (curryr dict-ref 'date-unix))))))))

  (define crashes
    (filter (λ (x) (> (dict-ref x 'tests-crashed) 0)) (apply append mainline-infos)))
  (define last-crash
    (if (null? crashes)
        #f
        (argmax (curryr dict-ref 'date-unix) crashes)))
  (define since-last-crash
    (and last-crash (/ (- (date->seconds (current-date)) (dict-ref last-crash 'date-unix)) (* 60 60 24))))
  
  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (meta ((charset "utf-8")))
      (title "Herbie Reports")
      (link ((rel "stylesheet") (href "index.css")))
      (script ((src "https://d3js.org/d3.v3.min.js") (charset "utf-8")))
      (script ((src "regression-chart.js")))
      (script ((src "report.js"))))
     (body
      (div
       ((id "large"))
       (div "Reports: " (span ((class "number")) ,(~a (length recent-folders))))
       (div "Mainline: " (span ((class "number")) ,(~a (length (apply append mainline-infos)))))
       (div "Branches: " (span ((class "number")) ,(~a (length branch-infos*))))
       (div "Crash-free: " (span ((class "number")) ,(if since-last-crash
                                                         (format "~ad" (inexact->exact (round since-last-crash)))
                                                         "∞"))))
      (ul ((id "toc"))
          ,@(for/list ([rows (append mainline-infos other-infos)])
              (define branch (dict-ref (first rows) 'branch))
              `(li (a ((href ,(format "#reports-~a" branch))) ,branch))))
      (figure
       (ul ((id "classes")))
       (svg ((id "accuracy-graph") (width "400")))
       (svg ((id "speed-graph") (width "400")))
       (ul ((id "suites")))
       (script "window.addEventListener('load', function(){draw_results(d3.select('#accuracy-graph'), d3.select('#speed-graph'))})"))
      (table
       ((id "reports"))
       ,@(apply
          append
          (for/list ([rows (append mainline-infos other-infos)])
            (print-rows rows #:name (dict-ref (first rows) 'branch)))))))
   out))

(module+ main
  (call-with-output-file "index.html"
    #:exists 'replace
    make-index-page)
  (call-with-output-file (build-path report-json-path "index.cache")
    #:exists 'replace
    (curry write-json (*cache*))))
