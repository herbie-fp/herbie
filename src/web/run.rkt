#lang racket

(require "../common.rkt" "../syntax/read.rkt" "../datafile.rkt")
(require "make-report.rkt" "thread-pool.rkt" "timeline.rkt")

(provide make-report rerun-report)

(define (make-report bench-dirs #:dir dir #:profile profile? #:debug debug? #:note note #:threads threads)
  (define tests (reverse (sort (append-map load-tests bench-dirs) test<?)))
  (run-tests tests #:dir dir #:profile profile? #:debug debug? #:note note #:threads threads))

(define (rerun-report json-file #:dir dir #:profile profile? #:debug debug? #:note note #:threads threads)
  (define data (read-datafile json-file))
  (define tests
    (for/list ([row (report-info-tests data)])
      (test (table-row-name row) (table-row-vars row)
            (table-row-input row) (table-row-output row)
            (table-row-target-prog row) (table-row-spec row)
            (table-row-pre row) (table-row-precision row))))
  (*flags* (report-info-flags data))
  (set-seed! (report-info-seed data))
  (*num-points* (report-info-points data))
  (*num-iterations* (report-info-iterations data))
  (run-tests tests #:dir dir #:profile profile? #:debug debug? #:note note #:threads threads))

(define (run-tests tests #:dir dir #:profile profile? #:debug debug? #:note note #:threads threads)
  (define seed (get-seed))
  (when (not (directory-exists? dir)) (make-directory dir))

  (define results
    (get-test-results tests #:threads threads #:seed seed #:profile profile? #:debug debug? #:dir dir))
  (define info (make-report-info (filter values results) #:note note #:seed seed))

  (write-datafile (build-path dir "results.json") info)
  (call-with-output-file (build-path dir "timeline.html")
    #:exists 'replace (curryr make-summary-html info dir))
  (copy-file (web-resource "report.js") (build-path dir "report.js") #t)
  (copy-file (web-resource "report.css") (build-path dir "report.css") #t)
  (copy-file (web-resource "arrow-chart.js") (build-path dir "arrow-chart.js") #t)
  (call-with-output-file (build-path dir "results.html")
    #:exists 'replace (curryr make-report-page info))

  ; Delete old files
  (let* ([expected-dirs (map string->path (filter identity (map table-row-link (report-info-tests info))))]
         [actual-dirs (filter (λ (name) (directory-exists? (build-path dir name))) (directory-list dir))]
         [extra-dirs (filter (λ (name) (not (member name expected-dirs))) actual-dirs)])
    (for ([subdir extra-dirs])
      (with-handlers ([exn:fail:filesystem? (const true)])
        (delete-directory/files (build-path dir subdir))))))

(define (test<? t1 t2)
  (cond
   [(and (test-output t1) (test-output t2))
    (string<? (test-name t1) (test-name t2))]
   [(and (not (test-output t1)) (not (test-output t2)))
    (string<? (test-name t1) (test-name t2))]
   [else
    ; Put things with an output first
    (test-output t1)]))
