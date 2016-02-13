#lang racket

(require "../common.rkt")
(require "datafile.rkt")
(require "make-index.rkt")

(define allowed-suites
  '("tutorial" "physics" "libraries" "mathematics" "hamming" "haskell", "numerical-analysis" "regression"))

(define (write-report-info folder info)
  (let ([info-file (build-path report-output-path "reports" folder "results.json")])
    (write-datafile info-file info)))

(define (same-tests? info1 info2)
  (and (report-info-tests info1) (report-info-tests info2)
       (set=?
        (map table-row-input (report-info-tests info1))
        (map table-row-input (report-info-tests info2)))))

(define (backfill-index)
  (define dirs (directory-list (build-path report-output-path "reports/")))

  (define folders
    (map (λ (dir) (cons dir (read-report-info dir)))
         (remove-duplicates
          (sort (filter name->timestamp dirs) > #:key name->timestamp)
          #:key name->timestamp)))

  (define reps
    (for/list ([suite allowed-suites])
      (for/first ([(folder info) (in-pairs folders)] #:when (equal? (report-info-note info) suite))
        info)))

  (for ([(folder info) (in-pairs folders)])
    (define suite
      (for/first ([suite allowed-suites] [rep reps] #:when (same-tests? info rep))
        suite))
    (when (or (and suite (not (equal? (report-info-note info) suite)))
              (and (not suite) (report-info-note info)))
      (eprintf "Updating ~a to ~a\n" folder suite)
      (set-report-info-note! info suite)
      (write-report-info folder info))))

(module+ main
  (backfill-index))
