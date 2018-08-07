#lang racket

(require "../common.rkt" "../formats/test.rkt" "../formats/datafile.rkt")
(require "make-report.rkt" "thread-pool.rkt")

(provide make-report rerun-report)

(define (make-report bench-dirs #:dir dir #:profile profile? #:note note #:threads threads)
  (define tests (reverse (sort (append-map load-tests bench-dirs) test<?)))
  (run-tests tests #:dir dir #:profile profile? #:note note #:threads threads))

(define (rerun-report json-file #:dir dir #:profile profile? #:note note #:threads threads)
  (define data (read-datafile json-file))
  ;; TODO: rerunning does not support preconditions. This should be fixed.
  (define tests
    (for/list ([row (report-info-tests data)])
      (test (table-row-name row) (table-row-vars row)
            (table-row-input row) (table-row-output row) #f #t 'TRUE)))
  (*flags* (report-info-flags data))
  (set-seed! (report-info-seed data))
  (*num-points* (report-info-points data))
  (*num-iterations* (report-info-iterations data))
  (run-tests tests #:dir dir #:profile profile? #:note note #:threads threads))

(define (run-tests tests #:dir dir #:profile profile? #:note note #:threads threads)
  (define seed (get-seed))
  (when (not (directory-exists? dir)) (make-directory dir))

  (define results
    (get-test-results tests #:threads threads #:seed seed #:profile profile? #:dir dir))
  (define info (make-report-info (map cdr (filter values results)) #:note note #:seed seed))

  (write-datafile (build-path dir "results.json") info)
  (make-report-page (build-path dir "report.html") info))

(define (test<? t1 t2)
  (cond
   [(and (test-output t1) (test-output t2))
    (string<? (test-name t1) (test-name t2))]
   [(and (not (test-output t1)) (not (test-output t2)))
    (string<? (test-name t1) (test-name t2))]
   [else
    ; Put things with an output first
    (test-output t1)]))
