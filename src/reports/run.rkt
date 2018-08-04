#lang racket

(require "../common.rkt" "../formats/test.rkt" "../formats/datafile.rkt")
(require "make-report.rkt" "thread-pool.rkt")

(provide make-report)

(define (make-report bench-dirs #:dir dir #:profile profile? #:note note #:threads threads)
  (define seed (get-seed))
  (when (not (directory-exists? dir)) (make-directory dir))

  (define tests (reverse (sort (append-map load-tests bench-dirs) test<?)))
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
