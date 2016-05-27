#lang racket

(require racket/date)
(require racket/cmdline)
(require srfi/13)
(require "make-report.rkt")
(require "../common.rkt")
(require "../programs.rkt")
(require "../points.rkt")
(require "../alternative.rkt")
(require "../formats/test.rkt")
(require "../formats/datafile.rkt")
(require "../glue.rkt")
(require "../formats/c.rkt")
(require "thread-pool.rkt")
(provide (all-defined-out))

(define *max-test-threads* #f)
(define *test-name* #f)

(define *profile?* #f)

(define (rerun-report json-file)
  (define dir report-output-path)
  (when (not (directory-exists? dir)) (make-directory dir))

  (define data (read-datafile json-file))
  (define tests
    (for/list ([row (report-info-tests data)])
      (test (table-row-name row) (table-row-vars row)
            (table-row-samplers row) (table-row-input row) (table-row-output row) #t)))
  (*flags* (report-info-flags data))
  (set-seed! (report-info-seed data))
  (*num-points* (report-info-points data))
  (*num-iterations* (report-info-iterations data))

  (define results (get-test-results tests #:threads *max-test-threads*
                                    #:seed (get-seed) #:profile *profile?*))
  (define info (make-report-info (filter identity results)))

  (write-datafile (build-path dir "results.json") info)
  (make-report-page (build-path dir "report.html") info)
  ; TODO: Uses the same expressions for float and double. This could be good to change.
  (compile-info dir info info))

(define (allowed-tests bench-dirs)
  (define unsorted-tests (append-map load-tests bench-dirs))
  (if *test-name*
      (filter (λ (t) (equal? *test-name* (test-name test))) unsorted-tests)
      (reverse (sort unsorted-tests test<?))))

(define (test<? t1 t2)
  (cond
   [(and (test-output t1) (test-output t2))
    (string<? (test-name t1) (test-name t2))]
   [(and (not (test-output t1)) (not (test-output t2)))
    (string<? (test-name t1) (test-name t2))]
   [else
    ; Put things with an output first
    (test-output t1)]))

(command-line
 #:program "herbie-rerun"
 #:once-each
 [("-p" "--profile") "Whether to profile each test"
  (set! *profile?* #t)]
 [("--threads") th "How many tests to run in parallel to use. Pass 'no' to use no threads (default), 'yes' to use the number of machine cores less one, and a number to use that many."
  (when (eq? (system-type 'os) 'macosx)
    (eprintf "WARNING Herbie does not support threads on OS X\n\tdue to a bug in MPFR. Herbie will attempt\n\tto execute anyway, but may fail.\n"))
  (set! *max-test-threads*
        (match th
          ["no" #f]
          ["yes" (max (- (processor-count) 1) 1)]
          [_ (string->number th)]))]
 #:args (json)
 (rerun-report json))
