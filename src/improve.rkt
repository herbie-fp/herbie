#lang racket
(require "formats/datafile.rkt" "reports/thread-pool.rkt" "formats/test.rkt" "common.rkt" "sandbox.rkt" "alternative.rkt")
(provide run-improve)

(define (print-outputs tests results p #:seed [seed #f])
  (when seed
    (fprintf p ";; seed: ~a\n\n" seed))
  (for ([res results] [test tests])
    (match-define (table-row name status start result target inf- inf+ result-elt vars input output time bits link) res)
    (match status
      [(? test-failure?)
       (fprintf p ";; Crash in ~a\n" name)
       (fprintf p "~a\n" `(FPCore ,vars ,input))]
      [(? test-timeout?)
       (fprintf p ";; ~a times out in ~as\n"
                (/ (*timeout*) 1000) name)
       (fprintf p "~a\n" `(FPCore ,vars ,input))]
      [_
       (fprintf p "~a\n" `(FPCore ,vars ,output))])))

(define (run-improve input output #:threads [threads #f])
  (define seed (get-seed))
  (define tests (load-tests input))
  (define results (get-test-results tests #:threads threads #:seed seed #:dir #f))

  (if (equal? output "-")
      (print-outputs tests results (current-output-port) #:seed seed)
      (call-with-output-file output #:exists 'replace (λ (p) (print-outputs tests results p #:seed seed)))))
