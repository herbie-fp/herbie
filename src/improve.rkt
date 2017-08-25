#lang racket
(require "formats/datafile.rkt" "reports/thread-pool.rkt" "formats/test.rkt" "common.rkt" "sandbox.rkt" "alternative.rkt")
(provide run-improve)

(define (build-fpcore test expr)
  (define vars (test-vars test))
  `(FPCore ,vars :name ,(test-name test) :pre ,(test-precondition test) ,expr))

(define (print-outputs tests results p #:seed [seed #f])
  (when seed
    (fprintf p ";; seed: ~a\n\n" seed))
  (for ([res results] [test tests])
    (match-define (table-row name status start result target inf- inf+ result-elt vars input output time bits link) res)
    (match status
      [(? test-failure?)
       (fprintf p ";; Crash in ~a\n" name)
       (write (build-fpcore test input) p)
       (newline p)]
      [(? test-timeout?)
       (fprintf p ";; ~a times out in ~as\n"
                (/ (*timeout*) 1000) name)
       (write (build-fpcore test input) p)
       (newline p)]
      [_
       (write (build-fpcore test output) p)
       (newline p)])))

(define (run-improve input output #:threads [threads #f])
  (define seed (get-seed))
  (define tests (load-tests input))
  (define results (get-test-results tests #:threads threads #:seed seed #:dir #f))

  (if (equal? output "-")
      (print-outputs tests results (current-output-port) #:seed seed)
      (call-with-output-file output #:exists 'replace (λ (p) (print-outputs tests results p #:seed seed)))))
