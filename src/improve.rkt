#lang racket
(require "formats/datafile.rkt" "web/thread-pool.rkt" "formats/test.rkt" "common.rkt" "sandbox.rkt" "alternative.rkt")
(provide run-improve)

(define (print-outputs tests results p #:seed [seed #f])
  (when seed
    (fprintf p ";; seed: ~a\n\n" seed))
  (for ([res results] [test tests] #:when res)
    (match-define (table-row name status pre start result target inf- inf+ start-est result-est vars input output time bits link) (cdr res))
    (match status
      ["error"
       (fprintf p ";; Error in ~a\n" name)
       (write (car res) p)
       (newline p)]
      ["crash"
       (fprintf p ";; Crash in ~a\n" name)
       (write (car res) p)
       (newline p)]
      ["timeout"
       (fprintf p ";; ~a times out in ~as\n"
                (/ (*timeout*) 1000) name)
       (write (car res) p)
       (newline p)]
      [(? string?)
       (write (car res) p)
       (newline p)])))

(define (run-improve input output #:threads [threads #f])
  (define seed (get-seed))
  (define tests (load-tests input))
  (define results (get-test-results tests #:threads threads #:seed seed #:profile #f #:debug #f #:dir #f))

  (if (equal? output "-")
      (print-outputs tests results (current-output-port) #:seed seed)
      (call-with-output-file output #:exists 'replace (λ (p) (print-outputs tests results p #:seed seed)))))
