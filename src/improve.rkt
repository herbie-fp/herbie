#lang racket
(require "reports/thread-pool.rkt" "formats/test.rkt" "common.rkt" "sandbox.rkt" "alternative.rkt")
(provide run-improve)

(define (print-outputs tests results p #:seed [seed #f])
  (when seed
    (fprintf p ";; seed: ~a\n\n" seed))
  (for ([output results] [test tests])
    (match output
      [(? test-result?)
       (fprintf p "~a\n" (unparse-test (alt-program (test-result-end-alt output))))]
      [(? test-failure?)
       (fprintf p ";; Crash in ~a\n" (test-name test))
       (let ([out (open-output-string)])
         (parameterize ([current-error-port out])
           ((error-display-handler) (exn-message exn) exn))
         (fprintf "; ~a" (string-replace (string-trim (get-output-string out) "\n" #:left? #f) "\n" "\n; ")))
       (fprintf p "~a\n" (unparse-test (test-input test)))]
      [(? test-timeout?)
       (fprintf p ";; ~as timeout in ~as\n (use --timeout to change timeout)\n"
                (/ (*timeout*) 1000) (test-name test))
       (fprintf p "~a\n" (unparse-test (test-input test)))])))

(define (run-improve input output #:threads [threads #f])
  (define seed (get-seed))
  (define tests (load-tests input))
  (define results (get-test-results tests #:threads threads #:seed seed #:dir #f))

  (if (equal? output "-")
      (print-outputs tests results (current-output-port) #:seed seed)
      (call-with-output-file output #:exists 'replace (λ (p) (print-outputs tests results p #:seed seed)))))
