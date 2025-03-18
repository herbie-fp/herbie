#lang racket

(require "../syntax/read.rkt"
         "../utils/common.rkt"
         "server.rkt")
(provide run-shell
         run-improve)

(define (get-shell-input)
  (printf "herbie> ")
  (with-handlers ([(or/c exn:fail:user? exn:fail:read?) (λ (e)
                                                          ((error-display-handler) (exn-message e) e)
                                                          (get-shell-input))])
    (define input
      (parameterize ([read-decimal-as-inexact false])
        (read-syntax "stdin" (current-input-port))))
    (cond
      [(eof-object? input)
       (printf "\n")
       eof]
      [else (parse-test input)])))

(define (job-result->fpcore result)
  (read (open-input-string (first (hash-ref result 'alternatives)))))

(define (print-improve-outputs tests results p #:seed [seed #f])
  (when seed
    (fprintf p ";; seed: ~a\n\n" seed))
  (for ([res results]
        [test tests]
        #:when res)
    (define test (hash-ref res 'test))
    (define name (test-name test))
    (match (hash-ref res 'status)
      ['failure
       (match-define (list 'exn type msg url locs traceback) (hash-ref res 'backend))
       (fprintf p ";; ~a in ~a\n" (if type "Error" "Crash") name)]
      ['timeout (fprintf p ";; ~a times out in ~as\n" (/ (*timeout*) 1000) name)]
      ['success (void)])
    (pretty-print (job-result->fpcore res) p 1)))

(define (run-improve input output #:threads [threads #f])
  (define seed (get-seed))
  (define tests (load-tests input))
  (start-job-server threads)
  (define ids
    (for/list ([test (in-list tests)])
      (start-job 'improve test #:seed seed #:pcontext #f #:profile? #f #:timeline-disabled? #f)))
  (define results
    (for/list ([id ids])
      (wait-for-job id)))

  (if (equal? output "-")
      (print-improve-outputs tests results (current-output-port) #:seed seed)
      (call-with-output-file output
                             #:exists 'replace
                             (λ (p) (print-improve-outputs tests results p #:seed seed)))))

(define (run-shell)
  (define seed (get-seed))
  (eprintf "Herbie ~a with seed ~a\n" *herbie-version* seed)
  (eprintf "Find help on https://herbie.uwplse.org/, exit with ~a\n"
           (match (system-type 'os)
             ['windows "Ctrl-Z Enter"]
             [_ "Ctrl-D"]))
  (start-job-server #f)
  (with-handlers ([exn:break? (λ (e) (exit 0))])
    (for ([test (in-producer get-shell-input eof-object?)]
          [idx (in-naturals)])
      (define result (wait-for-job (start-job 'improve test #:seed seed)))
      (match (hash-ref result 'status)
        ['success (pretty-print (job-result->fpcore result) (current-output-port) 1)]
        ['failure
         (match-define (list 'exn type msg url locs traceback) (hash-ref result 'backend))
         (printf "; ~a\n" msg)
         (for ([loc (in-list locs)])
           (match-define (list msg file line col pos) loc)
           (printf ";   ~a:~a~a: ~a\n" file line col msg))
         (printf "; See <https://herbie.uwplse.org/doc/~a/~a> for more.\n" *herbie-version* url)]
        ['timeout
         (printf "; Timeout in ~as (see --timeout option)\n" (/ (hash-ref result 'time) 1000))]))))
