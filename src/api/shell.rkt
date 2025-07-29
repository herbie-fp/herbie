#lang racket

(require "../syntax/platform.rkt"
         "../syntax/load-platform.rkt"
         "../syntax/read.rkt"
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
    (define name (hash-ref res 'name))
    (match (hash-ref res 'status)
      ["failure"
       (match-define (list 'exn type msg url locs traceback) (hash-ref res 'backend))
       (fprintf p ";; ~a in ~a\n" (if type "Error" "Crash") name)]
      ["timeout" (fprintf p ";; ~a times out in ~as\n" (/ (*timeout*) 1000) name)]
      ["success" (void)])
    (displayln (fpcore->string (job-result->fpcore res)) p)
    (newline)))

(define (run-improve input output #:threads [threads #f])
  (define seed (get-seed))
  (activate-platform! (*platform-name*))
  (define tests (load-tests input))
  (server-start threads)
  (define ids
    (for/list ([test (in-list tests)])
      (job-start 'improve test #:seed seed #:pcontext #f #:profile? #f #:timeline? #f)))
  (define results
    (for/list ([id ids])
      (job-wait id)))

  (if (equal? output "-")
      (print-improve-outputs tests results (current-output-port) #:seed seed)
      (call-with-output-file output
                             #:exists 'replace
                             (λ (p) (print-improve-outputs tests results p #:seed seed)))))

(define (run-shell)
  (define seed (get-seed))
  (activate-platform! (*platform-name*))
  (server-start #f)
  (eprintf "Find help on https://herbie.uwplse.org/, exit with ~a\n"
           (match (system-type 'os)
             ['windows "Ctrl-Z Enter"]
             [_ "Ctrl-D"]))
  (with-handlers ([exn:break? (λ (e) (exit 0))])
    (for ([test (in-producer get-shell-input eof-object?)]
          [idx (in-naturals)])
      (define result (job-wait (job-start 'improve test #:seed seed)))
      (match (hash-ref result 'status)
        ["success" (displayln (fpcore->string (job-result->fpcore result)))]
        ["failure"
         (match-define (list 'exn type msg url locs traceback) (hash-ref result 'backend))
         (printf "; ~a\n" msg)
         (for ([loc (in-list locs)])
           (match-define (list msg file line col pos) loc)
           (printf ";   ~a:~a~a: ~a\n" file line col msg))
         (printf "; See <https://herbie.uwplse.org/doc/~a/~a> for more.\n" *herbie-version* url)]
        ["timeout"
         (printf "; Timeout in ~as (see --timeout option)\n" (/ (hash-ref result 'time) 1000))]))))
