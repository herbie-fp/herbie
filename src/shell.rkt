#lang racket

(require "formats/test.rkt" "sandbox.rkt" "common.rkt" "alternative.rkt")
(provide run-shell)

(define (get-input)
  (printf "herbie> ")
  (with-handlers
      ([(or/c exn:fail:user? exn:fail:read?)
        (λ (e)
          ((error-display-handler) (exn-message e) e)
          (get-input))])
    (define input (read-syntax "stdin" (current-input-port)))
    (if (eof-object? input)
        (begin (printf "\n") eof)
        (parse-test input))))

(define (run-shell)
  (define seed (get-seed))
  (eprintf "Herbie ~a with seed ~a\n" *herbie-version* seed)
  (eprintf "Find help on <https://herbie.uwplse.org/>, exit with ~a\n"
           (match (system-type 'os) ['windows "Ctrl-Z Enter"] [_ "Ctrl-D"]))
  (with-handlers ([exn:break? (λ (e) (exit 0))])
    (for ([test (in-producer get-input eof-object?)] [idx (in-naturals)])
      (define output (get-test-result test #:seed seed))
      (match output
        [(? test-result?)
         (pretty-print (unparse-result output) (current-output-port) 1)]
        [(test-failure test bits exn time timeline)
         ((error-display-handler) (exn-message exn) exn)]
        [(test-timeout test bits time timeline)
         (printf "Timeout in ~as (see --timeout option)\n" (/ time 1000))]))))
