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
  (eprintf "Seed: ~a\n" seed)
  (with-handlers ([exn:break? (λ (e) (exit 0))])
    (for ([test (in-producer get-input eof-object?)] [idx (in-naturals)])
      (define output (get-test-result test #:seed seed))
      (match output
        [(? test-result?)
         (printf "~a\n" (unparse-test (alt-program (test-result-end-alt output))))]
        [(test-failure test bits exn time timeline)
         ((error-display-handler) (exn-message exn) exn)]
        [(test-timeout test bits time timeline)
         (printf "Timeout in ~as (see --timeout option)\n" (/ time 1000))]))))
