#lang racket

(require racket/date)
(require racket/cmdline)
(require srfi/13)
(require "make-report.rkt")
(require "../common.rkt")
(require "../programs.rkt")
(require "../points.rkt")
(require "../alternative.rkt")
(require "../test.rkt")
(require "../main.rkt")
(require "../compile/c.rkt")
(require "thread-pool.rkt")
(require "datafile.rkt")
(provide (all-defined-out))


(define (run-tests . bench-dirs)
  (define tests (append-map load-tests bench-dirs))
  (define seed (get-seed))
  (printf "Running ~a tests\nSeed: ~a\n" (length tests) seed)
  (for/and ([test tests])
    (match (get-test-result test "." #:seed seed)
      [(test-result test rdir time prec input output pts exs
                    start-errors end-error newpts newexs
                    start-newerrors end-newerrors target-newerrors)
       (printf "[ ~ams] ~a → ~a\t~a\n"
               (~a time #:width 8)
               (~r (errors-score start-newerrors) #:min-width 2 #:precision 0)
               (~r (errors-score end-newerrors) #:min-width 2 #:precision 0)
               (test-name test))
       (test-successful? test
                         (errors-score start-newerrors)
                         (and target-newerrors (errors-score target-newerrors))
                         (errors-score end-newerrors))]
      [(test-failure test prec exn time rdir)
       (printf "[   CRASH   ]\t\t\t~a\n" (test-name test))
       ((error-display-handler) (exn-message exn) exn)
       #f]
      [(test-timeout test prec rdir)
       (printf "[  timeout  ]\t\t\t~a\n" (test-name test))
       #f])))

(command-line
 #:program "travis.rkt"
 #:args bench-dir
 (exit (if (apply run-tests bench-dir) 0 1)))
