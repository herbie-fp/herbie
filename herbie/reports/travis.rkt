#lang racket

(require racket/date)
(require racket/cmdline)
(require "../common.rkt")
(require "../points.rkt")
(require "../alternative.rkt")
(require "../test.rkt")
(require "../sandbox.rkt")
(require "datafile.rkt")

(define (run-tests . bench-dirs)
  (define tests (append-map load-tests bench-dirs))
  (define seed (get-seed))
  (printf "Running Herbie on ~a tests...\nSeed: ~a\n" (length tests) seed)
  (for/and ([test tests])
    (match (get-test-result test "." #:seed seed)
      [(test-result test rdir time prec input output pts exs
                    start-errors end-error newpts newexs
                    start-newerrors end-newerrors target-newerrors)
       (printf "[ ~ams]\t(~a→~a)\t~a\n"
               (~a time #:width 8)
               (~r (errors-score start-newerrors) #:min-width 2 #:precision 0)
               (~r (errors-score end-newerrors) #:min-width 2 #:precision 0)
               (test-name test))
       (define success?
         (test-successful? test
                           (errors-score start-newerrors)
                           (and target-newerrors (errors-score target-newerrors))
                           (errors-score end-newerrors)))

       (when (not success?)
         (printf "Input: ~a\n" (alt-program input))
         (printf "Output:\n")
         (pretty-print (alt-program output))
         (when (test-output test) (printf "Target: ~a\n" (test-output test))))

       success?]
      [(test-failure test prec exn time rdir)
       (printf "[   CRASH   ]\t\t\t~a\n" (test-name test))
       ((error-display-handler) (exn-message exn) exn)
       #f]
      [(test-timeout test prec time rdir)
       (printf "[  timeout  ]\t\t\t~a\n" (test-name test))
       #f])))

(command-line
 #:program "travis.rkt"
 #:args bench-dir
 (exit (if (apply run-tests bench-dir) 0 1)))
