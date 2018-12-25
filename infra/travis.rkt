#lang racket

(require racket/date racket/cmdline)
(require "../src/common.rkt" "../src/points.rkt")
(require "../src/alternative.rkt" "../src/sandbox.rkt")
(require "../src/formats/test.rkt" "../src/formats/datafile.rkt")

(define (run-tests . bench-dirs)
  (define tests (append-map load-tests bench-dirs))
  (define seed (get-seed))
  (printf "Running Herbie on ~a tests (seed: ~a)...\n" (length tests) seed)
  (for/and ([test tests])
    (match (get-test-result test #:seed seed)
      [(test-success test bits time timeline
                     start-alt end-alt points exacts start-est-error end-est-error
                     newpoints newexacts start-error end-error target-error
                     baseline-error oracle-error all-alts)
       (printf "[ ~ams]\t(~a→~a)\t~a\n"
               (~a time #:width 8)
               (~r (errors-score start-error) #:min-width 2 #:precision 0)
               (~r (errors-score end-error) #:min-width 2 #:precision 0)
               (test-name test))
       (define success?
         (test-successful? test
                           (errors-score start-error)
                           (and target-error (errors-score target-error))
                           (errors-score end-error)))

       (when (not success?)
         (printf "Input: ~a\n" (alt-program start-alt))
         (printf "Output:\n")
         (pretty-print (alt-program end-alt))
         (when (test-output test) (printf "Target: ~a\n" (test-output test))))

       success?]
      [(test-failure test bits time timeline exn)
       (printf "[   CRASH   ]\t\t\t~a\n" (test-name test))
       ((error-display-handler) (exn-message exn) exn)
       #f]
      [(test-timeout test bits time timeline)
       (printf "[  timeout  ]\t\t\t~a\n" (test-name test))
       #f])))

(module+ main
  (define seed (random 1 (expt 2 31)))
  (set-seed! seed)
  (command-line
   #:program "travis.rkt"
   #:once-each
   [("--seed") rs "The random seed to use in point generation. If false (#f), a random seed is used'"
    (define given-seed (read (open-input-string rs)))
    (when given-seed (set-seed! given-seed))]
   #:args bench-dir
   (exit (if (apply run-tests bench-dir) 0 1))))
