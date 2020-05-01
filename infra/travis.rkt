#lang racket

(require racket/date racket/cmdline)
(require "../src/common.rkt" "../src/points.rkt" "../src/plugin.rkt")
(require "../src/alternative.rkt" "../src/sandbox.rkt")
(require "../src/syntax/read.rkt" "../src/datafile.rkt")

;; Load all the plugins
(load-herbie-plugins)

(define (test-successful? test input-bits target-bits output-bits)
  (match* ((test-output test) (test-expected test))
    [(_ #f) #t]
    [(_ (? number? n)) (>= n output-bits)]
    [(#f #t) (>= input-bits output-bits)]
    [(_ #t) (>= target-bits (- output-bits 1))]))

(define (run-tests . bench-dirs)
  (define tests (append-map load-tests bench-dirs))
  (define seed (pseudo-random-generator->vector (current-pseudo-random-generator)))
  (printf "Running Herbie on ~a tests, seed: ~a\n" (length tests) seed)
  (for/and ([test tests] [i (in-naturals)])
    (printf "~a/~a\t" (~a (+ 1 i) #:width 3 #:align 'right) (length tests))
    (match (get-test-result test #:seed seed)
      [(test-success test bits time timeline warnings
                     start-alt end-alt points exacts start-est-error end-est-error
                     newpoints newexacts start-error end-error target-error
                     baseline-error oracle-error all-alts)
       (printf "[ ~as]   ~a→~a\t~a\n"
               (~r (/ time 1000) #:min-width 7 #:precision '(= 3))
               (~r (errors-score start-error) #:min-width 2 #:precision 0)
               (~r (errors-score end-error) #:min-width 2 #:precision 0)
               (test-name test))
       (define success?
         (test-successful? test
                           (errors-score start-error)
                           (and target-error (errors-score target-error))
                           (errors-score end-error)))

       (when (not success?)
         (printf "\nInput (~a bits):\n" (errors-score start-error))
         (pretty-print (alt-program start-alt) (current-output-port) 1)
         (printf "\nOutput (~a bits):\n" (errors-score end-error))
         (pretty-print (alt-program end-alt) (current-output-port) 1)
         (when (test-output test)
           (printf "\nTarget (~a bits):\n" (errors-score target-error))
           (pretty-print (test-output test) (current-output-port) 1)))

       success?]
      [(test-failure test bits time timeline warnings exn)
       (printf "[  CRASH  ]\t\t~a\n" (test-name test))
       ((error-display-handler) (exn-message exn) exn)
       #f]
      [(test-timeout test bits time timeline warnings)
       (printf "[  TIMEOUT]\t\t~a\n" (test-name test))
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
