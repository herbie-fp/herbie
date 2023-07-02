#lang racket

(require "../src/common.rkt" "../src/points.rkt" "../src/load-plugin.rkt"
         "../src/alternative.rkt" "../src/sandbox.rkt" "../src/syntax/read.rkt"
         "../src/syntax/types.rkt")

(define *precision* (make-parameter #f))
(define *ignore-target* (make-parameter #f))

(define (test-successful? test input-bits target-bits output-bits)
  (if (*ignore-target*)
      #t
      (match* ((test-output test) (test-expected test))
       [(_ #f) #t]
       [(_ (? number? n)) (>= n output-bits)]
       [(#f #t) (>= input-bits output-bits)]
       [(_ #t) (>= target-bits (- output-bits 1))])))

(define (override-test-precision the-test repr)
  (struct-copy test the-test
               [output-repr-name (representation-name repr)]
               [var-repr-names
                (for/list ([var (in-list (test-vars the-test))])
                  (cons var (representation-name repr)))]))

(define (run-tests . bench-dirs)
  (define default-precision 
    (if (*precision*)
        (representation-name (*precision*))
        (*default-precision*)))
  (define tests
    (parameterize ([*default-precision* default-precision])
      (append-map load-tests bench-dirs)))
  (define seed (pseudo-random-generator->vector (current-pseudo-random-generator)))
  (printf "Running Herbie on ~a tests, seed: ~a\n" (length tests) seed)
  (for/and ([the-test tests] [i (in-naturals)])
    (printf "~a/~a\t" (~a (+ 1 i) #:width 3 #:align 'right) (length tests))
    (define the-test*
      (if (*precision*)
          (override-test-precision the-test (*precision*))
          the-test))
    (define result (run-herbie 'improve the-test* #:seed seed))
    (match-define (job-result test status time timeline warnings backend) result)
    (match status
      ['success
       (match-define 
        (improve-result preprocess pctxs start target end bogosity) backend)
       (match-define (alt-analysis start-alt _ start-error) start)
       (match-define (alt-analysis end-alt _ end-error) (first end))
       (define target-error (and target (alt-analysis-test-errors target)))

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
         (pretty-print (alt-expr start-alt) (current-output-port) 1)
         (printf "\nOutput (~a bits):\n" (errors-score end-error))
         (pretty-print (alt-expr end-alt) (current-output-port) 1)
         (when (test-output test)
           (printf "\nTarget (~a bits):\n" (errors-score target-error))
           (pretty-print (test-output test) (current-output-port) 1)))

       success?]
      ['failure
       (define exn backend)
       (printf "[  CRASH  ]\t\t~a\n" (test-name test))
       ((error-display-handler) (exn-message exn) exn)]
      ['timeout
       (printf "[  TIMEOUT]\t\t~a\n" (test-name test))
       #f])))

(module+ main
  ;; Load all the plugins
  (load-herbie-plugins)

  (define seed (random 1 (expt 2 31)))
  (set-seed! seed)

  (command-line
   #:program "travis.rkt"
   #:once-each
   [("--seed") rs "The random seed to use in point generation. If false (#f), a random seed is used'"
    (define given-seed (read (open-input-string rs)))
    (when given-seed (set-seed! given-seed))]
   [("--precision") prec "Which precision to use for tests"
    (*precision* (get-representation (string->symbol prec)))]
   [("--num-iters") num "The number of iterations to use for the main loop"
    (*num-iterations* (string->number num))]
   [("--pareto") "Enables Pherbie"
    (*pareto-mode* #t)
    (*ignore-target* #t)
    (*timeout* (* 1000 60 10))
    (disable-flag! 'rules 'numerics)] ; causes time to increase
   #:args bench-dir
   (exit (if (apply run-tests bench-dir) 0 1))))
