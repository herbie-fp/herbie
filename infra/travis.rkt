#lang racket

(require "../src/common.rkt" "../src/points.rkt" "../src/load-plugin.rkt"
         "../src/alternative.rkt" "../src/sandbox.rkt" "../src/syntax/read.rkt"
         "../src/interface.rkt")

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
                (for/list ([(var prec) (in-dict (test-var-reprs the-test))])
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
    (match (get-test-result the-test* #:seed seed)
      [(test-success test bits time timeline warnings
                     start-alt end-alts preprocess points exacts start-est-error end-est-error
                     newpoints newexacts start-error end-errors target-error
                     baseline-error oracle-error start-cost costs all-alts)
       (define end-error (car end-errors))
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
         (pretty-print (alt-program (car end-alts)) (current-output-port) 1)
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
   [("--pareto") num "Enables Pherbie with <num> iters"
    (*pareto-mode* #t)
    (*ignore-target* #t)
    (*num-iterations* (string->number num))
    (*timeout* (* 1000 60 10))
    (disable-flag! 'rules 'numerics)] ; causes time to increase
   #:args bench-dir
   (exit (if (apply run-tests bench-dir) 0 1))))
