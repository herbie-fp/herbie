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

(define *threads* #f)
(define *test-name* #f)

(define *profile?* #f)
(define *note* #f)

(define (make-report . bench-dirs)
  (define seed (get-seed))
  (define dir report-output-path)

  (when (not (directory-exists? dir)) (make-directory dir))

  (define tests (allowed-tests bench-dirs))
  (define results
    (get-test-results tests #:threads *threads* #:seed seed #:profile *profile?*))
  (define info (make-report-info (filter identity results) #:note *note*))

  (copy-file "herbie/compile/overhead.c" (build-path dir "overhead.c") #t)
  (copy-file "herbie/compile/overhead.mk" (build-path dir "Makefile") #t)

  (set-seed! seed)
  (write-datafile (build-path dir "results.json") info)
  (make-report-page (build-path dir "report.html") info)
  (log-exceptions (build-path dir "exceptions.rkt") info)
  ; TODO: Uses the same expressions for float and double. This could be good to change.
  (compile-info dir info info))

(define (allowed-tests bench-dirs)
  (define unsorted-tests (append-map load-tests bench-dirs))
  (if *test-name*
      (filter (λ (t) (equal? *test-name* (test-name test))) unsorted-tests)
      (reverse (sort unsorted-tests test<?))))

(define (test<? t1 t2)
  (cond
   [(and (test-output t1) (test-output t2))
    (string<? (test-name t1) (test-name t2))]
   [(and (not (test-output t1)) (not (test-output t2)))
    (string<? (test-name t1) (test-name t2))]
   [else
    ; Put things with an output first
    (test-output t1)]))

(command-line
 #:program "run"
 #:once-each
 [("-p" "--profile") "Whether to profile each test"
  (set! *profile?* #t)]
 [("-r" "--seed") rs "The random seed vector to use in point generation"
  (set-seed! (read (open-input-string rs)))]
 [("--threads") th "How many tests to run in parallel to use. Pass 'no' to use no threads (default), 'yes' to use the number of machine cores less one, and a number to use that many."
  (set! *threads*
        (match th ["no" #f] ["yes" (max (- (processor-count) 1) 1)] [_ (string->number th)]))]
 [("--fuel") fu "The amount of 'fuel' to use"
  (*num-iterations* (string->number fu))]
 [("--num-points") points "The number of points to use"
  (*num-points* (string->number points))]
 [("--run-single") test-name "If specified, run a single test (specify the test name)"
  (set! *test-name* test-name)]
 [("--note") note "Add a note for this run"
  (set! *note* note)]
 #:multi
 [("-o" "--option") tf "Toggle flags, specified in the form category:flag"
  (let ([split-strings (string-split tf ":")])
    (when (not (= 2 (length split-strings)))
      (error "Badly formatted input " tf))
    (toggle-flag! (string->symbol (car split-strings)) (string->symbol (cadr split-strings))))]
 #:args bench-dir
 (apply make-report bench-dir))
