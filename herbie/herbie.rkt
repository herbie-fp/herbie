#lang racket

;;; This file is the main command-line entry point to Herbie.
;;;
;;; USAGE:
;;; - `herbie`
;;;   run Herbie, with input from and output to the console
;;; - `herbie [file ...]`
;;;   run Herbie with input from the files and output to the console
;;; - `herbie -o results.json [file ...]`
;;;   as above, but with output to a single JSON file
;;; - `herbie -o graphs/ [file ...]`
;;;   as above, but with output to a folder of reports. The `/` is required
;;;
;;; File inputs must end in `.rkt` or `/` to be valid inputs.
;;;
;;; In the future, this might evolve to have subcommands as well,
;;; which you can distinguish due to the lack of a trailing `.rkt` or
;;; `/`.

(require "common.rkt")
(require "points.rkt")
(require "alternative.rkt")
(require "test.rkt")
(require "sandbox.rkt")

(define output-type (make-parameter 'console))
(define output-name (make-parameter #f))
(define early-exit? (make-parameter #f))
(define profile? (make-parameter #f))
(define threads (make-parameter #f))
(define note (make-parameter #f))

(define (herbie-input? fname)
  (or (not fname) ; Command line
      (and
       (not (file-name-from-path fname))
       (directory-exists? fname)) ; Directory of files
      (and
       (file-name-from-path fname)
       (regexp-match? #rx"\\.rkt$" (file-name-from-path fname))
       (file-exists? fname)))) ; Herbie input format 1 or 2

(define (in-herbie-files . files)
  (apply in-sequences (map file->input files)))

(define (file->input file)
  (cond
   [(not file)
    (sequence-map parse-test (in-port read (current-input-port)))]
   [(not (file-name-from-path file)) ; directory
    (apply sequence-append (map file->input (filter herbie-input? (directory-list file #:build? #t))))]
   [(regexp-match? #rx"\\.rkt$" (file-name-from-path file))
    (call-with-input-file file
      (λ (port) (map parse-test (sequence->list (in-port read port)))))]))

(define get-test-results #f)
(define (get-get-test-results)
  (when (not get-test-results)
    (set! get-test-results
          (dynamic-require "herbie/reports/thread-pool.rkt" 'get-test-results)))
  get-test-results)

(define write-datafile #f)
(define (get-write-datafile)
  (when (not write-datafile)
    (set! write-datafile (dynamic-require "herbie/reports/datafile.rkt" 'write-datafile)))
  write-datafile)

(define make-report-info #f)
(define (get-make-report-info)
  (when (not make-report-info)
    (set! make-report-info (dynamic-require "herbie/reports/datafile.rkt" 'make-report-info)))
  make-report-info)

(define compile-info #f)
(define (get-compile-info)
  (when (not compile-info)
    (set! write-datafile (dynamic-require "herbie/compile/c.rkt" 'compile-info)))
  compile-info)

(define make-report-page #f)
(define (get-make-report-page)
  (when (not make-report-page)
    (set! make-report-page (dynamic-require "herbie/reports/make-report.rkt" 'make-report-page)))
  make-report-page)

(define (in-herbie-output files #:seed seed)
  (match (output-type)
    [(or 'json 'console) ; TODO: Remove 'json the threads don't generate details
     (printf "Running Herbie...\nSeed: ~a\n" seed)
     (sequence-map (λ (test) (get-test-result test #:seed seed))
                   (apply in-herbie-files files))]
    [(or 'json 'report)
     ((get-get-test-results)
      #:threads (threads) #:seed seed #:profile (profile?)
      (reverse (sort (sequence->list (apply in-herbie-files files)) test<?)))]))

(define (run-herbie files)
  (define seed (get-seed))
  (define outputs
    (for/list ([output (in-herbie-output (if (null? files) '("-") files) #:seed seed)]
               [idx (in-naturals)] #:when output)
      (define success?
        (match output
          [(test-result 
            test time bits start-alt end-alt
            points exacts start-est-error end-est-error
            newpoints newexacts start-error end-error target-error
            timeline)
           (match (output-type)
             ['console (printf "~a\n" (alt-program end-alt))]
             [_ (printf "[ ~ams]\t(~a→~a)\t~a\n"
                        (~a time #:width 8)
                        (~r (errors-score start-error) #:min-width 2 #:precision 0)
                        (~r (errors-score end-error) #:min-width 2 #:precision 0)
                        (test-name test))])
           (test-successful?
            test (errors-score start-error)
            (and target-error (errors-score target-error))
            (errors-score end-error))]
          [(test-failure test bits exn time timeline)
           (match (output-type)
             ['console ((error-display-handler) (exn-message exn) exn)]
             [_ (printf "[   CRASH   ]\t\t\t~a\n" (test-name test))])
           #f]
          [(test-timeout test bits time timeline)
           (match (output-type)
             ['console
              (eprintf "Timeout in ~as; use --timeout to change timeout\n" (/ time 1000))]
             [_ (printf "[  timeout  ]\t\t\t~a\n" (test-name test))])
           #f]))
      (when (and (early-exit?) (not success?))
        (exit (+ 1 idx)))))
  
  (define info ((get-make-report-info) outputs #:note (note)))

  (when (equal? output-type 'json)
    (set-seed! seed)
    ((get-write-datafile) output-name info))
  (when (equal? output-type 'report)
    (set-seed! seed)
    ((get-write-datafile) (build-path output-name "results.json") info)
    (set-seed! seed)
    ((get-make-report-page) (build-path output-name "report.html") info)
    ((get-compile-info) output-name info info))) ; TODO: Split out C compilation

(module+ main
  (command-line
   #:program "herbie"
   #:once-each
   [("--profile") "Profile each test"
    (profile? #t)]
   [("--timeout") s "Timeout for each test (in seconds)"
    (*timeout* (* 1000 (string->number s)))]
   [("--seed") rs "The random seed vector to use in point generation"
    (set-seed! (read (open-input-string rs)))]
   [("--test") "Exit with failing status on the first unsuccessful input"
    (early-exit? #t)]
   [("--threads") th "Whether to use threads to run examples in parallel (yes|no|N)"
    (threads
     (match th
       ["no" #f]
       ["yes" (max (- (processor-count) 1) 1)]
       [_ (string->number th)]))]
   [("--num-iters") fu "The number of iterations of the main loop to use"
    (*num-iterations* (string->number fu))]
   [("--num-points") points "The number of points to use"
    (*num-points* (string->number points))]
   [("--note") text "Add a note for this run"
    (note text)]
   [("-o" "--output") fname "Where output should be placed. `-` for console text output, `[name].json` for JSON output, or `[dir]/` for a directory of reports."
    (output-type
          (match fname
            ["-" 'console]
            [(regexp #rx"\\.json$") 'json]
            [(regexp #rx"/$") 'report])) ; TODO : Handle error
    (output-name (if (equal? fname "-") #f (simplify-path fname)))]
   #:multi
   [("-f" "--feature") tf "Toggle flags, specified in the form category:flag"
    (let ([split-strings (string-split tf ":")])
      (when (not (= 2 (length split-strings)))
        (error "Badly formatted input " tf))
      (toggle-flag! (string->symbol (car split-strings)) (string->symbol (cadr split-strings))))]
   #:args files
   (match files
     [(list (? herbie-input? files) ...)
      (run-herbie files)]))) ; TODO : Handle error

