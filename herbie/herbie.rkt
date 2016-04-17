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

(define (in-herbie-files files)
  (if (null? files)
      (sequence-map parse-test (in-port read (current-input-port)))
      (all-herbie-tests files)))

(define (all-herbie-tests files)
  (apply append
   (for/list ([file files])
     (if (directory-exists? file)
         (all-herbie-tests (filter herbie-input? (directory-list file #:build? #t)))
         (call-with-input-file file
           (λ (port) (map parse-test (sequence->list (in-port read port)))))))))

(define (in-herbie-output files #:seed seed)
  (eprintf "Seed: ~a\n" seed)
  (sequence-map
   (λ (test) (get-test-result test #:seed seed))
   (in-herbie-files files)))

(define (run-herbie files)
  (define seed (get-seed))
  (for ([output (in-herbie-output files #:seed seed)]
        [idx (in-naturals)] #:when output)
    (define success?
      (match output
        [(test-result test time bits start-alt end-alt points exacts
                      start-est-error end-est-error newpoint newexacts
                      start-error end-error target-error timeline)
         (eprintf "[ ~ams]\t~a\t(~a→~a)\n"
                  (~a time #:width 8)
                  (test-name test)
                  (~r (errors-score start-error) #:min-width 2 #:precision 0)
                  (~r (errors-score end-error) #:min-width 2 #:precision 0))
         (when (not (early-exit?))
           (printf "~a\n" (alt-program end-alt)))
         (test-successful? test (errors-score start-error) (if target-error (errors-score target-error) #f) (errors-score end-error))]
        [(test-failure test bits exn time timeline)
         (eprintf "[   CRASH   ]\t~a\n" (test-name test))
         (when (not (early-exit?))
           (printf ";; Crash in ~a\n" (test-name test)))
         #f]
        [(test-timeout test bits time timeline)
         (eprintf "[  timeout  ]\t~a\n" (test-name test))
         (when (not (early-exit?))
           (printf ";; ~as timeout in ~a\n;; use --timeout to change timeout\n" (/ time 1000) (test-name test)))
         #f]))
    (when (and (early-exit?) (not success?))
      (exit (+ 1 idx)))))

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
   #:multi
   [("-f" "--feature") tf "Toggle flags, specified in the form category:flag"
    (let ([split-strings (string-split tf ":")])
      (when (not (= 2 (length split-strings)))
        (error "Badly formatted input " tf))
      (toggle-flag! (string->symbol (car split-strings)) (string->symbol (cadr split-strings))))]
   #:args files
   (run-herbie files))) ; TODO : Handle error

