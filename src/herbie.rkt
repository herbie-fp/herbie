#lang racket

;;; This file is the main command-line entry point to Herbie.
;;;
;;; File inputs must end in `.fpcore` or `/` to be valid inputs.
;;;
;;; In the future, this might evolve to have subcommands as well,
;;; which you can distinguish due to the lack of a trailing `.fpcore` or
;;; `/`.

(require syntax/parse racket/lazy-require)
(require "common.rkt" "errors.rkt" "points.rkt" "alternative.rkt"
         "formats/test.rkt" "sandbox.rkt" "multi-command-line.rkt")

(lazy-require
 ["web/demo.rkt" (run-demo)]
 ["reports/run.rkt" (make-report)])

#;(define threads (make-parameter #f))

(define (read-fpcore name port)
  (with-handlers
      ([(or/c exn:fail:user? exn:fail:read?)
        (λ (e)
          ((error-display-handler) (exn-message e) e)
          (read-fpcore name port))])
    (define input (read-syntax name port))
    (if (eof-object? input) eof (parse-test input))))

(define (herbie-input? fname)
  (or (not fname) ; Command line
      (and
       (not (file-name-from-path fname))
       (directory-exists? fname)) ; Directory of files
      (and
       (file-name-from-path fname)
       (regexp-match? #rx"\\.fpcore" (file-name-from-path fname))
       (file-exists? fname)))) ; Herbie input format 1 or 2

(define (in-herbie-files files)
  (if (null? files)
      (in-port (curry read-fpcore "stdin") (current-input-port))
      (all-herbie-tests files)))

(define (all-herbie-tests files)
  (apply append
   (for/list ([file files])
     (if (directory-exists? file)
         (all-herbie-tests (filter herbie-input? (directory-list file #:build? #t)))
         (call-with-input-file file
           (λ (port)
             (define file* (if (string? file) (string->path file) file))
             (port-count-lines! port)
             (sequence->list (in-port (curry read-fpcore file*) port))))))))

(define (in-herbie-output files #:seed seed)
  (eprintf "Seed: ~a\n" seed)
  (sequence-map
   (λ (test) (get-test-result test #:seed seed))
   (in-herbie-files files)))

(define (run-herbie files)
  (define seed (get-seed))
  (with-handlers ([exn:break? (λ (e) (exit 0))])
    (for ([output (in-herbie-output files #:seed seed)] [idx (in-naturals)]
          #:when output)
      (match output
        [(test-result test time bits start-alt end-alt points exacts
                      start-est-error end-est-error newpoint newexacts
                      start-error end-error target-error timeline)
         (eprintf "[ ~ams]\t~a\t(~a→~a)\n"
                  (~a time #:width 8)
                  (test-name test)
                  (~r (errors-score start-error) #:min-width 2 #:precision 1)
                  (~r (errors-score end-error) #:min-width 2 #:precision 1))
         (printf "~a\n" (unparse-test (alt-program end-alt)))]
        [(test-failure test bits exn time timeline)
         (eprintf "[   CRASH   ]\t~a\n" (test-name test))
         (printf ";; Crash in ~a\n" (test-name test))
         ((error-display-handler) (exn-message exn) exn)]
        [(test-timeout test bits time timeline)
         (eprintf "[  timeout  ]\t~a\n" (test-name test))
         (printf ";; ~as timeout in ~a\n;; use --timeout to change timeout\n" (/ time 1000) (test-name test))]))))

(module+ main
  (multi-command-line
   #:program "herbie"
   #:once-each
   [("--timeout") s "Timeout for each test (in seconds)"
    (*timeout* (* 1000 (string->number s)))]
   [("--seed") rs "The random seed vector to use in point generation. If false (#f), a random seed is used'"
    (define given-seed (read (open-input-string rs)))
    (when given-seed (set-seed! given-seed))]
   #;[("--threads") th "Whether to use threads to run examples in parallel (yes|no|N)"
    (threads
     (match th
       ["no" #f]
       ["yes" (max (- (processor-count) 1) 1)]
       [_ (string->number th)]))]
   [("--num-iters") fu "The number of iterations of the main loop to use"
    (*num-iterations* (string->number fu))]
   [("--num-points") points "The number of points to use"
    (*num-points* (string->number points))]
   #:multi
   [("-o" "--disable") tf "Disable flag formatted category:name"
    (define flag (parse-flag tf))
    (when (not flag)
      (raise-herbie-error "Invalid flag ~a" tf #:url "options.html"))
    (apply disable-flag! flag)]
   [("+o" "--enable") tf "Enable flag formatted category:name"
    (define flag (parse-flag tf))
    (when (not flag)
      (raise-herbie-error "Invalid flag ~a" tf #:url "options.html"))
    (apply enable-flag! flag)]

   #:subcommands
   ["shell"
    #:args ()
    (run-herbie '())]
   ["web"
    ;; TODO: --save-session out, --log out, --prefix prefix, --quiet
    #:args ()
    (run-demo #t)]
   ["improve"
    ;; TODO: --threads
    #:args (input output)
    (with-output-to-file output #:exists 'replace (λ () (run-herbie (list input))))]
   ["report"
    ;; TODO: --profile, --threads, --note
    #:args (input output)
    (make-report input #:dir output)]

   #:args files
   (run-herbie files)))
