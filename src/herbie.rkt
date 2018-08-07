#lang racket

(require racket/lazy-require)
(require "common.rkt" "multi-command-line.rkt" "sandbox.rkt" "errors.rkt"
         "syntax/syntax.rkt" "syntax/rules.rkt")

(lazy-require
 ["web/demo.rkt" (run-demo)]
 ["reports/run.rkt" (make-report rerun-report)]
 ["shell.rkt" (run-shell)]
 ["improve.rkt" (run-improve)])

(define (string->thread-count th)
  (match th ["no" #f] ["yes" (max (- (processor-count) 1) 1)] [_ (string->number th)]))

(define (check-operator-fallbacks!)
  (prune-operators!)
  (prune-rules!)
  (unless (null? (if (flag-set? 'precision 'double) (*unknown-d-ops*) (*unknown-f-ops*)))
    (eprintf "Warning: native ~a not supported on your system; "
             (string-join (map ~a (if (flag-set? 'precision 'double) (*unknown-d-ops*) (*unknown-f-ops*)))
                          ", "))
    (eprintf (if (flag-set? 'precision 'fallback) "fallbacks will be used.\n" "functions are disabled.\n"))
    (eprintf "See <https://herbie.uwplse.org/doc/~a/faq.html#native-ops> for more info.\n"
             *herbie-version*)))

(module+ main
  (define quiet? #f)
  (define demo-output #f)
  (define demo-log #f)
  (define demo-prefix "/")
  (define demo? #f)
  (define demo-port 8000)

  (define threads #f)
  (define report-profile? #f)
  (define report-note #f)

  (define seed (random 1 (expt 2 31)))
  (set-seed! seed)

  (multi-command-line
   #:program "herbie"
   #:once-each
   [("--timeout") s "Timeout for each test (in seconds)"
    (*timeout* (* 1000 (string->number s)))]
   [("--seed") rs "The random seed to use in point generation"
    (define given-seed (read (open-input-string rs)))
    (when given-seed (set-seed! given-seed))]
   [("--num-iters") fu "The number of iterations of the main loop to use"
    (*num-iterations* (string->number fu))]
   [("--num-points") points "The number of points to use"
    (*num-points* (string->number points))]
   #:multi
   [("-o" "--disable") tf "Disable a flag (formatted category:name)"
    (define flag (parse-flag tf))
    (when (not flag)
      (raise-herbie-error "Invalid flag ~a" tf #:url "options.html"))
    (apply disable-flag! flag)]
   [("+o" "--enable") tf "Enable a flag (formatted category:name)"
    (define flag (parse-flag tf))
    (when (not flag)
      (raise-herbie-error "Invalid flag ~a" tf #:url "options.html"))
    (apply enable-flag! flag)]

   #:subcommands
   [shell "Interact with Herbie from the shell"
    #:args ()
    (check-operator-fallbacks!)
    (run-shell)]
   [web "Interact with Herbie from your browser"
    #:once-each
    [("--port") port "Port to run the web shell on"
     (set! demo-port (string->number port))]
    [("--save-session") dir "The dir to place a report from submitted expressions"
     (set! demo-output dir)]
    [("--log") file "The file to write web access log to"
     (set! demo-log file)]
    [("--prefix") prefix "Prefix for proxying demo"
     (set! demo-prefix prefix)]
    [("--demo") "Run in Herbie web demo mode. Changes some text"
     (set! demo? true)]
    [("--quiet") "Print a smaller banner and don't start a browser."
     (set! quiet? true)]
    #:args ()
    (check-operator-fallbacks!)
    (run-demo #:quiet quiet? #:output demo-output #:log demo-log #:prefix demo-prefix #:demo? demo? #:port demo-port)]
   [improve "Run Herbie on an FPCore file, producing an FPCore file"
    #:once-each
    [("--threads") th "How many tests to run in parallel: 'yes', 'no', or a number"
     (set! threads (string->thread-count th))]
    #:args (input output)
    (check-operator-fallbacks!)
    (run-improve input output #:threads threads)]
   [report "Run Herbie on an FPCore file, producing an HTML report"
    #:once-each
    [("--note") note "Add a note for this run"
     (set! report-note note)]
    [("--threads") th "How many tests to run in parallel: 'yes', 'no', or a number"
     (set! threads (string->thread-count th))]
    [("--profile") "Whether to profile each run"
     (set! report-profile? true)]
    #:args (input output)
    (check-operator-fallbacks!)
    (make-report (list input) #:dir output #:profile report-profile? #:note report-note #:threads threads)]
   [reproduce "Rerun an HTML report"
    #:once-each
    [("--note") note "Add a note for this run"
     (set! report-note note)]
    [("--threads") th "How many tests to run in parallel: 'yes', 'no', or a number"
     (set! threads (string->thread-count th))]
    [("--profile") "Whether to profile each run"
     (set! report-profile? true)]
    #:args (input output)
    (check-operator-fallbacks!)
    (rerun-report input #:dir output #:profile report-profile? #:note report-note #:threads threads)]

   #:args files
   (begin
     (match files
       ['()
        (eprintf "Please specify a Herbie tool, such as `herbie shell`.\n")
        (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
       [(cons tool _)
        (eprintf "Unknown Herbie tool `~a`. See a list of available tools with `herbie --help`.\n" tool)
        (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]))))
