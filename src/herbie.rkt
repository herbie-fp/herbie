#lang racket

(require racket/lazy-require)
(require "common.rkt" "multi-command-line.rkt" "sandbox.rkt" "errors.rkt"
         "syntax/syntax.rkt" "syntax/rules.rkt")

(lazy-require
 ["web/demo.rkt" (run-demo)]
 ["web/run.rkt" (make-report rerun-report)]
 ["shell.rkt" (run-shell)]
 ["improve.rkt" (run-improve)])

(define (string->thread-count th)
  (match th ["no" #f] ["yes" (max (- (processor-count) 1) 1)] [_ (string->number th)]))

(define (check-operator-fallbacks!)
  (prune-operators!)
  (prune-rules!)
  (define unknown-ops (if (flag-set? 'precision 'double) (*unknown-d-ops*) (*unknown-f-ops*)))
  (unless (null? unknown-ops)
    (warn 'fallback #:url "faq.html#native-ops"
          "native ~a not supported on your system; ~a"
          (string-join (map ~a unknown-ops) ", ")
          (if (flag-set? 'precision 'fallback)
              "fallbacks will be used"
              "functions are disabled"))))

(module+ main
  (define quiet? #f)
  (define demo-output #f)
  (define demo-log #f)
  (define demo-prefix "/")
  (define demo? #f)
  (define demo-port 8000)
  (define demo-public #f)

  (define threads #f)
  (define report-profile? #f)
  (define report-note #f)
  (define report-debug? #f)

  (define seed (random 1 (expt 2 31)))
  (set-seed! seed)

  (multi-command-line
   #:program "herbie"
   #:once-each
   [("--timeout") s "Timeout for each test (in seconds)"
    (*timeout* (* 1000 (string->number s)))]
   [("--seed") int "The random seed to use in point generation"
    (define given-seed (read (open-input-string int)))
    (when given-seed (set-seed! given-seed))]
   [("--num-iters") num "The number of iterations of the main loop to use"
    (*num-iterations* (string->number num))]
   [("--num-points") num "The number of points to use"
    (*num-points* (string->number num))]
   #:multi
   [("-o" "--disable") flag "Disable a flag (formatted category:name)"
    (define tf (parse-flag flag))
    (when (not tf)
      (raise-herbie-error "Invalid flag ~a" flag #:url "options.html"))
    (apply disable-flag! tf)]
   [("+o" "--enable") flag "Enable a flag (formatted category:name)"
    (define tf (parse-flag flag))
    (when (not tf)
      (raise-herbie-error "Invalid flag ~a" flag #:url "options.html"))
    (apply enable-flag! tf)]

   #:subcommands
   [shell "Interact with Herbie from the shell"
    #:args ()
    (check-operator-fallbacks!)
    (run-shell)]
   [web "Interact with Herbie from your browser"
    #:once-each
    [("--port") port "Port to run the web shell on"
     (set! demo-port (string->number port))]
    [("--public") "Whether to listen on a public port (instead of localhost)"
     (set! demo-public #t)]
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
    [("--debug") "Whether to compute metrics and debug info"
     (set! report-debug? true)]
    #:args ()
    (check-operator-fallbacks!)
    (run-demo #:quiet quiet? #:output demo-output #:log demo-log #:prefix demo-prefix #:debug report-debug? #:demo? demo? #:port demo-port #:public? demo-public)]
   [improve "Run Herbie on an FPCore file, producing an FPCore file"
    #:once-each
    [("--threads") num "How many tests to run in parallel: 'yes', 'no', or a number"
     (set! threads (string->thread-count num))]
    #:args (input output)
    (check-operator-fallbacks!)
    (run-improve input output #:threads threads)]
   [report "Run Herbie on an FPCore file, producing an HTML report"
    #:once-each
    [("--note") note "Add a note for this run"
     (set! report-note note)]
    [("--threads") num "How many tests to run in parallel: 'yes', 'no', or a number"
     (set! threads (string->thread-count num))]
    [("--profile") "Whether to profile each run"
     (set! report-profile? true)]
    [("--debug") "Whether to compute metrics and debug info"
     (set! report-debug? true)]
    #:args (input output)
    (check-operator-fallbacks!)
    (make-report (list input) #:dir output #:profile report-profile? #:debug report-debug? #:note report-note #:threads threads)]
   [reproduce "Rerun an HTML report"
    #:once-each
    [("--note") note "Add a note for this run"
     (set! report-note note)]
    [("--threads") num "How many tests to run in parallel: 'yes', 'no', or a number"
     (set! threads (string->thread-count num))]
    [("--profile") "Whether to profile each run"
     (set! report-profile? true)]
    #:args (input output)
    (check-operator-fallbacks!)
    (rerun-report input #:dir output #:profile report-profile? #:debug report-debug? #:note report-note #:threads threads)]

   #:args files
   (begin
     (match files
       ['()
        (eprintf "Please specify a Herbie tool, such as `herbie shell`.\n")
        (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
       [(cons tool _)
        (eprintf "Unknown Herbie tool `~a`. See a list of available tools with `herbie --help`.\n" tool)
        (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]))))
