#lang racket

(require racket/lazy-require)
(require "common.rkt" "multi-command-line.rkt" "errors.rkt" "load-plugin.rkt" "sandbox.rkt")

;; Load all the plugins
(load-herbie-plugins)

(lazy-require
 ["web/demo.rkt" (run-demo)]
 ["web/run.rkt" (make-report rerun-report replot-report diff-report)]
 ["shell.rkt" (run-shell)]
 ["improve.rkt" (run-improve)])

(define (string->thread-count th)
  (match th ["no" #f] ["yes" (max (- (processor-count) 1) 1)] [_ (string->number th)]))

(define (string->flag s)
  (match (string-split s ":")
    [(list (app string->symbol category) (app string->symbol flag))
     (and
      (dict-has-key? all-flags category)
      (set-member? (dict-ref all-flags category) flag)
      (list category flag))]
    [_ #f]))

(module+ main
  (define quiet? #f)
  (define demo-output #f)
  (define demo-log #f)
  (define demo-prefix "/")
  (define demo? #f)
  (define demo-port 8000)
  (define demo-public #f)

  (define threads #f)
  (define report-note #f)

  (define pareto-timeout (* 1000 60 60 2))
  (define timeout-set? #f)

  (define seed (random 1 (expt 2 31)))
  (set-seed! seed)

  ; handle plugin warnings
  (print-warnings)

  (multi-command-line
   #:program "herbie"
   #:once-each
   [("--timeout") s "Timeout for each test (in seconds)"
    (set! timeout-set? #t)
    (*timeout* (* 1000 (string->number s)))]
   [("--seed") int "The random seed to use in point generation"
    (define given-seed (read (open-input-string int)))
    (when given-seed (set-seed! given-seed))]
   [("--num-iters") num "The number of iterations to use for the main loop"
    (*num-iterations* (string->number num))]
   [("--num-points") num "The number of points to use during sampling"
    (*num-points* (string->number num))]
   [("--num-enodes") num "The number of enodes to use during simplification"
    (*node-limit* (string->number num))]
   [("--num-analysis") num "The number of input analysis iterations to use"
    (*max-find-range-depth* (string->number num))]
   [("--pareto") "Enables Pareto-Herbie (Pherbie)"
    (*pareto-mode* #t)
    (unless timeout-set? (*timeout* pareto-timeout))]
   #:multi
   [("-o" "--disable") flag "Disable a flag (formatted category:name)"
    (define tf (string->flag flag))
    (when (not tf)
      (raise-herbie-error "Invalid flag ~a" flag #:url "options.html"))
    (apply disable-flag! tf)]
   [("+o" "--enable") flag "Enable a flag (formatted category:name)"
    (define tf (string->flag flag))
    (when (not tf)
      (raise-herbie-error "Invalid flag ~a" flag #:url "options.html"))
    (apply enable-flag! tf)]

   #:subcommands
   [shell "Interact with Herbie from the shell"
    #:args ()
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
    #:args ()
    (run-demo #:quiet quiet? #:output demo-output #:log demo-log #:prefix demo-prefix #:demo? demo? #:port demo-port #:public? demo-public)]
   [improve "Run Herbie on an FPCore file, producing an FPCore file"
    #:once-each
    [("--threads") num "How many tests to run in parallel: 'yes', 'no', or a number"
     (set! threads (string->thread-count num))]
    #:args (input output)
    (run-improve input output #:threads threads)]
   [report "Run Herbie on an FPCore file, producing an HTML report"
    #:once-each
    [("--note") note "Add a note for this run"
     (set! report-note note)]
    [("--threads") num "How many tests to run in parallel: 'yes', 'no', or a number"
     (set! threads (string->thread-count num))]
    [("--profile") "Whether to profile each run"
     (void)]
    #:args (input output)
    (make-report (list input) #:dir output #:note report-note #:threads threads)]
   [reproduce "Rerun an HTML report"
    #:once-each
    [("--note") note "Add a note for this run"
     (set! report-note note)]
    [("--threads") num "How many tests to run in parallel: 'yes', 'no', or a number"
     (set! threads (string->thread-count num))]
    [("--profile") "Whether to profile each run"
     (void)]
    #:args (input output)
    (rerun-report input #:dir output #:note report-note #:threads threads)]
   [replot "Regenerate plots for an HTML report"
    #:args (input output)
    (replot-report input #:dir output)]
   [diff "Diff two HTML reports"
    #:args (old new)
    (diff-report old new)]

   #:args files
   (match files
     ['()
      (eprintf "Please specify a Herbie tool, such as `herbie web`.\n")
      (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
     [(cons tool _)
      (eprintf "Unknown Herbie tool `~a`. See a list of available tools with `herbie --help`.\n" tool)
      (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)])))
