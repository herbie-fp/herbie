#lang racket

(require (only-in fpbench core->c)
         herbie/datafile
         herbie/load-plugin
         herbie/platform
         herbie/points
         herbie/sandbox
         herbie/syntax/read
         herbie/web/core2mkl
         herbie/web/thread-pool)

(load-herbie-builtins)

;; Also in src/improve.rkt
(define (in-table-row tr)
  (unless (table-row? tr)
    (raise-argument-error 'in-table-row "table-row?" tr))
  (match (table-row-status tr)
    [(or "error" "crash" "timeout")
     (raise-argument-error 'in-table-row "table row not valid result" tr)]
    [_
     (match-define (list _ (list best-cost best-err) other) (table-row-cost-accuracy tr))
     (define first (list best-cost best-err (table-row-output tr)))
     (in-list (cons first other))]))

;; Also in src/improve.rkt
(define (print-output p results)
  (for ([res results] #:when res)
    (match (table-row-status res)
      ["error" (void)]
      ["crash" (void)]
      ["timeout" (void)]
      [(? string?)
       (define results
         (for/list ([i (in-naturals 1)] [entry (in-table-row res)])
           (match-define (list _ _ expr) entry)
           (unparse-result
             (struct-copy table-row res
               [name (format "~a variant ~a" (table-row-name res) i)])
             #:expr expr)))
       (for ([result results])
         (writeln result))])))

;; An insane contraption.
;; Reads commands from stdin and writes results to stdout.
;; All output must be on a single line and terminated by a newline.
(define (run-server seed)
  (let loop ()
    (match (read)
      ; compile <lang:symbol> <core:expr>
      [(list 'compile args ...)
       (define-values (lang core)
         (match args
           [(list lang core) (values lang core)]
           [_ (error 'run-server "compile: malformed arguments ~a" args)]))
       (define output
         (case lang
           [(c) (core->c core "foo")]
           [(mkl) (core->mkl core "foo")]
           [else (error 'run-server "compile: unsupported language ~a" lang)]))
       (printf "~a\n" (string-replace output "\n" "\\n"))
       (loop)]
      ; cost <core:expr>
      [(list 'cost args ...)
       (define core
         (match args
           [(list core) core]
           [_ (error 'run-server "compile: malformed arguments ~a" args)]))
       (define test (parse-test (datum->syntax #f core)))
       (define result (run-herbie 'cost test #:seed seed))
       (define cost (job-result-backend result))
       (printf "~a\n" cost)
       (loop)]
      ; improve <bench:string> <threads:int>
      [(list 'improve args ...)
       (define-values (bench threads)
         (match args
           [(list bench threads) (values bench threads)]
           [_ (error 'run-server "improve: malformed arguments ~a" args)]))
       (define tests (load-tests bench))
       (define results (get-test-results tests #:threads threads #:seed seed #:profile #f #:dir #f))
       (print-output (current-output-port) results)
       (loop)]
      ; sample <num_points:int> <core:expr>
      [(list 'sample args ...)
       (define-values (num-points core)
         (match args
           [(list num-points core) (values num-points core)]
           [_ (error 'run-server "sample: malformed arguments ~a" args)]))
       (define test (parse-test (datum->syntax #f core)))
       (define pctx
         (parameterize ([*reeval-pts* num-points])
           (define result (run-herbie 'sample test #:seed seed))
           (job-result-backend result)))
       (printf "~a\n"
               (string-join
                  (for/list ([(pt _) (in-pcontext pctx)])
                    (string-join (map ~s pt) ","))
                  "|"))
       (loop)]
      ; exit
      [(list 'exit) (void)]
      ; <unknown>
      [cmd (error 'run-server "unknown command ~a" cmd)])))


(module+ main
  (define seed 1)
  (command-line
    #:program "cost"
    #:once-each
    [("--seed") _seed "Seed to use within Herbie"
     (set! seed (string->number _seed))]
    [("--platform") name "Platform to use"
     (*active-platform* (get-platform (string->symbol name)))
     (activate-platform! (*active-platform*))]
    #:args ()
    (run-server seed)))
