#lang racket

(require (only-in fpbench core->c)
         herbie/accelerator
         herbie/common
         herbie/datafile
         herbie/errors
         herbie/load-plugin
         herbie/pareto
         herbie/platform
         herbie/points
         herbie/sandbox
         herbie/syntax/read
         herbie/web/common
         herbie/web/core2mkl
         herbie/web/core2python3-10
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
       (for ([i (in-naturals 1)] [entry (in-table-row res)])
         (match-define (list cost err expr) entry)
         (define expr*
            (unparse-result res
                            #:expr expr
                            #:description (format "variant ~a" i)))
         (writeln expr* p)
         (writeln cost p)
         (writeln err p))])))

;; Replaces any unsupported accelerators in an FPCore
(define (remove-accelerators core)
  (define op-set (platform-operator-set (*active-platform*)))
  (define ops-in-pform (operator-set-operators op-set))
  (define accels (filter-not (curry set-member? ops-in-pform) (all-accelerators)))
  (define (remove expr) (expand-accelerators expr #:accelerators accels))
  (match core
    [`(FPCore ,id (,vars ...) ,props ... ,expr)
     `(FPCore ,id ,vars ,@props ,(remove expr))]
    [`(FPCore (,vars ...) ,props ... ,expr)
     `(FPCore ,vars ,@props ,(remove expr))]))

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
           [(python) (core->python core "foo")]
           [else (error 'run-server "compile: unsupported language ~a" lang)]))
       (printf "~a\n" (string-replace output "\n" "\\n"))
       (loop)]
      ; cost <core:expr>
      [(list 'cost args ...)
       (define core
         (match args
           [(list core) core]
           [_ (error 'run-server "cost: malformed arguments ~a" args)]))
       (define test (parse-test (datum->syntax #f core)))
       (define result (run-herbie 'cost test #:seed seed #:timeline-disabled? #t))
       (define cost (job-result-backend result))
       (printf "~a\n" cost)
       (loop)]
      ; desugar <core:expr>
      [(list 'desugar args ...)
       (define core
         (match args
           [(list core) core]
           [_ (error 'run-server "desugar: malformed arguments ~a" args)]))
       (define core* (remove-accelerators core))
       (with-handlers ([exn:fail:user:herbie:syntax?
                        (lambda (_)
                          (eprintf "Failed to parse ~a\n" core)
                          (writeln #f))])
          (parse-test (datum->syntax #f core*))
          (writeln core*))
       (loop)]
      ; error <for-sample:expr> <core:expr> ...
      [(list 'error args ...)
       (define-values (input-core eval-cores)
         (match args
           [(list input-core eval-cores ...) (values input-core eval-cores)]
           [_ (error 'run-server "error: malformed arguments ~a" args)]))
       (define test (parse-test (datum->syntax #f input-core)))
       (define pctx ; run a no iteration improve job to get the pcontexts
         (parameterize ([*num-iterations* 0])
           (define result (run-herbie 'improve test
                                      #:seed seed
                                      #:timeline-disabled? #t))
           (match-define (list train-pcontext test-pcontext) (improve-result-pctxs (job-result-backend result)))
           train-pcontext))
       (define test-errs ; evaluate errors on the eval cores
         (for/list ([eval-core (in-list eval-cores)])
           (define test (parse-test (datum->syntax #f eval-core)))
           (define test-errs (errors (test-input test) pctx (test-context test)))
           (errors-score test-errs)))
       (printf "~a\n" (string-join (map ~s test-errs) " "))
       (loop)]
      ; improve <core> <threads:int>
      [(list 'improve args ...)
       (define-values (cores threads)
         (match args
           [(list cores threads) (values cores threads)]
           [_ (error 'run-server "improve: malformed arguments ~a" args)]))
       (define tests (map (lambda (c) (parse-test (datum->syntax #f c))) cores))
       (define results (get-test-results tests #:threads threads #:seed seed #:profile #f #:dir #f))
       (print-output (current-output-port) results)
       (loop)]
      ; pareto <frontier:list> ...
      [(list 'pareto args ...)
       (define combined (pareto-combine args #:convex? #t))
       (displayln
         (string-join
           (for/list ([point (in-list combined)])
             (match-define (list cost err) point)
             (format "~a ~a" cost err))
           "|"))
       (loop)]
      ; read <bench:string>
      [(list 'read args ...)
       (define path
         (match args
           [(list path) path]
           [_ (error 'run-server "read: malformed arguments ~a" args)]))
       (for ([test (in-list (load-tests path))])
         (printf "~a\n" (string-replace (render-fpcore test) "\n" "")))
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
           (define result (run-herbie 'sample test #:seed seed #:timeline-disabled? #t))
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
     (*platform-name* (string->symbol name))
     (*active-platform* (get-platform (*platform-name*)))
     (activate-platform! (*active-platform*))]
    #:args ()
    (run-server seed)))
