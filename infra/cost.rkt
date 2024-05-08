#lang racket

(require json racket/date)
(require "../src/syntax/types.rkt")
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
         herbie/web/core2avx
         herbie/web/thread-pool)

(load-herbie-builtins)


(define (flags->list flags)
  (for*/list ([rec (hash->list flags)] [fl (cdr rec)])
    (format "~a:~a" (car rec) fl)))

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

(define (make-report-json results #:seed seed)
  (define basic-info (make-report-info (filter values results) #:seed seed))
  ; Copied from datafile.rkt
  (define (simplify-test test)
    (match test
      [(table-row name identifier status pre preprocess prec conversions vars
                  input output spec target-prog
                  start-bits end-bits target-bits start-est end-est
                  time link cost-accuracy)
       (define bits (representation-total-bits (get-representation prec)))
       (define cost-accuracy*
        (match cost-accuracy
          [(list) (list)]
          [(list start best others)
            (list start best
                  (for/list ([other (in-list others)])
                    (match-define (list cost error expr) other)
                    (list cost error (~a expr))))]))
       (make-hash
        `((name . ,name)
          (identifier . ,(~s identifier))
          (pre . ,(~s pre))
          (preprocess . ,(~s preprocess))
          (prec . ,(~s prec))
          (bits . ,(representation-total-bits (get-representation prec)))
          (conversions . ,(map (curry map ~s) conversions))
          (status . ,status)
          (start . ,start-bits)
          (end . ,end-bits)
          (target . ,target-bits)
          (start-est . ,start-est)
          (end-est . ,end-est)
          (vars . ,(if vars (map symbol->string vars) #f))
          (input . ,(~s input))
          (output . ,(~s (unparse-result test
                            #:expr output)))
          (spec . ,(~s spec))
          (target-prog . ,(~s target-prog))
          (time . ,time)
          (link . ,(~a link))
          (cost-accuracy . ,cost-accuracy*)))]))
  (match basic-info
      [(report-info date commit branch hostname seed flags points iterations note tests merged-cost-accuracy)
       (make-hash
        `((date . ,(date->seconds date))
          (commit . ,commit)
          (branch . ,branch)
          (hostname . ,hostname)
          (seed . ,(~a seed))
          (flags . ,(flags->list flags))
          (points . ,points)
          (iterations . ,iterations)
          (note . ,note)
          (tests . ,(map simplify-test tests))
          (merged-cost-accuracy . ,merged-cost-accuracy)))]))

;; Converts a Python-ized pcontext into a Racket pcontext
(define (python->pcontext pts&exs)
  (define pts (map first pts&exs))
  (define exs (map second pts&exs))
  (mk-pcontext pts exs))

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
           [(avx) (core->avx core "foo")]
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
      ; error <core> <points>
      [(list 'error args ...)
       (define-values (core points)
         (match args
           [(list core points) (values core points)]
           [_ (error 'run-server "error: malformed arguments ~a" args)]))
       (define test (parse-test (datum->syntax #f core)))
       (define pctx (python->pcontext points))
       (define result (run-herbie 'errors test #:pcontext pctx #:seed seed #:timeline-disabled? #t))
       (printf "~a\n" (errors-score (map second (job-result-backend result))))
       (loop)]
      ; improve <core> <threads:int> <dir>
      [(list 'improve args ...)
       (define-values (cores threads dir)
         (match args
           [(list cores threads dir) (values cores threads dir)]
           [_ (error 'run-server "improve: malformed arguments ~a" args)]))
       (define tests (map (lambda (c) (parse-test (datum->syntax #f c))) cores))
       (define results (get-test-results tests #:threads threads #:seed seed #:profile #f #:dir #f))
       (define info (make-report-json results #:seed seed))
       (call-with-atomic-output-file (build-path (symbol->string dir) "results.json") (λ (p name) (write-json info p)))
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
       (for ([t (in-list (load-tests path))])
         (define t* (struct-copy test t [output #f])) ; strip any `:alt` annotation
         (printf "~a\n" (string-replace (render-fpcore t*) "\n" "")))
       (loop)]
      ; sample <num_points:int> <core:expr>
      [(list 'sample args ...)
       (define-values (num-points core)
         (match args
           [(list num-points core) (values num-points core)]
           [_ (error 'run-server "sample: malformed arguments ~a" args)]))
       (define test (parse-test (datum->syntax #f core)))
       (parameterize ([*reeval-pts* num-points])
         (define result (run-herbie 'sample test #:seed seed #:timeline-disabled? #t))
         (displayln
           (match (job-result-status result)
             ['success
              (define pctx (job-result-backend result))
              (string-join
                 (for/list ([(pt gt) (in-pcontext pctx)])
                   (string-append
                     (string-join (map ~s pt) " ")
                     (format ",~s" gt)))
                 "|")]
             [_ #f])))
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
