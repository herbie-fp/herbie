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
         herbie/syntax/rules
         herbie/syntax/syntax
         herbie/syntax/sugar
         herbie/syntax/types
         herbie/web/common
         herbie/web/core2mkl
         herbie/web/core2python3-10
         herbie/web/core2avx
         herbie/web/core2numpy
         herbie/web/thread-pool)

(*warnings-disabled* true)
(load-herbie-builtins)

; Copied from <herbie>/syntax/read.rkt
(define (read-syntax* port name)
  (parameterize ([read-decimal-as-inexact false])
    (read-syntax port name)))

; For Herbie 2.0 compatability
(define (add-literals expr repr)
  (match expr
    [(? number?) (literal expr (representation-name repr))]
    [(? literal?) expr]
    [(? symbol?) expr]
    [(list 'if cond ift iff)
     (list 'if
           (add-literals cond (get-representation 'bool))
           (add-literals ift repr)
           (add-literals iff repr))]
    [(list impl args ...)
     (define itypes (impl-info impl 'itype))
     (cons impl (map add-literals args itypes))])) 

(define (resugar-core vars name precision pre spec output)
  (define repr (get-representation precision))
  (define expr* (add-literals output repr))
  `(FPCore ,vars
     :name ,name
     :precision ,precision
     ,@(if (empty? pre) '() `(:pre ,pre))
     ,@(if (empty? spec) '() `(:herbie-target ,spec))
     ,(prog->fpcore expr* repr)))

;; Converts a Python-ized pcontext into a Racket pcontext
(define (python->pcontext pts&exs)
  (define pts (map first pts&exs))
  (define exs (map second pts&exs))
  (mk-pcontext pts exs))

;; Desugars an expression
(define (desugar-expr expr ctx platform-name)
  (define platform (get-platform platform-name))
  (define impls (list->set (platform-impls platform)))
  (let/ec return
    (let loop ([expr expr])
      (match expr
        [(? literal?) expr]
        [(? symbol?) expr]
        [(list 'if cond ift iff)
         (list 'if (loop cond) (loop ift) (loop iff))]
        [(list (? (curry set-member? impls) impl) args ...)
         (cons impl (map loop args))]
        [(list (? (compose accelerator-exists? impl->operator) impl) args ...)
         (define repr (impl-info impl 'otype))
         (define op (impl->operator impl))
         (define spec (expand-accelerators (prog->spec expr) #:accelerators (list op)))
         (loop (spec->prog spec (struct-copy context ctx [repr repr])))]
        [_
         (eprintf "failed to desugar ~a\n" expr)
         (return #f)]))))

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
           [(numpy) (core->numpy core "foo")]
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
      ; desugar <core:expr> <platform:symbol>
      [(list 'desugar args ...)
       (define-values (core platform)
         (match args
           [(list core platform) (values core platform)]
           [_ (error 'run-server "desugar: malformed arguments ~a" args)]))
       (define test (parse-test (datum->syntax #f core)))
       (define expr* (desugar-expr (test-input test) (test-context test) platform))
       (cond
         [expr*
          (define core
            `(FPCore ,(test-vars test)
                     :name ,(test-name test)
                     ,(prog->fpcore expr* (test-output-repr test))))
          (writeln core)]
         [else
          (writeln #f)])
       (loop)]
      ; error <core> <points>
      [(list 'error args ...)
       (define-values (points cores)
         (match args
           [(list points cores ...) (values points cores)]
           [_ (error 'run-server "error: malformed arguments ~a" args)]))

       (define pctx (python->pcontext points))
       (define tests
         (for/list ([core (in-list cores)])
           (parse-test (datum->syntax #f core))))

       ; TODO: check that the contexts are the same
       (define ctx (test-context (first tests)))
       (define err-lsts (flip-lists (batch-errors (map test-input tests) pctx ctx)))
       (printf "~a\n" (string-join (map (compose ~a errors-score) err-lsts) " "))
       (loop)]
      ; improve <core> <threads:int> <dir>
      [(list 'improve args ...)
       (define-values (cores threads dir)
         (match args
           [(list cores threads dir) (values cores threads dir)]
           [_ (error 'run-server "improve: malformed arguments ~a" args)]))
       (define tests (map (lambda (c) (parse-test (datum->syntax #f c))) cores))
       (define results (get-test-results tests #:threads threads #:seed seed #:profile #f #:dir #f))
       (define info (make-report-info (filter values results) #:seed seed))
       (write-datafile (build-path (symbol->string dir) "herbie.json") info)
       (writeln "" (current-output-port))
       (loop)]
      [(list 'resugar args ...)
        (define-values (vars name precision pre spec output)
          (match args
            [(list vars name precision pre spec output) (values vars name precision pre spec output)]
            [_ (error 'run-server "resugar: malformed arguments ~a" args)]))
        (define core (resugar-core vars name precision pre spec output))
        (writeln core (current-output-port))
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
      ; reimplementation of `load-file`
      (call-with-input-file path
        (λ (port)
          (port-count-lines! port)
          (for ([stx (in-port (curry read-syntax* path) port)]
                [i (in-naturals)])
            (with-handlers ([exn:fail? (const (void))])
              (define t (parse-test stx))
              (define t* (struct-copy test t [output #f])) ; strip any `:alt` annotation
              (printf "~a]~a|" i (string-replace (render-fpcore t*) "\n" ""))))))
       (newline)
       (loop)]
      ; sample <num_points:int> <core:expr>
      [(list 'sample args ...)
       (define-values (num-points core)
         (match args
           [(list num-points core) (values num-points core)]
           [_ (error 'run-server "sample: malformed arguments ~a" args)]))
       (define test (parse-test (datum->syntax #f core)))
       (define result
         (with-handlers ([exn:fail:user:herbie? (lambda _ #f)])
           (parameterize ([*reeval-pts* num-points])
             (define result (run-herbie 'sample test #:seed seed #:timeline-disabled? #t))
             (match (job-result-status result)
               ['success
                (define pctx (job-result-backend result))
                (string-join
                  (for/list ([(pt gt) (in-pcontext pctx)])
                    (string-append
                      (string-join (map ~s pt) " ")
                      (format ",~s" gt)))
                   "|")]
               [_ #f]))))
       (displayln result)
       (loop)]
      ; supported <core:exprs>
      [(list 'supported cores ...)
       (for ([core (in-list cores)])
         (write
           (with-handlers ([exn:fail? (const #f)])
             (parse-test (datum->syntax #f core))
             #t))
         (display " "))
       (newline)
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
