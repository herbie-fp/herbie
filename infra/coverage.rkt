#lang racket

(require cover
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/path
         racket/runtime-path
         racket/set)

(define-runtime-path here ".")
(define repo-root (simplify-path (build-path here "..")))
(define main-file (simplify-path (build-path repo-root "src" "main.rkt")))
(define src-dir (simplify-path (build-path repo-root "src")))
(define default-seed "1")
(define default-benchmark "bench/hamming")

(define cover-namespace
  (begin
    (dynamic-require 'cover/cover #f)
    (module->namespace 'cover/cover)))

(define (cover-internal name)
  (parameterize ([current-namespace cover-namespace])
    (namespace-variable-value name)))

(define current-live-files (cover-internal 'current-live-files))
(define with-cover-loggersf (cover-internal 'with-cover-loggersf))
(define make-cover-load/use-compiled (cover-internal 'make-cover-load/use-compiled))
(define compile-file (cover-internal 'compile-file))
(define run-file! (cover-internal 'run-file!))
(define get-namespace/internal (cover-internal 'get-namespace))

(struct file-summary (path relevant-lines covered-lines relevant-chars covered-chars) #:transparent)

(define (collect-source-files)
  (sort (find-files (lambda (path)
                      (and (equal? (filename-extension path) #"rkt")
                           (not (regexp-match? #rx"/platforms/" (path->string path)))))
                    src-dir)
        path<?))

(define (collect-rackunit-files files)
  (filter (lambda (path) (regexp-match? #rx"module\\+ test" (file->string path))) files))

(define (path->coverage-key path)
  (path->string (simplify-path path)))

(define (benchmark-label benchmark)
  (define path (simple-form-path benchmark))
  (define name (path->string (file-name-from-path path)))
  (if (equal? (filename-extension path) #"fpcore")
      (path->string (path-replace-extension (file-name-from-path path) #""))
      name))

(define (default-output-dir benchmark output-root)
  (build-path output-root (format "~a-run" (benchmark-label benchmark))))

(define (default-html-dir benchmark output-root)
  (build-path output-root (format "~a-html" (benchmark-label benchmark))))

(define (build-default-herbie-args benchmark output-root seed)
  (list "report" "--seed" seed benchmark (path->string (default-output-dir benchmark output-root))))

(define (with-coverage-runtime env source-files thunk)
  (parameterize ([current-cover-environment env])
    (define cover-load/use-compiled
      (make-cover-load/use-compiled (map path->coverage-key source-files)))
    (parameterize ([current-load/use-compiled cover-load/use-compiled]
                   [current-namespace (get-namespace/internal)]
                   [current-live-files #f])
      (with-cover-loggersf thunk))))

(define (run-rackunit-tests env source-files test-files)
  (with-coverage-runtime env
                         source-files
                         (lambda ()
                           (for ([file (in-list test-files)])
                             (run-file! (path->coverage-key file) 'test #())))))

(define (run-benchmark-coverage source-files run-args)
  (define env (make-cover-environment))
  (with-coverage-runtime env
                         source-files
                         (lambda ()
                           (compile-file (path->coverage-key main-file))
                           (run-file! (path->coverage-key main-file) 'main run-args)))
  (get-test-coverage env))

(define (run-rackunit-coverage source-files test-files)
  (define env (make-cover-environment))
  (printf "Running RackUnit tests...\n")
  (run-rackunit-tests env source-files test-files)
  (get-test-coverage env))

(define (covered-char-status coverage path pos)
  (with-handlers ([exn:fail? (lambda (_) 'irrelevant)])
    (coverage (path->coverage-key path) pos)))

(define (merged-char-status coverages path pos)
  (define statuses
    (for/list ([coverage (in-list coverages)])
      (covered-char-status coverage path pos)))
  (cond
    [(member 'covered statuses) 'covered]
    [(member 'uncovered statuses) 'uncovered]
    [else 'irrelevant]))

(define (summarize-file coverages path)
  (define text (file->string path))
  (define relevant-lines (mutable-set))
  (define covered-lines (mutable-set))
  (define relevant-char-count 0)
  (define covered-char-count 0)
  (let loop ([chars (string->list text)]
             [pos 1]
             [line 1])
    (match chars
      ['()
       (file-summary path
                     (set-count relevant-lines)
                     (set-count covered-lines)
                     relevant-char-count
                     covered-char-count)]
      [(cons ch rest)
       (define status (merged-char-status coverages path pos))
       (unless (eq? status 'irrelevant)
         (set-add! relevant-lines line)
         (set! relevant-char-count (+ relevant-char-count 1))
         (when (eq? status 'covered)
           (set-add! covered-lines line)
           (set! covered-char-count (+ covered-char-count 1))))
       (loop rest
             (+ pos 1)
             (if (char=? ch #\newline)
                 (+ line 1)
                 line))])))

(define (summaries->totals summaries)
  (for/fold ([relevant-lines 0]
             [covered-lines 0]
             [relevant-chars 0]
             [covered-chars 0])
            ([summary (in-list summaries)])
    (values (+ relevant-lines (file-summary-relevant-lines summary))
            (+ covered-lines (file-summary-covered-lines summary))
            (+ relevant-chars (file-summary-relevant-chars summary))
            (+ covered-chars (file-summary-covered-chars summary)))))

(define (percent covered relevant)
  (if (zero? relevant)
      0.0
      (* 100.0 (/ covered relevant))))

(define (display-summary label summaries)
  (define relevant-summaries
    (filter (lambda (summary) (positive? (file-summary-relevant-lines summary))) summaries))
  (define-values (relevant-lines covered-lines relevant-chars covered-chars)
    (summaries->totals relevant-summaries))
  (printf "~a\n" label)
  (printf "  files with relevant coverage: ~a\n" (length relevant-summaries))
  (printf "  line coverage: ~a / ~a (~a%)\n"
          covered-lines
          relevant-lines
          (~r (percent covered-lines relevant-lines) #:precision '(= 2)))
  (printf "  char coverage: ~a / ~a (~a%)\n"
          covered-chars
          relevant-chars
          (~r (percent covered-chars relevant-chars) #:precision '(= 2)))
  (printf "  most uncovered files:\n")
  (for ([summary (in-list (take (sort relevant-summaries
                                      >
                                      #:key (lambda (s)
                                              (- (file-summary-relevant-lines s)
                                                 (file-summary-covered-lines s))))
                                (min 10 (length relevant-summaries))))])
    (printf "    ~a: ~a / ~a lines (~a%)\n"
            (find-relative-path repo-root (file-summary-path summary))
            (file-summary-covered-lines summary)
            (file-summary-relevant-lines summary)
            (~r (percent (file-summary-covered-lines summary) (file-summary-relevant-lines summary))
                #:precision '(= 2)))))

(module+ main
  (define html-output-dir #f)
  (define output-root (build-path repo-root "tmp-coverage"))
  (define run-label #f)
  (define run-rackunit? #f)
  (define seed default-seed)
  (define benchmark default-benchmark)
  (command-line
   #:program "coverage"
   #:once-each [("--benchmark")
                benchmark-path
                "Benchmark file or directory for the default Herbie run"
                (set! benchmark benchmark-path)]
   [("--rackunit") "Run RackUnit test submodules before the benchmark run" (set! run-rackunit? #t)]
   [("--seed") seed-value "Seed for the default Herbie run" (set! seed seed-value)]
   [("--output-root") dir "Directory for coverage HTML and report output" (set! output-root dir)]
   [("--html") dir "Directory for HTML coverage output" (set! html-output-dir dir)]
   [("--label") label "Label to print for this run" (set! run-label label)]
   #:args herbie-args
   (define effective-run-label (or run-label (benchmark-label benchmark)))
   (define effective-html-dir (or html-output-dir (default-html-dir benchmark output-root)))
   (define effective-herbie-args
     (if (null? herbie-args)
         (build-default-herbie-args benchmark output-root seed)
         herbie-args))
   (define source-files (collect-source-files))
   (define test-files (collect-rackunit-files source-files))
   (define run-args (vector->immutable-vector (list->vector effective-herbie-args)))
   (define benchmark-coverage (run-benchmark-coverage source-files run-args))
   (define coverages
     (if run-rackunit?
         (list benchmark-coverage (run-rackunit-coverage source-files test-files))
         (list benchmark-coverage)))
   (define summaries (map (curry summarize-file coverages) source-files))
   (define relevant-files
     (for/list ([summary (in-list summaries)]
                #:when (positive? (file-summary-relevant-chars summary)))
       (path->coverage-key (file-summary-path summary))))
   (when effective-html-dir
     (if run-rackunit?
         (printf "Skipping HTML output for merged benchmark+RackUnit coverage.\n")
         (begin
           (make-directory* effective-html-dir)
           (generate-html-coverage benchmark-coverage relevant-files effective-html-dir))))
   (display-summary effective-run-label summaries)))
