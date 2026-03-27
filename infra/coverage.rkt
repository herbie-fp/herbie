#lang racket

(require cover
         json
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
;; The full core RackUnit suite does not currently complete under `cover`;
;; `bsearch` and `regimes` currently trigger `cover` internal errors.
(define skipped-rackunit-coverage-files '("src/core/bsearch.rkt" "src/core/regimes.rkt"))

;; These core test modules were probed individually and completed cleanly
;; in the merged tutorial run, so include them by default.
(define stable-rackunit-coverage-files
  '("src/core/arrays.rkt" "src/core/batch-reduce.rkt"
                          "src/core/egg-herbie.rkt"
                          "src/core/egglog-herbie-tests.rkt"
                          "src/core/prove-rules.rkt"
                          "src/core/test-rules.rkt"))

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
  (filter (lambda (path)
            (and (regexp-match? #rx"module\\+ test" (file->string path))
                 ;; The syntax/utils tests add useful coverage quickly and
                 ;; reliably. Add the core modules that we have confirmed
                 ;; behave well under merged coverage too.
                 (or (regexp-match? #rx"/src/syntax/" (path->string path))
                     (regexp-match? #rx"/src/utils/" (path->string path))
                     (member (path->string (find-relative-path repo-root path))
                             stable-rackunit-coverage-files))
                 (not (member (path->string (find-relative-path repo-root path))
                              skipped-rackunit-coverage-files))))
          files))

(define (normalize-rackunit-additions rackunit-additions)
  (for/list ([path-string (in-list rackunit-additions)])
    (simple-form-path path-string)))

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

(define (run-benchmark-coverage source-files run-args)
  (define env (make-cover-environment))
  (with-coverage-runtime env
                         source-files
                         (lambda ()
                           (compile-file (path->coverage-key main-file))
                           (run-file! (path->coverage-key main-file) 'main run-args)))
  (get-test-coverage env))

(define (run-rackunit-coverages source-files test-files)
  (printf "Running RackUnit tests...\n")
  (flush-output)
  (for/fold ([coverages '()]
             #:result (reverse coverages))
            ([file (in-list test-files)])
    (printf "  RackUnit: ~a\n" (find-relative-path repo-root file))
    (flush-output)
    (with-handlers ([exn:fail? (lambda (e)
                                 (printf "Skipping RackUnit coverage for ~a\n" file)
                                 (printf "  ~a\n" (exn-message e))
                                 (flush-output)
                                 coverages)])
      (define env (make-cover-environment))
      (with-coverage-runtime env
                             source-files
                             (lambda ()
                               (compile-file (path->coverage-key file))
                               (run-file! (path->coverage-key file) 'test #())))
      (cons (get-test-coverage env) coverages))))

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

(define (segment->jsexpr path segment)
  (match-define (list status start end start-line start-col end-line end-col text) segment)
  (hasheq 'file
          (~a (find-relative-path repo-root path))
          'status
          (~a status)
          'start
          start
          'end
          end
          'start-line
          start-line
          'start-column
          start-col
          'end-line
          end-line
          'end-column
          end-col
          'text
          text))

(define (collect-file-segments coverages path)
  (define text (file->string path))
  (define len (string-length text))
  (let loop ([chars (string->list text)]
             [pos 1]
             [line 1]
             [col 1]
             [active #f]
             [segments '()])
    (define (finish active segments end-pos end-line end-col)
      (match active
        [#f segments]
        [(list status start-pos start-line start-col chars)
         (cons (list status
                     start-pos
                     end-pos
                     start-line
                     start-col
                     end-line
                     end-col
                     (list->string (reverse chars)))
               segments)]))
    (match chars
      ['() (reverse (finish active segments len line col))]
      [(cons ch rest)
       (define status (merged-char-status coverages path pos))
       (define next-line
         (if (char=? ch #\newline)
             (+ line 1)
             line))
       (define next-col
         (if (char=? ch #\newline)
             1
             (+ col 1)))
       (cond
         [(eq? status 'irrelevant)
          (loop rest (+ pos 1) next-line next-col #f (finish active segments pos line col))]
         [active
          (match-define (list active-status start-pos start-line start-col active-chars) active)
          (if (eq? status active-status)
              (loop rest
                    (+ pos 1)
                    next-line
                    next-col
                    (list active-status start-pos start-line start-col (cons ch active-chars))
                    segments)
              (loop rest
                    (+ pos 1)
                    next-line
                    next-col
                    (list status pos line col (list ch))
                    (finish active segments pos line col)))]
         [else
          (loop rest (+ pos 1) next-line next-col (list status pos line col (list ch)) segments)])])))

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

(define (detailed-report-path label output-root run-rackunit?)
  (build-path output-root
              (format "~a-~a-coverage.json" label (if run-rackunit? "merged" "benchmark"))))

(define (write-detailed-report label output-root run-rackunit? summaries coverages)
  (make-directory* output-root)
  (define relevant-summaries
    (filter (lambda (summary) (positive? (file-summary-relevant-lines summary))) summaries))
  (define-values (relevant-lines covered-lines relevant-chars covered-chars)
    (summaries->totals relevant-summaries))
  (define out-path (detailed-report-path label output-root run-rackunit?))
  (call-with-output-file
   out-path
   (lambda (out)
     (write-json (hasheq 'label
                         label
                         'kind
                         (if run-rackunit? "merged" "benchmark")
                         'line-coverage
                         (hasheq 'covered
                                 covered-lines
                                 'relevant
                                 relevant-lines
                                 'percent
                                 (percent covered-lines relevant-lines))
                         'char-coverage
                         (hasheq 'covered
                                 covered-chars
                                 'relevant
                                 relevant-chars
                                 'percent
                                 (percent covered-chars relevant-chars))
                         'files
                         (for/list ([summary (in-list relevant-summaries)])
                           (define path (file-summary-path summary))
                           (hasheq 'file
                                   (~a (find-relative-path repo-root path))
                                   'line-coverage
                                   (hasheq 'covered
                                           (file-summary-covered-lines summary)
                                           'relevant
                                           (file-summary-relevant-lines summary)
                                           'percent
                                           (percent (file-summary-covered-lines summary)
                                                    (file-summary-relevant-lines summary)))
                                   'char-coverage
                                   (hasheq 'covered
                                           (file-summary-covered-chars summary)
                                           'relevant
                                           (file-summary-relevant-chars summary)
                                           'percent
                                           (percent (file-summary-covered-chars summary)
                                                    (file-summary-relevant-chars summary)))
                                   'segments
                                   (map (curry segment->jsexpr path)
                                        (collect-file-segments coverages path)))))
                 out))
   #:exists 'replace)
  out-path)

(module+ main
  (define html-output-dir #f)
  (define output-root (build-path repo-root "tmp-coverage"))
  (define run-label #f)
  (define run-rackunit? #f)
  (define rackunit-additions '())
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
   #:multi [("--rackunit-add")
            path-string
            "Add a RackUnit file to the default stable subset"
            (set! rackunit-additions (cons path-string rackunit-additions))]
   #:args herbie-args
   (define effective-run-label (or run-label (benchmark-label benchmark)))
   (define effective-html-dir (or html-output-dir (default-html-dir benchmark output-root)))
   (define effective-herbie-args
     (if (null? herbie-args)
         (build-default-herbie-args benchmark output-root seed)
         herbie-args))
   (define source-files (collect-source-files))
   (define default-test-files (collect-rackunit-files source-files))
   (define test-files
     (remove-duplicates (append default-test-files
                                (normalize-rackunit-additions rackunit-additions))))
   (define run-args (vector->immutable-vector (list->vector effective-herbie-args)))
   (define benchmark-coverage (run-benchmark-coverage source-files run-args))
   (define coverages
     (if run-rackunit?
         (cons benchmark-coverage (run-rackunit-coverages source-files test-files))
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
   (define report-path
     (write-detailed-report effective-run-label output-root run-rackunit? summaries coverages))
   (printf "Detailed report: ~a\n" report-path)
   (display-summary effective-run-label summaries)))
