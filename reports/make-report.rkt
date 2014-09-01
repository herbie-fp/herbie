#lang racket

(require racket/date)
(require casio/common)
(require casio/programs)
(require casio/points)
(require casio/alternative)
(require casio/test)
(require casio/load-tests)
(require casio/main)
(require casio/texify)
(require reports/thread-pool)
(require reports/cmdline)
(provide (all-defined-out))

(define *graph-folder-name-length* 8)
(define *output-directory* "graphs")

(define *max-test-arity* #f)
(define *max-test-threads* (max (- (processor-count) 1) 1))

(define *profile?* #f)

(define (make-report . bench-dirs)
  (let* ([tests (allowed-tests bench-dirs)]
         [results
          (get-test-results tests (*num-iterations*)
                            #:profile *profile?*
                            #:threads *max-test-threads* #:dir *output-directory*)])

    (when (not (directory-exists? *output-directory*))
      (make-directory *output-directory*))

    (make-report-page "graphs/report.html" results)
    (make-data-file "graphs/results.rktdat" results)))

(define (command-result cmd) (string-trim (write-string (system cmd))))

(define (allowed-tests bench-dirs)
  (reverse
   (sort
    (apply append
           (for/list ([bench-dir bench-dirs])
             (filter (λ (test)
                        (or (not *max-test-arity*)
                            (<= (length (test-vars test)) *max-test-arity*)))
                     (load-tests bench-dir))))
    test<?)))

(define (test<? t1 t2)
  (cond
   [(and (test-output t1) (test-output t2))
    (string<? (test-name t1) (test-name t2))]
   [(and (not (test-output t1)) (not (test-output t2)))
    (string<? (test-name t1) (test-name t2))]
   [else
    ; Put things with an output first
    (test-output t1)]))

(define (format-time ms)
  (cond
   [(< ms 1000) (format "~a ms" (round ms))]
   [(< ms 60000) (format "~a s" (/ (round (/ ms 100.0)) 10))]
   [(< ms 3600000) (format "~a m" (/ (round (/ ms 6000.0)) 10))]
   [else (format "~a hr" (/ (round (/ ms 360000.0)) 10))]))

(define (make-data-file file results)
  (write-file file
    (for/list ([result results])
      (match result
        [(table-row name status start result target inf- inf+ result-est vars input output time bits link)
         (write `(,name (λ ,vars ,input) (λ ,vars ,output) #f ,bits ,time))])
      (newline)))
  (void))

(define (make-report-page file table-data)
  (let ([commit (command-result "git rev-parse HEAD")]
        [branch (command-result "git rev-parse --abbrev-ref HEAD")]
	[seed (~a (pseudo-random-generator->vector
		   (current-pseudo-random-generator)))])

    (define table-labels
      '("Test" "Start [bits]" "Result [bits]" "Target [bits]" "∞ ↔ ℝ" "Input" "Time"))

    (define-values (dir _name _must-be-dir?) (split-path file))

    (copy-file "reports/report.js" (build-path dir "report.js") #t)
    (copy-file "reports/report.css" (build-path dir "report.css") #t)
    (copy-file "reports/graph.css" (build-path dir "graph.css") #t)

    (define total-time (apply + (map table-row-time table-data)))
    (define total-passed
      (apply + (for/list ([row table-data])
                 (if (member (table-row-status row) '("gt-target" "eq-target")) 1 0))))
    (define total-available
      (apply + (for/list ([row table-data])
                 (if (not (equal? (table-row-status row) "ex-start")) 1 0))))
    (define total-crashes
      (apply + (for/list ([row table-data])
                 (if (equal? (table-row-status row) "crash") 1 0))))

    (define (display-bits r)
      (if r (/ (round (* r 10)) 10) ""))

    (write-file file
      (printf "<!doctype html>\n")
      (printf "<head>\n")
      (printf "<title>Casio test results</title>\n")
      (printf "<meta charset='utf-8' />")
      (printf "<link rel='stylesheet' type='text/css' href='report.css' />")

      (printf "<script src='report.js'></script>\n")
      (printf "<script src='~a'></script>" ; MathJax URL for prettifying programs
              "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
      (printf "</head>\n")
      (printf "<body>\n")
      (printf "<dl id='about'>\n")
      (printf "<dt>Date:</dt><dd>~a</dd>\n" (date->string (current-date)))
      (printf "<dt>Commit:</dt><dd>~a on ~a</dd>\n" commit branch)
      (printf "<dt>Seed Data:</dt><dd>~a</dd>\n" seed)
      (printf "<dt>Flags:</dt><dd id='flag-list'>")
      (for ([rec (hash->list (*flags*))])
        (for ([fl (cdr rec)])
          (printf "<kbd>~a:~a</kbd>" (car rec) fl)))
      (printf "</dd>")
      (printf "<dt>Sample points:</dt><dd>~a</dd>\n" (*num-points*))
      (printf "<dt>Iterations:</dt><dd>~a</dd>\n" (*num-iterations*))
      (printf "</dl>\n")

      (printf "<div id='large'>\n")
      (printf "<div>Time: <span class='number'>~a</span></div>\n"
              (format-time total-time))
      (printf "<div>Passed: <span class='number'>~a/~a</span></div>\n"
              total-passed total-available)
      (when (not (= total-crashes 0))
        (printf "<div>Crashes: <span class='number'>~a</span></div>\n"
                total-crashes))
      (printf "<div>Tests: <span class='number'>~a</span></div>\n"
              (length table-data))
      (printf "</div>\n")

      (printf "<table id='results'>\n")
      (printf "<thead><tr>")
      (for ([label table-labels])
        (printf "<th>~a</th>" label))
      (printf "</tr></thead>\n")

      (printf "<tbody>")
      (for ([result table-data])
        (printf "<tr class='~a'>" (table-row-status result))

        (printf "<td>~a</td>" (or (table-row-name result) ""))
        (printf "<td>~a</td>" (display-bits (table-row-start result)))

        (if (and (table-row-result result) (table-row-result-est result)
                 (> (abs (- (table-row-result result) (table-row-result-est result))) 1))
            (printf "<td class='bad-est'>[~a ≉] ~a </td>"
                    (display-bits (table-row-result-est result))
                    (display-bits (table-row-result result)))
            (printf "<td>~a</td>" (display-bits (table-row-result result))))

        (printf "<td>~a</td>" (display-bits (table-row-target result)))
        (printf "<td>~a~a</td>"
                (let ([inf- (table-row-inf- result)])
                  (if (and inf- (> inf- 0)) (format "+~a" inf-) ""))
                (let ([inf+ (table-row-inf+ result)])
                  (if (and inf+ (> inf+ 0)) (format "-~a" inf+) "")))

        (printf "<td>\\(~a\\)</td>" (or (texify-expression (table-row-input result)) ""))
        (printf "<td>~a</td>" (format-time (table-row-time result)))
        (if (table-row-link result)
          (printf "<td><a href='~a/graph.html'>[MORE]</a></td>" (table-row-link result))
          (printf "<td></td>"))
        (printf "</tr>\n"))
      (printf "</tbody>\n")
      (printf "</table>\n")
      (printf "</body>\n")
      (printf "</html>\n"))

    ; Delete old files
    (let* ([expected-dirs (map string->path (filter identity (map table-row-link table-data)))]
           [actual-dirs (filter (λ (name) (directory-exists? (build-path dir name))) (directory-list dir))]
           [extra-dirs (filter (λ (name) (not (member name expected-dirs))) actual-dirs)])
      (for ([subdir extra-dirs])
        (delete-directory/files (build-path dir subdir))))))

(define benches
  (command-line
   #:program "make-report"
   #:once-each
   [("-d") "Turn On Debug Messages (Warning: Very Verbose)"
    (*debug* #t)]
   [("-a") ma "Restrict maximum arity"
    (set! *max-test-arity* (string->number ma))]
   [("-p") "Whether to profile each test"
    (set! *profile?* #t)]
   [("-t") th "How many tests to run in parallel to use"
    (set! *max-test-threads* (string->number th))]
   [("-r") rs "The random seed vector to use in point generation"
    (vector->pseudo-random-generator!
     (current-pseudo-random-generator)
     (read (open-input-string rs)))]
   [("-n") fu "The amount of 'fuel' to use"
    (*num-iterations* (string->number fu))]
   #:multi
   [("-f") tf "Toggle flags, specified in the form category:flag"
    (let ([split-strings (string-split tf ":")])
      (when (not (= 2 (length split-strings)))
	(error "Badly formatted input " tf))
      (toggle-flag! (string->symbol (car split-strings)) (string->symbol (cadr split-strings))))]
   #:args bench-dir
  bench-dir))

(apply make-report benches)
