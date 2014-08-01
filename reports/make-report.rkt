#lang racket

(require racket/date)
(require casio/common)
(require casio/programs)
(require casio/points)
(require casio/alternative)
(require casio/test)
(require casio/load-tests)
(require casio/main)
(require reports/make-graph)
(require reports/thread-pool)
(require reports/cmdline)
(provide (all-defined-out))

(define *graph-folder-name-length* 8)
(define *output-directory* "graphs")

(define *max-test-args* #f)
(define *max-test-threads* (max (- (processor-count) 1) 1))

(define (make-report . bench-dirs)
  (let* ([tests (allowed-tests bench-dirs)]
         [results
          (get-test-results tests (*num-iterations*)
                            #:threads *max-test-threads* #:log-dir "logs")])

    (when (not (directory-exists? *output-directory*))
      (make-directory *output-directory*))

    (let* ([links
            (map make-graph-if-valid
                 results (map test-name tests) (range (length tests)))]
           [table-data (get-table-data results)])
      (make-report-page "graphs/report.html" table-data links)
      (make-data-file "graphs/results.rktdat" results))))

(define (command-result cmd) (string-trim (write-string (system cmd))))

(define (allowed-tests bench-dirs)
  (reverse
   (sort
    (apply append
           (for/list ([bench-dir bench-dirs])
             (filter (λ (test)
                        (or (not *max-test-args*)
                            (<= (length (test-vars test)) *max-test-args*)))
                     (load-tests bench-dir))))
    test<?)))

(define (test<? t1 t2)
  (cond
   ((or (and (test-output t1) (test-output t2))
        (and (not (test-output t1)) (not (test-output t2))))
    (string<? (test-name t1) (test-name t2)))
   (else
    (test-output t1))))

;; Returns #t if the graph was sucessfully made, #f is we had a crash during
;; the graph making process, or the test itself crashed.
(define (make-graph-if-valid result tname index)
  (let* ([rdir (graph-folder-path tname index)]
         [dir (build-path *output-directory* rdir)])
    (with-handlers ([(const #f) (λ _ #f)])
      (cond
       [(test-result? result)
        (when (not (directory-exists? dir))
          (make-directory dir))

        (make-graph (test-result-test result)
                    (test-result-end-alt result)
                    (test-result-newpoints result)
                    (test-result-start-error result)
                    (test-result-end-error result)
                    (test-result-target-error result)
		    (test-result-bits result)
                    dir)

        (build-path rdir "graph.html")]
       [(test-timeout? result)
        #f]
       [(test-failure? result)
        (when (not (directory-exists? dir))
          (make-directory dir))

        (write-file (build-path dir "graph.html")
          (printf "<html>\n")
          (printf "<head><meta charset='utf-8' /><title>Exception for ~a</title></head>\n"
                  (test-name (test-failure-test result)))
          (printf "<body>\n")
          (make-traceback (test-failure-exn result))
          (printf "</body>\n")
          (printf "</html>\n"))

        (build-path rdir "graph.html")]
       [else #f]))))

(define (make-traceback err)
  (printf "<h2 id='error-message'>~a</h2>\n" (first err))
  (printf "<ol id='traceback'>\n")
  (for ([fn&loc (second err)])
    (printf "<li><code>~a</code> in <code>~a</code></li>\n"
            (first fn&loc) (second fn&loc)))
  (printf "</ol>\n"))

(define (graph-folder-path tname index)
  (let* ([stripped-tname (string-replace tname #px"\\(| |\\)|/|'|\"" "")]
         [index-label (number->string index)]
         [name-bound (- *graph-folder-name-length* (string-length index-label))]
         [final-tname
          (substring stripped-tname 0
                     (min (string-length stripped-tname) name-bound))])
    (string-append index-label final-tname "/")))

(struct table-row
  (name status start result target inf- inf+ result-est input output time))

(define (get-table-data results)
  (for/list ([result results])
    (cond
     [(test-result? result)
      (let* ([name (test-name (test-result-test result))]
             [start-errors  (test-result-start-error  result)]
             [end-errors    (test-result-end-error    result)]
             [target-errors (test-result-target-error result)]

             [start-score (errors-score start-errors)]
             [end-score (errors-score end-errors)]
             [target-score (and target-errors (errors-score target-errors))]

             [est-start-score (errors-score (alt-errors (test-result-start-alt result)))]
             [est-end-score (errors-score (alt-errors (test-result-end-alt result)))])

          (let*-values ([(reals infs) (partition ordinary-float? (map - end-errors start-errors))]
                        [(good-inf bad-inf) (partition positive? infs)])
            (table-row name
                       (cond
                        [(not target-score) "no-compare"]
                        [(< end-score (- target-score 1)) "gt-target"]
                        [(< end-score (+ target-score 1)) "eq-target"]
                        [(> end-score (+ start-score 1)) "lt-start"]
                        [(> end-score (- start-score 1)) "eq-start"]
                        [(> end-score (+ target-score 1)) "lt-target"])
                       start-score
                       end-score
                       (and target-score target-score)
                       (length good-inf)
                       (length bad-inf)
                       est-end-score
                       (program-body (alt-program (test-result-start-alt result)))
                       (program-body (alt-program (test-result-end-alt result)))
                       (test-result-time result))))]
     [(test-failure? result)
      (table-row (test-name (test-failure-test result)) "crash"
                 #f #f #f #f #f #f (test-input (test-failure-test result)) #f
                 (test-failure-time result))]
     [(test-timeout? result)
      (table-row (test-name (test-timeout-test result)) "timeout"
                 #f #f #f #f #f #f (test-input (test-timeout-test result)) #f
                 (* 1000 60 5))])))

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
	[(test-result test-obj time bits
		      start-alt end-alt points exacts
		      new-points new-exacts start-error
		      end-error target-error)
	 (match test-obj
	   [(struct test (name vars input output))
	    (write `(,name ,input ,(alt-program end-alt) ,output ,bits))])]
	[(test-failure test-obj exn time)
	 (match test-obj
	   [(struct test (name vars input output))
	    (write `(,name ,input #f ,output))])]
	[(test-timeout test-obj)
	 (match test-obj
	   [(struct test (name vars input output))
	    (write `(,name input #f ,output))])])
      (newline)))
  (void))

(define (make-report-page file table-data links)
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
                 (if (not (equal? (table-row-status row) "no-compare")) 1 0))))
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
      (for ([result table-data] [link links])
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

        (printf "<td><code>~a</code></td>" (or (table-row-input result) ""))
        (printf "<td>~a</td>" (format-time (table-row-time result)))
        (if link
          (printf "<td><a href='~a'>[MORE]</a></td>" (path->string link))
          (printf "<td></td>"))
        (printf "</tr>\n"))
      (printf "</tbody>\n")
      (printf "</table>\n")
      (printf "</body>\n")
      (printf "</html>\n"))))

(define benches
  (command-line
   #:program "make-report"
   #:once-each
   [("-d") "Turn On Debug Messages (Warning: Very Verbose)"
    (*debug* #t)]
   [("-a") ma "How many arguments to allow"
    (set! *max-test-args* (string->number ma))]
   [("-p") th "How many tests to run in parallel to use"
    (set! *max-test-threads* (string->number th))]
   [("-r") rs "The random seed vector to use in point generation"
    (vector->pseudo-random-generator!
     (current-pseudo-random-generator)
     (read (open-input-string rs)))]
   [("-f") tf "Toggle flags, specified in the form category:flag"
    (let ([split-strings (string-split tf ":")])
      (when (not (= 2 (length split-strings)))
	(error "Badly formatted input " tf))
      (toggle-flag! (string->symbol (car split-strings)) (string->symbol (cadr split-strings))))]
   #:args bench-dir
  bench-dir))

(apply make-report benches)
