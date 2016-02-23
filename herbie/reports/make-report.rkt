#lang racket

(require racket/date)
(require "../common.rkt")
(require "datafile.rkt")
(provide (all-defined-out))

(provide make-report-page make-compare-page)

(define (format-time ms)
  (cond
   [(< ms 1000) (format "~a ms" (round ms))]
   [(< ms 60000) (format "~a s" (/ (round (/ ms 100.0)) 10))]
   [(< ms 3600000) (format "~a m" (/ (round (/ ms 6000.0)) 10))]
   [else (format "~a hr" (/ (round (/ ms 360000.0)) 10))]))

(define (display-bits r #:sign [sign #f])
  (cond
   [(not r) ""]
   [(and (r . > . 0) sign) (format "+~a" (/ (round (* r 10)) 10))]
   [else (format "~a" (/ (round (* r 10)) 10))]))

(define (log-exceptions file info)
  (define (print-test t)
    (printf "(lambda ~a\n  #:name ~s\n  ~a)\n\n"
            (for/list ([v (table-row-vars t)]
                       [s (table-row-samplers t)])
                      (list v s))
            (table-row-name t)
            (table-row-input t)))
  (match info
	 [(report-info date commit branch seed flags points iterations bit-width note tests)
	  (write-file file
		      (printf "; seed : ~a\n\n" seed)
		      (printf "; flags :\n")
		      (for ([fs (hash->list flags)])
			   (printf ";   ~a = ~a\n"
				   (~a (car fs) #:min-width 10)
				   (cdr fs)))
		      (printf "\n")
		      (for ([t tests])
			   (match (table-row-status t)
				  ["crash"
				   (printf "; crashed\n")
				   (print-test t)]
				  ["timeout"
				   (printf "; timed out\n")
				   (print-test t)]
				  [_ #f])))]))

(define (make-report-page file info)
  (match info
    [(report-info date commit branch seed flags points iterations bit-width note tests)

     (define table-labels
       '("Test" "Start" "Result" "Target" "∞ ↔ ℝ" "Time"))

     (define-values (dir _name _must-be-dir?) (split-path file))

     (copy-file "herbie/reports/report.js" (build-path dir "report.js") #t)
     (copy-file "herbie/reports/report.css" (build-path dir "report.css") #t)
     (copy-file "herbie/reports/graph.css" (build-path dir "graph.css") #t)
     (copy-file "herbie/reports/arrow-chart.js" (build-path dir "arrow-chart.js") #t)

     (define total-time (apply + (map table-row-time tests)))
     (define total-passed
       (for/sum ([row tests])
         (if (member (table-row-status row) '("gt-target" "eq-target" "imp-start")) 1 0)))
     (define total-available
       (for/sum ([row tests])
         (if (not (equal? (table-row-status row) "ex-start")) 1 0)))
     (define total-crashes
       (for/sum ([row tests])
         (if (equal? (table-row-status row) "crash") 1 0)))

     (define total-gained
       (for/sum ([row tests])
         (or (table-row-result row) 0)))
     (define total-start
       (for/sum ([row tests])
         (or (table-row-start row) 0)))

     (define (round* x)
       (inexact->exact (round x)))

     (define any-has-target? (ormap table-row-target tests))
     (define any-has-inf+/-?
       (for*/or ([test tests] [field (list table-row-inf- table-row-inf+)])
         (and (field test) (> (field test) 0))))

     (write-file file
       ; HTML cruft
       (printf "<!doctype html>\n")
       (printf "<head>\n")
       (printf "<title>Herbie test results</title>\n")
       (printf "<meta charset='utf-8' />")
       (printf "<link rel='stylesheet' type='text/css' href='report.css' />")

       ; Scripts: the report script, MathJax, D3, and graph-drawing code
       (printf "<script src='report.js'></script>\n")
       (printf "<script src='http://d3js.org/d3.v3.min.js' charset='utf-8'></script>\n")
       (printf "<script type='text/javascript' src='arrow-chart.js'></script>\n")
       (printf "</head>\n")
       (printf "<body onload='report()'>\n")

       ; Big bold numbers
       (printf "<div id='large'>\n")
       (printf "<div>Time: <span class='number'>~a</span></div>\n"
               (format-time total-time))
       (printf "<div>Passed: <span class='number'>~a/~a</span></div>\n"
               total-passed total-available)
       (when (not (= total-crashes 0))
         (printf "<div>Crashes: <span class='number'>~a</span></div>\n"
                 total-crashes))
       (printf "<div>Tests: <span class='number'>~a</span></div>\n"
               (length tests))
       (printf "<div>Bits: <span class='number'>~a/~a</span></div>\n"
               (round* (- total-start total-gained)) (round* total-start))
       (printf "</div>\n")

       ; The graph
       (printf "<figure><svg id='graph' width='400'></svg>\n")
       (printf "<script>window.addEventListener('load', function(){draw_results(d3.select('#graph'))})</script>\n")
       (printf "</figure>\n")

       ; Test badges
       (printf "<ul id='test-badges'>\n")
       (define sorted-tests
         (sort (map cons tests (range (length tests))) >
               #:key (λ (x) (or (table-row-start (car x)) 0))))
       (for ([(result id) (in-pairs sorted-tests)])
         (printf "<li class='badge ~a' title='~a (~a to ~a)' data-id='~a'>~a</li>\n"
                 (table-row-status result)
                 (html-escape-unsafe (table-row-name result))
                 (display-bits (table-row-start result))
                 (display-bits (table-row-result result))
                 id
                 (match (table-row-status result)
                   ["crash" "ERR"]
                   ["timeout" "TIME"]
                   [_ (display-bits (- (table-row-start result) (table-row-result result)) #:sign #t)])))
       (printf "</ul>\n")
       (printf "<hr style='clear:both;visibility:hidden'>\n")

       ; Run stats
       (printf "<table id='about'>\n")
       (printf "<tr><th>Date:</th><td>~a</td></tr>\n" (date->string (current-date)))
       (printf "<tr><th>Commit:</th><td>~a on ~a</td></tr>\n" commit branch)
       (printf "<tr><th>Points:</th><td>~a</td></tr>\n" (*num-points*))
       (printf "<tr><th>Fuel:</th><td>~a</td></tr>\n" (*num-iterations*))
       (printf "<tr><th>Seed:</th><td>~a</td></tr>\n" seed)
       (printf "<tr><th>Flags:</th><td id='flag-list'>")
       (for ([rec (hash->list (*flags*))])
         (for ([fl (cdr rec)])
           (printf "<kbd>~a:~a</kbd>" (car rec) fl)))
       (printf "</td></tr>")
       (printf "</table>\n")

       (define classes
         (filter identity (list (if any-has-target? #f 'no-target) (if any-has-inf+/-? #f 'no-inf))))

       ; Results table
       (printf "<table id='results' class='~a'>\n" (string-join (map ~a classes) " "))
       (printf "<thead><tr>")
       (for ([label table-labels])
         (printf "<th>~a</th>" label))
       (printf "</tr></thead>\n")
       (printf "</div>\n")

       (printf "<tbody>")
       (for ([result tests] [id (in-naturals)])
         (printf "<tr class='~a'>" (table-row-status result))

         (printf "<td>~a</td>" (html-escape-unsafe (or (table-row-name result) "")))
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
         (printf "<td>~a</td>" (format-time (table-row-time result)))
         (if (table-row-link result)
           (printf "<td><a id='link~a' href='~a/graph.html'>»</a></td>" id (table-row-link result))
           (printf "<td></td>"))
         (printf "</tr>\n"))
       (printf "</tbody>\n")
       (printf "</table>\n")
       (printf "</body>\n")
       (printf "</html>\n"))

     ; Delete old files
     (let* ([expected-dirs (map string->path (filter identity (map table-row-link tests)))]
            [actual-dirs (filter (λ (name) (directory-exists? (build-path dir name))) (directory-list dir))]
            [extra-dirs (filter (λ (name) (not (member name expected-dirs))) actual-dirs)])
       (for ([subdir extra-dirs])
         (delete-directory/files (build-path dir subdir))))]))

(define (make-compare-page out-file info1 info2)
  (match-let ([(report-info date1 commit1 branch1 seed1 flags1 points1 iterations1 bit-width1 note1 tests1)
               info1]
              [(report-info date2 commit2 branch2 seed2 flags2 points2 iterations2 bit-width2 note2 tests2)
               info2])
    (define table-labels
      '("Test" "Start" "Result" "Result" "Target" "∞ ↔ ℝ" "∞ ↔ ℝ" "Time" "Time"))

    (define-values (dir _name _must-be-dir?) (split-path out-file))

    (copy-file "herbie/reports/compare.css" (build-path dir "compare.css") #t)

    (define total-time1 (apply + (map table-row-time tests1)))
    (define total-time2 (apply + (map table-row-time tests2)))
    
    (define (total-passed tests)
      (for/sum ([row tests])
        (if (member (table-row-status row) '("gt-target" "eq-target" "imp-start")) 1 0)))
    (define total-passed1 (total-passed tests1))
    (define total-passed2 (total-passed tests2))
    
    (define (total-available tests)
      (for/sum ([row tests])
        (if (not (equal? (table-row-status row) "ex-start")) 1 0)))
    (define total-available1 (total-passed tests1))
    (define total-available2 (total-passed tests2))
    
    (define (total-crashes tests)
      (for/sum ([row tests])
        (if (equal? (table-row-status row) "crash") 1 0)))
    (define total-crashes1 (total-crashes tests1))
    (define total-crashes2 (total-crashes tests2))

    (define (total-gained tests)
      (for/sum ([row tests])
        (or (table-row-result row) 0)))
    (define total-gained1 (total-gained tests1))
    (define total-gained2 (total-gained tests2))
    
    (define (total-start tests)
      (for/sum ([row tests])
        (or (table-row-start row) 0)))
    (define total-start1 (total-start tests1))
    (define total-start2 (total-start tests2))

    (define sorted-tests1
      (sort (map cons tests1 (range (length tests1))) >
            #:key (λ (x) (or (table-row-start (car x)) 0))))
    (define sorted-tests2
      (sort (map cons tests2 (range (length tests2))) >
            #:key (λ (x) (or (table-row-start (car x)) 0))))

    (define (round* x)
      (inexact->exact (round x)))

    (write-file out-file
                ; HTML cruft
                (printf "<!doctype html>\n")
                (printf "<head>\n")
                (printf "<title>Herbie test results</title>\n")
                (printf "<meta charset='utf-8' />")
                (printf "<link rel='stylesheet' type='text/css' href='compare.css' />")

                ; Scripts: D3.
                (printf "<script src='http://d3js.org/d3.v3.min.js' charset='utf-8'></script>\n")
                (printf "</head>\n")
                (printf "<body>\n")

                ; Big bold numbers
                (printf "<div id='large'>\n")
                (printf "<div>Time: <span class='number'>~a</span> vs <span class='number'>~a</span></div>\n"
                        (format-time total-time1) (format-time total-time2))
                (printf "<div>Passed: <span class='number'>~a/~a</span> vs <span class='number'>~a/~a</span></div>\n"
                        total-passed1 total-available1
                        total-passed2 total-available2)
                (when (not (and (= total-crashes1 0) (= total-crashes2 0)))
                  (printf "<div>Crashes: <span class='number'>~a</span> vs <span class='number'>~a</span></div>\n"
                          total-crashes1 total-crashes2))
                (printf "<div>Tests: <span class='number'>~a</span> vs <span class='number'>~a</span></div>\n"
                        (length tests1) (length tests2))
                (printf "<div>Bits: <span class='number'>~a/~a</span> vs <span class='number'>~a/~a</span></div>\n"
                        (round* (- total-start1 total-gained1)) (round* total-start1)
                        (round* (- total-start2 total-gained2)) (round* total-start2))
                (printf "</div>\n")

                (define (badge-label result)
                  (match (table-row-status result)
                    ["crash" "ERR"]
                    ["timeout" "TIME"]
                    [_ (display-bits (- (table-row-start result)
                                        (table-row-result result))
                                     #:sign #t)]))

                ; Test badges
                (printf "<ul id='test-badges'>\n")
                (for ([name (remove-duplicates (map table-row-name (append tests1 tests2)))])
                  (define result1 (findf (compose (curry equal? name) table-row-name)
                                         tests1))
                  (define result2 (findf (compose (curry equal? name) table-row-name)
                                         tests2))
                  (printf "<li class='badge' title='~a (~a to ~a) vs. (~a to ~a)'>"
                          (html-escape-unsafe (table-row-name result1))
                          (display-bits (table-row-start result1))
                          (display-bits (table-row-result result1))
                          (display-bits (table-row-start result2))
                          (display-bits (table-row-result result2)))
                  (printf "<table><tbody><tr><td class='~a'>~a</td><td class='~a'>~a</td></tr></tbody></table>"
                          (table-row-status result1)
                          (badge-label result1)
                          (table-row-status result2)
                          (badge-label result2))
                  (printf "</li>"))
                (printf "</ul>\n")
                (printf "<hr style='clear:both;visibility:hidden'>\n")

                ; Run stats
                (printf "<table id='about'>\n")
                (printf "<tr><th>Date:</th><td class=hinfo-cell>~a<br>~a</td></tr>\n"
                        (date->string date1) (date->string date2))
                (printf "<tr><th>Commit:</th><td class=hinfo-cell>~a on ~a<br>~a on ~a</td></tr>\n"
                        commit1 branch1 commit2 branch2)
                (printf "<tr><th>Points:</th><td class=hinfo-cell>~a<br>~a</td></tr>\n"
                        points1 points2)
                (printf "<tr><th>Fuel:</th><td class=hinfo-cell>~a<br>~a</td></tr>\n"
                        iterations1 iterations2)
                (printf "<tr><th>Seed:</th><td class=hinfo-cell>~a<br>~a</td></tr>\n"
                        seed1 seed2)
                (printf "<tr><th>Flags:</th><td id='flag-list' class=hinfo-cell>")
                (for ([rec (hash->list flags1)])
                  (for ([fl (cdr rec)])
                    (printf "<kbd>~a:~a</kbd>" (car rec) fl)))
                ;; (printf "<p style=\"text-align:center; margin-top:5px; margin-bottom:5px\">vs.</p>")
                (printf "<br><br>")
                (for ([rec (hash->list flags2)])
                  (for ([fl (cdr rec)])
                    (printf "<kbd>~a:~a</kbd>" (car rec) fl)))
                (printf "</td></tr>")
                (printf "</table>\n")

                ; Results table
                (printf "<table id='results'>\n")
                (printf "<thead><tr>")
                (for ([label table-labels])
                  (printf "<th>~a</th>" label))
                (printf "</tr></thead>\n")
                (printf "</div>\n")

                (printf "<tbody>")
                (for ([name (remove-duplicates (map table-row-name (append tests1 tests2)))]
                      [id (in-naturals)])
                  (define result1 (findf (compose (curry equal? name) table-row-name)
                                         tests1))
                  (define result2 (findf (compose (curry equal? name) table-row-name)
                                         tests2))
                  (printf "<tr>")

                  (printf "<td>~a</td>" (html-escape-unsafe name))

                  ;; Some helper functions for displaying the different boxes for results
                  (define (display-bits-vs-other bits other)
                    (cond [(and (not bits) other)
                           (printf "<td>~a</td>" (display-bits other))]
                          [(and bits (not other))
                           (printf "<td>~a</td>" (display-bits bits))]
                          [(and (not bits) (not other))
                           (printf "<td></td>")]
                          [((abs (- bits other)) . > . 1)
                           (printf "<td>~a/~a</td>"
                                   (display-bits bits)
                                   (display-bits other))]
                          [#t
                           (printf "<td>~a</td>"
                                   (display-bits bits))]))

                  (define (display-bits-vs-est result est-result status)
                    (if (and result est-result
                             (> (abs (- result est-result)) 1))
                        (printf "<td class='bad-est ~a'>[~a ≉] ~a </td>"
                                status (display-bits est-result) (display-bits result))
                        (printf "<td class='~a'>~a</td>"
                                status (display-bits result))))

                  (define (display-num-infs inf- inf+)
                    (printf "<td class='infs'>~a~a</td>"
                            (if (and inf- (> inf- 0)) (format "+~a" inf-) "")
                            (if (and inf+ (> inf+ 0)) (format "-~a" inf+) "")))
                  
                  ;; The starting bits
                  (display-bits-vs-other (table-row-start result1) (table-row-start result2))

                  ;; The first result bits box
                  (display-bits-vs-est (table-row-result result1) (table-row-result-est result1)
                                       (table-row-status result1))
                  ;; The second result bits box
                  (display-bits-vs-est (table-row-result result2) (table-row-result-est result2)
                                       (table-row-status result2))

                  ;; The target bits
                  (display-bits-vs-other (table-row-target result1) (table-row-target result2))

                  ;; The number of points that went to infinity and back
                  (display-num-infs (table-row-inf- result1) (table-row-inf+ result1))
                  (display-num-infs (table-row-inf- result2) (table-row-inf+ result2))

                  ;; The time each run of the test took.
                  (printf "<td>~a</td>" (format-time (table-row-time result1)))
                  (printf "<td>~a</td>" (format-time (table-row-time result2)))
                  
                  (printf "</tr>\n"))
                (printf "</tbody>\n")
                (printf "</table>\n")
                (printf "</body>\n")
                (printf "</html>\n"))

    ; Delete old files
    (let* ([expected-dirs (map string->path (filter identity (map table-row-link tests1)))]
           [actual-dirs (filter (λ (name) (directory-exists? (build-path dir name))) (directory-list dir))]
           [extra-dirs (filter (λ (name) (not (member name expected-dirs))) actual-dirs)])
      (for ([subdir extra-dirs])
        (delete-directory/files (build-path dir subdir))))))

(define (render-json file)
  (define info (read-datafile file))

  (when (not (directory-exists? report-output-path))
    (make-directory report-output-path))

  (make-report-page (build-path report-output-path "report.html") info))

(define (render-json-compare file1 file2)
  (define info1 (read-datafile file1))
  (define info2 (read-datafile file2))

  (when (not (directory-exists? report-output-path))
    (make-directory report-output-path))

  (make-compare-page (build-path report-output-path "compare.html") info1 info2))

(define (render files)
  (if (= 1 (length files))
      (render-json (car files))
      (render-json-compare (car files) (cadr files))))

(module+ main
  (command-line
   #:program "make-report"
   #:args info-files
   (render info-files)))
