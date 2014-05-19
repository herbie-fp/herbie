#lang racket

(require racket/match)
(require racket/date)
(require reports/make-graph)
(require reports/tools-common)
(require casio/load-bench)
(require casio/test)
(require casio/common)
(require casio/points)
(require casio/main)
(require casio/programs)
(require casio/alternative)

(provide (all-defined-out))

(define *graph-folder-name-length* 8)
(define *handle-crashes* #t)
(define *output-directory* "graphs")

(define (make-report bench-dir)
  (let* ([tests (univariate-tests bench-dir)]
         [results (get-test-results tests)])

    (when (not (directory-exists? *output-directory*))
      (make-directory *output-directory*))

    (let* ([links
            (map make-graph-if-valid
                 results (map test-name tests) (range (length tests)))]
           [table-data (get-table-data results)])
      (make-report-page "graphs/report.html" table-data links))))

(define (command-result cmd) (string-trim (write-string (system cmd))))

(define (univariate-tests bench-dir)
  (filter (λ (test) (= 1 (length (test-vars test))))
	  (load-all #:bench-path-string bench-dir)))

(define (get-test-results tests)
  (progress-map get-test-result tests #:map-name "get-test-results" #:item-name-func test-name))

(define (progress-map f l #:map-name [name 'progress-map] #:item-name-func [item-name #f])
  (define total (length l))

  (idx-map
   (λ (elt idx)
      (let-values ([(results cpu-ms real-ms garbage-ms) (time-apply f (list elt))])
        (println name ": " (quotient (* 100 (1+ idx)) total) "%\t"
                 "[" (~a real-ms #:width 8) " milliseconds]\t\t"
                 (if item-name (item-name elt) ""))
        (car results)))
     l))

(struct test-result (test start-alt end-alt points exacts time))

(define (get-test-result test)

  (define (compute-result orig)
    (let-values ([(points exacts) (prepare-points orig)])
      (parameterize ([*points* points] [*exacts* exacts])
	(let* ([start-alt (make-alt orig)]
	       [end-alt (improve-with-points start-alt (*num-iterations*))])
	  (list start-alt end-alt points exacts)))))

  (define (handle-crash . _)
    (println "Crashed!")
    '(#f #f #f #f))

  (let ([start-prog (make-prog test)])

    (define-values (start-end-points-exacts cpu-ms real-ms garbage-ms)
      (time-apply
       (λ (orig)
          (with-handlers ([(const *handle-crashes*) handle-crash])
            (compute-result orig)))
       (list start-prog)))

    (match (car start-end-points-exacts)
      [`(,start ,end ,points ,exacts)
       (test-result test start end points exacts real-ms)])))

;; Returns #t if the graph was sucessfully made, #f is we had a crash during
;; the graph making process, or the test itself crashed.
(define (make-graph-if-valid result tname index)
  (let* ([rdir (graph-folder-path tname index)]
         [dir (build-path *output-directory* rdir)])
    (with-handlers ([(const #f) (λ _ #f)])
      (cond
       [(test-result-end-alt result)
        (when (not (directory-exists? dir))
          (make-directory dir))
        
        (make-graph (test-result-start-alt result)
                    (test-result-end-alt result)
                    (test-result-points result)
                    (test-result-exacts result)
                    (path->string dir)
                    '("reports/graph.css"))

        (build-path rdir "graph.html")]
       [else #f]))))

(define (graph-folder-path tname index)
  (let* ([stripped-tname (string-replace tname #px"\\(| |\\)|/" "")]
         [index-label (number->string index)]
         [name-bound (- *graph-folder-name-length* (string-length index-label))]
         [final-tname (substring stripped-tname 0 (min (string-length stripped-tname) name-bound))])
    (string-append index-label final-tname "/")))

(struct table-row (name status delta inf- inf+ input output time))

(define (get-table-data results)
  (for/list ([result results])
    (define name (test-name (test-result-test result)))

    (cond
     [(test-result-end-alt result)
      (let* ([start-errors (alt-errors (test-result-start-alt result))]
             [end-errors   (alt-errors (test-result-end-alt   result))]
             [good-errors
              (and (test-output (test-result-test result))
                   (errors (list 'λ (test-vars (test-result-test result))
                                 (test-output (test-result-test result)))
                           (test-result-points result)
                           (test-result-exacts result)))]
             [diff (errors-difference start-errors end-errors)]
             [total-score (errors-score diff)]
             [target-score (if good-errors (errors-diff-score end-errors good-errors) #f)])
        (let*-values ([(reals infs) (partition reasonable-error? diff)]
                      [(good-inf bad-inf) (partition positive? infs)])
          (table-row name
                     (cond
                      [(not good-errors) "no-compare"]
                      [(and target-score (> total-score (+ 1 target-score))) "gt-target"]
                      [(and target-score (< (abs (- total-score target-score)) 1)) "eq-target"]
                      [(< total-score -1) "lt-start"]
                      [(< total-score 1) "eq-start"]
                      [(and target-score (< total-score (- target-score 1))) "lt-target"])
                     (/ total-score (length diff))
                     (length good-inf)
                     (length bad-inf)
                     (program-body (alt-program (test-result-start-alt result)))
                     (program-body (alt-program (test-result-end-alt result)))
                     (test-result-time result))))]
     [else
      (table-row name "crash" #f #f #f #f (test-result-time result))])))

(define (format-time ms)
  (cond
   [(< ms 1000) (format "~a ms" ms)]
   [(< ms 60000) (format "~a s" (/ (round (/ ms 100.0)) 10))]
   [(< ms 3600000) (format "~a m" (/ (round (/ ms 6000.0)) 10))]
   [else (format "~a hr" (/ (round (/ ms 360000.0)) 10))]))

(define (make-report-page file table-data links)
  (let ([commit (command-result "git rev-parse HEAD")]
        [branch (command-result "git rev-parse --abbrev-ref HEAD")])

    (define table-labels
      '("Test" "Δ [bits]" "∞ → ℝ" "ℝ → ∞" "Input" "Output" "Time"))

    (define-values (dir _name _must-be-dir?) (split-path file))

    (copy-file-overwriting "reports/report.css"
                           (build-path dir "report.css"))

    (write-file file
      (printf "<!doctype html>\n")
      (printf "<head>\n")
      (printf "<title>Casio test results</title>\n")
      (printf "<meta charset='utf-8' />")
      (printf "<link rel='stylesheet' type='text/css' href='report.css' />")
      (printf "</head>\n")
      (printf "<body>\n")
      (printf "<dl id='about'>\n")
      (printf "<dt>Date:</dt><dl>~a</dl>\n" (date->string (current-date)))
      (printf "<dt>Commit:</dt><dl>~a on ~a</dl>\n" commit branch)
      (printf "</dl>\n")

      (printf "<table id='results'>\n")
      (printf "<thead><tr>")
      (for ([label table-labels])
        (printf "<th>~a</th>" label))
      (printf "</tr></thead>\n")

      (printf "<tbody>")
      (for ([result table-data] [link links])
        (printf "<tr class='~a'>" (table-row-status result))
        (printf "<td>~a</td>" (or (table-row-name result) ""))
        (printf "<td>~a</td>"
                (if (table-row-delta result)
                    (/ (round (* (table-row-delta result) 10)) 10)
                    ""))
        (printf "<td>~a</td>" (or (table-row-inf- result) ""))
        (printf "<td>~a</td>" (or (table-row-inf+ result) ""))
        (printf "<td>~a</td>" (or (table-row-input result) ""))
        (printf "<td>~a</td>" (or (table-row-output result) ""))
        (printf "<td>~a</td>" (format-time (table-row-time result)))
        (when link
          (printf "<td><a href='~a'>more</a></td>" (path->string link)))
        (printf "</tr>\n"))
      (printf "</tbody>\n")
      (printf "</table>\n")
      (printf "</body>\n")
      (printf "</html>\n"))))

;(define (make-test-graph testpath)
;  (let ([result (test-result (car (load-all #:bench-path-string testpath)))]
;	[dir "../reports/graph/"])
;    (text "Making graph...\n")
;    (when (not (directory-exists? dir)) (make-directory dir))
;    (make-graph (first result) (second result) (third result) (fourth result) dir
;	        '("../reports/graph.css"))))

(make-report
 (command-line
  #:program "make-report"
  #:multi [("-d") "Turn On Debug Messages (Warning: Very Verbose)" (*debug* #t)]
  #:args (bench-dir)
  bench-dir))
