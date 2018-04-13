#lang racket

(require racket/date (only-in xml write-xexpr))
(require "../common.rkt" "common.rkt")
(require "../formats/datafile.rkt")

(provide (all-defined-out))

(define (log-exceptions file info)
  (define (print-test t)
    (printf "(FPCore ~a\n  :name ~s\n  ~a)\n\n"
            (table-row-vars t)
            (table-row-name t)
            (table-row-input t)))
  (match info
	 [(report-info date commit branch hostname seed flags points iterations bit-width note tests)
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
				  ["error"
				   (printf "; errored\n")
				   (print-test t)]
				  ["crash"
				   (printf "; crashed\n")
				   (print-test t)]
				  ["timeout"
				   (printf "; timed out\n")
				   (print-test t)]
				  [_ #f])))]))

(define (web-resource name)
  (build-path web-resource-path name))

(define (badge-label result)
  (match (table-row-status result)
    ["error" "ERR"]
    ["crash" "!!!"]
    ["timeout" "TIME"]
    [_ (format-bits (- (table-row-start result) (table-row-result result)) #:sign #t)]))

(define (make-report-page file info)
  (match info
    [(report-info date commit branch hostname seed flags points iterations bit-width note tests)

     (define table-labels
       '("Test" "Start" "Result" "Target" "∞ ↔ ℝ" "Time"))

     (define help-text
       #hash(("Result" . "Color key:\nGreen: improved accuracy\nLight green: no initial error\nOrange: no accuracy change\nRed: accuracy worsened")
             ("Target" . "Color key:\nDark green: better than target\nGreen: matched target\nOrange: improved but did not match target\nYellow: no accuracy change\n")))

     (define-values (dir _name _must-be-dir?) (split-path file))

     (copy-file (web-resource "report.js") (build-path dir "report.js") #t)
     (copy-file (web-resource "report.css") (build-path dir "report.css") #t)
     (copy-file (web-resource "graph.css") (build-path dir "graph.css") #t)
     (copy-file (web-resource "arrow-chart.js") (build-path dir "arrow-chart.js") #t)

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

     (define sorted-tests
       (sort (map cons tests (range (length tests))) >
             #:key (λ (x) (or (table-row-start (car x)) 0))))

     (define classes
       (filter identity
               (list (if any-has-target? #f 'no-target)
                     (if any-has-inf+/-? #f 'no-inf))))

     (write-file file
       ; HTML cruft
       (printf "<!doctype html>\n")
       (write-xexpr
        `(html
          (head
           (title "Herbie results")
           (meta ((charset "utf-8")))
           (link ((rel "stylesheet") (type "text/css") (href "report.css")))
           (script ((src "report.js")))
           (script ((src "http://d3js.org/d3.v3.min.js") (charset "utf-8")))
           (script ((type "text/javascript") (src "arrow-chart.js"))))

          (body ((onload "report()"))
           (div ((id "large"))
            (div "Time: " (span ((class "number")) ,(format-time total-time)))
            (div "Passed: " (span ((class "number")) ,(~a total-passed) "/" ,(~a total-available)))
            ,(if (> total-crashes 0)
                 `(div "Crashes: " (span ((class "number")) ,(~a total-crashes)))
                 "")
            (div "Tests: " (span ((class "number")) ,(~a (length tests))))
            (div "Bits: " (span ((class "number"))
                                ,(~a (round* (- total-start total-gained)))
                                "/"
                                ,(~a (round* total-start)))))

           (figure
            (svg ((id "graph") (width "400")))
            (script "window.addEventListener('load', function(){draw_results(d3.select('#graph'))})")))

          (ul ((id "test-badges"))
           ,@(for/list ([(result id) (in-dict sorted-tests)])
               `(li ((class ,(format "badge ~a" (table-row-status result)))
                     (title ,(format "~a (~a to ~a)"
                                     (table-row-name result)
                                     (format-bits (table-row-start result))
                                     (format-bits (table-row-result result))))
                     (data-id ,(~a id)))
                    ,(badge-label result))))
          (hr ((style "clear:both;visibility:hidden")))

          (table ((id "about"))
           (tr (th "Date:") (td ,(date->string date)))
           (tr (th "Commit:") (td ,commit " on " ,branch))
           (tr (th "Points:") (td ,(~a (*num-points*))))
           (tr (th "Fuel:") (td ,(~a (*num-iterations*))))
           (tr (th "Seed:") (td ,(~a seed)))
           (tr (th "Flags:")
               (td ((id "flag-list"))
                   (div ((id "all-flags"))
                        ,@(for*/list ([(class flags) (*flags*)] [flag flags])
                            `(kbd ,(~a class) ":" ,(~a flag))))
                   (div ((id "changed-flags"))
                        ,@(if (null? (changed-flags))
                              '("default")
                              (for/list ([rec (changed-flags)])
                                (match-define (list delta class flag) rec)
                                `(kbd ,(match delta ['enabled "+o"] ['disabled "-o"])
                                      " " ,(~a class) ":" ,(~a flag))))))))

          (table ((id "results") (class ,(string-join (map ~a classes) " ")))
           (thead
            (tr ,@(for/list ([label table-labels])
                    (if (dict-has-key? help-text label)
                        `(th ,label " " (span ([class "help-button"] [title ,(dict-ref help-text label)]) "?"))
                        `(th ,label)))))
           (tbody
            ,@(for/list ([result tests] [id (in-naturals)])
                `(tr ((class ,(~a (table-row-status result))))
                     (td ,(or (table-row-name result) ""))
                     (td ,(format-bits (table-row-start result)))
                     (td ,(format-bits (table-row-result result)))
                     (td ,(format-bits (table-row-target result)))
                     (td ,(let ([inf- (table-row-inf- result)])
                            (if (and inf- (> inf- 0)) (format "+~a" inf-) ""))
                         ,(let ([inf+ (table-row-inf+ result)])
                            (if (and inf+ (> inf+ 0)) (format "-~a" inf+) "")))
                     (td ,(format-time (table-row-time result)))
                     ,(if (table-row-link result)
                          `(td
                            (a ((id ,(format "link~a" id))
                                (href ,(format "~a/graph.html" (table-row-link result))))
                               "»"))
                          ""))))))))

     ; Delete old files
     (let* ([expected-dirs (map string->path (filter identity (map table-row-link tests)))]
            [actual-dirs (filter (λ (name) (directory-exists? (build-path dir name))) (directory-list dir))]
            [extra-dirs (filter (λ (name) (not (member name expected-dirs))) actual-dirs)])
       (for ([subdir extra-dirs])
         (with-handlers ([exn:fail:filesystem? (const true)])
           (delete-directory/files (build-path dir subdir)))))]))

(define (make-compare-page out-file info1 info2)
  (match-let ([(report-info date1 commit1 branch1 hostname1 seed1 flags1 points1 iterations1 bit-width1 note1 tests1)
               info1]
              [(report-info date2 commit2 branch2 hostname2 seed2 flags2 points2 iterations2 bit-width2 note2 tests2)
               info2])
    (define table-labels
      '("Test" "Start" "Result" "Result" "Target" "∞ ↔ ℝ" "∞ ↔ ℝ" "Time" "Time"))

    (define-values (dir _name _must-be-dir?) (split-path out-file))

    (copy-file (web-resource "compare.css") (build-path dir "compare.css") #t)

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

    (define (double-large-number title val1 val2)
      `(div ,title ": " (span ([class "number"]) ,(~a val1))
            " vs " (span ([class "number"]) ,(~a val2))))


    (write-file out-file
     ; HTML cruft
     (printf "<!doctype html>\n")
     (write-xexpr
      `(html
        (head
         (title "Herbie test results")
         (meta ([charset "utf-8"]))
         (link ([rel "stylesheet"] [type "text/css"] [href "compare.css"]))
         (script ([src "http://d3js.org/d3.v3.min.js"] [charset "utf-8"])))
        (body
         (div ([id "large"])
          ,(double-large-number "Time" (format-time total-time1) (format-time total-time2))
          ,(double-large-number "Passed"
                                (format "~a/~a" total-time1 total-available1)
                                (format "~a/~a" total-time2 total-available2))
          ,(if (and (= total-crashes1 0) (= total-crashes2 0))
               ""
               (double-large-number "Crashes" total-crashes1 total-crashes2))
          ,(double-large-number "Tests" (length tests1) (length tests2))
          ,(double-large-number
            "Bits"
            (format "~a/~a" (round* (- total-start1 total-gained1)) (round* total-start1))
            (format "~a/~a" (round* (- total-start1 total-gained2)) (round* total-start2))))

         (ul ([id "test-badges"])
          ,@(for/list ([name (remove-duplicates (map table-row-name (append tests1 tests2)))])
              (define result1 (findf (compose (curry equal? name) table-row-name) tests1))
              (define result2 (findf (compose (curry equal? name) table-row-name) tests2))
              `(li ([class "badge"]
                    [title ,(format "~a (~a to ~a) vs. (~a to ~a)"
                                    (table-row-name result1)
                                    (format-bits (table-row-start result1))
                                    (format-bits (table-row-result result1))
                                    (format-bits (table-row-start result2))
                                    (format-bits (table-row-result result2)))])
                (table
                 (tbody
                  (tr
                   (td ([class ,(~a (table-row-status result1))]) ,(badge-label result1))
                   (td ([class ,(~a (table-row-status result2))]) ,(badge-label result1))))))))
         (hr ([style "clear:both;visibility:hidden"]))

         (table ([id "about"])
          (tr (th "Date:")
              (td ([class "hinfo-cell"]) ,(date->string date1) (br) ,(date->string date2)))
          (tr (th "Commit:")
              (td ([class "hinfo-cell"]) ,(~a commit1) " on " ,(~a branch1)
                  (br) ,(~a commit2) " on " ,(~a branch2)))
          (tr (th "Points:")
              (td ([class "hinfo-cell"]) ,(~a points1) (br) ,(~a points2)))
          (tr (th "Fuel:")
              (td ([class "hinfo-cell"]) ,(~a iterations1) (br) ,(~a iterations2)))
          (tr (th "Seed:")
              (td ([class "hinfo-cell"]) ,(~a seed1) (br) ,(~a seed2)))
          (tr (th "Flags:")
              (td ([id "flag-list"] [class "hinfo-cell"])
                  ,@(for*/list ([rec (hash->list flags1)] [fl (cdr rec)])
                      `(kbd ,(format "~a:~a" (car rec) fl)))
                  (br)
                  ,@(for*/list ([rec (hash->list flags2)] [fl (cdr rec)])
                      `(kbd ,(format "~a:~a" (car rec) fl))))))

         (table ([id "results"])
          (thead ,@(for/list ([label table-labels]) `(th ,(~a label))))
          (tbody
           ,@(for/list ([name (remove-duplicates (map table-row-name (append tests1 tests2)))]
                        [id (in-naturals)])
               (define result1 (findf (compose (curry equal? name) table-row-name)
                                         tests1))
               (define result2 (findf (compose (curry equal? name) table-row-name)
                                         tests2))
               ;; Some helper functions for displaying the different boxes for results
               (define (format-bits-vs-other bits other)
                 (cond [(and (not bits) other)
                        `(td ,(format-bits other))]
                       [(and bits (not other))
                        `(td ,(format-bits bits))]
                       [(and (not bits) (not other))
                        `(td)]
                       [((abs (- bits other)) . > . 1)
                        `(td ,(format-bits bits) "/" ,(format-bits other))]
                       [#t
                        `(td ,(format-bits bits))]))

               (define (format-bits-vs-est result est-result status)
                 (if (and result est-result
                          (> (abs (- result est-result)) 1))
                     `(td ([class ,(format "bad-est" status)])
                          "[" ,(format-bits est-result) " ≠] " ,(format-bits result))
                     `(td ([class ,(~a status)]) ,(format-bits result))))

               (define (display-num-infs inf- inf+)
                 `(td ([class "infs"])
                      ,(if (and inf- (> inf- 0)) (format "+~a" inf-) "")
                      ,(if (and inf+ (> inf+ 0)) (format "-~a" inf+) "")))

               `(tr
                 (td ,(~a name))
                 ;; The starting bits
                 ,(format-bits-vs-other (table-row-start result1) (table-row-start result2))
                 ;; The first result bits box
                 ,(format-bits-vs-est (table-row-result result1) (table-row-result-est result1)
                                      (table-row-status result1))
                 ;; The second result bits box
                 ,(format-bits-vs-est (table-row-result result2) (table-row-result-est result2)
                                      (table-row-status result2))
                 ;; The target bits
                 ,(format-bits-vs-other (table-row-target result1) (table-row-target result2))
                 ;; The number of points that went to infinity and back

                 ,(display-num-infs (table-row-inf- result1) (table-row-inf+ result1))
                 ,(display-num-infs (table-row-inf- result2) (table-row-inf+ result2))
                 (td ,(format-time (table-row-time result1)))
                 (td ,(format-time (table-row-time result2)))))))))))

    ; Delete old files
    (let* ([expected-dirs (map string->path (filter identity (map table-row-link tests1)))]
           [actual-dirs (filter (λ (name) (directory-exists? (build-path dir name))) (directory-list dir))]
           [extra-dirs (filter (λ (name) (not (member name expected-dirs))) actual-dirs)])
      (for ([subdir extra-dirs])
        (with-handlers ([exn? (const 'ok)])
          (delete-directory/files (build-path dir subdir)))))))

(define (render-json dir file)
  (define info (read-datafile file))

  (when (not (directory-exists? dir))
    (make-directory dir))

  (make-report-page (build-path dir "report.html") info))

(define (render-json-compare dir file1 file2)
  (define info1 (read-datafile file1))
  (define info2 (read-datafile file2))

  (when (not (directory-exists? dir))
    (make-directory dir))

  (make-compare-page (build-path dir "compare.html") info1 info2))

(define (render dir files)
  (if (= 1 (length files))
      (render-json dir (car files))
      (render-json-compare dir (car files) (cadr files))))

(module+ main
  (command-line
   #:program "make-report"
   #:args (dir info-files)
   (render dir info-files)))
