#lang racket

(require (only-in xml write-xexpr))
(require "../common.rkt" "../datafile.rkt" "../syntax/types.rkt" "../pareto.rkt"
         "common.rkt" "plot.rkt" "../syntax/read.rkt" "../sandbox.rkt")

(provide make-report-page)

(define (badge-label result)
  (match (table-row-status result)
    ["error" "ERR"]
    ["crash" "!!!"]
    ["timeout" "TIME"]
    [_ (format-bits (- (table-row-start result) (table-row-result result)) (test-output-repr (test-result-test result)) #:sign #t #:unit "%")]))

(define (format-subreports rss)
  (define (round* x) (inexact->exact (round x)))
  (match-define (list (cons reports paths) ...) rss)
  `(div ([id "subreport-table"])
    (table
      (tr (th "Subreport") (th "Time") (th "Passed") (th "Tests") (th "Bits"))
      ,@(for/list ([report reports] [path paths])
          (let ([index (path->string (build-path path "results.html"))]
                [time (apply + (map table-row-time (report-info-tests report)))]
                [passed (for/sum ([row (report-info-tests report)])
                          (if (member (table-row-status row)
                              '("gt-target" "eq-target" "imp-start"))
                              1 0))]
                [available (for/sum ([row (report-info-tests report)])
                            (if (not (equal? (table-row-status row) "ex-start")) 1 0))]
                [nbench (length (report-info-tests report))]
                [gained (for/sum ([row (report-info-tests report)])
                          (or (table-row-result row) 0))]
                [start (for/sum ([row (report-info-tests report)])
                          (or (table-row-start row) 0))])
            `(tr (td (a ([href ,index]) ,(report-info-note report)))
                 (td ,(format-time time))
                 (td ,(~a passed) "/" ,(~a available))
                 (td ,(~a nbench))
                 (td ,(~a (round* (- start gained))) "/" ,(~a (round* start)))))))))

(define (make-report-page out info dir #:merge-data [merge-data #f])
  (match-define (report-info date commit branch hostname seed flags points iterations note tests) info)

  (define table-labels
    '("Test" "Start" "Result" "Target" "Time"))

  (define help-text
    #hash(("Result" . "Color key:\nGreen: improved accuracy\nLight green: no initial error\nOrange: no accuracy change\nRed: accuracy worsened")
          ("Target" . "Color key:\nDark green: better than target\nGreen: matched target\nOrange: improved but did not match target\nYellow: no accuracy change\n")))

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

  (define sorted-tests
    (sort (map cons tests (range (length tests))) >
          #:key (λ (x) (or (table-row-start (car x)) 0))))

  (define classes
    (if (ormap table-row-target tests) '() '(no-target)))

  ;; HTML cruft
  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (title "Herbie results")
      (meta ((charset "utf-8")))
      (link ((rel "stylesheet") (type "text/css") (href "report.css")))
      (script ((src "report.js")))
      (script ([src "https://unpkg.com/mathjs@4.4.2/dist/math.min.js"]))
      (script ([src "https://unpkg.com/d3@6.7.0/dist/d3.min.js"]))
      (script ([src "https://unpkg.com/@observablehq/plot@0.4.3/dist/plot.umd.min.js"])))
 
     (body
      (nav ([id "links"])
       (div ([class "right"])
        (a ([href "timeline.html"]) "Metrics"))
       (div
        ,(if merge-data
            `(div ([id "subreports"])
                (a ([href "#results"]) "Results")
                (div ([id "with-subreports"])
                  ,(format-subreports merge-data)))
            `(div
              (a ([href "#results"]) "Results")
              (div ([id "subreports"] [style "display: none"]))))))

      (div ((id "large"))
       ,(render-large "Time" (format-time total-time))
       ,(render-large "Passed" (~a total-passed) "/" (~a total-available))
       ,(if (> total-crashes 0) (render-large "Crashes" (~a total-crashes)) "")
       ,(render-large "Tests" (~a (length tests)))
       ,(render-large "Bits" (~a (round* (- total-start total-gained))) "/" (~a (round* total-start))))

      (figure
       (div ([id "xy"])
            (h2 "Output vs Input Accuracy")
            (svg)
            (figcaption "Each point represents a Herbie run below. Its "
                        "horizontal position shows initial accuracy, "
                        "and vertical position shows final accuracy. "
                        "Points above the line are improved by Herbie."))
       (div ([id "pareto"])
            (h2 "Accuracy vs Cost")
            (svg)
            (figcaption "A joint cost-accuracy pareto curve for the "
                        "Herbie runs below. Accuracy is on the vertical "
                        "axis, and cost is on the vertical axis. Down "
                        "and to the left is better. The initial programs "
                        "are shown by the red square.")
            ))

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
                (td ,(format-bits (table-row-start result) (test-output-repr (test-result-test result)) #:unit "%"))
                (td ,(format-bits (table-row-result result) (test-output-repr (test-result-test result)) #:unit "%"))
                (td ,(format-bits (table-row-target result) (test-output-repr (test-result-test result)) #:unit "%"))
                (td ,(format-time (table-row-time result) #:min 1000))
                ,(if (table-row-link result)
                     `(td
                       (a ((id ,(format "link~a" id))
                           (href ,(format "~a/graph.html" (table-row-link result))))
                          "»"))
                     "")))))))
   out))
