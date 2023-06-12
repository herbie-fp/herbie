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
    [_ (format-accuracy (- (table-row-start result) (table-row-result result)) (representation-total-bits (get-representation (table-row-precision result))) #:sign #t #:unit "%")]))

(define (format-subreports rss)
  (define (round* x) (inexact->exact (round x)))
  (match-define (list (cons reports paths) ...) rss)
  `(div ([id "subreport-table"])
    (table
      (tr (th "Subreport") (th "Time") (th "Passed") (th "Tests") (th "Bits"))
      ,@(for/list ([report reports] [path paths])
          (let ([index (path->string (build-path path "index.html"))]
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
  (match-define (report-info date commit branch hostname seed flags points iterations note tests merged-cost-accuracy) info)

  ; (define reprs (map (compose get-representation table-row-precision) trs))

  (define table-labels
    '("Test" "Start" "Result" "Target" "Time"))

  (define help-text
    #hash(("Result" . "Color key:\nGreen: improved accuracy\nLight green: no initial error\nOrange: no accuracy change\nRed: accuracy worsened\nGray: timeout\nDark Gray: error")
        ("Target" . "Color key:\nDark green: better than target\nGreen: matched target\nOrange: improved but did not match target\nYellow: no accuracy change\n")))

  (define total-time (apply + (map table-row-time tests)))
  (define total-tests (length tests))
  (define total-timeouts
    (count (compose (curry equal? "timeout") table-row-status) tests))
  (define total-crashes
    (count (compose (curry equal? "crash") table-row-status) tests))
  (define total-start
    (for/sum ([t tests]) (or (table-row-start t) 0)))
  (define total-result
    (for/sum ([t tests]) (or (table-row-result t) 0)))
  (define maximum-accuracy
    (for/sum ([t (in-list tests)])
      (representation-total-bits
       (get-representation
        (table-row-precision t)))))
  (define speedup-at-initial-accuracy
    ;; The `frontier`'s accuracies are descending, so here we're searching from
    ;; the end backwards to get the cost of the first point with an accuracy
    ;; higher than that of the initial point's
    (cond
     [(null? merged-cost-accuracy) #f]
     [else
      (match-define (list (list _ initial-accuracy) frontier) merged-cost-accuracy)
      (for/first ([point (reverse frontier)]
                  #:when (> (second point) initial-accuracy))
        (first point))]))

  (define (round* x)
    (inexact->exact (round x)))

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
      (header
       (h1 ,(if note (string-titlecase note) "") " Results")
       (img ([src "./demo/logo-car.png"]))
       (nav
        (ul
         ,(if merge-data
            `(li ([id "subreports"])
                 (div ([id "with-subreports"])
                      ,(format-subreports merge-data)))
            "")
         (li (a ([href "timeline.html"]) "Metrics"))))
       (div
        ))

      (div ((id "large"))
       ,(render-comparison
         "Average Percentage Accurate"
         (format-accuracy total-start maximum-accuracy #:unit "%")
         (format-accuracy total-result maximum-accuracy #:unit "%"))
       ,(render-large "Time" (format-time total-time #:max 'minute))
       ,(render-large "Bad Runs" (~a (+ total-crashes total-timeouts)) "/" (~a total-tests)
                      #:title "Crashes and timeouts are considered bad runs.")
       ,(render-large "Speedup" 
                      (if speedup-at-initial-accuracy
                          (~r speedup-at-initial-accuracy #:precision 1)
                          "N/A")
                      "×"
                      #:title "Aggregate speedup of fastest alternative that improves accuracy."))

      (div ([class "figure-row"])
       (figure ([id "xy"])
        (h2 "Output vs Input Accuracy")
        (svg)
        (figcaption "Each point represents a Herbie run below. Its "
                    "horizontal position shows initial accuracy, "
                    "and vertical position shows final accuracy. "
                    "Points above the line are improved by Herbie."))

       (figure ([id "pareto"])
        (h2 "Accuracy vs Speed")
        (svg)
        (figcaption "A joint speed-accuracy pareto curve. Accuracy is "
                    "on the vertical axis, speed is on the horizontal "
                    "axis. Up and to the right is better. The initial "
                    "program is shown by the red square.")))

     (table ((id "results") (class ,(string-join (map ~a classes) " ")))
      (thead
       (tr ,@(for/list ([label table-labels])
               (if (dict-has-key? help-text label)
                   `(th ,label " " (span ([class "help-button"] [title ,(dict-ref help-text label)]) "?"))
                   `(th ,label)))))
      (tbody
       ,@(for/list ([result tests] [id (in-naturals)])
           (define bits (representation-total-bits (get-representation (table-row-precision result))))
           `(tr ((class ,(~a (table-row-status result))))
                (td ,(or (table-row-name result) ""))
                (td ,(format-accuracy (table-row-start result) bits #:unit "%"))
                (td ,(format-accuracy (table-row-result result) bits #:unit "%"))
                (td ,(format-accuracy (table-row-target result) bits #:unit "%"))
                (td ,(format-time (table-row-time result) #:min 'second))
                ,(if (table-row-link result)
                     `(td
                       (a ((id ,(format "link~a" id))
                           (href ,(format "~a/graph.html" (table-row-link result))))
                          "»"))
                     "")))))))
   out))
