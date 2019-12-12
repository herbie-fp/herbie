#lang racket

(require (only-in xml write-xexpr) json)
(require "../common.rkt" "../formats/datafile.rkt" "common.rkt")

(provide make-report-page)

(define (badge-label result)
  (match (table-row-status result)
    ["error" "ERR"]
    ["crash" "!!!"]
    ["timeout" "TIME"]
    [_ (format-bits (- (table-row-start result) (table-row-result result)) #:sign #t)]))

(define (make-report-page out info)
  (match-define (report-info date commit branch hostname seed flags points iterations bit-width note tests) info)

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
    (if (ormap table-row-target tests) '(no-target) '()))

  ;; HTML cruft
  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (title "Herbie results")
      (meta ((charset "utf-8")))
      (link ((rel "stylesheet") (type "text/css") (href "report.css")))
      (script ((src "report.js")))
      (script ((src "http://d3js.org/d3.v3.min.js") (charset "utf-8")))
      (script ((type "text/javascript") (src "arrow-chart.js"))))
 
     (body
      (nav ([id "links"])
       (div
        (a ([href "timeline.html"]) "Metrics"))
       (div
        (a ([href "#about"]) "Flags")
        (a ([href "#results"]) "Results")))

      (div ((id "large"))
       ,(render-large "Time" (format-time total-time))
       ,(render-large "Passed" (~a total-passed) "/" (~a total-available))
       ,(if (> total-crashes 0) (render-large "Crashes" (~a total-crashes)) "")
       ,(render-large "Tests" (~a (length tests)))
       ,(render-large "Bits" (~a (round* (- total-start total-gained))) "/" (~a (round* total-start))))

      (figure (svg ((id "graph") (class "arrow-chart") (width "400"))))

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
                (td ,(format-time (table-row-time result)))
                ,(if (table-row-link result)
                     `(td
                       (a ((id ,(format "link~a" id))
                           (href ,(format "~a/graph.html" (table-row-link result))))
                          "»"))
                     "")))))))
   out))
