#lang racket

(require (only-in xml write-xexpr))
(require "../common.rkt" "../datafile.rkt" "../interface.rkt" "../pareto.rkt"
         "common.rkt" "plot.rkt")

(provide make-report-page)

(define (try-list-accessor acc fail)
  (λ (l) (if (null? l) fail (acc l))))

(define (trs->pareto trs)
  (define cas (map table-row-cost-accuracy trs))
  (define starts (map (try-list-accessor first (list 0 0)) cas))
  (define ptss (map (try-list-accessor (λ (ca) (cons (second ca) (third ca)))
                                       (list (list 0 0)))
                    cas))
  (define reprs (map (compose get-representation table-row-precision) trs))

  (define start
    (for/fold ([x 0] [y 0] #:result (cons x y)) ([s starts])
      (values (+ x (first s)) (+ y (second s)))))
  (define ptss*
    (for/list ([pts ptss])
      (for/list ([pt pts])
        (cons (first pt) (second pt)))))
  (define ymax (apply + (map representation-total-bits reprs)))
  (values start (generate-pareto-curve ptss*) ymax))

(define (badge-label result)
  (match (table-row-status result)
    ["error" "ERR"]
    ["crash" "!!!"]
    ["timeout" "TIME"]
    [_ (format-bits (- (table-row-start result) (table-row-result result)) #:sign #t)]))

(define (make-report-page out info dir)
  (match-define (report-info date commit branch hostname seed flags points iterations note tests) info)

  (define-values (pareto-start pareto-points pareto-max) (trs->pareto tests))
  (cond
   [(not dir) (void)]
   [(> (length pareto-points) 1) ; generate the scatterplot if necessary
    (call-with-output-file (build-path dir "cost-accuracy.png")
      #:exists 'replace
      (λ (out) (make-full-cost-accuracy-plot pareto-max pareto-start pareto-points out)))]
   [else
    (when (file-exists? (build-path dir "cost-accuracy.png"))
      (delete-file (build-path dir "cost-accuracy.png")))])

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
      (script ((src "https://d3js.org/d3.v3.min.js") (charset "utf-8")))
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
                (td ,(format-time (table-row-time result) #:min 1000))
                ,(if (table-row-link result)
                     `(td
                       (a ((id ,(format "link~a" id))
                           (href ,(format "~a/graph.html" (table-row-link result))))
                          "»"))
                     "")))))
     ,(if (> (length pareto-points) 1)
         `(div ([id "scatterplot"] [style "margin-top: 2.5em"])
             (img ([width "800"] [height "300"] [title "cost-accuracy"]
                   [data-name "Cost Accuracy"] [src "cost-accuracy.png"])))
           "")))
   out))
