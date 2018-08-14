#lang racket

(require racket/date (only-in xml write-xexpr))
(require "../common.rkt" "../formats/datafile.rkt")

(provide make-report-page)

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
           (tr (th "Hostname:") (td ,hostname))
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

