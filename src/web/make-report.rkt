#lang racket

(require racket/date (only-in xml write-xexpr) json)
(require "../common.rkt" "../formats/datafile.rkt")

(provide make-report-page)

(define (badge-label result)
  (match (table-row-status result)
    ["error" "ERR"]
    ["crash" "!!!"]
    ["timeout" "TIME"]
    [_ (format-bits (- (table-row-start result) (table-row-result result)) #:sign #t)]))

(define (make-report-page file info)
  (match-define (report-info date commit branch hostname seed flags points iterations bit-width note tests) info)

  (define table-labels
    '("Test" "Start" "Result" "Target" "∞ ↔ ℝ" "Time"))

  (define help-text
    #hash(("Result" . "Color key:\nGreen: improved accuracy\nLight green: no initial error\nOrange: no accuracy change\nRed: accuracy worsened")
          ("Target" . "Color key:\nDark green: better than target\nGreen: matched target\nOrange: improved but did not match target\nYellow: no accuracy change\n")))

  (define-values (dir _name _must-be-dir?) (split-path file))

  (copy-file (web-resource "report.js") (build-path dir "report.js") #t)
  (copy-file (web-resource "report.css") (build-path dir "report.css") #t)
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
         (script "window.addEventListener('load', function(){draw_results(d3.select('#graph'))})"))

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
                       "")))))

       ,(render-timeline-summary info dir)))))

  ; Delete old files
  (let* ([expected-dirs (map string->path (filter identity (map table-row-link tests)))]
         [actual-dirs (filter (λ (name) (directory-exists? (build-path dir name))) (directory-list dir))]
         [extra-dirs (filter (λ (name) (not (member name expected-dirs))) actual-dirs)])
    (for ([subdir extra-dirs])
      (with-handlers ([exn:fail:filesystem? (const true)])
        (delete-directory/files (build-path dir subdir))))))

(define (render-timeline-summary info dir)
  (define blocks
    (for/list ([(type phase) (in-dict (summarize-timelines info dir))])
      `(div ([class ,(format "timeline-block timeline-~a" type)])
            (h3 ,(~a type)
                (span ([class "time"])
                      ,(format-time (apply + (map cdr (dict-ref phase 'time))))))
            (dl
             ,@(when-dict phase (method)
                          (render-phase-algorithm info method))
             ,@(when-dict phase (outcomes)
                          (render-phase-outcomes info outcomes))
             ,@(when-dict phase (times)
                          (render-phase-times info type times))
             ,@(when-dict phase (slowest)
                          (render-phase-slowest info slowest))
             ,@(when-dict phase (accuracy oracle baseline)
                          (render-phase-accuracy info accuracy oracle baseline))
             ,@(when-dict phase (rules)
                          (render-phase-rules info rules))))))

  `(section ([id "process-info"])
            (h1 "Details")
            ,@blocks))

(define (render-phase-algorithm info algorithm)
  `((dt "Algorithm")
    (dd (table ([class "times"])
               ,@(for/list ([alg (group-by identity (map cdr algorithm))])
                   `(tr (td ,(~a (length alg)) "×") (td ,(~a (car alg)))))))))

(define (render-phase-times info type times)
  `((dt "Calls")
    (dd (p ,(~a (length (append-map cdr times))) " calls:")
        (canvas ([id ,(format "calls-~a" type)]
                 [title "Weighted histogram; height corresponds to percentage of runtime in that bucket."]))
        (script "histogram(\"" ,(format "calls-~a" type) "\", " ,(jsexpr->string (append-map cdr times)) ")"))))

(define (render-phase-slowest info slowest)
  (define slowest*
    (append-map
     (compose (curry map (λ (x) (cons (dict-ref x 'expr) (dict-ref x 'time)))) cdr)
     slowest))
  (define top-slowest
    (take-up-to (sort slowest* > #:key cdr) 5))
  `((dt "Slowest")
    (dd (table ([class "times"])
               ,@(for/list ([(expr time) (in-dict top-slowest)])
                   `(tr (td ,(format-time time)) (td (pre ,(~a expr)))))))))

(define (render-phase-rules info rules)
  (define counts (make-hash))
  (for ([rc (append-map cdr rules)])
    (hash-update! counts (dict-ref rc 'rule) (curry + (dict-ref rc 'count)) 0))

  `((dt "Rules")
    (dd (table ([class "times"])
          ,@(for/list ([(rule count) (in-dict (sort (hash->list counts) > #:key cdr))])
              `(tr (td ,(~a count) "×") (td (code ,(~a rule)))))))))

(define (render-phase-accuracy info accuracy oracle baseline)
  (define rows
    (for/list ([(res acc) (in-dict accuracy)]
               [(_1 ora) (in-dict oracle)]
               [(_2 bas) (in-dict baseline)])
        (list (- acc ora)
              (if (= bas ora)
                  (if (= bas acc) 1 -inf.0)
                  (/ (- bas acc) (- bas ora)))
              res)))

  (define top-bits-remaining
    (take-up-to (sort rows > #:key first) 5))

  (define total-gained
    (for/sum ([row (report-info-tests info)])
      (or (table-row-result row) 0)))

  `((dt "Accuracy")
    (dd (p "Total " ,(format-bits (apply + (map first rows))) "b" " remaining"
            " (",(~r (* (/ (apply + (map first rows)) total-gained) 100) #:precision 1) "%)")
        (table ([class "times"])
               ,@(for/list ([row (in-list top-bits-remaining)])
                   `(tr (td ,(format-bits (first row)) "b")
                        (td ,(~r (* (second row) 100) #:precision 1) "%")
                        (td (a ([href ,(format "~a/graph.html" (table-row-link (third row)))])
                               ,(or (table-row-name (third row)) "")))))))))

(define (hash->cons key1 key2 val)
  (cons (dict-ref val key1) (dict-ref val key2)))

(define (render-phase-outcomes info outcomes)
  (define entries (append-map cdr outcomes))
  (define (key x) (map (curry hash-ref x) '(program category precision)))

  (define merged
    (for/hash ([rows (group-by key entries)])
      (values (key (first rows))
              (cons (apply + (map (curryr hash-ref 'count) rows))
                    (apply + (map (curryr hash-ref 'time) rows))))))

  `((dt "Results")
    (dd (table ([class "times"])
         ,@(for/list ([(outcome number) (in-sorted-dict merged #:key cdr)])
             (match-define (cons count time) number)
             (match-define (list prog category prec) outcome)
             `(tr (td ,(format-time time)) (td ,(~a count) "×")
                  (td ,(~a prog)) (td ,(~a prec)) (td ,(~a category))))))))

(define (summarize-timelines info dir)
  (define tls
    (filter identity
            (for/list ([res (report-info-tests info)])
              (with-handlers ([(const #t) (const #f)])
                (cons res (call-with-input-file (build-path dir (table-row-link res) "timeline.json") read-json))))))

  (define types (make-hash))
  (for ([(res tl) (in-dict tls)] #:when true [event tl] [next (cdr tl)])
    (define data (dict-ref! types (dict-ref event 'type) make-hash))
    (define time (- (dict-ref next 'time) (dict-ref event 'time)))
    (dict-set! data 'time (cons (cons res time) (dict-ref data 'time '())))
    (for ([(k v) (in-dict event)] #:unless (equal? k 'time))
      (dict-set! data k (cons (cons res v) (dict-ref data k '())))))
  (sort (hash->list types) >
        #:key (λ (x) (apply + (map cdr (dict-ref (cdr x) 'time))))))
