#lang racket
(require json (only-in xml write-xexpr xexpr?))
(require "../common.rkt" "../formats/test.rkt" "../sandbox.rkt" "../formats/datafile.rkt" "common.rkt" "../float.rkt")
(provide make-timeline make-timeline-json make-summary-html)

(define timeline-phase? (hash/c symbol? any/c))
(define timeline? (listof timeline-phase?))

;; This first part handles timelines for a single Herbie run

(define (make-timeline result out)
  (match-define (test-result test bits fulltime timeline warnings) result)
  (unless (andmap (curryr hash-has-key? 'time) timeline)
    (pretty-print timeline))

  (define time
    (apply + (for/list ([phase timeline] [next (cdr timeline)])
               (- (dict-ref next 'time) (dict-ref phase 'time)))))

  (fprintf out "<!doctype html>\n")
  (write-xexpr
    `(html
      (head
       (meta ([charset "utf-8"]))
       (title "Metrics for " ,(~a (test-name test)))
       (link ([rel "stylesheet"] [type "text/css"] [href "../report.css"]))
       (script ([src "../report.js"])))
      (body
       ,(render-menu '() '(("Report" . "graph.html")))
       (section ((id "process-info"))
         (h1 "Details")
         (p ((class "header"))
            "Time bar (total: " (span ((class "number")) ,(format-time time)) ")")
         ,(render-timeline timeline)
         ,@(for/list ([curr timeline] [n (in-naturals)] [next (cdr timeline)])
             (render-phase curr n next)))))
    out))

(define/contract (render-timeline timeline)
  (-> timeline? xexpr?)
  `(div ((class "timeline"))
        ,@(for/list ([curr timeline] [n (in-naturals)] [next (cdr timeline)])
            `(div
              ([class ,(format "timeline-phase timeline-~a" (dict-ref curr 'type))]
               [data-id ,(format "timeline~a" n)]
               [data-type ,(~a (dict-ref curr 'type))]
               [data-timespan ,(~a (- (dict-ref next 'time) (dict-ref curr 'time)))])
              ))))

(define/contract (render-phase curr n next)
  (-> timeline-phase? integer? timeline-phase? xexpr?)
  `(div ([class ,(format "timeline-block timeline-~a" (dict-ref curr 'type))]
         [id ,(format "timeline~a" n)])
    (h3 ,(~a (dict-ref curr 'type))
        (span ([class "time"])
         ,(format-time (- (dict-ref next 'time) (dict-ref curr 'time)))))
    (dl
     ,@(dict-call curr render-phase-method 'method)
     ,@(dict-call curr render-phase-locations 'locations)
     ,@(dict-call curr render-phase-accuracy 'accuracy 'oracle 'baseline)
     ,@(dict-call curr render-phase-pruning 'kept-alts 'done-alts 'min-error)
     ,@(dict-call curr render-phase-rules 'rules)
     ,@(dict-call curr render-phase-counts 'inputs 'outputs)
     ,@(dict-call curr render-phase-times 'times #:extra (list n))
     ,@(dict-call curr render-phase-bstep 'bstep)
     ,@(dict-call curr render-phase-egraph 'egraph)
     ,@(dict-call curr render-phase-outcomes 'outcomes))))

(define (dict-call d f #:default [default '()] #:extra [extra '()] . args)
  (if (andmap (curry dict-has-key? d) args)
      (apply f (append extra (map (curry dict-ref d) args)))
      default))

(define (render-phase-method method)
  `((dt "Algorithm") (dd ,(~a method))))

(define (render-phase-locations locations)
  `((dt "Local error")
    (dd (p "Found " ,(~a (length locations)) " expressions with local error:")
        (table ([class "times"])
               ,@(for/list ([(expr err) (in-dict locations)])
                   `(tr (td ,(format-bits (car err)) "b") (td (pre ,(~a expr)))))))))

(define (render-phase-bstep iters)
  `((dt "Steps")
    (dd (table ([class "times"])
               (tr (th "Iters") (th ([colspan "2"]) "Range") (th "Point"))
               ,@(for/list ([iter iters])
                   (match-define (list v1 v2 iters pt) iter)
                   `(tr (td ,(~a iters)) 
                        (td (pre ,(~a v1))) (td (pre ,(~a v2)))
                        (td (pre ,(~a pt)))))))))

(define (render-phase-egraph iters)
  (define costs (map third iters))
  (define last-useful-iter
    (last (filter (compose (curry = (apply min costs)) third) iters)))
  `((dt "Iterations")
    (dd (p "Useful iterations: " ,(~a (first last-useful-iter))
           " (" ,(format-time (fourth last-useful-iter)) ")")
        (table ([class "times"])
          (tr (th "Iter") (th "Nodes") (th "Cost"))
          ,@(for/list ([row (reverse iters)])
              (match-define (list iter nodes cost t) row)
              `(tr (td ,(~a iter)) (td ,(~a nodes)) (td ,(~a cost))))))))


(define (render-phase-accuracy accuracy oracle baseline)
  (define percentage
    (if (= baseline oracle)
        (if (= baseline accuracy) "100" "-∞")
        (~r (* (/ (- baseline accuracy) (- baseline oracle)) 100) #:precision 1)))

  `((dt "Accuracy")
    (dd (p ,percentage "% (" ,(format-bits (- accuracy oracle)) "b" " remaining)")
        (p "Error of " ,(format-bits accuracy) "b"
           " against oracle of " ,(format-bits oracle) "b"
           " and baseline of " ,(format-bits baseline) "b"))))

(define (render-phase-pruning kept-alts done-alts min-error)
  `((dt "Pruning")
    (dd (p ,(~a (+ kept-alts done-alts)) " alts after pruning (" ,(~a kept-alts) " fresh and " ,(~a done-alts) " done)")
        (p "Merged error: " ,(format-bits min-error) "b"))))

(define (render-phase-rules rules)
  (define counts (make-hash))
  (for ([(rule count) (in-dict rules)])
    (dict-update! counts count (curry cons rule) '()))

  `((dt "Rules")
    (dd (table ([class "times"])
               ,@(for/list ([(count rules) (in-dict (sort (hash->list counts) > #:key car))])
                   `(tr (td ,(~a count) "×")
                        (td ,@(for/list ([rule rules]) `(code ,(~a rule) " ")))))))))

(define (render-phase-counts inputs outputs)
  `((dt "Counts") (dd ,(~a inputs) " → " ,(~a outputs))))

(define (render-phase-times n times)
  `((dt "Calls")
    (dd ,(~a (length times)) " calls:"
        (canvas ([id ,(format "calls-~a" n)]
                 [title "Weighted histogram; height corresponds to percentage of runtime in that bucket."]))
        (script "histogram(\"" ,(format "calls-~a" n) "\", " ,(jsexpr->string (map second times)) ")")
        (table ([class "times"])
               ,@(for/list ([(expr time) (in-dict times)])
                   `(tr (td ,(format-time (car time))) (td (pre ,(~a expr)))))))))

(define (render-phase-outcomes outcomes)
  `((dt "Results")
    (dd (table ([class "times"])
               ,@(for/list ([(outcome number) (in-sorted-dict outcomes #:key cdr)])
                   (match-define (cons count time) number)
                   (match-define (list prog category prec) outcome)
                   `(tr (td ,(format-time time)) (td ,(~a count) "×")
                        (td ,(~a prog)) (td ,(~a prec)) (td ,(~a category))))))))

(define (make-timeline-json result out)
  (define timeline (test-result-timeline result))
  (define ((cons->hash k1 f1 k2 f2) c) (hash k1 (f1 (car c)) k2 (f2 (cdr c))))

  (define/match (value-map k v)
    [('method v) (~a v)]
    [('type v) (~a v)]
    [('locations v) (map (cons->hash 'expr ~a 'error identity) v)]
    [('rules v) (map (cons->hash 'rule ~a 'count identity) v)]
    [('times v) (map (λ (x) (cons (~a (car x)) (cdr x))) v)]
    [('outcomes v)
     (for/list ([(outcome number) (in-dict v)])
       (match-define (cons count time) number)
       (match-define (list prog category prec) outcome)
       (hash 'count count 'time time
             'program (~a prog) 'category (~a category) 'precision prec))]
    [('bstep v)
     (define (flval-wrapper x) (flval x (infer-representation x)))
     (map (λ (x) (map (curryr apply '())
                      (list flval-wrapper flval-wrapper identity flval-wrapper) x))
          v)]
    [(_ v) v])

  (define data
    (for/list ([event timeline])
      (for/hash ([(k v) (in-dict event)])
        (values k (value-map k v)))))

  (write-json data out))

;; This next part handles summarizing several timelines into one details section for the report page.

(define (make-summary-html out info dir)
  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (title "Herbie results")
      (meta ((charset "utf-8")))
      (link ((rel "stylesheet") (type "text/css") (href "report.css")))
      (script ((src "report.js"))))
     (body
      ,(render-timeline-summary info (summarize-timelines info dir))))
   out))

(define (phase-time phase)
  (apply + (map cdr (dict-ref phase 'time))))

(define (render-timeline-summary info summary)
  (define total-time (apply + (map phase-time (dict-values summary))))

  (define blocks
    (for/list ([(type phase) (in-dict summary)])
      (define time (phase-time phase))
      `(div ([class ,(format "timeline-block timeline-~a" type)])
            (h3 ,(~a type)
                (span ([class "time"]) ,(format-time time)
                      " (" ,(~r (* (/ time total-time) 100) #:precision '(= 1)) "%)"))
            (dl
             ,@(dict-call phase render-summary-algorithm 'method)
             ,@(dict-call phase render-summary-outcomes 'outcomes)
             ,@(dict-call phase #:extra (list type) render-summary-times 'times)
             ,@(dict-call phase #:extra (list info) render-summary-accuracy 'accuracy 'oracle 'baseline)
             ,@(dict-call phase render-summary-rules 'rules)))))

  `(section ([id "process-info"])
            (h1 "Details")
            ,@blocks))

(define (render-summary-algorithm algorithm)
  `((dt "Algorithm")
    (dd (table ([class "times"])
               ,@(for/list ([alg (group-by identity (map cdr algorithm))])
                   `(tr (td ,(~a (length alg)) "×") (td ,(~a (car alg)))))))))

(define (render-summary-times type times)
  (define top-slowest
    (take-up-to (sort (append-map cdr times) > #:key cadr) 5))

  `((dt "Calls")
    (dd (p ,(~a (length (append-map cdr times))) " calls:")
        (canvas ([id ,(format "calls-~a" type)]
                 [title "Weighted histogram; height corresponds to percentage of runtime in that bucket."]))
        (script "histogram(\"" ,(format "calls-~a" type) "\", " ,(jsexpr->string (map second (append-map cdr times))) ")")
        (dd (table ([class "times"])
                   ,@(for/list ([(expr time) (in-dict top-slowest)])
                       `(tr (td ,(format-time (car time))) (td (pre ,(~a expr))))))))))

(define (render-summary-rules rules)
  (define counts (make-hash))
  (for ([rc (append-map cdr rules)])
    (hash-update! counts (dict-ref rc 'rule) (curry + (dict-ref rc 'count)) 0))
  (define counts-grouped (make-hash))
  (for ([(rule count) (in-dict counts)])
    (dict-update! counts-grouped count (curry cons rule) '()))

  `((dt "Rules")
    (dd (table ([class "times"])
          ,@(for/list ([(count rules) (in-dict (sort (hash->list counts-grouped) > #:key car))])
              `(tr (td ,(~a count) "×")
                   (td ,@(for/list ([rule rules]) `(code ,(~a rule) " ")))))))))

(define (render-summary-accuracy info accuracy oracle baseline)
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
            " (" ,(~r (* (/ (apply + (map first rows)) total-gained) 100) #:precision 1) "%)")
        (p "Threshold costs " ,(format-bits (apply + (filter (curry > 1) (map first rows)))) "b"
           " (" ,(~r (* (/ (apply + (filter (curry > 1) (map first rows))) total-gained) 100) #:precision 1) "%)")
        (table ([class "times"])
               ,@(for/list ([row (in-list top-bits-remaining)])
                   `(tr (td ,(format-bits (first row)) "b")
                        (td ,(if (rational? (second row))
                               (~r (* (second row) 100) #:precision 1)
                               "-∞")
                            "%")
                        (td (a ([href ,(format "~a/graph.html" (table-row-link (third row)))])
                               ,(or (table-row-name (third row)) "")))))))))

(define (render-summary-outcomes outcomes)
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
