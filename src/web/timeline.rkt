#lang racket
(require json (only-in xml write-xexpr xexpr?))
(require "../common.rkt" "../formats/test.rkt" "../sandbox.rkt")
(provide make-timeline make-timeline-json)

(define timeline-phase? (listof (cons/c symbol? any/c)))
(define timeline? (listof timeline-phase?))

(define (make-timeline result out profile?)
  (match-define (test-result test bits fulltime timeline) result)

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
      (body ([onload "timeline()"])
       (section ((id "process-info"))
         (h1 "Details")
         (p ((class "header"))
            "Time bar (total: " (span ((class "number")) ,(format-time time)) ")"
            (a ((class "attachment") (href "debug.txt")) "Debug log")
            ,(if profile?
                 `(a ((class "attachment") (href "profile.txt")) "Profile")
                 ""))
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
     ,@(dict-call curr #:default '() render-phase-method 'method)
     ,@(dict-call curr #:default '() render-phase-locations 'locations)
     ,@(dict-call curr #:default '() render-phase-accuracy 'accuracy 'oracle 'baseline)
     ,@(dict-call curr #:default '() render-phase-pruning 'kept-alts 'done-alts 'min-error)
     ,@(dict-call curr #:default '() render-phase-rules 'rules)
     ,@(dict-call curr #:default '() render-phase-counts 'input 'output)
     ,@(dict-call curr #:default '() render-phase-times 'times #:extra (list n))
     ,@(dict-call curr #:default '() render-phase-slowest 'slowest)
     ,@(dict-call curr #:default '() render-phase-outcomes 'outcomes))))

(define (dict-call d f #:default [default #f] #:extra [extra '()] . args)
  (if (andmap (curry dict-has-key? d) args)
      (apply f (append (map (curry dict-ref d) args) extra))
      default))

(define (render-phase-method method)
  `((dt "Algorithm") (dd ,(~a method))))

(define (render-phase-locations locations)
  `((dt "Local error")
    (dd (p "Found " ,(~a (length locations)) " expressions with local error:")
        (table ([class "times"])
               ,@(for/list ([(expr err) (in-dict locations)])
                   `(tr (td ,(format-bits err) "b") (td (pre ,(~a expr)))))))))

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
  `((dt "Rules")
    (dd (table ([class "times"])
               ,@(for/list ([(rule count) (in-dict rules)])
                   `(tr (td ,(~a count) "×") (td (code ,(~a rule)))))))))

(define (render-phase-counts inputs outputs)
  `((dt "Counts") (dd ,(~a inputs) " → " ,(~a outputs))))

(define (render-phase-times times n)
  `((dt "Calls")
    (dd ,(~a (length times)) " calls:"
        (canvas ([id ,(format "calls-~a" n)]
                 [title "Weighted histogram; height corresponds to percentage of runtime in that bucket."]))
        (script "histogram(\"" ,(format "calls-~a" n) "\", " ,(jsexpr->string times) ")"))))

(define (render-phase-slowest slowest)
  `((dt "Slowest")
    (dd (table ([class "times"])
               ,@(for/list ([(expr time) (in-dict slowest)])
                   `(tr (td ,(format-time time)) (td (pre ,(~a expr)))))))))

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
  (define (cons->hash k1 f1 k2 f2 c) (hash k1 (f1 (car c)) k2 (f2 (cdr c))))

  (define/match (value-map k v)
    [('method v) (~a v)]
    [('type v) (~a v)]
    [('locations v) (map (curry cons->hash 'expr ~a 'error identity) v)]
    [('slowest v) (map (curry cons->hash 'expr ~a 'time identity) v)]
    [('rules v) (map (curry cons->hash 'rule ~a 'count identity) v)]
    [('outcomes v)
     (for/list ([(outcome number) (in-dict v)])
       (match-define (cons count time) number)
       (match-define (list prog category prec) outcome)
       (hash 'count count 'time time
             'program (~a prog) 'category (~a category) 'precision prec))]
    [(_ v) v])

  (define data
    (for/list ([event timeline])
     (for/hash ([(k v) (in-dict event)])
       (values k (value-map k v)))))

  (write-json data out))
