#lang racket
(require json (only-in xml write-xexpr xexpr?) racket/date)
(require "../common.rkt" "../syntax/read.rkt" "../sandbox.rkt"
         "../datafile.rkt" "common.rkt" "../float.rkt"
         "../interface.rkt" "../timeline.rkt")
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
       ,(render-menu
         '(("Timeline" . "#process-info")
           ("Profile" . "#profile"))
         '(("Report" . "graph.html")))
       (section ((id "process-info"))
         (h1 "Details")
         (p ((class "header"))
            "Time bar (total: " (span ((class "number")) ,(format-time time)) ")")
         ,(render-timeline timeline)
         ,@(for/list ([curr timeline] [n (in-naturals)] [next (cdr timeline)])
             (render-phase curr n next)))
       ,(render-profile)))
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
     ,@(dict-call curr render-phase-filtered 'filtered)
     ,@(dict-call curr render-phase-pruning 'kept 'min-error)
     ,@(dict-call curr render-phase-rules 'rules)
     ,@(dict-call curr render-phase-counts 'inputs 'outputs)
     ,@(dict-call curr render-phase-times #:extra n 'times)
     ,@(dict-call curr render-phase-bstep 'bstep)
     ,@(dict-call curr render-phase-egraph 'egraph)
     ,@(dict-call curr render-phase-outcomes 'outcomes))))

(define (if-cons test x l)
  (if test (cons x l) l))

(define (dict-call d f #:default [default '()] #:extra [extra (void)] . args)
  (if (andmap (curry dict-has-key? d) args)
      (apply f (if-cons (not (void? extra)) extra (map (curry dict-ref d) args)))
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

(define (render-phase-pruning kept min-error)
  (define (altnum kind [col #f])
    (define rec (hash-ref kept kind))
    (match col [#f (first rec)] [0 (- (first rec) (second rec))] [1 (second rec)]))
  (define kept-alts (+ (altnum 'new 1) (altnum 'fresh 1)))
  (define done-alts (+ (altnum 'done 1) (altnum 'picked 1)))
  `((dt "Pruning")
    (dd (p ,(~a (+ kept-alts done-alts)) " alts after pruning (" ,(~a kept-alts) " fresh and " ,(~a done-alts) " done)")
        (table ([class "states"])
         (thead
          (tr (th) (th "Pruned") (th "Kept") (th "Total")))
         (tbody
          ,@(for/list ([type '(new fresh picked done)])
              `(tr (th ,(string-titlecase (~a type)))
                   (td ,(~a (altnum type 0)))
                   (td ,(~a (altnum type 1)))
                   (td ,(~a (altnum type))))))
         (tfoot
          (tr (th "Total")
              (td ,(~a (apply + (map (curryr altnum 0) '(new fresh picked done)))))
              (td ,(~a (apply + (map (curryr altnum 1) '(new fresh picked done)))))
              (td ,(~a (apply + (map altnum '(new fresh picked done))))))))
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

(define (render-phase-filtered filtered)
  (match-define (list to from) filtered)
  (if (> from 0)
      `((dt "Filtered") (dd ,(~a from) " candidates to " ,(~a to) " candidates"
                            " (" ,(~r (* (- 1 (/ to from)) 100) #:precision '(= 1)) "%)"))
      '()))

(define (render-phase-outcomes outcomes)
  `((dt "Results")
    (dd (table ([class "times"])
               ,@(for/list ([(outcome number) (in-sorted-dict outcomes #:key cdr)])
                   (match-define (cons count time) number)
                   (match-define (list prog category prec) outcome)
                   `(tr (td ,(format-time time)) (td ,(~a count) "×")
                        (td ,(~a prog)) (td ,(~a prec)) (td ,(~a category))))))))

(define (make-timeline-json result out precision)
  (define repr (get-representation precision))
  (define timeline (test-result-timeline result))

  (write-json (timeline->json timeline repr) out))

;; This next part handles summarizing several timelines into one details section for the report page.

(define (make-summary-html out info timeline)
  (match-define (report-info date commit branch hostname seed flags points iterations note tests) info)

  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (title "Herbie results")
      (meta ((charset "utf-8")))
      (link ((rel "stylesheet") (type "text/css") (href "report.css")))
      (script ((src "report.js"))))
     (body
       ,(render-menu '(("About" . "#about") ("Timeline" . "#process-info") ("Profile" . "#profile"))
                     '(("Report" . "results.html")))

      (table ((id "about"))
       (tr (th "Date:") (td ,(date->string date)))
       (tr (th "Commit:") (td (abbr ([title ,commit]) ,(with-handlers ([exn:fail:contract? (const commit)]) (substring commit 0 8))) " on " ,branch))
       (tr (th "Hostname:") (td ,hostname " with Racket " ,(version)))
       (tr (th "Seed:") (td ,(~a seed)))
       (tr (th "Parameters:") (td ,(~a (*num-points*)) " points "
                                  "for " ,(~a (*num-iterations*)) " iterations"))
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

      ,(render-timeline-summary timeline)
      ,(render-profile)))
   out))

(define (render-timeline-summary timeline)
  (define total-time (apply + (map (curryr dict-ref 'time) timeline)))

  (define blocks
    (for/list ([phase (in-list timeline)])
      (define time (dict-ref phase 'time))
      (define type (dict-ref phase 'type))
      `(div ([class ,(format "timeline-block timeline-~a" type)])
            (h3 ,(~a type)
                (span ([class "time"]) ,(format-time time)
                      " (" ,(~r (* (/ time total-time) 100) #:precision '(= 1)) "%)"))
            (dl
             ,@(dict-call phase render-summary-algorithm 'method)
             ,@(dict-call phase render-summary-outcomes 'outcomes)
             ,@(dict-call phase render-summary-times #:extra type 'times)
             ,@(dict-call phase render-summary-accuracy 'accuracy 'oracle 'baseline 'name 'link)
             ,@(dict-call phase render-summary-filtered 'filtered)
             ,@(dict-call phase render-summary-rules 'rules)))))

  `(section ([id "process-info"])
            (h1 "Details")
            ,@blocks))

(define (render-summary-algorithm algorithm)
  `((dt "Algorithm")
    (dd (table ([class "times"])
               ,@(for/list ([alg (group-by identity algorithm)])
                   `(tr (td ,(~a (length alg)) "×") (td ,(~a (car alg)))))))))

(define (render-summary-times type times)
  `((dt "Calls")
    (dd (p ,(~a (length times)) " calls:")
        (canvas ([id ,(format "calls-~a" type)]
                 [title "Weighted histogram; height corresponds to percentage of runtime in that bucket."]))
        (script "histogram(\"" ,(format "calls-~a" type) "\", " ,(jsexpr->string (map second times)) ")")
        (dd (table ([class "times"])
                   ,@(for/list ([(expr time) (in-dict (sort times > #:key cadr))]
                                [_ (in-range 5)])
                       `(tr (td ,(format-time (car time))) (td (pre ,(~a expr))))))))))

(define (render-summary-rules rules)
  (define counts-grouped (make-hash))
  (for ([(rule count) (in-dict rules)])
    (dict-update! counts-grouped count (curry cons rule) '()))

  `((dt "Rules")
    (dd (table ([class "times"])
          ,@(for/list ([(count rules) (in-dict (sort (hash->list counts-grouped) > #:key car))])
              `(tr (td ,(~a count) "×")
                   (td ,@(for/list ([rule rules]) `(code ,(~a rule) " ")))))))))

;; TODO unconverted
(define (render-summary-accuracy accuracy oracle baseline name link)
  (define rows
    (sort
     (for/list ([acc accuracy] [ora oracle] [bas baseline] [name name] [link link])
       (list (- acc ora)
             (if (= bas ora)
                 (if (= bas acc) 1 -inf.0)
                 (/ (- bas acc) (- bas ora)))
             link
             name))
     > #:key first))

  (define bits (map first rows))
  (define total-remaining (apply + accuracy))

  `((dt "Accuracy")
    (dd (p "Total " ,(format-bits (apply + bits)) "b" " remaining"
            " (" ,(~r (* (/ (apply + bits) total-remaining) 100) #:precision 1) "%)")
        (p "Threshold costs " ,(format-bits (apply + (filter (curry > 1) bits))) "b"
           " (" ,(~r (* (/ (apply + (filter (curry > 1) bits)) total-remaining) 100) #:precision 1) "%)")
        (table ([class "times"])
               ,@(for/list ([row (in-list rows)] [_ (in-range 5)])
                   (match-define (list left fraction link name) row)
                   `(tr (td ,(format-bits left) "b")
                        (td ,(if (infinite? fraction) "-∞" (~r (* fraction 100) #:precision 1)) "%")
                        (td (a ([href ,(format "~a/graph.html" link)]) ,(or name "")))))))))

(define (render-summary-filtered filtered)
  (match-define (list from to) filtered)
  `((dt "Filtered")
    (dd ,(~a from) " candidates to " ,(~a to) " candidates"
        " (" ,(~r (if (> from 0) (* (- 1 (/ to from)) 100) 0) #:precision '(= 1)) "%)")))

(define (render-summary-outcomes outcomes)
  `((dt "Results")
    (dd (table ([class "times"])
         ,@(for/list ([data (sort outcomes > #:key (curryr dict-ref 'time))])
             `(tr (td ,(format-time (dict-ref data 'time)))
                  (td ,(~a (dict-ref data 'count)) "×")
                  (td ,(~a (dict-ref data 'program)))
                  (td ,(~a (dict-ref data 'precision)))
                  (td ,(~a (dict-ref data 'category)))))))))

(define (render-profile)
  `(section ([id "profile"])
    (h1 "Profiling")
    (p ([class "load-text"]) "Loading profile data...")))
