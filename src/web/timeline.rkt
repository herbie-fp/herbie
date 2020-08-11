#lang racket
(require json (only-in xml write-xexpr xexpr?) racket/date)
(require "../common.rkt" "../syntax/read.rkt" "../sandbox.rkt"
         "../datafile.rkt" "common.rkt")
(provide make-timeline)

(define timeline-phase? (hash/c symbol? any/c))
(define timeline? (listof timeline-phase?))

;; This first part handles timelines for a single Herbie run

(define (make-timeline name timeline out #:info [info #f])
  (fprintf out "<!doctype html>\n")
  (write-xexpr
    `(html
      (head
       (meta ([charset "utf-8"]))
       (title "Metrics for " ,(~a name))
       (link ([rel "stylesheet"] [type "text/css"] [href ,(if info "report.css" "../report.css")]))
       (script ([src ,(if info "report.js" "../report.js")])))
      (body
       ,(render-menu
         (list/true
          (and info '("About" . "#about"))
          '("Timeline" . "#process-info")
          '("Profile" . "#profile"))
         `(("Report" . ,(if info "results.html" "graph.html"))))
       ,(if info (render-about info) "")
       ,(render-timeline timeline)
       ,(render-profile)))
    out))

(define/contract (render-timeline timeline)
  (-> timeline? xexpr?)
  (define time (apply + (map (curryr dict-ref 'time) timeline)))
  `(section ([id "process-info"])
     (h1 "Details")
     (p ((class "header"))
        "Time bar (total: " (span ((class "number")) ,(format-time time)) ")")
     (div ((class "timeline"))
        ,@(for/list ([n (in-naturals)] [curr timeline])
            `(div
              ([class ,(format "timeline-phase timeline-~a" (dict-ref curr 'type))]
               [data-id ,(format "timeline~a" n)]
               [data-type ,(~a (dict-ref curr 'type))]
               [data-timespan ,(~a (dict-ref curr 'time))])
              )))
     ,@(for/list ([phase timeline] [n (in-naturals)])
         (render-phase phase n time))))

(define/contract (render-phase curr n total-time)
  (-> timeline-phase? integer? real? xexpr?)
  (match-define (dict 'time time 'type type) curr)

  `(div ([class ,(format "timeline-block timeline-~a" type)] [id ,(format "timeline~a" n)])
        (h3 ,(~a type)
            (span ([class "time"])
                  ,(format-time time) " (" ,(format-percent (/ time total-time)) ")"))
        (dl
         ,@(dict-call curr render-phase-algorithm 'method)
         ,@(dict-call curr render-phase-locations 'locations)
         ,@(dict-call curr render-phase-accuracy 'accuracy 'oracle 'baseline 'name 'link)
         ,@(dict-call curr render-phase-pruning 'kept 'min-error)
         ,@(dict-call curr render-phase-rules 'rules)
         ,@(dict-call curr render-phase-counts 'inputs 'outputs)
         ,@(dict-call curr render-phase-times #:extra n 'times)
         ,@(dict-call curr render-phase-bstep 'bstep)
         ,@(dict-call curr render-phase-egraph 'egraph)
         ,@(dict-call curr render-phase-sampling 'sampling)
         ,@(dict-call curr render-phase-outcomes 'outcomes)
         ,@(dict-call curr render-phase-compiler 'compiler)
         )))

(define (if-cons test x l)
  (if test (cons x l) l))

(define (dict-call d f #:default [default '()] #:extra [extra (void)] . args)
  (if (andmap (curry dict-has-key? d) args)
      (apply f (if-cons (not (void? extra)) extra (map (curry dict-ref d) args)))
      default))

(define (render-phase-algorithm algorithm)
  `((dt "Algorithm")
    (dd (table ([class "times"])
               ,@(for/list ([alg (group-by identity algorithm)])
                   `(tr (td ,(~a (length alg)) "×") (td ,(~a (car alg)))))))))


(define (render-phase-locations locations)
  `((dt "Local error")
    (dd (p "Found " ,(~a (length locations)) " expressions with local error:")
        (table ([class "times"])
               ,@(for/list ([rec (in-list locations)])
                   (match-define (list expr err) rec)
                   `(tr (td ,(format-bits err) "b") (td (pre ,(~a expr)))))))))

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


(define (format-percent num)
  (string-append
   (if (infinite? num)
       (if (> num 0) "+∞" "-∞")
       (~r (* num 100) #:precision 1))
   "%"))

(define (average . values)
  (/ (apply + values) (length values)))

(define (render-phase-sampling sampling)
  (define total (round (apply + (cdr (car sampling)))))
  `((dt "Search")
    (dd (table ([class "times"])
         (tr (th "True") (th "Other") (th "False") (th "Iter"))
         ,@(for/list ([rec (in-list sampling)])
             (match-define (list n wt wo wf) rec)
             `(tr (td ,(format-percent (/ wt total)))
                  (td ,(format-percent (/ wo total)))
                  (td ,(format-percent (/ wf total)))
                  (td ,(~a n))))))))

(define (render-phase-accuracy accuracy oracle baseline name link)
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
            " (" ,(format-percent (/ (apply + bits) total-remaining)) ")"
        (p "Threshold costs " ,(format-bits (apply + (filter (curry > 1) bits))) "b"
           " (" ,(format-percent (/ (apply + (filter (curry > 1) bits)) total-remaining)) ")")
        ,@(if (> (length rows) 1)
              `((table ([class "times"])
                  ,@(for/list ([row (in-list rows)] [_ (in-range 5)])
                      (match-define (list left fraction link name) row)
                      `(tr (td ,(format-bits left) "b")
                           (td ,(format-percent (* fraction 100)))
                           (td (a ([href ,(format "~a/graph.html" link)]) ,(or name "")))))))
              '())))))
  
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
  (define by-count (make-hash))
  (for ([(rule count) (in-dict rules)])
    (dict-update! by-count count (curry cons rule) '()))

  `((dt "Rules")
    (dd (table ([class "times"])
          ,@(for/list ([(count rules) (in-sorted-dict by-count #:key first)])
              `(tr (td ,(~a count) "×")
                   (td ,@(for/list ([rule rules]) `(code ,(~a rule) " ")))))))))

(define (render-phase-counts inputs outputs)
  `((dt "Counts") (dd ,(~a inputs) " → " ,(~a outputs))))

(define (render-phase-times n times)
  `((dt "Calls")
    (dd (p ,(~a (length times)) " calls:")
        (canvas ([id ,(format "calls-~a" n)]
                 [title "Weighted histogram; height corresponds to percentage of runtime in that bucket."]))
        (script "histogram(\"" ,(format "calls-~a" n) "\", " ,(jsexpr->string (map second times)) ")")
        (table ([class "times"])
               ,@(for/list ([(expr time) (in-sorted-dict times #:key second)] [_ (in-range 5)])
                   `(tr (td ,(format-time (car time))) (td (pre ,(~a expr)))))))))

(define (render-phase-compiler compiler)
  (match-define (list size compiled) compiler)
  `((dt "Compiler")
    (dd (p "Compiled " ,(~a size) " to " ,(~a compiled) " computations "
           "(" ,(format-percent (- 1 (/ compiled size))) " saved)"))))

(define (render-phase-outcomes outcomes)
  `((dt "Results")
    (dd (table ([class "times"])
         ,@(for/list ([data (sort outcomes > #:key (curryr dict-ref 'time))])
             `(tr (td ,(format-time (dict-ref data 'time)))
                  (td ,(~a (dict-ref data 'count)) "×")
                  (td ,(~a (dict-ref data 'program)))
                  (td ,(~a (dict-ref data 'precision)))
                  (td ,(~a (dict-ref data 'category)))))))))

;; This next part handles summarizing several timelines into one details section for the report page.

(define (render-about info)
  (match-define (report-info date commit branch hostname seed flags points iterations note tests) info)

  `(table ((id "about"))
     (tr (th "Date:") (td ,(date->string date)))
     (tr (th "Commit:")
         (td (abbr ([title ,commit])
                   ,(with-handlers ([exn:fail:contract? (const commit)])
                      (substring commit 0 8)))
             " on " ,branch))
     (tr (th "Hostname:") (td ,hostname " with Racket " ,(version)))
     (tr (th "Seed:") (td ,(~a seed)))
     (tr (th "Parameters:")
         (td ,(~a (*num-points*)) " points for " ,(~a (*num-iterations*)) " iterations"))
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
                                " " ,(~a class) ":" ,(~a flag)))))))))

(define (render-profile)
  `(section ([id "profile"])
    (h1 "Profiling")
    (p ([class "load-text"]) "Loading profile data...")))
