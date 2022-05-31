#lang racket
(require json (only-in xml write-xexpr xexpr?) racket/date)
(require "../common.rkt" "../datafile.rkt" "common.rkt")
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
         (list
          (and info '("About" . "#about"))
          '("Timeline" . "#process-info")
          '("Profile" . "#profile"))
         (if info 
             `(("Report" . "results.html"))
             `(("Details" . "graph.html"))))
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
                  ,(format-time time) " (" ,(format-percent time total-time) ")"))
        (dl
         ,@(dict-call curr render-phase-algorithm 'method)
         ,@(dict-call curr render-phase-locations 'locations)
         ,@(dict-call curr render-phase-accuracy 'accuracy 'oracle 'baseline 'name 'link)
         ,@(dict-call curr render-phase-pruning 'kept)
         ,@(dict-call curr render-phase-error 'min-error)
         ,@(dict-call curr render-phase-rules 'rules)
         ,@(dict-call curr render-phase-egraph 'egraph)
         ,@(dict-call curr render-phase-egraph-stop 'egraph-stop)
         ,@(dict-call curr render-phase-counts 'count)
         ,@(dict-call curr render-phase-alts 'alts)
         ,@(dict-call curr render-phase-times #:extra n 'times)
         ,@(dict-call curr render-phase-bstep 'bstep)
         ,@(dict-call curr render-phase-sampling 'sampling)
         ,@(dict-call curr (curryr simple-render-phase "Symmetry") 'symmetry)
         ,@(dict-call curr (curryr simple-render-phase "Remove") 'remove-preprocessing)
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
          (thead (tr (td "New") (td "Error") (td "Program")))
          ,@(for/list ([rec (in-list locations)])
              (match-define (list expr err new?) rec)
              `(tr (td ,(if new? "✓" ""))
                  (td ,(format-bits err) "b")
                  (td (pre ,(~a expr)))))))))

(define (render-phase-bstep iters)
  `((dt "Steps")
    (dd (table ([class "times"])
               (tr (th "Iters") (th ([colspan "2"]) "Range") (th "Point"))
               ,@(for/list ([rec (in-list iters)])
                   (match-define (list v1 v2 iters pt) rec)
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
          ,@(for/list ([rec (in-list (reverse iters))])
              (match-define (list iter nodes cost t) rec)
              `(tr (td ,(~a iter)) (td ,(~a nodes)) (td ,(~a cost))))))))

(define (render-phase-egraph-stop data)
  (match-define (list (list reasons counts) ...) data)
  `((dt "Stop Event")
    (dd
      (table ([class "times"])
        ,@(for/list ([reason reasons] [count counts])
          `(tr (td ,(~a count) "×")
                (td ,(~a reason))))))))

(define (format-percent num den)
  (string-append
   (if (zero? den)
       (cond [(positive? num) "+∞"] [(zero? num) "0"] [(negative? num) "-∞"])
       (~r (* (/ num den) 100) #:precision 1))
   "%"))

(define (average . values)
  (/ (apply + values) (length values)))

(define (render-phase-sampling sampling)
  (define total (round (apply + (cdr (car sampling)))))
  `((dt "Search")
    (dd (table ([class "times"])
         (tr (th "True") (th "Other") (th "False") (th "Iter"))
         ,@(for/list ([rec (in-list (sort sampling < #:key first))])
             (match-define (list n wt wo wf) rec)
             `(tr (td ,(format-percent wt total))
                  (td ,(format-percent wo total))
                  (td ,(format-percent wf total))
                  (td ,(~a n))))))))

(define (simple-render-phase info name)
  (if (> (length (first info)) 0)
  `((dt ,name)
    (dd ,@(map (lambda (s) `(p ,(~a s))) (first info))))
  empty))

(define (render-phase-accuracy accuracy oracle baseline name link)
  (define rows
    (sort
     (for/list ([acc accuracy] [ora oracle] [bas baseline] [name name] [link link])
       (list (- acc ora)
             (- bas acc)
             link
             name))
     > #:key first))

  (define bits (map first rows))
  (define total-remaining (apply + accuracy))

  `((dt "Accuracy")
    (dd (p "Total " ,(format-bits (apply + bits)) "b" " remaining"
            " (" ,(format-percent (apply + bits) total-remaining) ")"
        (p "Threshold costs " ,(format-bits (apply + (filter (curry > 1) bits))) "b"
           " (" ,(format-percent (apply + (filter (curry > 1) bits)) total-remaining) ")")
        ,@(if (> (length rows) 1)
              `((table ([class "times"])
                  ,@(for/list ([rec (in-list rows)] [_ (in-range 5)])
                      (match-define (list left gained link name) rec)
                      `(tr (td ,(format-bits left) "b")
                           (td ,(format-percent gained (+ left gained)))
                           (td (a ([href ,(format "~a/graph.html" link)]) ,(or name "")))))))
              '())))))

(define (render-phase-pruning kept-data)
  (match-define (list kept) kept-data)
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
              (td ,(~a (apply + (map altnum '(new fresh picked done)))))))))))

(define (render-phase-error min-error-table)
  (match-define (list min-error) min-error-table)
  `((dt "Error")
    (dd ,(format-bits min-error) "b")))

(define (render-phase-rules rules)
  `((dt "Rules")
    (dd (table ([class "times"])
          ,@(for/list ([rec (in-list (sort rules > #:key second))] [_ (in-range 5)])
              (match-define (list rule count) rec)
              `(tr (td ,(~a count) "×")
                   (td (code ,(~a rule) " "))))))))

(define (render-phase-counts alts)
  (match-define (list (list inputs outputs)) alts)
  `((dt "Counts") (dd ,(~a inputs) " → " ,(~a outputs))))

(define (render-phase-alts alts)
  `((dt "Alt Table")
    (dd (table ([class "times"])
         (thead (tr (td "Status") (td "Error") (td "Program")))
         ,@(for/list ([rec (in-list alts)])
             (match-define (list expr status score) rec)
             `(tr
               ,(match status
                  ["next" `(td (span ([title "Selected for next iteration"]) "▶"))]
                  ["done" `(td (span ([title "Selected in a prior iteration"]) "✓"))]
                  ["fresh" `(td)])
               (td ,(format-bits score) "b")
               (td (pre ,expr))))))))

(define (render-phase-times n times)
  `((dt "Calls")
    (dd (p ,(~a (length times)) " calls:")
        (canvas ([id ,(format "calls-~a" n)]
                 [title "Weighted histogram; height corresponds to percentage of runtime in that bucket."]))
        (script "histogram(\"" ,(format "calls-~a" n) "\", " ,(jsexpr->string (map second times)) ")")
        (table ([class "times"])
               ,@(for/list ([rec (in-list (sort times > #:key second))] [_ (in-range 5)])
                   (match-define (list expr time) rec)
                   `(tr (td ,(format-time time)) (td (pre ,(~a expr)))))))))

(define (render-phase-compiler compiler)
  (match-define (list (list sizes compileds) ...) compiler)
  (define size (apply + sizes))
  (define compiled (apply + compileds))
  `((dt "Compiler")
    (dd (p "Compiled " ,(~a size) " to " ,(~a compiled) " computations "
           "(" ,(format-percent (- size compiled) size) " saved)"))))

(define (render-phase-outcomes outcomes)
  `((dt "Results")
    (dd (table ([class "times"])
         ,@(for/list ([rec (in-list (sort outcomes > #:key fourth))])
             (match-define (list prog precision category time count) rec)
             `(tr (td ,(format-time time)) (td ,(~a count) "×") (td ,(~a prog))
                  (td ,(~a precision)) (td ,(~a category))))))))

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
                        (for/list ([rec (in-list (changed-flags))])
                          (match-define (list delta class flag) rec)
                          `(kbd ,(match delta ['enabled "+o"] ['disabled "-o"])
                                " " ,(~a class) ":" ,(~a flag)))))))))

(define (render-profile)
  `(section ([id "profile"])
    (h1 "Profiling")
    (p ([class "load-text"]) "Loading profile data...")))
