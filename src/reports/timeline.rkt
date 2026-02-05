#lang racket
(require json
         (only-in xml write-xexpr xexpr?)
         racket/date)
(require "../utils/common.rkt"
         "../api/datafile.rkt"
         "common.rkt"
         "../syntax/platform.rkt"
         "../syntax/types.rkt"
         "../utils/float.rkt"
         "../config.rkt"
         "../syntax/batch.rkt")
(provide make-timeline)

(define timeline-phase? (hash/c symbol? any/c))
(define timeline? (listof timeline-phase?))

;; This first part handles timelines for a single Herbie run

(define (make-timeline name timeline #:info [info #f] #:path [path "."])
  `(html (head (meta ([charset "utf-8"]))
               (title "Metrics for " ,(~a name))
               (link ([rel "stylesheet"] [type "text/css"]
                                         [href ,(if info "report.css" "../report.css")]))
               (script ([src ,(if info "report.js" "../report.js")])))
         (body ,(render-menu (~a name)
                             #:path path
                             (if info
                                 `(("Report" . "index.html"))
                                 `(("Details" . "graph.html"))))
               ,(if info
                    (render-about info)
                    "")
               ,(render-timeline timeline)
               ,(render-profile))))

(define/contract (render-timeline timeline)
  (-> timeline? xexpr?)
  (define time (apply + (map (curryr dict-ref 'time) timeline)))
  `(section
    ([id "process-info"])
    (p ((class "header")) "Time bar (total: " (span ((class "number")) ,(format-time time)) ")")
    (div ((class "timeline"))
         ,@(for/list ([n (in-naturals)]
                      [curr timeline])
             `(div ((class ,(format "timeline-phase timeline-~a" (dict-ref curr 'type)))
                    [data-id ,(format "timeline~a" n)]
                    [data-type ,(~a (dict-ref curr 'type))]
                    [data-timespan ,(~a (dict-ref curr 'time))]))))
    ,@(for/list ([phase timeline]
                 [n (in-naturals)])
        (render-phase phase n time))))

(define/contract (render-phase curr n total-time)
  (-> timeline-phase? integer? real? xexpr?)
  (define time (dict-ref curr 'time))
  (define type (dict-ref curr 'type))

  `(div ((class ,(format "timeline-block timeline-~a" type)) [id ,(format "timeline~a" n)])
        (h3 (a ([href ,(format "#timeline~a" n)]) ,(~a type))
            (span ((class "time")) ,(format-time time) " (" ,(format-percent time total-time) ")"))
        (dl ,@(dict-call curr render-phase-memory 'memory 'gc-time)
            ,@(dict-call curr render-phase-algorithm 'method)
            ,@(dict-call curr render-phase-accuracy 'accuracy 'oracle 'baseline 'name 'link 'repr)
            ,@(dict-call curr render-phase-pruning 'kept)
            ,@(dict-call curr render-phase-error 'min-error)
            ,@(dict-call curr render-phase-egraph 'egraph)
            ,@(dict-call curr render-phase-stop 'stop)
            ,@(dict-call curr render-phase-counts 'count)
            ,@(dict-call curr render-phase-alts 'alts)
            ,@(dict-call curr render-phase-inputs 'inputs 'outputs)
            ,@(dict-call curr render-phase-times 'times)
            ,@(dict-call curr render-phase-series 'series)
            ,@(dict-call curr render-phase-bstep 'bstep)
            ,@(dict-call curr render-phase-branches 'branch)
            ,@(dict-call curr render-phase-sampling 'sampling)
            ,@(dict-call curr (curryr simple-render-phase "Symmetry") 'symmetry)
            ,@(dict-call curr render-phase-outcomes 'outcomes)
            ,@(dict-call curr render-phase-compiler 'compiler)
            ,@(dict-call curr render-phase-mixed-sampling 'mixsample)
            ,@(dict-call curr render-phase-bogosity 'bogosity)
            ,@(dict-call curr render-phase-allocations 'allocations))))

(define/reset id-counter 0)

(define (make-id)
  (id-counter (+ 1 (id-counter)))
  (id-counter))

(define (dict-call d f . args)
  (if (andmap (curry dict-has-key? d) args)
      (apply f (map (curry dict-ref d) args))
      '()))

(define (render-phase-algorithm algorithm)
  `((dt "Algorithm")
    (dd (table ((class "times"))
               ,@(for/list ([alg (in-list (sort (group-by identity algorithm) > #:key length))])
                   `(tr (td ,(~r (length alg) #:group-sep " ") "×") (td ,(~a (car alg)))))))))

(define (render-phase-bogosity bogosity)
  (match-define (list domain-info) bogosity)
  (define total (round (apply + (hash-values domain-info))))

  (define tags '(valid unknown infinite unsamplable invalid precondition))

  `((dt "Bogosity") (dd (div ((class "bogosity"))
                             ,@(for/list ([tag tags])
                                 `(div ((class ,(format "bogosity-~a" tag))
                                        [data-id ,(format "bogosity-~a" tag)]
                                        [data-type ,(~a tag)]
                                        [data-timespan ,(~a (hash-ref domain-info tag 0))]
                                        [title
                                         ,(format "~a (~a)"
                                                  tag
                                                  (format-percent (hash-ref domain-info tag 0)
                                                                  total))])))))))

(define (format-value v)
  (cond
    [(real? v) (~a v)]
    [(equal? (hash-ref v 'type) "real") (hash-ref v 'value)]
    [else
     (define repr-name (hash-ref v 'type))
     (define repr (get-representation (read (open-input-string repr-name))))
     (value->string ((representation-ordinal->repr repr) (string->number (hash-ref v 'ordinal)))
                    repr)]))

(define (render-phase-bstep iters)
  `((dt "Steps") (dd (table (tr (th "Time") (th "Left") (th "Right"))
                            ,@(for/list ([rec (in-list iters)])
                                (match-define (list time v1 v2) rec)
                                `(tr (td ,(format-time time))
                                     (td (pre ,(format-value v1)))
                                     (td (pre ,(format-value v2)))))))))

(define (render-phase-egraph iters)
  (define costs (map third iters))
  (define last-useful-iter (last (filter (compose (curry = (apply min costs)) third) iters)))
  `((dt "Iterations") (dd (p "Useful iterations: "
                             ,(~a (first last-useful-iter))
                             " ("
                             ,(format-time (fourth last-useful-iter))
                             ")")
                          (table ((class "times"))
                                 (tr (th "Iter") (th "Nodes") (th "Cost"))
                                 ,@(for/list ([rec (in-list (reverse iters))])
                                     (match-define (list iter nodes cost t) rec)
                                     `(tr (td ,(~a iter)) (td ,(~a nodes)) (td ,(~a cost))))))))

(define (render-phase-stop data)
  (match-define (list (list reasons counts) ...) (sort data > #:key second))
  `((dt "Stop Event") (dd (table ((class "times"))
                                 ,@(for/list ([reason reasons]
                                              [count counts])
                                     `(tr (td ,(~r count #:group-sep " ") "×") (td ,(~a reason))))))))

(define (average . values)
  (/ (apply + values) (length values)))

(define (render-phase-mixed-sampling mixsample)
  (define total-time (apply + (map first mixsample)))
  (define (format-memory-bytes bytes)
    (format "~a MiB" (~r (/ bytes (expt 2 20)) #:precision '(= 1))))
  `((dt "Precisions")
    (dd (details
         (summary "Click to see histograms. Total time spent on operations: "
                  ,(format-time total-time))
         ,@(map first
                (sort (for/list ([rec (in-list (group-by second mixsample))]) ; group by operator
                        ; rec = '('(time op precision) ... '(time op precision))
                        (define n (random 100000))
                        (define op (second (car rec)))
                        (define precisions (map third rec))
                        (define times (map first rec))
                        (define memories (map fourth rec))

                        (define time-per-op (round (apply + times)))
                        (define memory-per-op (apply + memories))

                        (list `(details (summary (code ,op)
                                                 ": "
                                                 ,(format-time time-per-op)
                                                 " ("
                                                 ,(format-percent time-per-op total-time)
                                                 " of total, "
                                                 ,(format-memory-bytes memory-per-op)
                                                 ")")
                                        (canvas ([id ,(format "calls-~a" n)]
                                                 [title
                                                  "Histogram of precisions of the used operation"]))
                                        (script "histogram(\""
                                                ,(format "calls-~a" n)
                                                "\", "
                                                ,(jsexpr->string precisions)
                                                ", "
                                                ,(jsexpr->string times)
                                                ", "
                                                "{\"max\" : "
                                                ,(~a (*max-mpfr-prec*))
                                                "})"))
                              time-per-op))
                      >
                      #:key second))))))

(define (render-phase-sampling sampling)
  (define total (round (apply + (hash-values (cadr (car sampling))))))
  (define fields
    '(("Valid" . valid) ("Unknown" . unknown)
                        ("Precondition" . precondition)
                        ("Infinite" . infinite)
                        ("Domain" . invalid)
                        ("Can't" . unsamplable)))
  `((dt "Search") (dd (table ((class "times"))
                             (tr (th "Probability")
                                 ,@(for/list ([(name sym) (in-dict fields)])
                                     `(th ,name))
                                 (th "Iter"))
                             ,@(for/list ([(n table) (in-dict (sort sampling < #:key first))])
                                 `(tr (td ,(format-percent (hash-ref (car table) 'valid 0)
                                                           (+ (hash-ref (car table) 'valid 0)
                                                              (hash-ref (car table) 'unknown 0))))
                                      ,@(for/list ([(name sym) (in-dict fields)])
                                          `(td ,(format-percent (hash-ref (car table) sym 0) total)))
                                      (td ,(~a n))))))))

(define (simple-render-phase info name)
  (if (positive? (length (first info)))
      `((dt ,name) (dd ,@(map (lambda (s) `(p ,(~a s))) (first info))))
      empty))

(define (render-phase-accuracy accuracy oracle baseline repr-name)
  (define rows
    (sort (for/list ([acc accuracy]
                     [ora oracle]
                     [bas baseline])
            (list (- acc ora) (- bas acc)))
          >
          #:key first))

  (define bits (map first rows))
  (define total-remaining (apply + accuracy))
  (define repr (get-representation (read (open-input-string (car repr-name)))))

  `((dt "Accuracy") (dd (p "Total "
                           ,(format-bits (apply + bits) #:unit #t)
                           " remaining"
                           " ("
                           ,(format-percent (apply + bits) total-remaining)
                           ")"
                           (p "Threshold costs "
                              ,(format-bits (apply + (filter (curry > 1) bits)) repr)
                              "b"
                              " ("
                              ,(format-percent (apply + (filter (curry > 1) bits)) total-remaining)
                              ")")
                           ,@(if (> (length rows) 1)
                                 `((table ((class "times"))
                                          ,@(for/list ([rec (in-list rows)]
                                                       [_ (in-range 5)])
                                              (match-define (list left gained) rec)
                                              `(tr (td ,(format-bits left #:unit #t))
                                                   (td ,(format-percent gained (+ left gained)))))))
                                 '())))))

(define (render-phase-pruning kept-data)
  (match-define (list kept) kept-data)
  (define (altnum kind [col #f])
    (define rec (hash-ref kept kind))
    (match col
      [#f (first rec)]
      [0 (- (first rec) (second rec))]
      [1 (second rec)]))
  (define kept-alts (+ (altnum 'new 1) (altnum 'fresh 1)))
  (define done-alts (+ (altnum 'done 1) (altnum 'picked 1)))
  `((dt "Pruning")
    (dd (p ,(~a (+ kept-alts done-alts))
           " alts after pruning ("
           ,(~a kept-alts)
           " fresh and "
           ,(~a done-alts)
           " done)")
        (table
         ((class "states"))
         (thead (tr (th) (th "Pruned") (th "Kept") (th "Total")))
         (tbody ,@(for/list ([type '(new fresh picked done)])
                    `(tr (th ,(string-titlecase (~a type)))
                         (td ,(~r (altnum type 0) #:group-sep " "))
                         (td ,(~r (altnum type 1) #:group-sep " "))
                         (td ,(~r (altnum type) #:group-sep " ")))))
         (tfoot
          (tr (th "Total")
              (td ,(~r (apply + (map (curryr altnum 0) '(new fresh picked done))) #:group-sep " "))
              (td ,(~r (apply + (map (curryr altnum 1) '(new fresh picked done))) #:group-sep " "))
              (td ,(~r (apply + (map altnum '(new fresh picked done))) #:group-sep " "))))))))

(define (render-phase-memory mem gc-time)
  (match-define (list live alloc) (car mem))
  `((dt "Memory") (dd ,(~r (/ live (expt 2 20)) #:group-sep " " #:precision '(= 1))
                      "MiB live, "
                      ,(~r (/ alloc (expt 2 20)) #:group-sep " " #:precision '(= 1))
                      "MiB allocated; "
                      ,(format-time gc-time)
                      " collecting garbage")))

(define (render-phase-error min-error-table)
  (match-define (list min-error repr-name) (car min-error-table))
  (define repr (get-representation (read (open-input-string repr-name))))
  `((dt "Accuracy") (dd ,(format-accuracy min-error repr #:unit "%") "")))

(define (render-phase-counts alts)
  `((dt "Counts") ,@(for/list ([rec (in-list alts)])
                      (match-define (list inputs outputs) rec)
                      `(dd ,(~r inputs #:group-sep " ") " → " ,(~r outputs #:group-sep " ")))))

(define (render-phase-alts alts)
  `((dt "Alt Table")
    (dd (details (summary "Click to see full alt table")
                 (table ((class "times"))
                        (thead (tr (th "Status") (th "Accuracy") (th "Program")))
                        ,@
                        (for/list ([rec (in-list alts)])
                          (match-define (list batch-jsexpr status score repr-name) rec)
                          (define repr (get-representation (read (open-input-string repr-name))))
                          `(tr ,(match status
                                  ["next" `(td (span ([title "Selected for next iteration"]) "▶"))]
                                  ["done" `(td (span ([title "Selected in a prior iteration"]) "✓"))]
                                  ["fresh" `(td)])
                               (td ,(format-accuracy score repr #:unit "%") "")
                               (td (pre ,(jsexpr->batch-exprs batch-jsexpr))))))))))

(define (render-phase-times times)
  (define hist-id (make-id))
  `((dt "Calls")
    (dd (p ,(~r (length times) #:group-sep " ") " calls:")
        (canvas ([id ,(format "calls-~a" hist-id)]
                 [title
                  "Weighted histogram; height corresponds to percentage of runtime in that bucket."]))
        (script ,(format "histogram('calls-~a', " hist-id) ,(jsexpr->string (map first times)) ")")
        (table ((class "times"))
               ,@(for/list ([rec (in-list (sort times > #:key first))]
                            [_ (in-range 5)])
                   (match-define (list time batch-jsexpr) rec)
                   `(tr (td ,(format-time time)) (td (pre ,(jsexpr->batch-exprs batch-jsexpr)))))))))

(define (render-phase-series times)
  (define hist-id (make-id))
  `((dt "Calls")
    (dd (p ,(~a (length times)) " calls:")
        (canvas ([id ,(format "calls-~a" hist-id)]
                 [title
                  "Weighted histogram; height corresponds to percentage of runtime in that bucket."]))
        (script ,(format "histogram('calls-~a', " hist-id) ,(jsexpr->string (map first times)) ")")
        (table ((class "times"))
               (thead (tr (th "Time") (th "Variable") (th "Point")))
               ,@(for/list ([rec (in-list (sort times > #:key first))]
                            [_ (in-range 5)])
                   (match-define (list time var transform) rec)
                   `(tr (td ,(format-time time)) (td (pre ,var)) (td ,transform)))))))

(define (render-phase-compiler compiler)
  (match-define (list (list sizes compileds) ...) compiler)
  (define size (apply + sizes))
  (define compiled (apply + compileds))
  `((dt "Compiler") (dd (p "Compiled "
                           ,(~r size #:group-sep " ")
                           " to "
                           ,(~r compiled #:group-sep " ")
                           " computations "
                           "("
                           ,(format-percent (- size compiled) size)
                           " saved)"))))

(define (render-phase-branches branches)
  `((dt "Results") (dd (table ((class "times"))
                              (thead (tr (th "Accuracy") (th "Segments") (th "Branch")))
                              ,@(for/list ([rec (in-list branches)])
                                  (match-define (list batch-jsexpr score splits repr-name) rec)
                                  (define repr
                                    (get-representation (read (open-input-string repr-name))))
                                  `(tr (td ,(format-accuracy score repr #:unit "%") "")
                                       (td ,(~a splits))
                                       (td (pre ,(jsexpr->batch-exprs batch-jsexpr)))))))))

(define (render-phase-outcomes outcomes)
  `((dt "Samples") (dd (table ((class "times"))
                              ,@(for/list ([rec (in-list (sort outcomes > #:key first))])
                                  (match-define (list time precision category count) rec)
                                  `(tr (td ,(format-time time))
                                       (td ,(~r count #:group-sep " ") "×")
                                       (td ,(~a precision))
                                       (td ,(~a category))))))))

(define (batch-jsexpr? x)
  (and (hash? x) (hash-has-key? x 'nodes)))

(define (jsexpr->exprs x)
  (if (batch-jsexpr? x)
      (jsexpr->batch-exprs x)
      (string-join (map ~a x) "\n")))

(define (render-phase-inputs inputs outputs)
  `((dt "Calls") (dd ,@(for/list ([input-jsexpr inputs]
                                  [output-jsexpr outputs]
                                  [n (in-naturals 1)])
                         `(details (summary "Call " ,(~a n))
                                   (table (thead (tr (th "Inputs")))
                                          (tr (td (pre ,(jsexpr->exprs input-jsexpr)))))
                                   (table (thead (tr (th "Outputs")))
                                          (tr (td (pre ,(jsexpr->exprs output-jsexpr))))))))))

(define (render-phase-allocations allocations)
  (define sorted (sort allocations > #:key second))
  (define total (apply + (map second sorted)))
  `((dt "Allocations")
    (dd (table ((class "times"))
               (thead (tr (th "Phase") (th "Allocated") (th "Percent")))
               ,@(for/list ([rec (in-list sorted)])
                   (match-define (list type alloc) rec)
                   `(tr (td ,(~a type))
                        (td ,(~r (/ alloc (expt 2 20)) #:group-sep " " #:precision '(= 1)) " MiB")
                        (td ,(format-percent alloc total))))))))

;; This next part handles summarizing several timelines into one details section for the report page.

(define (render-about info)
  (match-define (report-info date
                             commit
                             branch
                             seed
                             flags
                             points
                             iterations
                             tests
                             merged-cost-accuracy)
    info)

  `(table ((id "about"))
          (tr (th "Date:") (td ,(date->string date)))
          (tr (th "Commit:")
              (td (abbr ([title ,commit])
                        ,(with-handlers ([exn:fail:contract? (const commit)])
                           (substring commit 0 8)))
                  " on "
                  ,branch))
          (tr (th "Seed:") (td ,(~a seed)))
          (tr (th "Parameters:")
              (td ,(~a (*num-points*)) " points for " ,(~a (*num-iterations*)) " iterations"))
          (tr (th "Flags:")
              (td ((id "flag-list"))
                  (div ((id "all-flags"))
                       ,@(for*/list ([(class flags) (*flags*)]
                                     [flag flags])
                           `(kbd ,(~a class) ":" ,(~a flag))))
                  (div ((id "changed-flags"))
                       ,@(if (null? (changed-flags))
                             '("default")
                             (for/list ([rec (in-list (changed-flags))])
                               (match-define (list delta class flag) rec)
                               `(kbd ,(match delta
                                        ['enabled "+o"]
                                        ['disabled "-o"])
                                     " "
                                     ,(~a class)
                                     ":"
                                     ,(~a flag)))))))))

(define (render-profile)
  `(section ([id "profile"]) (h1 "Profiling") (p ((class "load-text")) "Loading profile data...")))
