#lang racket

(require json)
(require "../syntax/read.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/alternative.rkt"
         "../utils/float.rkt"
         "../core/points.rkt"
         "../api/sandbox.rkt"
         "common.rkt"
         "timeline.rkt"
         "plot.rkt"
         "make-graph.rkt"
         "traceback.rkt")
(provide all-pages
         make-page
         page-error-handler)

(define (all-pages result)
  (define good? (eq? (job-result-status result) 'success))
  (define default-pages '("graph.html" "timeline.html" "timeline.json"))
  (define success-pages '("points.json"))
  (append default-pages (if good? success-pages empty)))

(define ((page-error-handler result page out) e)
  (define test (job-result-test result))
  (eprintf "Error generating `~a` for \"~a\":\n  ~a\n" page (test-name test) (exn-message e))
  (eprintf "context:\n")
  (for ([(fn loc) (in-dict (continuation-mark-set->context (exn-continuation-marks e)))])
    (match loc
      [(srcloc file line col _ _) (eprintf "  ~a:~a:~a: ~a\n" file line col (or fn "(unnamed)"))]
      [#f (eprintf "  ~a\n" fn)]))
  (parameterize ([current-error-port out])
    (display "<!doctype html><pre>" out)
    ((error-display-handler) (exn-message e) e)
    (display "</pre>" out)))

(define (make-page page out result output? profile?)
  (define test (job-result-test result))
  (define status (job-result-status result))
  (define ctx (test-context test))
  (match page
    ["graph.html"
     (match status
       ['success
        (define command (job-result-command result))
        (match command
          ['improve (make-graph result out output? profile?)]
          [else (dummy-graph command out)])]
       ['timeout (make-traceback result out profile?)]
       ['failure (make-traceback result out profile?)]
       [_ (error 'make-page "unknown result type ~a" status)])]
    ["timeline.html" (make-timeline (test-name test) (job-result-timeline result) out #:path "..")]
    ["timeline.json" (write-json (job-result-timeline result) out)]
    ["points.json" (write-json (make-points-json result ctx) out)]))
