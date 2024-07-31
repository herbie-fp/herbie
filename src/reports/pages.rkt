#lang racket

(require json)
(require "../syntax/read.rkt"
         "../api/sandbox.rkt"
         "timeline.rkt"
         "plot.rkt"
         "make-graph.rkt"
         "traceback.rkt")
(provide all-pages
         make-page
         page-error-handler)

(define (all-pages result-hash)
  (define good? (eq? (hash-ref result-hash 'status) 'success))
  (define default-pages '("graph.html" "timeline.html" "timeline.json"))
  (define success-pages '("points.json"))
  (append default-pages (if good? success-pages empty)))

(define ((page-error-handler result-hash page out) e)
  (define test (hash-ref result-hash 'test))
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

(define (make-page page out result-hash output? profile?)
  (eprintf "make-page\n")
  (define test (hash-ref result-hash 'test))
  (define status (hash-ref result-hash 'status))
  (define ctx (test-context test))
  (match page
    ["graph.html"
     (match status
       ['success (make-graph result-hash out output? profile?)]
       ['timeout (make-traceback result-hash out profile?)]
       ['failure (make-traceback result-hash out profile?)]
       [_ (error 'make-page "unknown result type ~a" status)])]
    ["timeline.html" (make-timeline (test-name test) (job-result-timeline result-hash) out #:path "..")]
    ["timeline.json" (write-json (job-result-timeline result-hash) out)]
    ["points.json" (write-json (make-points-json result-hash ctx) out)]))