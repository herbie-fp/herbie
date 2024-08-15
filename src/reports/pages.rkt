#lang racket

(require json)
(require "../syntax/read.rkt"
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
  (define test (hash-ref result-hash 'test))
  (define status (hash-ref result-hash 'status))
  (match page
    ["graph.html"
     (match status
       ['success
        (define command (hash-ref result-hash 'command))
        (match command
          ["improve" (make-graph result-hash out output? profile?)]
          [else (dummy-graph command out)])]
       ['timeout (make-traceback result-hash out)]
       ['failure (make-traceback result-hash out)]
       [_ (error 'make-page "unknown result type ~a" status)])]
    ["timeline.html"
     (make-timeline (test-name test) (hash-ref result-hash 'timeline) out #:path "..")]
    ["timeline.json" (write-json (hash-ref result-hash 'timeline) out)]
    ["points.json" (write-json (make-points-json result-hash) out)]))
