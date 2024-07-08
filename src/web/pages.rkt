#lang racket

(require json (only-in fpbench fpcore? supported-by-lang? core->js js-header))
(require "../syntax/read.rkt" "../syntax/sugar.rkt" "../syntax/syntax.rkt" "../syntax/types.rkt"
         "../alternative.rkt" "../float.rkt" "../points.rkt" "../sandbox.rkt" "../datafile.rkt"
         "common.rkt" "timeline.rkt" "plot.rkt" "make-graph.rkt" "traceback.rkt")
(provide all-pages all-pages-modified make-page make-page-modifed page-error-handler)

(define (unique-values pts idx)
  (length (remove-duplicates (map (curryr list-ref idx) pts))))

(define (all-pages-modified result)
  (define good? (eq? (hash-ref result 'status) 'success))
  (define default-pages '("graph.html" "timeline.html" "timeline.json"))
  (define success-pages '("interactive.js" "points.json"))
  (append default-pages (if good? success-pages empty)))

(define (all-pages result)
  (define good? (eq? (job-result-status result) 'success))
  (define default-pages '("graph.html" "timeline.html" "timeline.json"))
  (define success-pages '("interactive.js" "points.json"))
  (append default-pages (if good? success-pages empty)))

(define ((page-error-handler result page out) e)
  (define test (job-result-test result))
  (eprintf "Error generating `~a` for \"~a\":\n  ~a\n"
           page (test-name test) (exn-message e))
  (parameterize ([current-error-port out])
    (display "<!doctype html><pre>" out)
    ((error-display-handler) (exn-message e) e)
    (display "</pre>" out)))

#| 
Well I know I need to to pass the needed result data to generate a page from table row. Which I don't think is enough I might make a new type. and just add the data I need to generate the same data. This is kinda annoying but I guess it's all part of the plan.
|#
(define (make-page-modifed page out result output? profile?)
  (eprintf "result: ~a\n" result)
  (define test (hash-ref 'test result))
  (define status (hash-ref 'status result))
  (define ctx (hash-ref 'ctx result))
  (match page
    ["graph.html"
     (match status
       ['success (make-graph-modified result out output? (get-interactive-js result ctx) profile?)]
       ['timeout (make-traceback result out profile?)]
       ['failure (make-traceback result out profile?)]
       [_ (error 'make-page "unknown result type ~a" status)])]
    ["interactive.js"
     (make-interactive-js result out ctx)]
    ["timeline.html"
     (make-timeline (test-name test) (job-result-timeline result) out #:path "..")]
    ["timeline.json"
     (write-json (job-result-timeline result) out)]
    ["points.json"
     (make-points-json result out ctx)]))

(define (make-page page out result output? profile?)
  (define test (job-result-test result))
  (define status (job-result-status result))
  (define ctx (test-context test))
  (match page
    ["graph.html"
     (match status
       ['success (make-graph result out output? (get-interactive-js result ctx) profile?)]
       ['timeout (make-traceback result out profile?)]
       ['failure (make-traceback result out profile?)]
       [_ (error 'make-page "unknown result type ~a" status)])]
    ["interactive.js"
     (make-interactive-js result out ctx)]
    ["timeline.html"
     (make-timeline (test-name test) (job-result-timeline result) out #:path "..")]
    ["timeline.json"
     (write-json (job-result-timeline result) out)]
    ["points.json"
     (make-points-json result out ctx)]))

(define (get-interactive-js result ctx)
  (match-define (job-result _ _ _ _ _ _
                 (improve-result _ _ start _ end _)) result)
  (define start-expr (alt-expr (alt-analysis-alt start)))
  (define end-expr (alt-expr (alt-analysis-alt (car end))))
  (define start-fpcore (program->fpcore start-expr ctx))
  (define end-fpcore (program->fpcore end-expr ctx))
  (and (fpcore? start-fpcore) (fpcore? end-fpcore)
       (supported-by-lang? start-fpcore "js")
       (supported-by-lang? end-fpcore "js")
       (string-append
          (js-header "Math")  ; pow, fmax, fmin will not work without this
          (core->js start-fpcore "start")
          (core->js end-fpcore "end"))))

(define (make-interactive-js result out ctx)
  (define repr (context-repr ctx))
  (define js-text (get-interactive-js result ctx))
  (when (string? js-text)
    (display js-text out)))

(define (ulps->bits-tenths x)
  (string->number (real->decimal-string (ulps->bits x) 1)))

(define (make-points-json result out repr)
  (match-define (job-result _ test _ _ _ _ 
                 (improve-result _ pctxs start targets end _) ) result)
  (define repr (test-output-repr test))
  (define start-errors (alt-analysis-test-errors start))

  (define target-errors (map alt-analysis-test-errors targets))

  (define end-errors (map alt-analysis-test-errors end))
  (define-values (newpoints _) (pcontext->lists (second pctxs)))

  ; Immediately convert points to reals to handle posits
  (define points 
    (for/list ([point newpoints])
      (for/list ([x point])
        (repr->real x repr))))

  (define json-points
    (for/list ([point points])
      (for/list ([value point]) 
        (real->ordinal value repr))))

  (define vars (test-vars test))
  (define bits (representation-total-bits repr))
  (define start-error (map ulps->bits-tenths start-errors))
  (define target-error (map (lambda (alt-error) (map ulps->bits-tenths alt-error)) target-errors))
  (define end-error (map ulps->bits-tenths (car end-errors)))

  (define target-error-entries
    (for/list ([i (in-naturals)] [error-value (in-list target-error)])
        (cons (format "target~a" (+ i 1)) error-value)))

  (define error-entries (list* (cons "start" start-error) (cons "end" end-error) target-error-entries))

  (define ticks 
    (for/list ([idx (in-range (length vars))])
      ; We want to bail out since choose-ticks will crash otherwise
      (let/ec return 
        (define points-at-idx (map (curryr list-ref idx) points))
        (when (= (unique-values newpoints idx) 1)
          (return #f))
        (define real-ticks (choose-ticks (apply min points-at-idx) (apply max points-at-idx) repr))
        (for/list ([val real-ticks]) 
          (define tick-str
            (if (or (= val 0) (< 0.01 (abs val) 100))
                (~r (exact->inexact val) #:precision 4)
                (string-replace (~r val #:notation 'exponential #:precision 0) "1e" "e")))
          (list tick-str (real->ordinal val repr))))))

  (define end-alt (alt-analysis-alt (car end)))
  (define splitpoints 
    (for/list ([var vars]) 
      (define split-var? (equal? var (regime-var end-alt)))
      (if split-var?
          (for/list ([val (regime-splitpoints end-alt)])
            (real->ordinal (repr->real val repr) repr))
          '())))

  ; NOTE ordinals *should* be passed as strings so we can detect truncation if
  ;   necessary, but this isn't implemented yet. 
  ; Fields:
  ;   bits: int representing the maximum possible bits of error
  ;   vars: array of n string variable names
  ;   points: array of size m like [[x0, x1, ..., xn], ...] where x0 etc. 
  ;     are ordinals representing the real input values
  ;   error: JSON dictionary where keys are {start, end, target1, ..., targetn}.
  ;          Each key's value holds an array like [y0, ..., ym] where y0 etc are
  ;          bits of error for the output on each point
  ;   ticks: array of size n where each entry is 13 or so tick values as [ordinal, string] pairs
  ;   splitpoints: array with the ordinal splitpoints
  (define json-obj `#hasheq(
    (bits . ,bits)
    (vars . ,(map symbol->string vars))
    (points . ,json-points) 
    (error . ,error-entries)
    (ticks_by_varidx . ,ticks)
    (splitpoints_by_varidx . ,splitpoints)))
  
  (write-json json-obj out))
