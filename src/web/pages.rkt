#lang racket

(require json (only-in fpbench fpcore? supported-by-lang? core->js js-header))
(require "../syntax/read.rkt" "../syntax/sugar.rkt" "../syntax/syntax.rkt" "../syntax/types.rkt"
         "../alternative.rkt" "../float.rkt" "../points.rkt" "../sandbox.rkt"
         "common.rkt" "timeline.rkt" "plot.rkt" "make-graph.rkt" "traceback.rkt")
(provide all-pages make-page page-error-handler)

(define (unique-values pts idx)
  (length (remove-duplicates (map (curryr list-ref idx) pts))))

(define (all-pages result)
  (define good? (eq? (test-result-status result) 'success))
  (define default-pages '("graph.html" "timeline.html" "timeline.json"))
  (define success-pages '("interactive.js" "points.json"))
  (if good? (append default-pages success-pages) default-pages))

(define ((page-error-handler result page) e)
  (define test (test-result-test result))
  ((error-display-handler)
   (format "Error generating `~a` for \"~a\":\n~a\n" page (test-name test) (exn-message e))
   e))

(define (make-page page out result profile?)
  (define test (test-result-test result))
  (define ctx (test-context test))
  (match page
    ["graph.html"
     (match (test-result-status result)
       ['success (make-graph result out (get-interactive-js result ctx) profile?)]
       ['timeout (make-graph result out (get-interactive-js result ctx) profile?)]
       ['failure (make-traceback result out profile?)]
       [_ (error 'make-page "unknown result type ~a" (test-result-status result))])]
    ["interactive.js"
     (make-interactive-js result out ctx)]
    ["timeline.html"
     (make-timeline (test-name test) (test-result-timeline result) out)]
    ["timeline.json"
     (write-json (test-result-timeline result) out)]
    ["points.json"
     (make-points-json result out ctx)]))

(define (get-interactive-js result ctx)
  (define start-expr (alt-expr (alt-result-alt (test-result-start result))))
  (define end-expr (alt-expr (alt-result-alt (car (test-result-end result)))))
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

(define (make-points-json result out repr)
  (match-define (test-result test _ _ _ _ _ _ pctxs start target end) result)
  (define repr (test-output-repr test))
  (define start-errors (alt-result-test-errors start))
  (define target-errors (and target (alt-result-test-errors target)))
  (define end-errors (map alt-result-test-errors end))
  (define-values (newpoints _) (pcontext->lists (second pctxs)))

  (define (ulps->bits-tenths x)
    (string->number (real->decimal-string (ulps->bits x) 1)))

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
  (define target-error (and target-errors (map ulps->bits-tenths target-errors)))
  (define end-error (map ulps->bits-tenths (car end-errors)))

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

  (define end-alt (alt-result-alt (car (test-result-end result))))
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
  ;   error: object with fields {start, target, end}, where each field holds 
  ;     an array like [y0, ..., ym] where y0 etc are bits of error for the output 
  ;     on each point
  ;   ticks: array of size n where each entry is 13 or so tick values as [ordinal, string] pairs
  ;   splitpoints: array with the ordinal splitpoints
  (define json-obj `#hasheq(
    (bits . ,bits)
    (vars . ,(map symbol->string vars))
    (points . ,json-points) 
    (error . ,`#hasheq(
      (start . ,start-error)
      (target . ,target-error)
      (end . ,end-error)))
    (ticks_by_varidx . ,ticks)
    (splitpoints_by_varidx . ,splitpoints)))
  
  (write-json json-obj out))
