#lang racket

(require (only-in fpbench fpcore? supported-by-lang? core->js js-header) json)
(require "../alternative.rkt" "../syntax/read.rkt" "../sandbox.rkt" )
(require "common.rkt" "timeline.rkt" "plot.rkt" "make-graph.rkt" "traceback.rkt"
        "../syntax/sugar.rkt" "../float.rkt" "../syntax/types.rkt" "../syntax/syntax.rkt")
(provide all-pages make-page page-error-handler)

(define (unique-values pts idx)
  (length (remove-duplicates (map (curryr list-ref idx) pts))))

(define (all-pages result)
  (define test (test-result-test result))
  (define good? (test-success? result))
  (define others
    (if good?
      (let ([other (cdr (test-success-end-alts result))]
            [vars (test-vars test)])
        (if (and good? (< (* (length other) (length vars)) 100))
            (build-list (length other) (λ (x) (format "o~a" x)))
            '()))
      '()))

  (define pages
    `("graph.html"
      ,(and good? "interactive.js")
      "timeline.html" "timeline.json"
      ,(and good? "points.json")))
  (filter identity pages))

(define ((page-error-handler result page) e)
  (define test (test-result-test result))
  ((error-display-handler)
   (format "Error generating `~a` for \"~a\":\n~a\n" page (test-name test) (exn-message e))
   e))

(define (make-page page out result profile?)
  (define test (test-result-test result))
  (define repr (test-output-repr test))
  (match page
    ["graph.html"
     (match result
       [(? test-success?) (make-graph result out (get-interactive-js result repr) profile?)]
       [(? test-timeout?) (make-traceback result out profile?)]
       [(? test-failure?) (make-traceback result out profile?)])]
    ["interactive.js"
     (make-interactive-js result out repr)]
    ["timeline.html"
     (make-timeline (test-name test) (test-result-timeline result) out)]
    ["timeline.json"
     (write-json (test-result-timeline result) out)]
    ["points.json"
     (make-points-json result out repr)]))

(define (get-interactive-js result repr)
  (define start-prog (alt-program (test-success-start-alt result)))
  (define end-prog (alt-program (car (test-success-end-alts result))))
  (define start-fpcore (program->fpcore (resugar-program start-prog repr)))
  (define end-fpcore (program->fpcore (resugar-program end-prog repr)))
  (and (fpcore? start-fpcore) (fpcore? end-fpcore)
       (supported-by-lang? start-fpcore "js")
       (supported-by-lang? end-fpcore "js")
       (string-append
          (js-header "Math")  ; pow, fmax, fmin will not work without this
          (core->js start-fpcore "start")
          (core->js end-fpcore "end"))))

(define (make-interactive-js result out repr)
  (define js-text (get-interactive-js result repr))
  (when (string? js-text)
    (display js-text out)))

(define (make-points-json result out repr)
  ; Immediately convert points to reals to handle posits
  (define points 
    (for/list ([point (test-success-newpoints result)])
      (for/list ([x point])
        (repr->real x repr))))
  (define bit-width (representation-total-bits repr))
  
  (define json-points (for/list ([point points]) (for/list ([value point]) 
    (real->ordinal value repr))))
  (define (ulps->bits-tenths x) (string->number (real->decimal-string (ulps->bits x) 1)))
  (define start-error (map (lambda (err) (ulps->bits-tenths err)) (test-success-start-error result)))
  (define end-error (map (lambda (err) (ulps->bits-tenths err)) 
    ((compose car test-success-end-errors) result)))
  (define target-error (if (test-success-target-error result) 
    (map (lambda (err) (ulps->bits-tenths err)) (test-success-target-error result)) #f))
  (define vars (test-vars (test-result-test result)))
  (define ticks 
    (for/list ([idx (in-range (length vars))]) (let/ec return 
      (define points-at-idx (for/list ([point points]) (list-ref point idx)))
      ; We bail out since choose-ticks will crash otherwise
      (if (= (unique-values (test-success-newpoints result) idx) 1) (return #f) #f) 
      (define real-ticks (choose-ticks (apply min points-at-idx) (apply max points-at-idx) repr))
      (for/list ([val real-ticks]) 
        (define tick-str (if (or (= val 0) (< 0.01 (abs val) 100))
           (~r (exact->inexact val) #:precision 4)
           (string-replace (~r val #:notation 'exponential #:precision 0) "1e" "e")))
        (list 
          tick-str
          (real->ordinal val repr))))))
  (define end-alt (car (test-success-end-alts result)))

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
    (bits . ,bit-width)
    (vars . ,(for/list ([var vars]) (symbol->string var)))
    (points . ,json-points) 
    (error . ,`#hasheq(
      (start . ,start-error)
      (target . ,target-error)
      (end . ,end-error)))
    (ticks_by_varidx . ,ticks)
    (splitpoints_by_varidx . ,splitpoints)))
  
  (write-json json-obj out))
