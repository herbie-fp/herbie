#lang racket

(require (only-in fpbench fpcore? supported-by-lang? core->js js-header) json)
(require "../alternative.rkt" "../syntax/read.rkt" "../sandbox.rkt" "../interface.rkt")
(require "common.rkt" "timeline.rkt" "plot.rkt" "make-graph.rkt" "traceback.rkt" "../programs.rkt"
        "../syntax/sugar.rkt" "../float.rkt")
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
      ,@(for/list ([v (test-vars test)] [idx (in-naturals)]
                   #:when good? [type (append (list "" "r" "g" "b") others)]
                   #:unless (and (equal? type "g") (not (test-output test)))
                   ;; Don't generate a plot with only one X value else plotting throws an exception
                   #:when (> (unique-values (test-success-newpoints result) idx) 1))
          (format "plot-~a~a.png" idx type))
      ,(and good? (>= (length (test-success-end-alts result)) 2) "cost-accuracy.png")
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
    ["cost-accuracy.png"
     (make-cost-accuracy-plot result out)]
    ["points.json"
     (make-points-json result out repr)]
    [(regexp #rx"^plot-([0-9]+).png$" (list _ idx))
     (make-axis-plot result out (string->number idx))]
    [(regexp #rx"^plot-([0-9]+)([rbg]).png$" (list _ idx letter))
     (make-points-plot result out (string->number idx) (string->symbol letter))]
    [(regexp #rx"^plot-([0-9]+)(o[0-9]+).png$" (list _ idx letter))
     (make-points-plot result out (string->number idx) letter)]))

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

(require debug/repl)
(define (make-points-json result out repr)
  (define points (test-success-newpoints result))
  (define exacts (test-success-newexacts result))
  (debug-repl)
  ; (define json-points (for/list ([point points]) (for/list ([value point]) (value->json value repr))))
  ;(define json-exacts (map (lambda (x) (value->json x repr)) exacts))
  (define bit-width (representation-total-bits repr))
  (define json-points (for/list ([point points]) (for/list ([value point]) (real->ordinal value repr))))
  (define start-error (map (lambda (err) (ulps->bits err)) (test-success-start-error result)))
  ; potential issue with end-errors (not like the others)
  (define end-error (map (lambda (err) (ulps->bits err)) ((compose car test-success-end-errors) result)))
  (define target-error (if (test-success-target-error result) (map (lambda (err) (ulps->bits err)) (test-success-target-error result)) #f))
  ; this is wrong, it returns a procedure or something
  ; choose-ticks would work if I can just get the min and max points
  (define vars (test-vars (test-result-test result)))
  ; use (list-ref sublist idx) on the real version of the lists in json_points to get 
  ; real min and max values for this var
  ; output is real, then map to ordinal to make the pair 
  ;(define ticks (for/list ([idx (in-range (length vars))]) (choose-ticks (min ) (max ) repr))
  (define end-alt (car (test-success-end-alts result)))
  ; 0.5 * sqrt(2.0 * (sqrt(xre * xre + xim * xim) + xre)) has splitpoints (for xre)
  (define splitpoints (regime-splitpoints end-alt))

  ; Note ordinals should be passed as strings so we can detect truncation if necessary.
  ; parts:
  ; bits: int
  ; points: array of size m like [[x0, x1, ..., xn], ...] where x0 etc. 
  ; are ordinals representing the real input values
  ; error: object with fields {start, target, end}, where each field holds 
  ; an array like [y0, ..., ym] where y0 etc are bits of error for the output 
  ; on each point
  ; ticks: array of size n where each entry is 13 or so tick values as [ordinal, string] pairs
  ; splitpoints: array with the ordinal splitpoints
  (define json-obj `#hasheq(
    (bits . ,bit-width) 
    (points . ,json-points) 
    (error . ,`#hasheq(
      (start . ,start-error)
      (target . ,target-error)
      (end . ,end-error)))
    ;(ticks . ,ticks)
    (splitpoints . ,splitpoints)))
    
  ; (exacts . ,json-exacts)))
  (write-json json-obj out))