#lang racket
(require profile math/bigfloat racket/engine json)
(require "common.rkt" "errors.rkt" "debug.rkt" "points.rkt" "programs.rkt"
         "mainloop.rkt" "alternative.rkt" "timeline.rkt" (submod "timeline.rkt" debug)
         "interface.rkt" "datafile.rkt" "syntax/read.rkt" "syntax/rules.rkt" "profile.rkt"
         (submod "syntax/rules.rkt" internals) "syntax/syntax.rkt" "conversions.rkt"
         "syntax/sugar.rkt")

(provide get-test-result *reeval-pts* *timeout*
         (struct-out test-result) (struct-out test-success)
         (struct-out test-failure) (struct-out test-timeout)
         get-table-data unparse-result)


;; These cannot move between threads!
(struct test-result (test bits time timeline warnings))
(struct test-success test-result
  (start-alt end-alt points exacts start-est-error end-est-error
   newpoints newexacts start-error end-error target-error
   baseline-error oracle-error all-alts))
(struct test-failure test-result (exn))
(struct test-timeout test-result ())

(define *reeval-pts* (make-parameter 8000))
(define *timeout* (make-parameter (* 1000 60 5/2)))

(define (get-p&es context)
  (for/lists (pts exs)
      ([(pt ex) (in-pcontext context)])
    (values pt ex)))

(define (get-test-result test
                         #:seed [seed #f]
                         #:profile [profile? #f]
                         #:debug [debug? #f]
                         #:debug-port [debug-port #f]
                         #:debug-level [debug-level #f])
  (define timeline #f)
  (define output-prec (test-output-prec test))
  (define output-repr (get-representation output-prec))
  (*needed-reprs* (list output-repr (get-representation 'bool)))

  (define (compute-result test)
    (parameterize ([*debug-port* (or debug-port (*debug-port*))]
                   [*timeline-disabled* false] [*warnings-disabled* true])
      (define start-time (current-inexact-milliseconds))
      (when seed (set-seed! seed))
      (random) ;; Child process uses deterministic but different seed from evaluator
      (match debug-level
        [(cons x y) (set-debug-level! x y)]
        [_ (void)])

      (generate-conversions (test-conversions test))
      (with-handlers ([exn? (curry on-exception start-time)])
        (define alt
          (run-improve (test-program test)
                       (*num-iterations*)
                       #:precondition (test-precondition test)
                       #:specification (test-specification test)
                       #:precision output-prec))

        (define context (*pcontext*))
        (when seed (set-seed! seed))
        (timeline-event! 'sample)
        (define newcontext
          (parameterize ([*num-points* (*reeval-pts*)])
            (prepare-points (test-specification test) (test-precondition test) output-repr (*sampler*))))
        (define fns
          (map (λ (alt) (eval-prog (alt-program alt) 'fl output-repr))
               (remove-duplicates (*all-alts*))))

        (define end-errs (errors (alt-program alt) newcontext output-repr))
        (define baseline-errs (baseline-error fns context newcontext output-repr))
        (define oracle-errs (oracle-error fns newcontext output-repr))

        (timeline-adjust! 'regimes 'oracle (errors-score oracle-errs))
        (timeline-adjust! 'regimes 'accuracy (errors-score end-errs))
        (timeline-adjust! 'regimes 'baseline (errors-score baseline-errs))
        (timeline-adjust! 'regimes 'name (test-name test))
        (timeline-adjust! 'regimes 'link ".")

        (debug #:from 'regime-testing #:depth 1
               "End program error score:" (errors-score end-errs))
        (when (test-output test)
          (debug #:from 'regime-testing #:depth 1
                 "Target error score:" (errors-score
                                         (errors (test-target test) newcontext output-repr))))

        (define-values (points exacts) (get-p&es context))
        (define-values (newpoints newexacts) (get-p&es newcontext))
        (test-success test
                      (bf-precision)
                      (- (current-inexact-milliseconds) start-time)
                      (timeline-extract output-repr)
                      warning-log (make-alt (test-program test)) alt points exacts
                      (errors (test-program test) context output-repr)
                      (errors (alt-program alt) context output-repr)
                      newpoints newexacts
                      (errors (test-program test) newcontext output-repr)
                      end-errs
                      (if (test-output test)
                          (errors (test-target test) newcontext output-repr)
                          #f)
                      baseline-errs
                      oracle-errs
                      (*all-alts*)))))

  (define (on-exception start-time e)
    (parameterize ([*timeline-disabled* false])
      (timeline-event! 'end))
    (test-failure test (bf-precision)
                  (- (current-inexact-milliseconds) start-time) (timeline-extract output-repr)
                  warning-log e))

  (define (in-engine _)
    (set! timeline *timeline*)
    (if profile?
        (parameterize ([current-output-port (or profile? (current-output-port))])
          (profile-thunk
           (λ () (compute-result test))
           #:order 'total
           #:render (λ (p order) (write-json (profile->json p)))))
        (compute-result test)))

  (define eng (engine in-engine))
  (if (engine-run (*timeout*) eng)
      (begin
        (engine-result eng))
      (parameterize ([*timeline-disabled* false])
        (timeline-load! timeline)
        (test-timeout test (bf-precision) (*timeout*) (timeline-extract output-repr) '()))))

(define (dummy-table-row result status link)
  (define test (test-result-test result))
  (define repr (test-output-repr test))
  (table-row (test-name test) status
             (resugar-program (program-body (test-precondition test)) repr)
             (test-output-prec test)
             (test-vars test)
             (resugar-program (test-input test) repr) #f
             (resugar-program (test-spec test) repr)
             (and (test-output test) (resugar-program (test-output test) repr))
             #f #f #f #f #f (test-result-time result)
             (test-result-bits result) link))

(define (get-table-data result link)
  (define test (test-result-test result))

  (cond
   [(test-success? result)
    (define name (test-name test))
    (define start-errors  (test-success-start-error  result))
    (define end-errors    (test-success-end-error    result))
    (define target-errors (test-success-target-error result))

    (define start-score (errors-score start-errors))
    (define end-score (errors-score end-errors))
    (define target-score (and target-errors (errors-score target-errors)))
    (define est-start-score (errors-score (test-success-start-est-error result)))
    (define est-end-score (errors-score (test-success-end-est-error result)))

    (define status
      (if target-score
          (cond
           [(< end-score (- target-score 1)) "gt-target"]
           [(< end-score (+ target-score 1)) "eq-target"]
           [(> end-score (+ start-score 1)) "lt-start"]
           [(> end-score (- start-score 1)) "eq-start"]
           [(> end-score (+ target-score 1)) "lt-target"])
          (cond
           [(and (< start-score 1) (< end-score (+ start-score 1))) "ex-start"]
           [(< end-score (- start-score 1)) "imp-start"]
           [(< end-score (+ start-score 1)) "apx-start"]
           [else "uni-start"])))

    (struct-copy table-row (dummy-table-row result status link)
                 [output (resugar-program
                           (program-body (alt-program (test-success-end-alt result)))
                           (test-output-repr test))]
                 [start start-score] [result end-score] [target target-score]
                 [start-est est-start-score] [result-est est-end-score])]
   [(test-failure? result)
    (define status (if (exn:fail:user:herbie? (test-failure-exn result)) "error" "crash"))
    (dummy-table-row result status link)]
   [(test-timeout? result)
    (dummy-table-row result "timeout" link)]))

(define (unparse-result row)
  `(FPCore ,(table-row-vars row)
     :herbie-status ,(string->symbol (table-row-status row))
     :herbie-time ,(table-row-time row)
     :herbie-error-input 
     ([,(*num-points*) ,(table-row-start-est row)]
      [,(*reeval-pts*) ,(table-row-start row)])
     :herbie-error-output
     ([,(*num-points*) ,(table-row-result-est row)]
      [,(*reeval-pts*) ,(table-row-result row)])
     ,@(if (table-row-target row)
           `(:herbie-error-target ([,(*reeval-pts*) ,(table-row-target row)]))
           '())
     :name ,(table-row-name row)
     :precision ,(table-row-precision row)
     ,@(if (eq? (table-row-pre row) 'TRUE) '() `(:pre ,(table-row-pre row)))
     ,@(if (table-row-target-prog row) `(:herbie-target ,(table-row-target-prog row)) '())
     ,(table-row-output row)))
