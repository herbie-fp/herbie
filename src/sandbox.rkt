#lang racket
(require profile math/bigfloat racket/engine)
(require "common.rkt" "errors.rkt" "debug.rkt" "float.rkt" "points.rkt" "programs.rkt"
         "mainloop.rkt" "alternative.rkt" "timeline.rkt" (submod "timeline.rkt" debug)
         "interface.rkt" "formats/datafile.rkt" "formats/test.rkt")

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
(define *timeout* (make-parameter (* 1000 60 10)))

(define (get-p&es context)
  (for/lists (pts exs)
      ([(pt ex) (in-pcontext context)])
    (values pt ex)))

(define (get-test-result test #:seed [seed #f] #:debug [debug? #f]
                         #:profile [profile? #f] #:debug-port [debug-port #f] #:debug-level [debug-level #f])

  (define timeline #f)
  (define output-prec (test-output-prec test))
  (define output-repr (get-representation output-prec))

  (define (compute-result test)
    (parameterize ([*debug-port* (or debug-port (*debug-port*))])
      (when seed (set-seed! seed))
      (random) ;; Child process uses deterministic but different seed from evaluator
      (match debug-level
        [(cons x y) (set-debug-level! x y)]
        [_ (void)])
      (with-handlers ([exn? (λ (e) (timeline-event! 'end) `(error ,(bf-precision) ,warning-log ,e))])
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
            (prepare-points (test-specification test) (test-precondition test) output-prec)))
        (timeline-event! 'end)
        (define end-err (errors-score (errors (alt-program alt) newcontext output-repr)))

        (define all-alts (remove-duplicates (*all-alts*)))
        (define baseline-errs
          (baseline-error (map (λ (alt) (eval-prog (alt-program alt) 'fl output-repr)) all-alts) context newcontext output-repr))
        (define oracle-errs
          (oracle-error (map (λ (alt) (eval-prog (alt-program alt) 'fl output-repr)) all-alts) newcontext output-repr))

        (debug #:from 'regime-testing #:depth 1
               "Baseline error score:" (errors-score baseline-errs))
        (debug #:from 'regime-testing #:depth 1
               "Oracle error score:" (errors-score oracle-errs))
            
        ;; The cells are stored in reverse order, so this finds last regimes invocation
        (for/first ([cell (unbox timeline)]
                    #:when (equal? (dict-ref cell 'type) 'regimes))
          (dict-set! cell 'oracle (errors-score oracle-errs))
          (dict-set! cell 'accuracy end-err)
          (dict-set! cell 'baseline (errors-score baseline-errs)))
        
        (debug #:from 'regime-testing #:depth 1
               "End program error score:" end-err)
        (when (test-output test)
          (debug #:from 'regime-testing #:depth 1
                 "Target error score:" (errors-score
                                         (errors (test-target test) newcontext output-repr))))
        `(good ,(bf-precision) ,warning-log
               ,(make-alt (test-program test)) ,alt ,context ,newcontext
               ,baseline-errs ,oracle-errs ,all-alts))))

  (define (in-engine _)
    (set! timeline *timeline*)
    (if profile?
        (parameterize ([current-output-port (or profile? (current-output-port))])
          (profile (compute-result test)))
        (compute-result test)))

  (let* ([start-time (current-inexact-milliseconds)] [eng (engine in-engine)])
    (engine-run (*timeout*) eng)

    (match (engine-result eng)
      [`(good ,bits ,warnings ,start ,end ,context ,newcontext
              ,baseline-errs ,oracle-errs ,all-alts)
       (define-values (newpoints newexacts) (get-p&es newcontext))
       (define-values (points exacts) (get-p&es context))
       (test-success test
                     bits
                     (- (current-inexact-milliseconds) start-time)
                     (reverse (unbox timeline))
                     warnings start end points exacts
                     (errors (alt-program start) context output-repr)
                     (errors (alt-program end) context output-repr)
                     newpoints newexacts
                     (errors (alt-program start) newcontext output-repr)
                     (errors (alt-program end) newcontext output-repr)
                     (if (test-output test)
                         (errors (test-target test) newcontext output-repr)
                         #f)
                     baseline-errs
                     oracle-errs
                     all-alts)]
      [`(error ,bits ,warnings ,e)
       (test-failure test bits (- (current-inexact-milliseconds) start-time) (reverse (unbox timeline)) warnings e)]
      [#f
       (define timeline*
         (reverse 
          (cons (hash 'type 'end 'time (current-inexact-milliseconds))
                (unbox timeline))))
       (test-timeout test (bf-precision) (*timeout*) timeline* '())])))

(define (dummy-table-row result status link)
  (define test (test-result-test result))
  (table-row (test-name test) status
             (resugar-program (test-precondition test) (test-output-prec test))
             (test-output-prec test)
             (test-vars test)
             (resugar-program (test-input test) (test-output-prec test)) #f
             (resugar-program (test-spec test) (test-output-prec test))
             (and (test-output test)
                  (resugar-program (test-output test) (test-output-prec test)))
             #f #f #f #f #f #f #f (test-result-time result)
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

    (define binary64 (get-representation 'binary64))
    ;; TODO: this is broken because errors are always ordinary values now!
    (define-values (reals infs) (partition (curryr ordinary-value? binary64)
                                           (map - end-errors start-errors)))
    (define-values (good-inf bad-inf) (partition positive? infs))

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
                           (test-output-prec test))]
                 [start start-score] [result end-score] [target target-score]
                 [start-est est-start-score] [result-est est-end-score] [inf- (length good-inf)] [inf+ (length bad-inf)])]
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
