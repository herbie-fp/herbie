#lang racket
(require profile)
(require math/bigfloat)
(require racket/engine)

(require "common.rkt" "errors.rkt")
(require "debug.rkt")
(require "mainloop.rkt")
(require "formats/datafile.rkt")
(require "programs.rkt")
(require "points.rkt")
(require "formats/test.rkt")
(require "alternative.rkt")

(provide get-test-result *reeval-pts* *timeout*
         (struct-out test-result) (struct-out test-success)
         (struct-out test-failure) (struct-out test-timeout)
         get-table-data unparse-result)


; For things that don't leave a thread
(struct test-result (test bits time timeline))
(struct test-success test-result
  (start-alt end-alt points exacts start-est-error end-est-error
   newpoints newexacts start-error end-error target-error
   baseline-error oracle-error all-alts))
(struct test-failure test-result (exn))
(struct test-timeout test-result ())

(define *reeval-pts* (make-parameter 8000))
(define *timeout* (make-parameter (* 1000 60 10)))

(define (get-p&es context)
  (call-with-values
      (λ ()
        (for/lists (pts exs)
            ([(pt ex) (in-pcontext context)])
          (values pt ex)))
    list))

(define (get-test-result test #:seed [seed #f] #:debug [debug? #f]
                         #:profile [profile? #f] #:debug-port [debug-port #f] #:debug-level [debug-level #f])

  (define (compute-result test)
    (parameterize ([*debug-port* (or debug-port (*debug-port*))])
      (when seed (set-seed! seed))
      (random) ;; Child process uses deterministic but different seed from evaluator
      (match debug-level
        [(cons x y) (set-debug-level! x y)]
        [_ (void)])
      (with-handlers ([exn? (λ (e) `(error ,e ,(bf-precision)))])
        (define alt
          (run-improve (test-program test)
                       (*num-iterations*)
                       #:precondition (test-precondition test)
                       #:precision (test-precision test)))
        (define context (*pcontext*))
        (when seed (set-seed! seed))
        (define log! (timeline-event! 'sample))
        (define prepare-log (make-hash))
        (define newcontext
          (parameterize ([*num-points* (*reeval-pts*)])
            (prepare-points (test-program test) (test-precondition test) (test-precision test) #:log prepare-log)))
        (log! 'method (sampling-method (test-program test) (test-precondition test)))
        (log! 'outcomes prepare-log)
        (timeline-event! 'end)
        (define end-err (errors-score (errors (alt-program alt) newcontext)))

        (define all-alts (remove-duplicates (*all-alts*)))
        (define baseline-errs
          (baseline-error (map (λ (alt) (eval-prog (alt-program alt) 'fl)) all-alts) context newcontext))
        (define oracle-errs
          (oracle-error (map (λ (alt) (eval-prog (alt-program alt) 'fl)) all-alts) newcontext))

        (debug #:from 'regime-testing #:depth 1
               "Baseline error score:" (errors-score baseline-errs))
        (debug #:from 'regime-testing #:depth 1
               "Oracle error score:" (errors-score oracle-errs))
            
        (for/first ([cell (shellstate-timeline (^shell-state^))]
                    #:when (equal? (dict-ref (unbox cell) 'type) 'regimes))
          ;; Since the cells are stored in reverse order this is the last regimes invocation
          (set-box! cell (list* (cons 'oracle (errors-score oracle-errs))
                                (cons 'accuracy end-err)
                                (cons 'baseline (errors-score baseline-errs))
                                (unbox cell))))
        
        (debug #:from 'regime-testing #:depth 1
               "End program error score:" end-err)
        (when (test-output test)
          (debug #:from 'regime-testing #:depth 1
                 "Target error score:" (errors-score (errors (test-target test) newcontext))))
        `(good ,(make-alt (test-program test)) ,alt ,context ,newcontext
               ,(^timeline^) ,(bf-precision) ,baseline-errs ,oracle-errs ,all-alts))))

  (define (in-engine _)
    (if profile?
        (parameterize ([current-output-port (or profile? (current-output-port))])
          (profile (compute-result test)))
        (compute-result test)))

  (let* ([start-time (current-inexact-milliseconds)] [eng (engine in-engine)])
    (engine-run (*timeout*) eng)

    (match (engine-result eng)
      [`(good ,start ,end ,context ,newcontext ,timeline ,bits ,baseline-errs
              ,oracle-errs ,all-alts)
       (match-define (list newpoints newexacts) (get-p&es newcontext))
       (match-define (list points exacts) (get-p&es context))
       (define start-prog (alt-program start))
       (define end-prog (alt-program end))
       (define start-resugared (make-alt
         (list 'λ (program-variables start-prog)
               (resugar-program (program-body start-prog)))))
       (define end-resugared (struct-copy alt end
         [program (list 'λ (program-variables start-prog)
                        (resugar-program (program-body end-prog)))]))
       (test-success test
                     bits
                     (- (current-inexact-milliseconds) start-time)
                     timeline
                     start-resugared end-resugared points exacts
                     (errors (alt-program start) context)
                     (errors (alt-program end) context)
                     newpoints newexacts
                     (errors (alt-program start) newcontext)
                     (errors (alt-program end) newcontext)
                     (if (test-output test)
                         (errors (test-target test) newcontext)
                         #f)
                     baseline-errs
                     oracle-errs
                     all-alts)]
      [`(error ,e ,bits)
       (test-failure test bits (- (current-inexact-milliseconds) start-time) (^timeline^) e)]
      [#f
       (test-timeout test (bf-precision) (*timeout*) (^timeline^))])))

(define (get-table-data result link)
  (cons (unparse-result result) (get-table-data* result link)))

(define (dummy-table-row result status link)
  (define test (test-result-test result))
  (table-row (test-name test) status (test-precondition test)
             #f #f #f #f #f #f #f (test-vars test) (test-input test) #f
             (test-result-time result) (test-result-bits result) link))

(define (get-table-data* result link)
  (define test (test-result-test result))

  (cond
   [(test-success? result)
    (let* ([name (test-name test)]
           [start-errors  (test-success-start-error  result)]
           [end-errors    (test-success-end-error    result)]
           [target-errors (test-success-target-error result)]

           [start-score (errors-score start-errors)]
           [end-score (errors-score end-errors)]
           [target-score (and target-errors (errors-score target-errors))]

           [est-start-score (errors-score (test-success-start-est-error result))]
           [est-end-score (errors-score (test-success-end-est-error result))])

      (let*-values ([(reals infs) (partition ordinary-value? (map - end-errors start-errors))]
                    [(good-inf bad-inf) (partition positive? infs)])
        (table-row name
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
                        [else "uni-start"]))
                   (test-precondition test)
                   start-score
                   end-score
                   (and target-score target-score)
                   (length good-inf)
                   (length bad-inf)
                   est-start-score
                   est-end-score
                   (program-variables (alt-program (test-success-start-alt result)))
                   (program-body (alt-program (test-success-start-alt result)))
                   (program-body (alt-program (test-success-end-alt result)))
                   (test-result-time result)
                   (test-result-bits result)
                   link)))]
   [(test-failure? result)
    (define status (if (exn:fail:user:herbie? (test-failure-exn result)) "error" "crash"))
    (dummy-table-row result status link)]
   [(test-timeout? result)
    (dummy-table-row result "timeout" link)]))

(define (unparse-result result)
  (match result
    [(test-success test bits time timeline
                   start-alt end-alt points exacts start-est-error end-est-error
                   newpoints newexacts start-error end-error target-error
                   baseline-error oracle-error all-alts)
     `(FPCore ,(test-vars test)
              :herbie-status success
              :herbie-time ,time
              :herbie-bits-used ,bits
              :herbie-error-input
              ([,(*num-points*) ,(errors-score start-est-error)]
               [,(*reeval-pts*) ,(errors-score start-error)])
              :herbie-error-output
              ([,(*num-points*) ,(errors-score end-est-error)]
               [,(*reeval-pts*) ,(errors-score end-error)])
              ,@(if target-error
                    `(:herbie-error-target
                      ([,(*reeval-pts*) ,(errors-score target-error)]))
                    '())
              :name ,(test-name test)
              :precision ,(test-precision test)
              ,@(if (eq? (test-precondition test) 'TRUE)
                    '()
                    `(:pre ,(resugar-program (test-precondition test))))
              ,@(if (test-output test)
                    `(:herbie-target ,(test-output test))
                    '())
              ,(program-body (alt-program end-alt)))]
    [(test-failure test bits time timeline exn)
     `(FPCore ,(test-vars test)
              :herbie-status ,(if (exn:fail:user:herbie? (test-failure-exn result)) 'error 'crash)
              :herbie-time ,time
              :herbie-bits-used ,bits
              :name ,(test-name test)
              :precision ,(test-precision test)
              ,@(if (eq? (test-precondition test) 'TRUE)
                    '()
                    `(:pre ,(test-precondition test)))
              ,@(if (test-output test)
                    `(:herbie-target ,(test-output test))
                    '())
              ,(test-input test))]
    [(test-timeout test bits time timeline)
     `(FPCore ,(test-vars test)
              :herbie-status timeout
              :herbie-time ,time
              :herbie-bits-used ,bits
              :name ,(test-name test)
              :precision ,(test-precision test)
              ,@(if (eq? (test-precondition test) 'TRUE)
                    '()
                    `(:pre ,(test-precondition test)))
              ,@(if (test-output test)
                    `(:herbie-target ,(test-output test))
                    '())
              ,(test-input test))]))
