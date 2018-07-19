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
         (struct-out test-result) (struct-out test-failure) (struct-out test-timeout)
         get-table-data unparse-result)


; For things that don't leave a thread
(struct test-result
  (test time bits
   start-alt end-alt points exacts start-est-error end-est-error
   newpoints newexacts start-error end-error target-error timeline))
(struct test-failure (test bits exn time timeline))
(struct test-timeout (test bits time timeline))

(define *reeval-pts* (make-parameter 8000))
(define *timeout* (make-parameter (* 1000 60 10)))

(define (get-p&es context)
  (call-with-values
      (λ ()
        (for/lists (pts exs)
            ([(pt ex) (in-pcontext context)])
          (values pt ex)))
    list))

(define (get-test-result test #:seed [seed #f] #:setup! [setup! #f]
                         #:profile [profile? #f] #:debug [debug? #f])
  (define (on-error e) `(error ,e ,(bf-precision)))

  (define (compute-result test)
    (parameterize ([*debug-port* (or debug? (*debug-port*))])
      (when seed (set-seed! seed))
      (random) ;; Child process uses deterministic but different seed from evaluator
      (when setup! (setup!))
      (with-handlers ([exn? on-error])
        (define alt
          (run-improve (test-program test)
                       (*num-iterations*)
                       #:precondition (test-precondition test)))
        (define context (*pcontext*))
        (when seed (set-seed! seed))
        (define newcontext
          (parameterize ([*num-points* (*reeval-pts*)])
            (prepare-points (test-program test) (test-precondition test))))
        `(good ,(make-alt (test-program test)) ,alt ,context ,newcontext
               ,(^timeline^) ,(bf-precision)))))

  (define (in-engine _)
    (if profile?
        (parameterize ([current-output-port (or profile? (current-output-port))])
          (profile (compute-result test)))
        (compute-result test)))
  
  (let* ([start-time (current-inexact-milliseconds)] [eng (engine in-engine)])
    (engine-run (*timeout*) eng)

    (match (engine-result eng)
      [`(good ,start ,end ,context ,newcontext ,timeline ,bits)
       (match-define (list newpoints newexacts) (get-p&es newcontext))
       (match-define (list points exacts) (get-p&es context))
       (test-result test 
                    (- (current-inexact-milliseconds) start-time)
                    bits
                    start end points exacts
                    (errors (alt-program start) context)
                    (errors (alt-program end) context)
                    newpoints newexacts
                    (errors (alt-program start) newcontext)
                    (errors (alt-program end) newcontext)
                    (if (test-output test)
                        (errors (test-target test) newcontext)
                        #f)
                    timeline)]
      [`(error ,e ,bits)
       (test-failure test bits e (- (current-inexact-milliseconds) start-time) (^timeline^))]
      [#f
       (test-timeout test (bf-precision) (*timeout*) (^timeline^))])))

(define (get-table-data result link)
  (cons (unparse-result result) (get-table-data* result link)))

(define (get-table-data* result link)
  (cond
   [(test-result? result)
    (let* ([name (test-name (test-result-test result))]
           [start-errors  (test-result-start-error  result)]
           [end-errors    (test-result-end-error    result)]
           [target-errors (test-result-target-error result)]

           [start-score (errors-score start-errors)]
           [end-score (errors-score end-errors)]
           [target-score (and target-errors (errors-score target-errors))]

           [est-start-score (errors-score (test-result-start-est-error result))]
           [est-end-score (errors-score (test-result-end-est-error result))])

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
                   start-score
                   end-score
                   (and target-score target-score)
                   (length good-inf)
                   (length bad-inf)
                   est-start-score
                   est-end-score
                   (program-variables (alt-program (test-result-start-alt result)))
                   (program-body (alt-program (test-result-start-alt result)))
                   (program-body (alt-program (test-result-end-alt result)))
                   (test-result-time result)
                   (test-result-bits result)
                   link)))]
   [(test-failure? result)
    (define test (test-failure-test result))
    (table-row (test-name test) (if (exn:fail:user:herbie? (test-failure-exn result)) "error" "crash")
               #f #f #f #f #f #f #f (test-vars test) (test-input test) #f
               (test-failure-time result) (test-failure-bits result) link)]
   [(test-timeout? result)
    (define test (test-timeout-test result))
    (table-row (test-name (test-timeout-test result)) "timeout"
               #f #f #f #f #f #f #f (test-vars test) (test-input test) #f
               (test-timeout-time result) (test-timeout-bits result) link)]))

(define (unparse-result result)
  (match result
    [(test-result test time bits
                  start-alt end-alt points exacts start-est-error end-est-error
                  newpoints newexacts start-error end-error target-error timeline)
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
              ,@(if (eq? (test-precondition test) 'TRUE)
                    '()
                    `(:pre ,(test-precondition test)))
              ,@(if (test-output test)
                    `(:herbie-target ,(test-output test))
                    '())
              ,(program-body (alt-program end-alt)))]
    [(test-failure test bits exn time timeline)
     `(FPCore ,(test-vars test)
              :herbie-status ,(if (exn:fail:user:herbie? (test-failure-exn result)) 'error 'crash)
              :herbie-time ,time
              :herbie-bits-used ,bits
              :name ,(test-name test)
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
              ,@(if (eq? (test-precondition test) 'TRUE)
                    '()
                    `(:pre ,(test-precondition test)))
              ,@(if (test-output test)
                    `(:herbie-target ,(test-output test))
                    '())
              ,(test-input test))]))
