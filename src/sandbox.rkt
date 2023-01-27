#lang racket
(require profile math/bigfloat racket/engine json)
(require "syntax/read.rkt" "syntax/sugar.rkt" "syntax/types.rkt"
         "alternative.rkt" "common.rkt" "conversions.rkt" "cost.rkt"
         "datafile.rkt" "errors.rkt"
         "mainloop.rkt" "preprocess.rkt" "points.rkt" "profile.rkt"
         "programs.rkt" "timeline.rkt" (submod "timeline.rkt" debug))

(provide get-test-result *reeval-pts* *timeout*
         (struct-out test-result) (struct-out test-success)
         (struct-out test-failure) (struct-out test-timeout)
         get-table-data unparse-result)

;; These cannot move between threads!
(struct test-result (test bits time timeline warnings))
(struct test-success test-result
  (start-alt end-alts preprocess points exacts
   start-est-error end-est-error newpoints newexacts
   start-error end-errors target-error
   start-cost end-costs all-alts))
(struct test-failure test-result (exn))
(struct test-timeout test-result ())

(define *reeval-pts* (make-parameter 8000))
(define *timeout* (make-parameter (* 1000 60 5/2)))

;; true if Racket CS <= 8.2
(define cs-places-workaround?
  (let ([major (string->number (substring (version) 0 1))]
        [minor (string->number (substring (version) 2 3))]
        [rest  (substring (version) 3)])
    (or (< major 8)
        (and (= major 8) (< minor 2))
        (and (= major 8) (= minor 2) (zero? (string-length rest))))))

(define (get-p&es context)
  (for/lists (pts exs)
      ([(pt ex) (in-pcontext context)])
    (values pt ex)))

(define (get-sample test)
  (define output-repr (test-output-repr test))
  (define context (test-context test))
  (*needed-reprs* (list output-repr (get-representation 'bool)))

  (match-define (cons domain-stats joint-pcontext)
    (parameterize ([*num-points* (+ (*num-points*) (*reeval-pts*))])
      (setup-context!
        (or (test-specification test) (test-program test)) (test-precondition test)
        output-repr)))
  (define-values (train-pcontext test-pcontext)
    (split-pcontext joint-pcontext (*num-points*) (*reeval-pts*))) 

  (define-values (points exacts) (get-p&es test-pcontext))
  (for/list ([point points] [exact exacts]) (list point exact)))

(define (run-herbie test)
  (define seed (get-seed))
  (random) ;; Child process uses deterministic but different seed from evaluator
  
  (define output-repr (test-output-repr test))
  (define context (test-context test))
  (*needed-reprs* (list output-repr (get-representation 'bool)))
  (generate-prec-rewrites (test-conversions test))

  (match-define (cons domain-stats joint-pcontext)
                (parameterize ([*num-points* (+ (*num-points*) (*reeval-pts*))])
                  (setup-context!
                   (or (test-specification test) (test-program test)) (test-precondition test)
                   output-repr)))
  (timeline-push! 'bogosity domain-stats)
  (define-values (train-pcontext test-pcontext)
    (split-pcontext joint-pcontext (*num-points*) (*reeval-pts*))) 

  (define alts
    (run-improve! (test-program test) train-pcontext (*num-iterations*)
                  #:specification (test-specification test)
                  #:preprocess (test-preprocess test)))

  (when seed (set-seed! seed))
  (define processed-test-pcontext
    (preprocess-pcontext test-pcontext (*herbie-preprocess*) context))

  (define end-errs
    (flip-lists
     (batch-errors (map alt-program alts) processed-test-pcontext context)))

  (timeline-adjust! 'regimes 'name (test-name test))
  (timeline-adjust! 'regimes 'link ".")

  (define-values (points exacts) (get-p&es train-pcontext))
  (define-values (newpoints newexacts) (get-p&es processed-test-pcontext))
  (test-success test
                (bf-precision)
                #f
                (timeline-extract)
                warning-log (make-alt (test-program test)) alts
                (*herbie-preprocess*) points exacts
                (errors (test-program test) train-pcontext context)
                (errors (alt-program (car alts)) train-pcontext context)
                newpoints newexacts
                (errors (test-program test) processed-test-pcontext context)
                end-errs
                (if (test-output test)
                    (errors (test-target test) processed-test-pcontext context)
                    #f)
                (program-cost (test-program test) output-repr)
                (map (curryr alt-cost output-repr) alts)
                (*all-alts*)))

;; Ugly, but struct-copy doesn't do the right thing with inheritance
(define (add-time result time)
  (match-define (test-success test bits _time timeline warnings
                              start-alt end-alts preprocess points exacts
                              start-est-error end-est-error newpoints newexacts
                              start-error end-errors target-error
                              start-cost end-costs all-alts) result)
  (test-success test bits time timeline warnings
                start-alt end-alts preprocess points exacts
                start-est-error end-est-error newpoints newexacts
                start-error end-errors target-error
                start-cost end-costs all-alts))

(define (get-test-result command test #:seed [seed #f] #:profile [profile? #f])
  (define timeline #f)

  (define (compute-result test)
    (parameterize ([*timeline-disabled* false]
                   [*warnings-disabled* true])
      (define start-time (current-inexact-milliseconds))
      (rollback-improve!)
      (when seed (set-seed! seed))
      (with-handlers ([exn? (curry on-exception start-time)])
        (define out
          (match command
            ['improve (run-herbie test)]
            ['sample (get-sample test)]))
        (print-warnings)
        (if (eq? command 'sample) out (add-time out (- (current-inexact-milliseconds) start-time))))))

  (define (on-exception start-time e)
    (parameterize ([*timeline-disabled* false])
      (timeline-event! 'end))
    (print-warnings)
    (test-failure test (bf-precision)
                  (- (current-inexact-milliseconds) start-time) (timeline-extract)
                  warning-log e))

  (define (in-engine _)
    (set! timeline *timeline*)
    (if profile?
        (profile-thunk
         (λ () (compute-result test))
         #:order 'total
         #:render (λ (p order) (write-json (profile->json p) profile?)))
        (compute-result test)))

  ;; CS versions <= 8.2: problems with scheduler cause places to stay
  ;; in a suspended state
  (when cs-places-workaround?
    (thread (lambda () (sync (system-idle-evt)))))

  (define eng (engine in-engine))
  (if (engine-run (*timeout*) eng)
      (engine-result eng)
      (parameterize ([*timeline-disabled* false])
        (timeline-load! timeline)
        (timeline-compact! 'outcomes)
        (print-warnings)
        (test-timeout test (bf-precision) (*timeout*) (timeline-extract) '()))))

(define (dummy-table-row result status link)
  (define test (test-result-test result))
  (define repr (test-output-repr test))
  (table-row (test-name test) (test-identifier test) status
             (resugar-program (program-body (test-precondition test)) repr)
             (if (test-success? result) (test-success-preprocess result) (test-preprocess test))
             (representation-name (test-output-repr test))
             (map (curry map representation-name) (test-conversions test))
             (test-vars test)
             (resugar-program (test-input test) repr) #f
             (resugar-program (test-spec test) repr)
             (and (test-output test) (resugar-program (test-output test) repr))
             #f #f #f #f #f (test-result-time result)
             (test-result-bits result) link '()))

(define (get-table-data result link)
  (define test (test-result-test result))
  (cond
   [(test-success? result)
    (define name (test-name test))
    (define start-errors  (test-success-start-error result))
    (define end-errorss   (test-success-end-errors result))
    (define target-errors (test-success-target-error result))
    (define start-prog    (alt-program (test-success-start-alt result)))
    (define end-progs     (map alt-program (test-success-end-alts result)))
    (define costs         (test-success-end-costs result))

    (define start-score (errors-score start-errors))
    (define end-scores (map errors-score end-errorss))
    (define end-score (car end-scores))
    (define target-score (and target-errors (errors-score target-errors)))
    (define est-start-score (errors-score (test-success-start-est-error result)))
    (define est-end-score (errors-score (test-success-end-est-error result)))
    (define end-exprs (map (λ (p) (program-body (resugar-program p (test-output-repr test)))) end-progs))

    (define cost&accuracy
      (list (list (program-cost start-prog (test-output-repr test)) start-score)
            (list (car costs) (car end-scores))
            (map list (cdr costs) (cdr end-scores) (cdr end-exprs))))

    (define fuzz 0.1)
    (define status
      (if target-score
          (cond
           [(< end-score (- target-score fuzz)) "gt-target"]
           [(< end-score (+ target-score fuzz)) "eq-target"]
           [(> end-score (+ start-score fuzz)) "lt-start"]
           [(> end-score (- start-score fuzz)) "eq-start"]
           [(> end-score (+ target-score fuzz)) "lt-target"])
          (cond
           [(and (< start-score 1) (< end-score (+ start-score 1))) "ex-start"]
           [(< end-score (- start-score 1)) "imp-start"]
           [(< end-score (+ start-score fuzz)) "apx-start"]
           [else "uni-start"])))

    (struct-copy table-row (dummy-table-row result status link)
                 [output (car end-exprs)]
                 [start start-score] [result end-score] [target target-score]
                 [start-est est-start-score] [result-est est-end-score]
                 [cost-accuracy cost&accuracy])]
   [(test-failure? result)
    (define status (if (exn:fail:user:herbie? (test-failure-exn result)) "error" "crash"))
    (dummy-table-row result status link)]
   [(test-timeout? result)
    (dummy-table-row result "timeout" link)]))

(define (unparse-result row)
  (define top
    (if (table-row-identifier row)
        (list (table-row-identifier row) (table-row-vars row))
        (list (table-row-vars row))))
  `(FPCore ,@top
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
     :herbie-conversions ,(table-row-conversions row)
     ,@(if (eq? (table-row-pre row) 'TRUE) '() `(:pre ,(table-row-pre row)))
     ,@(if (equal? (table-row-preprocess row) empty) '() `(:herbie-preprocess ,(table-row-preprocess row)))
     ,@(if (table-row-target-prog row) `(:herbie-target ,(table-row-target-prog row)) '())
     ,(table-row-output row)))
