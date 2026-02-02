#lang racket

(require racket/engine
         json)

(require "../syntax/read.rkt"
         "../syntax/syntax.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
         "../syntax/load-platform.rkt"
         "../syntax/batch.rkt"
         "../core/localize.rkt"
         "../utils/alternative.rkt"
         "../core/compiler.rkt"
         "../utils/common.rkt"
         "datafile.rkt"
         "../utils/errors.rkt"
         "../utils/float.rkt"
         "../core/sampling.rkt"
         "../core/mainloop.rkt"
         "../syntax/platform.rkt"
         "../core/programs.rkt"
         "../core/points.rkt"
         "../core/explain.rkt"
         "../utils/profile.rkt"
         "../utils/timeline.rkt"
         (submod "../utils/timeline.rkt" debug))

(provide run-herbie
         get-table-data-from-hash
         *reeval-pts*
         (struct-out job-result)
         (struct-out improve-result)
         (struct-out alt-analysis)
         get-spec-sample)

(struct job-result (command test status time timeline profile warnings backend))
(struct improve-result (pcontext start target end))
(struct alt-analysis (alt errors) #:prefab)

;; API users can supply their own, weird set of points, in which case
;; the first 256 are training points and everything is test points.
;; For backwards compatibility, exactly 8256 points are split as
;; Herbie expects (first 256 training, rest are test).

(define (partition-pcontext joint-pcontext)
  (define num-points (pcontext-length joint-pcontext))
  (cond
    [(= num-points (+ (*num-points*) (*reeval-pts*)))
     (split-pcontext joint-pcontext (*num-points*) (*reeval-pts*))]
    [else
     ; the training set will just be up to the first (*num-points*)
     ; the testing set will just be the entire set
     (define training-count (min (*num-points*) num-points))
     (define testing-count (- num-points training-count))
     (define-values (train-pcontext _) (split-pcontext joint-pcontext training-count testing-count))
     (values train-pcontext joint-pcontext)]))

;; API Functions

;; The main Herbie function
(define (get-alternatives test joint-pcontext)
  (unless joint-pcontext
    (error 'get-alternatives "cannnot run without a pcontext"))

  (define-values (train-pcontext test-pcontext) (partition-pcontext joint-pcontext))
  (define alternatives (run-improve! (test-input test) (test-spec test) (*context*) train-pcontext))

  ;; compute error/cost for input expression
  (define start-expr (test-input test))
  (define start-alt (make-alt start-expr))
  (define start-errs (errors start-expr test-pcontext (*context*)))
  (define start-alt-data (alt-analysis start-alt start-errs))

  ;; optionally compute error/cost for input expression
  (define target-alt-data
    ;; When in platform, evaluate error
    (for/list ([(expr is-valid?) (in-dict (test-output test))]
               #:when is-valid?)
      (define target-expr (fpcore->prog expr (*context*)))
      (define target-errs (errors target-expr test-pcontext (*context*)))
      (alt-analysis (make-alt target-expr) target-errs)))

  ;; compute error/cost for output expression
  ;; and sort alternatives by accuracy + cost on testing subset
  (define test-errs (exprs-errors (map alt-expr alternatives) test-pcontext (*context*)))
  (define sorted-end-exprs (sort-alts alternatives test-errs))
  (define end-exprs (map (compose alt-expr car) sorted-end-exprs))
  (define end-errs (map cdr sorted-end-exprs))
  (define end-data (map alt-analysis alternatives end-errs))

  (improve-result test-pcontext start-alt-data target-alt-data end-data))

(define (get-cost test)
  (define cost-proc (platform-cost-proc (*active-platform*)))
  (define output-repr (context-repr (*context*)))
  (cost-proc (test-input test) output-repr))

(define (get-errors test pcontext)
  (unless pcontext
    (error 'get-errors "cannnot run without a pcontext"))

  (define-values (_ test-pcontext) (partition-pcontext pcontext))
  (define errs (errors (test-input test) test-pcontext (*context*)))
  (for/list ([(pt _) (in-pcontext test-pcontext)]
             [err (in-list errs)])
    (cons pt err)))

(define (get-explanations test pcontext)
  (unless pcontext
    (error 'explain "cannot run without a pcontext"))

  (define-values (fperrors
                  sorted-explanations-table
                  confusion-matrix
                  maybe-confusion-matrix
                  total-confusion-matrix
                  freqs)
    (explain (test-input test) (*context*) pcontext))

  sorted-explanations-table)

;; Given a test and a sample of points, computes the local error at every node in the expression
;; returning a tree of errors that mirrors the structure of the expression.
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-local-error test pcontext)
  (unless pcontext
    (error 'get-local-error "cannnot run without a pcontext"))

  (local-error-as-tree (test-input test) (*context*) pcontext))

(define (get-sample test)
  (random) ;; Tick the random number generator, for backwards compatibility
  (define specification (prog->spec (or (test-spec test) (test-input test))))
  (define precondition (prog->spec (test-pre test)))
  (define-values (batch brfs) (progs->batch (list specification)))
  (define sample
    (parameterize ([*num-points* (+ (*num-points*) (*reeval-pts*))])
      (sample-points precondition batch brfs (list (*context*)))))
  (apply mk-pcontext sample))

(define (get-spec-sample spec)
  (random) ;; Tick the random number generator, for backwards compatibility
  (define sample
    (parameterize ([*num-points* (+ (*num-points*) (*reeval-pts*))])
      (sample-points `(TRUE) (list spec) (list (*context*)))))
  (apply mk-pcontext sample))

;;
;;  Public interface
;;

(define (run-herbie command
                    test
                    #:seed [seed #f]
                    #:pcontext [pcontext #f]
                    #:profile? [profile? #f]
                    #:timeline? [timeline? #f])
  (define timeline #f)
  (define profile #f)

  (define (on-exception start-time e)
    (parameterize ([*timeline-disabled* (not timeline?)])
      (timeline-event! 'end)
      (define time (- (current-inexact-milliseconds) start-time))
      (match command
        ['improve (job-result command test 'failure time (timeline-extract) #f (warning-log) e)]
        [_ (raise e)])))

  (define (on-timeout)
    (parameterize ([*timeline-disabled* (not timeline?)])
      (timeline-load! timeline)
      (timeline-event! 'end)
      (match command
        ['improve
         (job-result command test 'timeout (*timeout*) (timeline-extract) #f (warning-log) #f)]
        [_ (raise-arguments-error 'run-herbie "command timed out" "command" command)])))

  (define (compute-result)
    (parameterize ([*timeline-disabled* (not timeline?)])
      (define start-time (current-inexact-milliseconds))
      (reset!)
      (*context* (test-context test))
      (activate-platform! (*platform-name*))
      (set! timeline (*timeline*))
      (when seed
        (set-seed! seed))
      (with-handlers ([exn? (curry on-exception start-time)])
        (timeline-event! 'start) ; Prevents the timeline from being empty.
        (define result
          (match command
            ['alternatives (get-alternatives test pcontext)]
            ['cost (get-cost test)]
            ['errors (get-errors test pcontext)]
            ['explanations (get-explanations test pcontext)]
            ['improve (get-alternatives test (get-sample test))]
            ['local-error (get-local-error test pcontext)]
            ['sample (get-sample test)]
            [_ (raise-arguments-error 'compute-result "unknown command" "command" command)]))
        (timeline-event! 'end)
        (define time (- (current-inexact-milliseconds) start-time))
        (job-result command test 'success time (timeline-extract) #f (warning-log) result))))

  (define (in-engine _)
    (cond
      [profile?
       (define result (profile-thunk compute-result (λ (p) (set! profile (profile->json p)))))
       (struct-copy job-result result [profile profile])]
      [else (compute-result)]))

  (define run-custodian (make-custodian))
  ;; Branch on whether or not we should run inside an engine
  (begin0 (parameterize ([current-custodian run-custodian])
            (define eng (engine in-engine))
            (if (engine-run (*timeout*) eng)
                (engine-result eng)
                (on-timeout)))
    (custodian-shutdown-all run-custodian)))

(define (dummy-table-row-from-hash result-hash status link)
  (define test (car (load-tests (open-input-string (hash-ref result-hash 'test)))))
  (define repr (test-output-repr test))
  (table-row (test-name test)
             (test-identifier test)
             status
             (prog->fpcore (test-pre test) (test-context test))
             (representation-name repr)
             '() ; TODO: eliminate field
             (test-vars test)
             (map car (hash-ref result-hash 'warnings))
             (prog->fpcore (test-input test) (test-context test))
             #f
             (prog->fpcore (test-spec test) (test-context test))
             (test-output test)
             #f
             #f
             #f
             (hash-ref result-hash 'time)
             link
             '()))

(define (get-table-data-from-hash result-hash link)
  (define test (car (load-tests (open-input-string (hash-ref result-hash 'test)))))
  (define backend (hash-ref result-hash 'backend))
  (define status (hash-ref result-hash 'status))
  (match status
    ["success"
     (define start (hash-ref backend 'start))
     (define targets (hash-ref backend 'target))
     (define end (hash-ref backend 'end))
     (define expr-cost (platform-cost-proc (*active-platform*)))
     (define repr (test-output-repr test))

     ; starting expr analysis
     (define start-expr (read (open-input-string (hash-ref start 'expr))))
     (define start-score (errors-score (hash-ref start 'errors)))
     (define start-cost (hash-ref start 'cost))

     (define target-cost-score
       (for/list ([target targets])
         (define target-expr (read (open-input-string (hash-ref target 'expr))))
         (define tar-cost (hash-ref target 'cost))
         (define tar-score (errors-score (hash-ref target 'errors)))

         (list tar-cost tar-score)))

     ; Important to calculate value of status
     (define best-score
       (if (null? target-cost-score)
           target-cost-score
           (apply min (map second target-cost-score))))

     (define end-exprs
       (for/list ([end-analysis (in-list end)])
         (read (open-input-string (hash-ref end-analysis 'expr)))))
     (define end-scores
       (for/list ([end-analysis (in-list end)])
         (errors-score (hash-ref end-analysis 'errors))))
     (define end-costs (map (curryr hash-ref 'cost) end))

     ; terribly formatted pareto-optimal frontier
     (define (round3 x)
       (/ (round (* x 1000)) 1000.0))
     (define cost&accuracy
       (list (list (round3 start-cost) (round3 start-score))
             (list (round3 (car end-costs)) (round3 (car end-scores)))
             (map (λ (c s) (list (round3 c) (round3 s))) (cdr end-costs) (cdr end-scores))))

     (define fuzz 0.1)
     (define end-score (car end-scores))
     (define status
       (cond
         [(not (null? best-score))
          (cond
            [(< end-score (- best-score fuzz)) "gt-target"]
            [(< end-score (+ best-score fuzz)) "eq-target"]
            [(> end-score (+ start-score fuzz)) "lt-start"]
            [(> end-score (- start-score fuzz)) "eq-start"]
            [(> end-score (+ best-score fuzz)) "lt-target"])]

         [(and (< start-score 1) (< end-score (+ start-score 1))) "ex-start"]
         [(< end-score (- start-score 1)) "imp-start"]
         [(< end-score (+ start-score fuzz)) "apx-start"]
         [else "uni-start"]))

     (struct-copy table-row
                  (dummy-table-row-from-hash result-hash status link)
                  [start start-score]
                  [target target-cost-score]
                  [result end-score]
                  [output (car end-exprs)]
                  [cost-accuracy cost&accuracy])]
    ["failure"
     (match-define (list 'exn type _ ...) backend)
     (define status (if type "error" "crash"))
     (dummy-table-row-from-hash result-hash status link)]
    ["timeout" (dummy-table-row-from-hash result-hash "timeout" link)]))
