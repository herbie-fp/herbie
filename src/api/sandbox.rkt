#lang racket

(require profile
         racket/engine
         json)

(require "../syntax/read.rkt"
         "../syntax/syntax.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
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
         "../core/points.rkt"
         "../core/explain.rkt"
         "../core/preprocess.rkt"
         "../utils/profile.rkt"
         "../utils/timeline.rkt"
         (submod "../utils/timeline.rkt" debug))

(provide run-herbie
         get-table-data-from-hash
         *reeval-pts*
         *timeout*
         (struct-out job-result)
         (struct-out improve-result)
         (struct-out alt-analysis))

(struct job-result (command test status time timeline profile warnings backend))
(struct improve-result (preprocess pctxs start target end))
(struct alt-analysis (alt train-errors test-errors) #:prefab)

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
  ;; TODO: Ignoring all user-provided preprocessing right now
  (define alternatives (run-improve! (test-input test) (test-spec test) (*context*) train-pcontext))

  (define preprocessing (alt-preprocessing (first alternatives)))
  (define test-pcontext* (preprocess-pcontext (*context*) test-pcontext preprocessing))

  ;; compute error/cost for input expression
  (define start-expr (test-input test))
  (define start-alt (make-alt-preprocessing start-expr (test-preprocess test)))
  (define start-train-errs (errors start-expr train-pcontext (*context*)))
  (define start-test-errs (errors start-expr test-pcontext* (*context*)))
  (define start-alt-data (alt-analysis start-alt start-train-errs start-test-errs))

  ;; optionally compute error/cost for input expression
  (define target-alt-data
    ;; When in platform, evaluate error
    (for/list ([(expr is-valid?) (in-dict (test-output test))]
               #:when is-valid?)
      (define target-expr (fpcore->prog expr (*context*)))
      (define target-train-errs (errors target-expr train-pcontext (*context*)))
      (define target-test-errs (errors target-expr test-pcontext* (*context*)))

      (alt-analysis (make-alt target-expr) target-train-errs target-test-errs)))

  ;; compute error/cost for output expression
  (define end-exprs (map alt-expr alternatives))
  (define end-train-errs (batch-errors end-exprs train-pcontext (*context*)))
  (define end-test-errs (batch-errors end-exprs test-pcontext* (*context*)))
  (define end-data (map alt-analysis alternatives end-train-errs end-test-errs))

  ;; bundle up the result
  (timeline-adjust! 'regimes 'name (test-name test))
  (timeline-adjust! 'regimes 'link ".")

  (define pctxs (list train-pcontext test-pcontext*))
  (improve-result preprocessing pctxs start-alt-data target-alt-data end-data))

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
    (list pt err)))

(define (get-explanations test pcontext)
  (unless pcontext
    (error 'explain "cannot run without a pcontext"))

  (*pcontext* pcontext)
  (define-values (fperrors
                  sorted-explanations-table
                  confusion-matrix
                  maybe-confusion-matrix
                  total-confusion-matrix
                  freqs)
    (explain (test-input test) (*context*) (*pcontext*)))

  sorted-explanations-table)

;; Given a test and a sample of points, computes the local error at every node in the expression
;; returning a tree of errors that mirrors the structure of the expression.
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-local-error test pcontext)
  (unless pcontext
    (error 'get-local-error "cannnot run without a pcontext"))

  (*pcontext* pcontext)
  (local-error-as-tree (test-input test) (*context*)))

(define (get-sample test)
  (random) ;; Tick the random number generator, for backwards compatibility
  (define specification (prog->spec (or (test-spec test) (test-input test))))
  (define precondition (prog->spec (test-pre test)))
  (define sample
    (parameterize ([*num-points* (+ (*num-points*) (*reeval-pts*))])
      (sample-points precondition (list specification) (list (*context*)))))
  (apply mk-pcontext sample))

;;
;;  Public interface
;;

(define (run-herbie command
                    test
                    #:seed [seed #f]
                    #:pcontext [pcontext #f]
                    #:profile? [profile? #f]
                    #:timeline-disabled? [timeline-disabled? #f])
  (define timeline #f)
  (define profile #f)

  (define (on-exception start-time e)
    (parameterize ([*timeline-disabled* timeline-disabled?])
      (timeline-event! 'end)
      (define time (- (current-inexact-milliseconds) start-time))
      (match command
        ['improve (job-result command test 'failure time (timeline-extract) #f (warning-log) e)]
        [_ (raise e)])))

  (define (on-timeout)
    (parameterize ([*timeline-disabled* timeline-disabled?])
      (timeline-load! timeline)
      (timeline-event! 'end)
      (match command
        ['improve
         (job-result command test 'timeout (*timeout*) (timeline-extract) #f (warning-log) #f)]
        [_ (error 'run-herbie "command ~a timed out" command)])))

  (define (compute-result)
    (parameterize ([*timeline-disabled* timeline-disabled?])
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
            [_ (error 'compute-result "unknown command ~a" command)]))
        (timeline-event! 'end)
        (define time (- (current-inexact-milliseconds) start-time))
        (job-result command test 'success time (timeline-extract) #f (warning-log) result))))

  (define (in-engine _)
    (cond
      [profile?
       (define result
         (profile-thunk compute-result
                        #:order 'total
                        #:delay 0.05
                        #:render (λ (p order) (set! profile (profile->json p)))))
       (struct-copy job-result result [profile profile])]
      [else (compute-result)]))

  ;; Branch on whether or not we should run inside an engine
  (define eng (engine in-engine))
  (if (engine-run (*timeout*) eng)
      (engine-result eng)
      (on-timeout)))

(define (dummy-table-row-from-hash result-hash status link)
  (define test (car (load-tests (open-input-string (hash-ref result-hash 'test)))))
  (define repr (test-output-repr test))
  (define preprocess
    (if (eq? (hash-ref result-hash 'status) 'success)
        (hash-ref (hash-ref result-hash 'backend) 'preprocessing)
        (test-preprocess test)))
  (table-row (test-name test)
             (test-identifier test)
             status
             (prog->fpcore (test-pre test) (test-context test))
             preprocess
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
     (define start-train-score (errors-score (hash-ref start 'train-score)))
     (define start-test-score (errors-score (hash-ref start 'errors)))
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
     (define end-train-scores
       (for/list ([end-analysis (in-list end)])
         (errors-score (hash-ref end-analysis 'train-score))))
     (define end-test-scores
       (for/list ([end-analysis (in-list end)])
         (errors-score (hash-ref end-analysis 'errors))))
     (define end-costs (map (curryr hash-ref 'cost) end))

     ; terribly formatted pareto-optimal frontier
     (define cost&accuracy
       (list (list start-cost start-test-score)
             (list (car end-costs) (car end-test-scores))
             (map list (cdr end-costs) (cdr end-test-scores) (cdr end-exprs))))

     (define fuzz 0.1)
     (define end-est-score (car end-train-scores))
     (define end-score (car end-test-scores))
     (define status
       (cond
         [(not (null? best-score))
          (cond
            [(< end-score (- best-score fuzz)) "gt-target"]
            [(< end-score (+ best-score fuzz)) "eq-target"]
            [(> end-score (+ start-test-score fuzz)) "lt-start"]
            [(> end-score (- start-test-score fuzz)) "eq-start"]
            [(> end-score (+ best-score fuzz)) "lt-target"])]

         [else
          (cond
            [(and (< start-test-score 1) (< end-score (+ start-test-score 1))) "ex-start"]
            [(< end-score (- start-test-score 1)) "imp-start"]
            [(< end-score (+ start-test-score fuzz)) "apx-start"]
            [else "uni-start"])]))

     (struct-copy table-row
                  (dummy-table-row-from-hash result-hash status link)
                  [start-est start-train-score]
                  [start start-test-score]
                  [target target-cost-score]
                  [result-est end-est-score]
                  [result end-score]
                  [output (car end-exprs)]
                  [cost-accuracy cost&accuracy])]
    ["failure"
     (match-define (list 'exn type _ ...) backend)
     (define status (if type "error" "crash"))
     (dummy-table-row-from-hash result-hash status link)]
    ["timeout" (dummy-table-row-from-hash result-hash "timeout" link)]))
