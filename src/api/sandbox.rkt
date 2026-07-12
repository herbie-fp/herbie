#lang racket

(require racket/engine
         racket/random
         math/flonum
         json)

(require "../syntax/read.rkt"
         "../syntax/platform-state.rkt"
         "../syntax/syntax.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
         "../syntax/load-platform.rkt"
         "../syntax/batch.rkt"
         "../core/localize.rkt"
         "../core/alternative.rkt"
         "../core/compiler.rkt"
         "../utils/common.rkt"
         "datafile.rkt"
         "../utils/errors.rkt"
         "../core/sampling.rkt"
         "../core/mainloop.rkt"
         "../syntax/platform.rkt"
         "../core/programs.rkt"
         "../core/points.rkt"
         "../core/taylor-cover.rkt"
         "../core/explain.rkt"
         "../utils/profile.rkt"
         "../utils/timeline.rkt"
         (submod "../utils/timeline.rkt" debug))

(provide run-herbie
         get-table-data-from-hash
         *reeval-pts*
         (struct-out job-result)
         (struct-out improve-result)
         (struct-out alt-analysis))

(struct job-result (command test status time timeline profile warnings backend))
(struct improve-result (pcontext start target end))
(struct alt-analysis (alt errors) #:prefab)
(struct prepared-test (spec pre batch brfs) #:transparent)

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

(define (prepare-test test)
  (define specification (test-spec test))
  (define precondition (test-pre test))
  (define-values (batch brfs) (progs->batch (list specification) #:ctx (*context*)))
  (prepared-test specification precondition batch brfs))

(define (sort-alt-analyses alts errss)
  (sort (map alt-analysis alts errss)
        (lambda (x y)
          (define x-errs (alt-analysis-errors x))
          (define y-errs (alt-analysis-errors y))
          (define x-score (errors-score x-errs))
          (define y-score (errors-score y-errs))
          (define x-cost (alt-cost (alt-analysis-alt x)))
          (define y-cost (alt-cost (alt-analysis-alt y)))
          (or (< x-score y-score) (and (equal? x-score y-score) (< x-cost y-cost))))))

;; The main Herbie function
(define (get-alternatives test train-pcontext test-pcontext covers)
  (unless train-pcontext
    (error 'get-alternatives "cannnot run without a pcontext"))

  (define initial-expr
    (if (equal? (prog->spec (test-input test)) (test-spec test))
        (test-input test)
        (approx (test-spec test) (test-input test))))
  (define alternatives (run-improve! initial-expr (test-spec test) (*context*) train-pcontext))

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
  (define end-data
    (let* ([report-alts (wrap-taylor-cover-alts alternatives covers)]
           [test-errs (exprs-errors (map alt-expr report-alts) test-pcontext (*context*))])
      (sort-alt-analyses report-alts test-errs)))

  (improve-result test-pcontext start-alt-data target-alt-data end-data))

(define (get-cost test)
  (define cost-proc (platform-cost-proc (*active-platform*)))
  (cost-proc (test-input test)))

(define (get-errors test pcontext)
  (unless pcontext
    (error 'get-errors "cannnot run without a pcontext"))

  (define-values (_ test-pcontext) (partition-pcontext pcontext))
  (define errs (errors (test-input test) test-pcontext (*context*)))
  (for/list ([(pt _) (in-pcontext test-pcontext)]
             [err (in-flvector errs)])
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

(define (sample-points prepared precondition count)
  (define sample
    (parameterize ([*num-points* count])
      (sample-points precondition
                     (prepared-test-batch prepared)
                     (prepared-test-brfs prepared)
                     (list (context-repr (*context*))))))
  (apply mk-pcontext sample))

(define (sample-test-points prepared precondition)
  (sample-points prepared precondition (+ (*num-points*) (*reeval-pts*))))

(define (get-taylor-covers prepared)
  (compute-taylor-covers (prepared-test-spec prepared) (prepared-test-pre prepared) (*context*)))

(define (get-search-sample prepared precondition)
  (define rng-state (pseudo-random-generator->vector (current-pseudo-random-generator)))
  (define old-warnings (set-copy (warnings)))
  (define old-warning-log (warning-log))
  (with-handlers ([exn:fail:user:herbie:sampling?
                   (lambda (_)
                     (current-pseudo-random-generator (vector->pseudo-random-generator rng-state))
                     (warnings old-warnings)
                     (warning-log old-warning-log)
                     #f)])
    (sample-points prepared precondition (*num-points*))))

(define (get-sample test)
  (random) ;; Tick the random number generator, for backwards compatibility
  (define prepared (prepare-test test))
  (sample-test-points prepared (prepared-test-pre prepared)))

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
      (activate-platform! (platform-serialize))
      (set! timeline (*timeline*))
      (when seed
        (set-seed! seed))
      (with-handlers ([exn? (curry on-exception start-time)])
        (timeline-event! 'start) ; Prevents the timeline from being empty.
        (define result
          (match command
            ['alternatives
             (define-values (train-pcontext test-pcontext) (partition-pcontext pcontext))
             (get-alternatives test train-pcontext test-pcontext '())]
            ['cost (get-cost test)]
            ['errors (get-errors test pcontext)]
            ['explanations (get-explanations test pcontext)]
            ['improve
             (random) ;; Tick the random number generator, for backwards compatibility
             (define prepared (prepare-test test))
             (define report-pcontext (sample-test-points prepared (prepared-test-pre prepared)))
             (define-values (report-train-pcontext report-test-pcontext)
               (partition-pcontext report-pcontext))
             (define covers (get-taylor-covers prepared))
             (define search-precondition
               (taylor-covers-precondition (prepared-test-pre prepared) covers))
             (define search-train-pcontext
               (and (pair? covers) (get-search-sample prepared search-precondition)))
             (get-alternatives test
                               (or search-train-pcontext report-train-pcontext)
                               report-test-pcontext
                               (if search-train-pcontext
                                   covers
                                   '()))]
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
  (begin0 (parameterize ([current-custodian run-custodian])
            (define eng (engine in-engine))
            (if (engine-run (*timeout*) eng)
                (engine-result eng)
                (on-timeout)))
    (custodian-shutdown-all run-custodian)))

(define (dummy-table-row-from-hash result-hash status link)
  (define test (load-test (open-input-string (hash-ref result-hash 'test))))
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
  (define test (load-test (open-input-string (hash-ref result-hash 'test))))
  (define backend (hash-ref result-hash 'backend))
  (define status (hash-ref result-hash 'status))
  (match status
    ["success"
     (define start (hash-ref backend 'start))
     (define targets (hash-ref backend 'target))
     (define end (hash-ref backend 'end))

     ; starting expr analysis
     (define start-expr (read (open-input-string (hash-ref start 'expr))))
     (define start-score (errors-score (list->flvector (hash-ref start 'errors))))
     (define start-cost (hash-ref start 'cost))

     (define target-cost-score
       (for/list ([target targets])
         (define target-expr (read (open-input-string (hash-ref target 'expr))))
         (define tar-cost (hash-ref target 'cost))
         (define tar-score (errors-score (list->flvector (hash-ref target 'errors))))

         (list tar-cost tar-score)))

     ; Important to calculate value of status
     (define best-score
       (if (null? target-cost-score)
           target-cost-score
           (apply min (map second target-cost-score))))

     (define end-exprs
       (for/list ([end-analysis (in-list end)])
         (read (open-input-string (hash-ref end-analysis 'expr)))))
     (define end-expr-strings (map (curryr hash-ref 'expr) end))
     (define end-scores
       (for/list ([end-analysis (in-list end)])
         (errors-score (list->flvector (hash-ref end-analysis 'errors)))))
     (define end-costs (map (curryr hash-ref 'cost) end))

     ; terribly formatted pareto-optimal frontier
     (define (round3 x)
       (/ (round (* x 1000)) 1000.0))
     (define cost&accuracy
       (list (list (round3 start-cost) (round3 start-score))
             (list (round3 (car end-costs)) (round3 (car end-scores)) (car end-expr-strings))
             (map (λ (c s expr) (list (round3 c) (round3 s) expr))
                  (cdr end-costs)
                  (cdr end-scores)
                  (cdr end-expr-strings))))

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
