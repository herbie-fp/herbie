#lang racket

(require profile math/bigfloat racket/engine json rival)
(require "syntax/read.rkt" "syntax/sugar.rkt" "syntax/types.rkt"
         "alternative.rkt" "common.rkt" "conversions.rkt" "cost.rkt"
         "datafile.rkt" "errors.rkt" "float.rkt" "sampling.rkt"
         "mainloop.rkt" "preprocess.rkt" "points.rkt" "profile.rkt"
         "programs.rkt" "timeline.rkt" (submod "timeline.rkt" debug)
         "core/localize.rkt" "ground-truth.rkt")

(provide get-alternatives get-calculation get-cost get-errors
         get-exacts get-local-error get-sample get-test-result
         (struct-out job-result) (struct-out improve-result) (struct-out alt-analysis)
         get-table-data unparse-result *reeval-pts* *timeout*)

(struct job-result (test status time timeline warnings backend))
(struct improve-result (preprocess pctxs start target end))
(struct alt-analysis (alt train-errors test-errors))

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

(define (get-exacts test pts)
  (define repr (test-output-repr test))
  (define starting-precision (*starting-prec*))
  (define <-bf (representation-bf->repr repr))
  (define fn (make-search-func (test-pre test) (list (test-input test)) (test-context test)))
  (for/list ([pt pts])
    (define-values (status precision out)
        (ival-eval fn pt #:precision starting-precision))
    (define exs (map (compose <-bf ival-lo) out))
    (cons pt exs)))

(define (get-calculation test pts)
  (define fn (eval-prog (test-input test) 'fl (test-context test)))
  (for/list ([pt pts])
    (define val (apply fn pt))
    (cons pt (list val))))

(define (get-cost test)
  (expr-cost (test-input test) (test-output-repr test)))

;; Translates points from the API endpoint
;; into the expected pcontext
(define (compute-pcontexts pts+exs ctx)
  (define output-repr (context-repr ctx))
  (define var-reprs (context-var-reprs ctx))

  (define-values (pts exs)
    (for/lists (pts exs) ([entry (in-list pts+exs)])
      (match-define (list pt ex) entry)
      (values (map real->repr pt var-reprs) (real->repr ex output-repr))))

  (define joint-pcontext (mk-pcontext pts exs))
  (define-values (train-pcontext test-pcontext)
    (cond
      [(= (length pts+exs) (+ (*num-points*) (*reeval-pts*)))
        ; got the expected amount of points
        ; will partition into training and testing set
        (split-pcontext joint-pcontext (*num-points*) (*reeval-pts*))]
      [else
        ; the training set will just be up to the first 256
        ; the testing set will just be the entire set
        (define training-count (min 256 (length pts+exs)))
        (define-values (train-pcontext _)
          (split-pcontext joint-pcontext training-count (- (length pts+exs) training-count)))
        (values train-pcontext joint-pcontext)]))

  (values joint-pcontext train-pcontext test-pcontext))

;; Given a test and a sample of points, returns the test points.
(define (get-sample test)
  (define output-repr (test-output-repr test))
  (define context (test-context test))
  (*needed-reprs* (list output-repr (get-representation 'bool)))

  (match-define (cons domain-stats joint-pcontext)
    (parameterize ([*num-points* (+ (*num-points*) (*reeval-pts*))])
      (setup-context!
       (test-vars test)
       (or (test-spec test) (test-input test)) (test-pre test)
       output-repr)))

  (define-values (train-pcontext test-pcontext)
    (split-pcontext joint-pcontext (*num-points*) (*reeval-pts*))) 

  (for/list ([(pt ex) (in-pcontext test-pcontext)])
    (list pt ex)))

;; Given a test and a sample of points, computes the error at each point.
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-errors test pts+exs #:seed [seed #f] #:profile [profile? #f])
  (define output-repr (test-output-repr test))
  (*context* (test-context test))
  (*needed-reprs* (list output-repr (get-representation 'bool)))
  (generate-prec-rewrites (test-conversions test))

  (when seed (set-seed! seed))
  (random) ;; Child process uses deterministic but different seed from evaluator

  (define-values (joint-pcontext train-pcontext test-pcontext)
    (compute-pcontexts pts+exs (*context*)))

  (define processed-pcontext (preprocess-pcontext test-pcontext (*herbie-preprocess*) (*context*)))
  (define errs (errors (test-input test) processed-pcontext (*context*)))

  (for/list ([(pt _) (in-pcontext test-pcontext)] [err (in-list errs)])
    (list pt (format-bits (ulps->bits err)))))

;; Given a test and a sample of points, computes the local error at every node in the expression
;; returning a tree of errors that mirrors the structure of the expression.
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-local-error test pts+exs #:seed [seed #f] #:profile [profile? #f])
  (define output-repr (test-output-repr test))
  (*context* (test-context test))
  (*needed-reprs* (list output-repr (get-representation 'bool)))
  (generate-prec-rewrites (test-conversions test))

  (when seed (set-seed! seed))
  (random) ;; Child process uses deterministic but different seed from evaluator

  (define-values (joint-pcontext train-pcontext test-pcontext)
    (compute-pcontexts pts+exs (*context*)))

  (define processed-pcontext
    (make-preprocess-pcontext (test-input test)
                              test-pcontext
                              (*num-iterations*)
                              #:specification (test-spec test)
                              #:preprocess (test-preprocess test)))

  (*pcontext* processed-pcontext)
  (local-error-as-tree (test-input test) (*context*)))

;; Given a test and a sample of points, returns a list of improved alternatives
;; and both the test set of points and processed test set of points.
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-alternatives test pts+exs #:seed [seed #f] #:profile [profile? #f])
  ;; This is usually run in `compute-result`
  (rollback-improve!)
  (when seed (set-seed! seed))

  ;; `run-herbie` starts here
  ;; (define seed (get-seed))
  (random) ;; Child process uses deterministic but different seed from evaluator

  (define output-repr (test-output-repr test))
  (*context* (test-context test))
  (*needed-reprs* (list output-repr (get-representation 'bool)))
  (generate-prec-rewrites (test-conversions test))

  (define-values (joint-pcontext train-pcontext test-pcontext)
    (compute-pcontexts pts+exs (*context*)))

  (define alts
    (run-improve! (test-input test) train-pcontext (*num-iterations*)
                  #:specification (test-spec test)
                  #:preprocess (test-preprocess test)))

  (when seed (set-seed! seed))
  (define processed-test-pcontext
    (preprocess-pcontext test-pcontext (*herbie-preprocess*) context))

  (values alts test-pcontext processed-test-pcontext))

(define (run-herbie test)
  (define seed (get-seed))
  (random) ;; Child process uses deterministic but different seed from evaluator
  
  (define repr (test-output-repr test))
  (define ctx (test-context test))
  (*needed-reprs* (list repr (get-representation 'bool)))
  (generate-prec-rewrites (test-conversions test))

  (match-define (cons domain-stats joint-pcontext)
    (parameterize ([*num-points* (+ (*num-points*) (*reeval-pts*))])
      (setup-context! (test-vars test)
                      (or (test-spec test) (test-input test))
                      (test-pre test)
                      repr)))
  (timeline-push! 'bogosity domain-stats)
  (define-values (train-pcontext test-pcontext)
    (split-pcontext joint-pcontext (*num-points*) (*reeval-pts*))) 

  (define end-alts
    (run-improve! (test-input test) train-pcontext (*num-iterations*)
                  #:specification (test-spec test)
                  #:preprocess (test-preprocess test)))

  (when seed (set-seed! seed))
  (define preprocess (*herbie-preprocess*))
  (define processed-test-pcontext (preprocess-pcontext test-pcontext preprocess ctx))
  
  ;; compute error/cost for input expression
  (define start-expr (test-input test))
  (define start-alt (make-alt start-expr))
  (define start-train-errs (errors start-expr train-pcontext ctx))
  (define start-test-errs (errors start-expr processed-test-pcontext ctx))
  (define start-alt-data (alt-analysis start-alt start-train-errs start-test-errs))

  ;; optionally compute error/cost for input expression
  (define target-alt-data
    (cond
      [(test-output test)
       (define target-expr (test-output test))
       (define target-train-errs (errors target-expr train-pcontext ctx))
       (define target-test-errs (errors target-expr processed-test-pcontext ctx))
       (alt-analysis (make-alt target-expr) target-train-errs target-test-errs)]
      [else
       #f]))

  ;; compute error/cost for output expression
  (define end-exprs (map alt-expr end-alts))
  (define end-train-errs (flip-lists (batch-errors end-exprs train-pcontext ctx)))
  (define end-test-errs (flip-lists (batch-errors end-exprs processed-test-pcontext ctx)))
  (define end-alts-data (map alt-analysis end-alts end-train-errs end-test-errs))

  ;; bundle up the result
  (timeline-adjust! 'regimes 'name (test-name test))
  (timeline-adjust! 'regimes 'link ".")

  (define pctxs (list train-pcontext processed-test-pcontext))
  (improve-result preprocess pctxs start-alt-data target-alt-data end-alts-data))

(define (get-test-result command test #:seed [seed #f] #:profile [profile? #f])
  (define timeline #f)

  (define (compute-result test)
    (parameterize ([*timeline-disabled* false]
                   [*warnings-disabled* true])
      (define start-time (current-inexact-milliseconds))
      (rollback-improve!)
      (set! timeline (*timeline*))
      (when seed (set-seed! seed))
      (define result
        (with-handlers ([exn? (curry on-exception start-time)])
          (begin
            (define backend-result
              (match command
                ['improve (run-herbie test)]
                ['sample (get-sample test)]))
            (define time (- (current-inexact-milliseconds) start-time))
            (job-result test 'success time (timeline-extract) (warning-log) backend-result))))
      (print-warnings)
      result))

  (define (on-exception start-time e)
    (parameterize ([*timeline-disabled* false])
      (timeline-event! 'end)
      (print-warnings)
      (define time (- (current-inexact-milliseconds) start-time))
      (job-result test 'failure time (timeline-extract) (warning-log) e)))

  (define (in-engine _)
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
        (job-result test 'timeout (*timeout*) (timeline-extract) (warning-log) #f))))

(define (dummy-table-row result status link)
  (define test (job-result-test result))
  (define repr (test-output-repr test))
  (define preprocess
    (if (eq? (job-result-status result) 'success)
             (improve-result-preprocess (job-result-backend result))
             (test-preprocess test)))
  (table-row (test-name test) (test-identifier test) status
             (resugar-program (test-pre test) repr)
             preprocess
             (representation-name repr)
             (map (curry map representation-name) (test-conversions test))
             (test-vars test)
             (resugar-program (test-input test) repr) #f
             (resugar-program (test-spec test) repr)
             (and (test-output test) (resugar-program (test-output test) repr))
             #f #f #f #f #f (job-result-time result) link '()))

(define (get-table-data result link)
  (match-define (job-result test status time _ _ backend) result)
  (match status
    ['success
     (match-define (improve-result _ _ start target end) backend)
     (define repr (test-output-repr test))
    
     ; starting expr analysis
     (match-define (alt-analysis start-alt start-train-errs start-test-errs) start)
     (define start-expr (alt-expr start-alt))
     (define start-train-score (errors-score start-train-errs))
     (define start-test-score (errors-score start-test-errs))
     (define start-cost (expr-cost start-expr repr))

     ; target analysis for comparison
     (define target-score (and target (errors-score (alt-analysis-test-errors target))))
     
     ; analysis of output expressions
     (define-values (end-exprs end-train-scores end-test-scores end-costs)
       (for/lists (l1 l2 l3 l4) ([result end])
         (match-define (alt-analysis alt train-errors test-errors) result)
         (values (alt-expr alt)
                 (errors-score train-errors)
                 (errors-score test-errors)
                 (expr-cost (alt-expr alt) repr))))

     ; terribly formatted pareto-optimal frontier
     (define cost&accuracy
       (list (list start-cost start-test-score)
             (list (car end-costs) (car end-test-scores))
             (map list (cdr end-costs) (cdr end-test-scores) (cdr end-exprs))))
 
     (define fuzz 0.1)
     (define end-est-score (car end-train-scores))
     (define end-score (car end-test-scores))
     (define status
       (if target-score
           (cond
            [(< end-score (- target-score fuzz)) "gt-target"]
            [(< end-score (+ target-score fuzz)) "eq-target"]
            [(> end-score (+ start-test-score fuzz)) "lt-start"]
            [(> end-score (- start-test-score fuzz)) "eq-start"]
            [(> end-score (+ target-score fuzz)) "lt-target"])
           (cond
            [(and (< start-test-score 1) (< end-score (+ start-test-score 1))) "ex-start"]
            [(< end-score (- start-test-score 1)) "imp-start"]
            [(< end-score (+ start-test-score fuzz)) "apx-start"]
            [else "uni-start"])))

     (struct-copy table-row (dummy-table-row result status link)
                  [start-est start-train-score] [start start-test-score]
                  [target target-score]
                  [result-est end-est-score] [result end-score]
                  [output (car end-exprs)] [cost-accuracy cost&accuracy])]
    ['failure
     (define exn backend)
     (define status (if (exn:fail:user:herbie? exn) "error" "crash"))
     (dummy-table-row result status link)]
    ['timeout
     (dummy-table-row result "timeout" link)]
    [_
     (error 'get-table-data "unknown result type ~a"status)]))

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
