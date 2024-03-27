#lang racket

(require profile
         racket/engine
         json)

(require "syntax/read.rkt"
         "syntax/sugar.rkt"
         "syntax/types.rkt"
         "core/localize.rkt"
         "alternative.rkt"
         "compiler.rkt"
         "common.rkt"
         "datafile.rkt"
         "errors.rkt"
         "float.rkt"
         "ground-truth.rkt"
         "mainloop.rkt"
         "platform.rkt"
         "points.rkt"
         "preprocess.rkt"
         "profile.rkt"
         "timeline.rkt"
         (submod "timeline.rkt" debug))

(provide run-herbie
         get-table-data
         unparse-result
         *reeval-pts*
         *timeout*
         (struct-out job-result)
         (struct-out improve-result)
         (struct-out alt-analysis))

(struct job-result (test status time timeline warnings backend))
(struct improve-result (preprocess pctxs start target end bogosity))
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

;; Partitions a joint pcontext into a training and testing set
(define (partition-pcontext joint-pcontext ctx)
  (define num-points (pcontext-length joint-pcontext))
  (cond
    [(= num-points (+ (*num-points*) (*reeval-pts*)))
     ; got the expected amount of points
     ; will partition into training and testing set
     (split-pcontext joint-pcontext (*num-points*) (*reeval-pts*))]
    [else
     ; the training set will just be up to the first 256
     ; the testing set will just be the entire set
     ; TODO: where is 256 coming from?
     (define training-count (min 256 num-points))
     (define testing-count (- num-points training-count))
     (define-values (train-pcontext _) (split-pcontext joint-pcontext training-count testing-count))
     (values train-pcontext joint-pcontext)]))

;;
;;  API endpoint backends
;;

;; Given a test, computes the program cost of the input expression
(define (get-cost test)
  (define cost-proc (platform-cost-proc (*active-platform*)))
  (define output-repr (context-repr (*context*)))
  (cost-proc (test-input test) output-repr))

;; Given a test and a sample of points, returns the test points.
(define (get-sample test)
  (define repr (test-output-repr test))
  (match-define (cons _ joint-pcontext)
    (parameterize ([*num-points* (+ (*num-points*) (*reeval-pts*))])
      (setup-context! (test-vars test)
                      (prog->spec (or (test-spec test) (test-input test)))
                      (prog->spec (test-pre test))
                      repr)))

  (define-values (_ test-pcontext)
    (split-pcontext joint-pcontext (*num-points*) (*reeval-pts*)))
  test-pcontext)

;; Given a test and a sample of points, computes the error at each point.
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-errors test pcontext)
  (unless pcontext
    (error 'get-errors "cannnot run without a pcontext"))

  (define-values (_ test-pcontext) (partition-pcontext pcontext (*context*)))
  (define errs (errors (test-input test) test-pcontext (*context*)))
  (for/list ([(pt _) (in-pcontext test-pcontext)] [err (in-list errs)])
    (list pt err)))

;; Given a test and a sample of points, the ground truth of each point
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-exacts test pcontext)
  (unless pcontext
    (error 'get-exacts "cannnot run without a pcontext"))
  (define-values (train-pcontext test-pcontext) (partition-pcontext pcontext (*context*)))
  (define-values (pts _) (pcontext->lists test-pcontext))
  (define fn (eval-progs-real 
              (list (prog->spec (test-input test)))
              (list (*context*))))
  (for/list ([pt pts])
    (list pt (car (apply fn pt)))))

;; Given a test and a sample of points,
;; the floating-point result at each point
(define (get-calculation test pcontext)
  (unless pcontext
    (error 'get-calculation "cannnot run without a pcontext"))

  (define-values (train-pcontext test-pcontext) (partition-pcontext pcontext (*context*)))
  (define-values (pts _) (pcontext->lists test-pcontext))

  (define fn (compile-prog (test-input test) (test-context test)))
  (for/list ([pt pts])
    (list pt (apply fn pt))))

;; Given a test and a sample of points, computes the local error at every node in the expression
;; returning a tree of errors that mirrors the structure of the expression.
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-local-error test pcontext)
  (unless pcontext
    (error 'get-local-error "cannnot run without a pcontext"))

  (define-values (train-pcontext test-pcontext) (partition-pcontext pcontext (*context*)))
  (*pcontext* test-pcontext)
  (local-error-as-tree (test-input test) (*context*)))

;; TODO: What in the timeline needs fixing with these changes?

;; Given a test and a sample of points, returns a list of improved alternatives
;; and both the test set of points and processed test set of points.
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-alternatives test pcontext seed)
  (unless pcontext
    (error 'get-alternatives "cannnot run without a pcontext"))

  (define-values (train-pcontext test-pcontext) (partition-pcontext pcontext context))
  ;; TODO: Ignoring all user-provided preprocessing right now
  (define-values (alternatives preprocessing)
    (run-improve! (test-input test) (test-spec test) (*context*) train-pcontext))
  (define test-pcontext*
    (preprocess-pcontext (*context*) test-pcontext preprocessing))
  (when seed (set-seed! seed))
  (list alternatives test-pcontext test-pcontext*))

;; Improvement backend for generating reports
;; A more heavyweight version of `get-alternatives`
(define (get-alternatives/report test)
  (define seed (get-seed))
  (random) ;; Child process uses deterministic but different seed from evaluator
  
  (define repr (test-output-repr test))
  (define ctx (test-context test))
  (match-define (cons domain-stats joint-pcontext)
    (parameterize ([*num-points* (+ (*num-points*) (*reeval-pts*))])
      (setup-context! (test-vars test)
                      (prog->spec (or (test-spec test) (test-input test)))
                      (prog->spec (test-pre test))
                      repr)))
  (timeline-push! 'bogosity domain-stats)
  (define-values (train-pcontext test-pcontext)
    (split-pcontext joint-pcontext (*num-points*) (*reeval-pts*)))
  ;; TODO: Ignoring all user-provided preprocessing right now
  (define-values (end-alts preprocessing)
    (run-improve! (test-input test) (test-spec test) (*context*) train-pcontext))
  (define test-pcontext*
    (preprocess-pcontext ctx test-pcontext preprocessing))
  (when seed (set-seed! seed))
  
  ;; compute error/cost for input expression
  (define start-expr (test-input test))
  (define start-alt (make-alt start-expr))
  (define start-train-errs (errors start-expr train-pcontext ctx))
  (define start-test-errs (errors start-expr test-pcontext* ctx))
  (define start-alt-data (alt-analysis start-alt start-train-errs start-test-errs))
  
  ;; optionally compute error/cost for input expression
  (define target-alt-data
    (cond
      [(test-output test)
       (define target-expr (test-output test))
       (define target-train-errs (errors target-expr train-pcontext ctx))
       (define target-test-errs (errors target-expr test-pcontext* ctx))
       (alt-analysis (make-alt target-expr) target-train-errs target-test-errs)]
      [else
       #f]))

  ;; compute error/cost for output expression
  (define end-exprs (map alt-expr end-alts))
  (define end-train-errs (flip-lists (batch-errors end-exprs train-pcontext ctx)))
  (define end-test-errs (flip-lists (batch-errors end-exprs test-pcontext* ctx)))
  (define end-alts-data (map alt-analysis end-alts end-train-errs end-test-errs))

  ;; bundle up the result
  (timeline-adjust! 'regimes 'name (test-name test))
  (timeline-adjust! 'regimes 'link ".")

  (define pctxs (list train-pcontext test-pcontext*))
  (improve-result preprocessing pctxs start-alt-data target-alt-data end-alts-data domain-stats))

;;
;;  Public interface
;;

(define (run-herbie command test
                    #:seed [seed #f]
                    #:pcontext [pcontext #f]
                    #:profile? [profile? #f]
                    #:timeline-disabled? [timeline-disabled? #f])
  (define timeline #f)

  ;; CS versions <= 8.2: problems with scheduler cause places to stay
  ;; in a suspended state
  (when cs-places-workaround?
    (thread (lambda () (sync (system-idle-evt)))))

  (define (on-exception start-time e)
    (parameterize ([*timeline-disabled* timeline-disabled?])
      (timeline-event! 'end)
      (define time (- (current-inexact-milliseconds) start-time))
      (match command 
        ['improve (job-result test 'failure time (timeline-extract) (warning-log) e)]
        [_ (raise e)])))

  (define (on-timeout)
    (parameterize ([*timeline-disabled* timeline-disabled?])
      (timeline-load! timeline)
      (timeline-compact! 'outcomes)
      (job-result test 'timeout (*timeout*) (timeline-extract) (warning-log) #f)))

  (define (compute-result test)
    (parameterize ([*timeline-disabled* timeline-disabled?]
                   [*warnings-disabled* false])
      (define start-time (current-inexact-milliseconds))
      (rollback-improve!)
      (*context* (test-context test))
      (*active-platform* (get-platform (*platform-name*)))
      (activate-platform! (*active-platform*))
      (set! timeline (*timeline*))
      (when seed (set-seed! seed))
      (with-handlers ([exn? (curry on-exception start-time)])
        (define result
          (match command 
            ['alternatives (get-alternatives test pcontext seed)]
            ['evaluate (get-calculation test pcontext)]
            ['cost (get-cost test)]
            ['errors (get-errors test pcontext)]
            ['exacts (get-exacts test pcontext)]
            ['improve (get-alternatives/report test)]
            ['local-error (get-local-error test pcontext)]
            ['sample (get-sample test)]
            [_ (error 'compute-result "unknown command ~a" command)]))
        (define time (- (current-inexact-milliseconds) start-time))
        (job-result test 'success time (timeline-extract) (warning-log) result))))
  
  (define (in-engine _)
    (if profile?
        (profile-thunk
         (λ () (compute-result test))
         #:order 'total
         #:delay 0.001
         #:render (λ (p order) (write-json (profile->json p) profile?)))
        (compute-result test)))

  ;; Branch on whether or not we should run inside an engine
  (define eng (engine in-engine))
  (if (engine-run (*timeout*) eng)
      (engine-result eng)
      (on-timeout)))

(define (dummy-table-row result status link)
  (define test (job-result-test result))
  (define repr (test-output-repr test))
  (define preprocess
    (if (eq? (job-result-status result) 'success)
             (improve-result-preprocess (job-result-backend result))
             (test-preprocess test)))
  (table-row (test-name test) (test-identifier test) status
             (prog->fpcore (test-pre test) repr)
             preprocess
             (representation-name repr)
             (map (curry map representation-name) (test-conversions test))
             (test-vars test)
             (prog->fpcore (test-input test) repr) #f
             (prog->fpcore (test-spec test) repr)
             (and (test-output test) (prog->fpcore (test-output test) repr))
             #f #f #f #f #f (job-result-time result) link '()))

(define (get-table-data result link)
  (match-define (job-result test status time _ _ backend) result)
  (match status
    ['success
     (match-define (improve-result _ _ start target end _) backend)
     (define expr-cost (platform-cost-proc (*active-platform*)))
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

(define (unparse-result row #:expr [expr #f] #:description [descr #f])
  (define repr (get-representation (table-row-precision row)))
  (define expr* (or expr (table-row-output row)))
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
     ,@(if descr `(:description ,(~a descr)) '())
     :precision ,(table-row-precision row)
     :herbie-conversions ,(table-row-conversions row)
     ,@(if (eq? (table-row-pre row) 'TRUE) '() `(:pre ,(table-row-pre row)))
     ,@(if (equal? (table-row-preprocess row) empty) '() `(:herbie-preprocess ,(table-row-preprocess row)))
     ,@(if (table-row-target-prog row) `(:herbie-target ,(table-row-target-prog row)) '())
     ,(prog->fpcore expr* repr)))
