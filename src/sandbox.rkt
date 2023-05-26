#lang racket
(require profile math/bigfloat racket/engine json rival)
(require "syntax/read.rkt" "syntax/sugar.rkt" "syntax/types.rkt"
         "alternative.rkt" "common.rkt" "conversions.rkt" "cost.rkt"
         "datafile.rkt" "errors.rkt" "float.rkt" "sampling.rkt"
         "mainloop.rkt" "preprocess.rkt" "points.rkt" "profile.rkt"
         "programs.rkt" "timeline.rkt" (submod "timeline.rkt" debug)
         "core/localize.rkt" "ground-truth.rkt")

(provide run-herbie get-table-data unparse-result
         (struct-out test-result) (struct-out test-success)
         (struct-out test-failure) (struct-out test-timeout)
         *reeval-pts* *timeout*)

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

(define (pcontext->lists context)
  (for/lists (pts exs) ([(pt ex) (in-pcontext context)])
    (values pt ex)))

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

;; All backends run this before hand
;; Sets global variables
(define (setup-backend! test)
  (define repr (test-output-repr test))
  (*context* (test-context test))
  (*needed-reprs* (list repr (get-representation 'bool)))
  (generate-prec-rewrites (test-conversions test)))

;;
;;  API endpoint backends
;;

;; Given a test, computes the program cost of the input expression
(define (get-cost test)
  (setup-backend! test)
  (expr-cost (test-input test) (test-output-repr test)))

;; Given a test and a sample of points, returns the test points.
(define (get-sample test)
  (setup-backend! test)
  (define repr (test-output-repr test))
  (match-define (cons domain-stats joint-pcontext)
    (parameterize ([*num-points* (+ (*num-points*) (*reeval-pts*))])
      (setup-context! (test-vars test)
                      (or (test-spec test) (test-input test))
                      (test-pre test)
                      repr)))

  (define-values (train-pcontext test-pcontext)
    (split-pcontext joint-pcontext (*num-points*) (*reeval-pts*))) 
  (for/list ([(pt ex) (in-pcontext test-pcontext)])
    (list pt ex)))

;; Given a test and a sample of points, computes the error at each point.
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-errors test pcontext)
  (setup-backend! test)
  (unless pcontext
    (error 'get-errors "cannnot run without a pcontext"))

  (define-values (_ test-pcontext) (partition-pcontext pcontext (*context*)))
  (define errs (errors (test-input test) test-pcontext (*context*)))

  (for/list ([(pt _) (in-pcontext test-pcontext)] [err (in-list errs)])
    (list pt (format-bits (ulps->bits err)))))

;; Given a test and a sample of points, the ground truth of each point
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-exacts test pcontext)
  (setup-backend! test)
  (unless pcontext
    (error 'get-exacts "cannnot run without a pcontext"))

  (define repr (test-output-repr test))
  (define-values (train-pcontext test-pcontext) (partition-pcontext pcontext (*context*)))
  (define-values (pts _) (pcontext->lists test-pcontext))

  (define fn (make-search-func (test-pre test) (list (test-input test)) (*context*)))
  (for/list ([pt pts])
    (define-values (result prec exs) (ival-eval fn pt))
    (list pt (match exs
               [(list (ival lo hi)) ((representation-bf->repr repr) lo)]
               [(? nan?) (real->repr +nan.0 repr)]))))

;; Given a test and a sample of points,
;; the floating-point result at each point
(define (get-calculation test pcontext)
  (setup-backend! test)
  (unless pcontext
    (error 'get-calculation "cannnot run without a pcontext"))

  (define-values (train-pcontext test-pcontext) (partition-pcontext pcontext (*context*)))
  (define-values (pts _) (pcontext->lists test-pcontext))

  (define fn (eval-prog (test-input test) 'fl (test-context test)))
  (for/list ([pt pts])
    (list pt (apply fn pt))))

;; Given a test and a sample of points, computes the local error at every node in the expression
;; returning a tree of errors that mirrors the structure of the expression.
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-local-error test pcontext)
  (setup-backend! test)
  (unless pcontext
    (error 'get-local-error "cannnot run without a pcontext"))

  (define-values (train-pcontext test-pcontext) (partition-pcontext pcontext (*context*)))
  (*pcontext* test-pcontext)
  (local-error-as-tree (test-input test) (*context*)))

;; Given a test and a sample of points, returns a list of improved alternatives
;; and both the test set of points and processed test set of points.
;; If the sample contains the expected number of points, i.e., `(*num-points*) + (*reeval-pts*)`,
;; then the first `*num-points*` will be discarded and the rest will be used for evaluation,
;; otherwise the entire set is used.
(define (get-alternatives test pcontext seed)
  (setup-backend! test)
  (unless pcontext
    (error 'get-alternatives "cannnot run without a pcontext"))

  (define-values (train-pcontext test-pcontext) (partition-pcontext pcontext (*context*)))
  (define alts
    (run-improve! (test-input test) train-pcontext (*num-iterations*)
                  #:specification (test-spec test)
                  #:preprocess (test-preprocess test)))

  (when seed (set-seed! seed))
  (define processed-pcontext (preprocess-pcontext test-pcontext (*herbie-preprocess*) context))
  (list alts test-pcontext processed-pcontext))

;; Improvement backend for generating reports
;; A more heavyweight version of `get-alternatives`
(define (get-alternatives/report test)
  (setup-backend! test)

  (define seed (get-seed))
  (random) ;; Child process uses deterministic but different seed from evaluator
  
  (define repr (test-output-repr test))
  (define context (test-context test))
  (match-define (cons domain-stats joint-pcontext)
    (parameterize ([*num-points* (+ (*num-points*) (*reeval-pts*))])
      (setup-context! (test-vars test)
                      (or (test-spec test) (test-input test))
                      (test-pre test)
                      repr)))
  (timeline-push! 'bogosity domain-stats)
  (define-values (train-pcontext test-pcontext)
    (split-pcontext joint-pcontext (*num-points*) (*reeval-pts*))) 

  (define alts
    (run-improve! (test-input test) train-pcontext (*num-iterations*)
                  #:specification (test-spec test)
                  #:preprocess (test-preprocess test)))

  (when seed (set-seed! seed))
  (define processed-test-pcontext
    (preprocess-pcontext test-pcontext (*herbie-preprocess*) context))

  (define end-errs
    (flip-lists
     (batch-errors (map alt-expr alts) processed-test-pcontext context)))

  (timeline-adjust! 'regimes 'name (test-name test))
  (timeline-adjust! 'regimes 'link ".")

  (define-values (points exacts) (pcontext->lists train-pcontext))
  (define-values (newpoints newexacts) (pcontext->lists processed-test-pcontext))
  (test-success test
                (bf-precision)
                #f
                (timeline-extract)
                (warning-log) (make-alt (test-input test)) alts
                (*herbie-preprocess*) points exacts
                (errors (test-input test) train-pcontext context)
                (errors (alt-expr (car alts)) train-pcontext context)
                newpoints newexacts
                (errors (test-input test) processed-test-pcontext context)
                end-errs
                (and (test-output test) (errors (test-output test) processed-test-pcontext context))
                (expr-cost (test-input test) repr)
                (map (curryr alt-cost repr) alts)
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

;;
;;  Public interface
;;

(define (run-herbie command test
                    #:seed [seed #f]
                    #:pcontext [pcontext #f]
                    #:engine? [engine? #t]
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
      (print-warnings)
      (match command 
        ['improve (test-failure test (bf-precision)
                                (- (current-inexact-milliseconds) start-time)
                                (timeline-extract) (warning-log) e)]
        [_ (raise e)])))

  (define (compute-result test)
    (parameterize ([*timeline-disabled* timeline-disabled?]
                   [*warnings-disabled* true])
      (define start-time (current-inexact-milliseconds))
      (rollback-improve!)
      (set! timeline (*timeline*))
      (when seed (set-seed! seed))
      (define out
        (with-handlers ([exn? (curry on-exception start-time)])
          (match command 
            ['alternatives (get-alternatives test pcontext seed)]
            ['evaluate (get-calculation test pcontext)]
            ['cost (get-cost test)]
            ['errors (get-errors test pcontext)]
            ['exacts (get-exacts test pcontext)]
            ['improve (get-alternatives/report test)]
            ['local-error (get-local-error test pcontext)]
            ['sample (get-sample test)]
            [_ (error 'compute-result "unknown command ~a" command)])))
      (print-warnings)
      (if (test-result? out)
          (add-time out (- (current-inexact-milliseconds) start-time))
          out)))
  
  (define (in-engine _)
    (if profile?
        (profile-thunk
         (λ () (compute-result test))
         #:order 'total
         #:render (λ (p order) (write-json (profile->json p) profile?)))
        (compute-result test)))

  ;; Branch on whether or not we should run inside an engine
  (cond
    [engine?
     (define eng (engine in-engine))
     (if (engine-run (*timeout*) eng)
         (engine-result eng)
         (parameterize ([*timeline-disabled* timeline-disabled?])
           (timeline-load! timeline)
           (timeline-compact! 'outcomes)
           (print-warnings)
           (match command 
             ['improve (test-timeout test (bf-precision) (*timeout*) (timeline-extract) '())]
             [_ (error 'run-herbie "command ~a timed out" command)])))]
    [else
     (in-engine #f)]))

(define (dummy-table-row result status link)
  (define test (test-result-test result))
  (define repr (test-output-repr test))
  (table-row (test-name test) (test-identifier test) status
             (resugar-program (test-pre test) repr)
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
    (define start-expr    (alt-expr (test-success-start-alt result)))
    (define end-exprs     (map alt-expr (test-success-end-alts result)))
    (define costs         (test-success-end-costs result))

    (define start-score (errors-score start-errors))
    (define end-scores (map errors-score end-errorss))
    (define end-score (car end-scores))
    (define target-score (and target-errors (errors-score target-errors)))
    (define est-start-score (errors-score (test-success-start-est-error result)))
    (define est-end-score (errors-score (test-success-end-est-error result)))
    (define end-exprs* (map (λ (p) (resugar-program p (test-output-repr test))) end-exprs))

    (define cost&accuracy
      (list (list (expr-cost start-expr (test-output-repr test)) start-score)
            (list (car costs) (car end-scores))
            (map list (cdr costs) (cdr end-scores) (cdr end-exprs*))))

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
                 [output (car end-exprs*)]
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
