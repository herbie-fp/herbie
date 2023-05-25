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
         (struct-out test-result) get-table-data unparse-result
         *reeval-pts* *timeout*)

;; Cannot move between threads
(struct alt-result (alt train-error test-error cost))
(struct test-result (test status time timeline warnings exn
                     preprocess train-pctx test-pctx start target end))

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

(define (get-exacts test pts)
  (define repr (test-output-repr test))
  (define starting-precision (*starting-prec*))
  (define <-bf (representation-bf->repr repr))
  (define fn (make-search-func (test-precondition test) (list (test-program test)) (test-context test)))
  (for/list ([pt pts])
    (define-values (status precision out)
        (ival-eval fn pt #:precision starting-precision))
    (define exs (map (compose <-bf ival-lo) out))
    (cons pt exs)))

(define (get-calculation test pts)
  (define fn (eval-prog (test-program test) 'fl (test-context test)))
  (for/list ([pt pts])
    (define val (apply fn pt))
    (cons pt (list val))))

(define (get-cost test)
    (program-cost (test-program test) (test-output-repr test)))


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
        (or (test-specification test) (test-program test)) (test-precondition test)
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
  (define errs (errors (test-program test) processed-pcontext (*context*)))

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
    (make-preprocess-pcontext (test-program test)
                              test-pcontext
                              (*num-iterations*)
                              #:specification (test-specification test)
                              #:preprocess (test-preprocess test)))

  (*pcontext* processed-pcontext)
  (local-error-as-tree (test-program test) (*context*)))

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
    (run-improve! (test-program test) train-pcontext (*num-iterations*)
                  #:specification (test-specification test)
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
                  (setup-context! (or (test-specification test) (test-program test))
                                  (test-precondition test)
                                  repr)))
  (timeline-push! 'bogosity domain-stats)
  (define-values (train-pcontext test-pcontext)
    (split-pcontext joint-pcontext (*num-points*) (*reeval-pts*))) 

  (define end-alts
    (run-improve! (test-program test) train-pcontext (*num-iterations*)
                  #:specification (test-specification test)
                  #:preprocess (test-preprocess test)))

  (when seed (set-seed! seed))
  (define preprocess (*herbie-preprocess*))
  (define processed-test-pcontext (preprocess-pcontext test-pcontext preprocess ctx))

  (timeline-adjust! 'regimes 'name (test-name test))
  (timeline-adjust! 'regimes 'link ".")
  
  (define start-alt (make-alt start-prog))
  (define start-prog (alt-program start-alt))
  (define start-cost (program-cost start-prog repr))
  (define start-train-errs (errors start-prog train-pcontext ctx))
  (define start-test-errs (errors start-prog processed-test-pcontext ctx))
  (define start-alt-data (alt-result start-alt start-train-errs start-test-errs start-cost))

  (define target-alt-data
    (cond
      [(test-target test)
       (define target-prog (test-target test))
       (define target-cost (program-cost target-prog repr))
       (define target-train-errs (errors target-prog train-pcontext ctx))
       (define target-test-errs (errors target-prog processed-test-pcontext ctx))
       (alt-result (make-alt target-prog) target-train-errs target-test-errs target-cost)]
      [else
       #f]))

  (define end-progs (map alt-program end-alts))
  (define end-costs (map program-cost end-progs repr))
  (define end-target-errs (flip-lists (batch-errors end-progs train-pcontext ctx)))
  (define end-test-errs (flip-lists (batch-errors end-progs processed-test-pcontext ctx)))
  (define end-alt-data (map alt-result end-alts end-target-errs end-test-errs end-costs))

  ; (define-values (points exacts) (get-p&es train-pcontext))
  ; (define-values (newpoints newexacts) (get-p&es processed-test-pcontext))

  (test-result test 'success #f (timeline-extract) (warning-log) #f
               preprocess train-pcontext processed-test-pcontext
               start-alt-data target-alt-data end-alt-data))

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
          (match command
            ['improve (run-herbie test)]
            ['sample (get-sample test)])))
      (print-warnings)
      (define total-time (- (current-inexact-milliseconds) start-time))
      (if (test-result? result)
          (struct-copy test-result result [time total-time])
          result)))

  (define (on-exception start-time e)
    (parameterize ([*timeline-disabled* false])
      (timeline-event! 'end)
      (print-warnings)
      (define total-time (- (current-inexact-milliseconds) start-time))
      (define timeline (timeline-extract))
      (define preprocess (test-preprocess test))
      (test-result test 'failure total-time timeline (warning-log) e preprocess #f #f #f #f #f)))

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
        (define timeline (timeline-extract))
        (define preprocess (test-preprocess test))
        (test-result test 'timeout (*timeout*) timeline (warning-log) #f preprocess #f #f #f #f #f))))

(define (dummy-table-row result status link)
  (define test (test-result-test result))
  (define repr (test-output-repr test))
  (define preprocess (test-result-preprocess result))
  (table-row (test-name test) (test-identifier test) status
             (resugar-program (program-body (test-precondition test)) repr)
              preprocess
             (representation-name (test-output-repr test))
             (map (curry map representation-name) (test-conversions test))
             (test-vars test)
             (resugar-program (test-input test) repr) #f
             (resugar-program (test-spec test) repr)
             (and (test-output test) (resugar-program (test-output test) repr))
             #f #f #f #f #f (test-result-time result) link '()))

(define (get-table-data result link)
  (match (test-result-status result)
    ['success
     (define test (test-result-test result))
     (define name (test-name test))
     (define repr (test-output-repr test))

     (define start (test-result-start result))
     (define start-prog (alt-program (alt-result-alt start)))
     (define start-train-score (errors-score (alt-result-train-error start)))
     (define start-test-score (errors-score (alt-result-test-error start)))
     (define start-cost (alt-result-cost repr))
     
     (define target (test-result-target result))
     (define target-score (and target (errors-score (alt-result-test-error target))))

     (define end (test-result-end result))
     (define end-progs (map (compose alt-program alt-result-alt) end))
     (define end-train-scores (map (compose errors-score alt-result-train-error) end))
     (define end-test-scores (map (compose errors-score alt-result-test-error) end))
     (define end-costs (map alt-result-cost end))
     (define end-exprs (map (λ (p) (program-body (resugar-program p repr))) end-progs))
 
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
     (define exn (test-result-exn result))
     (define status (if (exn:fail:user:herbie? exn) "error" "crash"))
     (dummy-table-row result status link)]
    ['timeout
     (dummy-table-row result "timeout" link)]
    [_
     (error 'get-table-data "Unknown table result type ~a" (test-result-status result))]))

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
