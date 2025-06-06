#lang racket

(require openssl/sha1)
(require (only-in xml write-xexpr))

(require "../syntax/load-plugin.rkt"
         "../syntax/read.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/errors.rkt"
         "../utils/float.rkt"
         "../core/points.rkt"
         "../reports/common.rkt"
         "../reports/history.rkt"
         "../reports/pages.rkt"
         "../reports/plot.rkt"
         "datafile.rkt"
         "sandbox.rkt"
         (submod "../utils/timeline.rkt" debug))

(provide job-path
         job-start
         job-status
         job-wait
         job-results
         job-timeline
         server-start
         server-improve-results
         server-count
         server-up?)

(define (log msg . args)
  (when false
    (apply eprintf msg args)))

;; Job-specific public API
(define (job-path id)
  (format "~a.~a" id *herbie-commit*))

(define (job-start command
                   test
                   #:seed [seed #f]
                   #:pcontext [pcontext #f]
                   #:profile? [profile? #f]
                   #:timeline? [timeline? #f])
  (define job-id (manager-ask 'start manager command test seed pcontext profile? timeline?))
  (log "Job ~a, Qed up for program: ~a\n" job-id (test-name test))
  job-id)

(define (job-status job-id)
  (log "Checking on: ~a.\n" job-id)
  (manager-ask 'check job-id))

(define (job-wait job-id)
  (define finished-result (manager-ask 'wait manager job-id))
  (log "Done waiting for: ~a\n" job-id)
  finished-result)

(define (job-results job-id)
  (log "Getting result for job: ~a.\n" job-id)
  (manager-ask 'result job-id))

(define (job-timeline job-id)
  (log "Getting timeline for job: ~a.\n" job-id)
  (manager-ask 'timeline job-id))

;; Whole-server public methods

(define (server-start threads)
  (cond
    [threads
     (eprintf "Starting Herbie ~a with ~a workers and seed ~a...\n"
              *herbie-version*
              threads
              (get-seed))]
    [else (eprintf "Starting Herbie ~a with seed ~a...\n" *herbie-version* (get-seed))])

  (set! manager
        (if threads
            (make-manager threads)
            'basic)))

(define (server-improve-results)
  (log "Getting improve results.\n")
  (manager-ask 'improve))

(define (server-count)
  (define job-list (manager-ask 'count))
  (apply + job-list))

(define (server-up?)
  (match manager
    [(? place? x) (not (sync/timeout 0 (place-dead-evt x)))]
    ['basic #t]))

;; Interface for two manager types (threaded and basic)

(define manager #f)

(define (manager-ask msg . args)
  (log "Asking manager: ~a, ~a.\n" msg args)
  (match manager
    [(? place? x) (apply manager-ask-threaded x msg args)]
    ['basic (apply manager-ask-basic msg args)]))

(define (manager-ask-threaded manager msg . args)
  (define-values (a b) (place-channel))
  (place-channel-put manager (list* msg b args))
  (place-channel-get a))

(define (manager-ask-basic msg . args)
  (match (list* msg args) ; public commands
    [(list 'start 'basic command test seed pcontext profile? timeline?)
     (define job (herbie-command command test seed pcontext profile? timeline?))
     (define job-id (compute-job-id job))
     (hash-set! queued-jobs job-id job)
     job-id]
    [(list 'wait 'basic job-id)
     (define command (hash-ref queued-jobs job-id))
     (define result (herbie-do-server-job command job-id))
     (hash-set! completed-jobs job-id result)
     result]
    [(list 'result job-id) (hash-ref completed-jobs job-id #f)]
    [(list 'timeline job-id) (hash-ref completed-jobs job-id #f)]
    [(list 'check job-id) (and (hash-ref completed-jobs job-id #f) job-id)]
    [(list 'count) (list 0 0)]
    [(list 'improve)
     (for/list ([(job-id result) (in-hash completed-jobs)]
                #:when (equal? (hash-ref result 'command) "improve"))
       result)]))

;; Implementation of threaded manager

(struct herbie-command (command test seed pcontext profile? timeline?) #:prefab)

(define queued-jobs (make-hash))
(define completed-jobs (make-hash))

(define (compute-job-id job-info)
  (sha1 (open-input-string (~s job-info))))

(define-syntax (place/context* stx)
  (syntax-case stx ()
    [(_ name #:parameters (params ...) body ...)
     (with-syntax ([(fresh ...) (generate-temporaries #'(params ...))])
       #'(let ([fresh (params)] ...)
           (place/context name
                          (parameterize ([params fresh] ...)
                            body ...))))]))

(define (make-manager worker-count)
  (place/context*
   ch
   #:parameters (*flags* *num-iterations*
                         *num-points*
                         *timeout*
                         *reeval-pts*
                         *node-limit*
                         *max-find-range-depth*
                         *pareto-mode*
                         *platform-name*
                         *loose-plugins*
                         *functions*)
   (parameterize ([current-error-port (open-output-nowhere)]) ; hide output
     (load-herbie-plugins))
   ; not sure if the above code is actaully needed.
   (define busy-workers (make-hash))
   (define waiting-workers (make-hash))
   (define current-jobs (make-hash))
   (when (eq? worker-count #t)
     (set! worker-count (processor-count)))
   (for ([i (in-range worker-count)])
     (hash-set! waiting-workers i (make-worker i)))
   (log "~a workers ready.\n" (hash-count waiting-workers))
   (define waiting (make-hash))
   (log "Manager waiting to assign work.\n")
   (for ([i (in-naturals)])
     (match (place-channel-get ch)

       ;; Private API
       [(list 'assign self)
        (define reassigned (make-hash))
        (for ([(wid worker) (in-hash waiting-workers)]
              [(jid command) (in-hash queued-jobs)])
          (log "Starting worker [~a] on [~a].\n" jid (test-name (herbie-command-test command)))
          ; Check if the job is already in progress.
          (unless (hash-has-key? current-jobs jid)
            (place-channel-put worker (list 'apply self command jid))
            (hash-set! reassigned wid jid)
            (hash-set! busy-workers wid worker)))
        ; remove X many jobs from the Q and update waiting-workers
        (for ([(wid jid) (in-hash reassigned)])
          (hash-remove! waiting-workers wid)
          (hash-remove! queued-jobs jid))]
       ; Job is finished save work and free worker. Move work to 'send state.
       [(list 'finished self wid job-id result)
        (log "Job ~a finished, saving result.\n" job-id)
        (hash-set! completed-jobs job-id result)
        ; move worker to waiting list
        (hash-remove! current-jobs job-id)
        (hash-set! waiting-workers wid (hash-ref busy-workers wid))
        (hash-remove! busy-workers wid)
        (log "waiting job ~a completed\n" job-id)
        (place-channel-put self (list 'send job-id result))
        (place-channel-put self (list 'assign self))]
       [(list 'send job-id result)
        (log "Sending result for ~a.\n" job-id)
        (for ([handle (hash-ref waiting job-id '())])
          (place-channel-put handle result))
        (hash-remove! waiting job-id)]

       ;; Public API
       [(list 'start handler self command test seed pcontext profile? timeline?)
        (define job (herbie-command command test seed pcontext profile? timeline?))
        (define job-id (compute-job-id job))
        (place-channel-put handler job-id)
        ; Check if the work has been completed already if not assign the work.
        (cond
          [(hash-has-key? completed-jobs job-id)
           (place-channel-put self (list 'send job-id (hash-ref completed-jobs job-id)))]
          [else
           (hash-set! queued-jobs job-id job)
           (place-channel-put self (list 'assign self))])]
       [(list 'wait handler self job-id)
        (log "Waiting for job: ~a\n" job-id)
        ; first we add the handler to the wait list.
        (hash-update! waiting job-id (curry append (list handler)) '())
        (define result (hash-ref completed-jobs job-id #f))
        ; check if the job is completed or not.
        (unless (false? result)
          (log "Done waiting for job: ~a\n" job-id)
          ; we have a result to send.
          (place-channel-put self (list 'send job-id result)))]
       ; Get the result for the given id, return false if no work found.
       [(list 'result handler job-id) (place-channel-put handler (hash-ref completed-jobs job-id #f))]
       [(list 'timeline handler job-id)
        (define wid (hash-ref current-jobs job-id #f))
        (cond
          [wid
           (log "Worker[~a] working on ~a.\n" wid job-id)
           (define-values (a b) (place-channel))
           (place-channel-put (hash-ref busy-workers wid) (list 'timeline b))
           (define requested-timeline (place-channel-get a))
           (place-channel-put handler requested-timeline)]
          [else
           (log "Job complete, no timeline, send result.\n")
           (place-channel-put handler (hash-ref completed-jobs job-id #f))])]
       [(list 'check handler job-id)
        (place-channel-put handler (and (hash-has-key? completed-jobs job-id) job-id))]
       ; Returns the current count of working workers.
       [(list 'count handler)
        (define counts (list (hash-count busy-workers) (hash-count queued-jobs)))
        (log "Currently ~a jobs in progress, ~a jobs in queue.\n" (first counts) (second counts))
        (place-channel-put handler counts)]
       ; Retreive the improve results for results.json
       [(list 'improve handler)
        (define improved-list
          (for/list ([(job-id result) (in-hash completed-jobs)]
                     #:when (equal? (hash-ref result 'command) "improve"))
            result))
        (place-channel-put handler improved-list)]))))

;; Implementation of threaded worker

(define (warn-single-threaded-mpfr)
  (local-require ffi/unsafe)
  (local-require math/private/bigfloat/mpfr)
  (unless ((get-ffi-obj 'mpfr_buildopt_tls_p mpfr-lib (_fun -> _bool)))
    (warn 'mpfr-threads "Your MPFR is single-threaded. Herbie will work but be slower than normal.")))

(define (make-worker worker-id)
  (warn-single-threaded-mpfr)

  (place/context*
   ch
   #:parameters (*flags* *num-iterations*
                         *num-points*
                         *timeout*
                         *reeval-pts*
                         *node-limit*
                         *max-find-range-depth*
                         *pareto-mode*
                         *platform-name*
                         *loose-plugins*
                         *functions*)
   (parameterize ([current-error-port (open-output-nowhere)]) ; hide output
     (load-herbie-plugins))
   (define worker-thread
     (thread (λ ()
               (let loop ([seed #f])
                 (match (thread-receive)
                   [(work manager worker-id job-id command)
                    (log "run-job: ~a, ~a\n" worker-id job-id)
                    (define out-result (herbie-do-server-job command job-id))
                    (log "Job: ~a finished, returning work to manager\n" job-id)
                    (place-channel-put manager (list 'finished manager worker-id job-id out-result))])
                 (loop seed)))))
   (define timeline #f)
   (define current-job-id #f)
   (for ([_ (in-naturals)])
     (match (place-channel-get ch)
       [(list 'apply manager command job-id)
        (set! timeline (*timeline*))
        (set! current-job-id job-id)
        (log "[~a] working on [~a].\n" job-id (test-name (herbie-command-test command)))
        (thread-send worker-thread (work manager worker-id job-id command))]
       [(list 'timeline handler)
        (log "Timeline requested from worker[~a] for job ~a\n" worker-id current-job-id)
        (place-channel-put handler (reverse (unbox timeline)))]))))

(struct work (manager worker-id job-id job))

;; Worker internals

(define (herbie-do-server-job command job-id)
  (define herbie-result (wrapper-run-herbie command job-id))
  (define basic-output ((get-json-converter command) herbie-result job-id))
  ;; Add default fields that all commands have
  (hash-set* basic-output
             'job
             job-id
             'path
             (job-path job-id)
             'command
             (~a (herbie-command-command command))
             'name
             (test-name (herbie-command-test command))
             'status
             (~a (job-result-status herbie-result))
             'time
             (job-result-time herbie-result)
             'warnings
             (job-result-warnings herbie-result)))

(define (wrapper-run-herbie cmd job-id)
  (log "Started ~a job (~a): ~a\n"
       (herbie-command-command cmd)
       job-id
       (test-name (herbie-command-test cmd)))
  (begin0 (run-herbie (herbie-command-command cmd)
                      (herbie-command-test cmd)
                      #:seed (herbie-command-seed cmd)
                      #:pcontext (herbie-command-pcontext cmd)
                      #:profile? (herbie-command-profile? cmd)
                      #:timeline? (herbie-command-timeline? cmd))
    (log "Completed ~a job (~a)\n" (herbie-command-command cmd) job-id)))

;; JSON conversion stuff

(define (get-json-converter command)
  (match (herbie-command-command command)
    ['alternatives make-alternatives-result]
    ['cost make-cost-result]
    ['errors make-error-result]
    ['explanations make-explanation-result]
    ['improve make-alternatives-result]
    ['local-error make-local-error-result]
    ['sample make-sample-result]
    [_ (error 'compute-result "unknown command ~a" command)]))

(define (make-explanation-result herbie-result job-id)
  (hasheq 'explanation (job-result-backend herbie-result)))

(define (make-local-error-result herbie-result job-id)
  (hasheq 'tree (job-result-backend herbie-result)))

(define (make-sample-result herbie-result job-id)
  (define test (job-result-test herbie-result))
  (define pctx (job-result-backend herbie-result))
  (define repr (context-repr (test-context test)))
  (hasheq 'points (pcontext->json pctx repr)))

(define (make-cost-result herbie-result job-id)
  (hasheq 'cost (job-result-backend herbie-result)))

(define (make-error-result herbie-result job-id)
  (define test (job-result-test herbie-result))
  (define errs
    (for/list ([(pt err) (in-dict (job-result-backend herbie-result))])
      (list (for/list ([val (in-vector pt)]
                       [repr (in-list (context-var-reprs (test-context test)))])
              (value->json val repr))
            (format-bits (ulps->bits err)))))
  (hasheq 'points errs))

(define (make-alternatives-result herbie-result job-id)
  (define test (job-result-test herbie-result))
  (define backend (job-result-backend herbie-result))
  (define timeline (job-result-timeline herbie-result))
  (define profile (job-result-profile herbie-result))

  (define ctx (test-context test))
  (define-values (altns pcontext)
    (cond
      [(equal? (job-result-status herbie-result) 'success)
       (define altns (map alt-analysis-alt (improve-result-end backend)))
       (define pcontext (improve-result-pcontext backend))
       (values altns pcontext)]
      [else (values '() #f)]))

  (define errcache
    (cond
      [(equal? (job-result-status herbie-result) 'success)
       (define all-alts
         (map alt-analysis-alt
              (append (list (improve-result-start backend))
                      (improve-result-target backend)
                      (improve-result-end backend))))
       (define exprs (append-map collect-expressions all-alts))
       (make-hash (map cons exprs (batch-errors exprs pcontext ctx)))]
      [else #f]))

  (define test-fpcore
    (alt->fpcore test (make-alt-preprocessing (test-input test) (test-preprocess test))))

  (define fpcores
    (if (equal? (job-result-status herbie-result) 'success)
        (for/list ([altn (in-list altns)])
          (~s (alt->fpcore test altn)))
        (list (~s test-fpcore))))

  (define backend-hash
    (match (job-result-status herbie-result)
      ['success (backend-improve-result-hash-table backend test errcache)]
      ['timeout #f]
      ['failure (exception->datum backend)]))

  (define histories
    (for/list ([altn (in-list altns)]
               [analysis (if (hash? backend-hash)
                             (hash-ref backend-hash 'end)
                             '())])
      (define history (read (open-input-string (hash-ref analysis 'history))))
      (define block `(div ([id "history"]) (ol ,@history)))
      (call-with-output-string (curry write-xexpr block))))

  (define derivations
    (for/list ([altn (in-list altns)])
      (render-json altn pcontext (test-context test) errcache)))

  (hasheq 'test
          (~s test-fpcore)
          'timeline
          timeline
          'profile
          profile
          'alternatives ; FIXME: currently used by Odyssey but should maybe be in 'backend?
          fpcores
          'histories ; FIXME: currently used by Odyssey but should switch to 'derivations below
          histories
          'derivations
          derivations
          'backend
          backend-hash))

(define (backend-improve-result-hash-table backend test errcache)
  (define repr (context-repr (test-context test)))
  (define pcontext (improve-result-pcontext backend))
  (hasheq 'preprocessing
          (map ~s (improve-result-preprocess backend))
          'pcontext
          (pcontext->json pcontext repr)
          'start
          (analysis->json (improve-result-start backend) pcontext test errcache)
          'target
          (map (curryr analysis->json pcontext test errcache) (improve-result-target backend))
          'end
          (map (curryr analysis->json pcontext test errcache) (improve-result-end backend))))

(define (analysis->json analysis pcontext test errcache)
  (define repr (context-repr (test-context test)))
  (match-define (alt-analysis alt test-errors) analysis)
  (define cost (alt-cost alt repr))

  (define history (render-history alt pcontext (test-context test) errcache))

  (define vars (test-vars test))
  (define splitpoints
    (for/list ([var (in-list vars)])
      (if (equal? var (regime-var alt))
          (for/list ([val (regime-splitpoints alt)])
            (real->ordinal (repr->real val repr) repr))
          '())))

  (hasheq 'expr
          (~s (alt-expr alt))
          'history
          (~s history)
          'errors
          test-errors
          'cost
          cost
          'splitpoints
          splitpoints))

(define (alt->fpcore test altn)
  `(FPCore ,@(filter identity (list (test-identifier test)))
           ,(for/list ([var (in-list (test-vars test))])
              (define repr (dict-ref (test-var-repr-names test) var))
              (if (equal? repr (test-output-repr-name test))
                  var
                  (list '! ':precision repr var)))
           :name
           ,(test-name test)
           :precision
           ,(test-output-repr-name test)
           ,@(if (eq? (test-pre test) '(TRUE))
                 '()
                 `(:pre ,(prog->fpcore (test-pre test) (test-context test))))
           ,@(if (equal? (test-spec test) empty)
                 '()
                 `(:herbie-spec ,(prog->fpcore (test-spec test) (test-context test))))
           ,@(if (equal? (alt-preprocessing altn) empty)
                 '()
                 `(:herbie-preprocess ,(alt-preprocessing altn)))
           ,@(if (equal? (test-expected test) #t)
                 '()
                 `(:herbie-expected ,(test-expected test)))
           ,@(apply append
                    (for/list ([(target enabled?) (in-dict (test-output test))]
                               #:when enabled?)
                      `(:alt ,target)))
           ,(prog->fpcore (alt-expr altn) (test-context test))))
